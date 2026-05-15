;;; emacs-minibuffer.el --- Emacs C minibuffer.c port on top of nelisp-emacs-compat  -*- lexical-binding: t; -*-

;; Phase 1 module 5/6 per nelisp-emacs Doc 01 (LOCKED-2026-04-25-v2).
;; Layer: nelisp-emacs (Layer 2 extension on top of NeLisp).
;; Namespace: `emacs-minibuffer-' so loading inside a host Emacs does
;; NOT shadow `read-from-minibuffer', `y-or-n-p', `completing-read', etc.
;;
;; Foundation contracts:
;;   - `nelisp-emacs-compat' (T39 SHIPPED) provides the buffer struct
;;     (`nelisp-ec-buffer'), point/insert/delete-region/buffer-substring
;;     primitives.  We never `setf' its struct slots from this module
;;     except to read them (= treat as opaque).
;;   - `emacs-buffer'  (T119 SHIPPED) provides extended buffer state
;;     (text-properties / undo / modification tick).
;;   - `emacs-window'  (T135 SHIPPED) provides the window tree;
;;     the minibuffer window is a *dedicated* leaf created on demand.
;;   - `emacs-keymap'  (T136 SHIPPED) provides the keymap chain and
;;     the read-event plug-in (`emacs-keymap--read-event-fn').
;;
;; API surface (~18 public APIs across 5 categories):
;;
;;   A. core readers  (4 APIs)
;;      read-from-minibuffer / read-string / read-no-blanks-input
;;      read-key
;;
;;   B. typed readers  (5 APIs)
;;      read-buffer / read-file-name / read-directory-name
;;      read-passwd / read-number
;;
;;   C. confirmation  (2 APIs)
;;      y-or-n-p / yes-or-no-p
;;
;;   D. completion  (5 APIs + 2 special vars)
;;      completing-read / completing-read-default
;;      try-completion / all-completions / test-completion
;;      minibuffer-completion-table (var) /
;;      minibuffer-completion-confirm (var)
;;
;;   E. minibuffer state / control  (8 APIs + 3 special vars)
;;      minibufferp / active-minibuffer-window
;;      minibuffer-window / minibuffer-prompt / minibuffer-contents
;;      exit-minibuffer / abort-recursive-edit / minibuffer-message
;;      minibuffer-prompt-end / minibuffer-prompt-width
;;      minibuffer-history (var) / minibuffer-default (var) /
;;      minibuffer-message-timeout (var)
;;
;; Plug-in pattern (= matches emacs-keymap T136):
;;   `emacs-minibuffer--read-fn' is a defcustom of (PROMPT INITIAL DEFAULT
;;   HIST KEYMAP READ) -> string.  When nil (default) the built-in line
;;   reader is used; ERT can plug in a deterministic one.  In addition,
;;   `emacs-minibuffer--y-or-n-fn' / `emacs-minibuffer--key-fn' allow
;;   the confirmation and single-key readers to be steered the same way.
;;
;; Non-goals (deferred per task spec):
;;   - completion framework (= MVP supports list/obarray/function tables
;;     with try-completion, but no ido / helm / vertico)
;;   - history full integration (= MVP is in-memory list per HIST symbol)
;;   - real C-g abort interrupt path (= signal 'quit is what we use)
;;   - resize-mini-windows / pixel-precise prompt rendering

;;; Code:

(require 'cl-lib)
(require 'nelisp-emacs-compat)
(require 'emacs-buffer)
(require 'emacs-window)
(require 'emacs-keymap)

;;; Errors

(define-error 'emacs-minibuffer-error
  "emacs-minibuffer error")
(define-error 'emacs-minibuffer-no-input
  "No input available for the minibuffer reader" 'emacs-minibuffer-error)
(define-error 'emacs-minibuffer-bad-default
  "DEFAULT must be a string, list of strings, or nil"
  'emacs-minibuffer-error)

;;; Customization / plug-in slots

(defcustom emacs-minibuffer-prompt-properties nil
  "PLIST of text properties applied to minibuffer prompts.
MVP: stored verbatim, not actually rendered.  Phase 9c/redisplay will
honour them.  Default nil keeps the inserted prompt plain text."
  :type '(plist :key-type symbol :value-type sexp)
  :group 'emacs-minibuffer)

(defcustom emacs-minibuffer-default-history-symbol 'emacs-minibuffer-history
  "Symbol used as the default history list when the caller passes no HIST."
  :type 'symbol
  :group 'emacs-minibuffer)

(defcustom emacs-minibuffer-message-timeout 2
  "Seconds the `minibuffer-message' overlay would persist (informational only)."
  :type 'number
  :group 'emacs-minibuffer)

(defcustom emacs-minibuffer-completion-ignore-case nil
  "If non-nil, completion matching is case-insensitive.
Mirrors host Emacs `completion-ignore-case'.  Honoured by
`emacs-minibuffer-try-completion', `emacs-minibuffer-all-completions',
`emacs-minibuffer-test-completion', and the shared internal helpers."
  :type 'boolean
  :group 'emacs-minibuffer)

(defvar emacs-minibuffer--read-fn nil
  "Function used to read a line from the minibuffer.
Signature: (PROMPT INITIAL DEFAULT HIST KEYMAP READ) -> STRING.
nil = use the built-in line reader (drains
`emacs-minibuffer--input-queue').  ERT plugs in deterministic fns.")

(defvar emacs-minibuffer--key-fn nil
  "Function used by `emacs-minibuffer-read-key'.
Signature: (PROMPT) -> EVENT (= integer or symbol).  nil = use the
built-in reader (drains `emacs-minibuffer--input-queue').")

(defvar emacs-minibuffer--y-or-n-fn nil
  "Function used by `emacs-minibuffer-y-or-n-p' / `yes-or-no-p'.
Signature: (PROMPT) -> BOOLEAN.  nil = the built-in reader matches
\"y\" / \"yes\" against the queue.")

;;; Module state

(defvar emacs-minibuffer--depth 0
  "Current nesting depth (= 0 outside any minibuffer read).")

(defvar emacs-minibuffer--buffers nil
  "Stack of `nelisp-ec-buffer' objects, one per active read.
The CAR is the topmost (= currently active) minibuffer buffer.")

(defvar emacs-minibuffer--prompts nil
  "Stack of prompt strings (PARALLEL to `emacs-minibuffer--buffers').")

(defvar emacs-minibuffer--prompt-ends nil
  "Stack of prompt-end positions (PARALLEL stacks).
Each value is the buffer position immediately after the prompt text.")

(defvar emacs-minibuffer--window nil
  "Dedicated minibuffer leaf window, allocated lazily on first read.")

(defvar emacs-minibuffer--saved-window nil
  "Window that was selected when the read started (restored on exit).")

(defvar emacs-minibuffer--input-queue nil
  "FIFO of pending lines / events used by the built-in reader.
Each element is either a string (= a complete input line) or one of the
symbols :abort / :exit consumed by the typed-reader entry points.
ERT can prefill this list with `emacs-minibuffer-feed-input'.")

(defvar emacs-minibuffer-history nil
  "Default history list (= what `emacs-minibuffer-history-symbol' resolves to).")

(defvar emacs-minibuffer-default nil
  "Most recent DEFAULT value passed to a reader.  Diagnostic only.")

(defvar minibuffer-completion-table nil
  "Completion table for the active read, or nil.
Set by `emacs-minibuffer-completing-read', restored on exit.")

(defvar minibuffer-completion-confirm nil
  "When non-nil, `emacs-minibuffer-completing-read' insists on a hit.")

;;; Internal helpers

(defun emacs-minibuffer--ensure-window ()
  "Ensure the dedicated minibuffer window exists.
Phase 1 — we just stash a leaf created via `emacs-window-split-window'
on the implicit root.  When the host environment is not running with
`emacs-window' tree initialized this returns the symbol :stub which
satisfies `windowp'-checks via `emacs-minibuffer-active-minibuffer-window'."
  (unless emacs-minibuffer--window
    (setq emacs-minibuffer--window
          (condition-case _err
              (let* ((root (emacs-window-selected-window))
                     (mini (emacs-window-split-window root 1 'below)))
                ;; Mark with a parameter so `minibufferp' on its buffer works.
                (emacs-window-set-window-parameter mini 'minibuffer t)
                mini)
            (error :stub))))
  emacs-minibuffer--window)

(defun emacs-minibuffer--allocate-buffer ()
  "Create + register a fresh minibuffer buffer (= nelisp-ec-buffer)."
  (let* ((depth (1+ emacs-minibuffer--depth))
         (name  (format " *Minibuf-%d*" depth))
         (buf   (nelisp-ec-generate-new-buffer name)))
    buf))

(defun emacs-minibuffer--push (buf prompt prompt-end)
  (push buf        emacs-minibuffer--buffers)
  (push prompt     emacs-minibuffer--prompts)
  (push prompt-end emacs-minibuffer--prompt-ends)
  (cl-incf emacs-minibuffer--depth))

(defun emacs-minibuffer--pop ()
  "Drop the topmost frame.  Returns the buffer that was popped."
  (let ((buf (pop emacs-minibuffer--buffers)))
    (pop emacs-minibuffer--prompts)
    (pop emacs-minibuffer--prompt-ends)
    (cl-decf emacs-minibuffer--depth)
    (when buf
      (ignore-errors (nelisp-ec-kill-buffer buf)))
    buf))

(defun emacs-minibuffer--insert-prompt (buf prompt)
  "Insert PROMPT into BUF, return the prompt-end position."
  (nelisp-ec-with-current-buffer buf
    (nelisp-ec-insert prompt)
    ;; If prompt-properties non-nil, attach them via emacs-buffer.
    (when emacs-minibuffer-prompt-properties
      (let ((plist emacs-minibuffer-prompt-properties))
        (while plist
          (emacs-buffer-put-text-property
           1 (1+ (length prompt)) (car plist) (cadr plist) buf)
          (setq plist (cddr plist)))))
    (nelisp-ec-point)))

(defun emacs-minibuffer--insert-initial (buf initial)
  "Insert INITIAL (string or (STRING . POS) cons) into BUF."
  (when initial
    (let ((s (cond
              ((stringp initial) initial)
              ((and (consp initial) (stringp (car initial))) (car initial))
              (t (signal 'emacs-minibuffer-error (list initial))))))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert s)))))

(defun emacs-minibuffer--current-buffer ()
  "Return the topmost active minibuffer buffer, or nil."
  (car emacs-minibuffer--buffers))

(defun emacs-minibuffer--current-prompt-end ()
  "Return the prompt-end of the topmost active minibuffer, or nil."
  (car emacs-minibuffer--prompt-ends))

(defun emacs-minibuffer--default-as-string (default)
  "Normalize DEFAULT into a single string for prompt-display purposes.
DEFAULT may be nil, a string, or a list of strings — the first element
of a list is what Emacs prints in the prompt."
  (cond
   ((null default) nil)
   ((stringp default) default)
   ((and (listp default) (cl-every #'stringp default)) (car default))
   (t (signal 'emacs-minibuffer-bad-default (list default)))))

(defun emacs-minibuffer--push-history (hist value)
  "Push VALUE onto the history list represented by HIST.
HIST is a symbol or (SYMBOL . OFFSET) cons; we treat the offset as
informational.  Empty VALUE strings are NOT added (= matches Emacs)."
  (let ((sym (cond
              ((null hist) emacs-minibuffer-default-history-symbol)
              ((symbolp hist) hist)
              ((and (consp hist) (symbolp (car hist))) (car hist))
              (t emacs-minibuffer-default-history-symbol))))
    (when (and (stringp value) (not (string-empty-p value)))
      (unless (boundp sym) (set sym nil))
      (set sym (cons value (symbol-value sym))))))

(defun emacs-minibuffer--read-line-default
    (prompt initial _default _hist _keymap _read)
  "Built-in line reader — pops one entry from the input queue.
Returns the line as a string.  Accepts optional `:abort' / `:exit'
sentinels for control-flow tests."
  (ignore prompt initial)
  (when (null emacs-minibuffer--input-queue)
    (signal 'emacs-minibuffer-no-input (list prompt)))
  (let ((next (pop emacs-minibuffer--input-queue)))
    (cond
     ((eq next :abort) (signal 'quit nil))
     ((eq next :exit)  "")
     ((stringp next)   next)
     (t (signal 'emacs-minibuffer-error (list "unrecognized input" next))))))

(defun emacs-minibuffer--read-line (prompt initial default hist keymap read)
  (let ((fn (or emacs-minibuffer--read-fn
                #'emacs-minibuffer--read-line-default)))
    (funcall fn prompt initial default hist keymap read)))

(defun emacs-minibuffer--with-frame (prompt initial body)
  "Run BODY (a thunk) inside a fresh minibuffer frame.
Pushes a new buffer + prompt + prompt-end stack entry, ensures the
window exists, calls the thunk, and pops the frame on normal *or*
abnormal exit.  Returns the BODY's value."
  (emacs-minibuffer--ensure-window)
  (let* ((buf       (emacs-minibuffer--allocate-buffer))
         (prompt-end (emacs-minibuffer--insert-prompt buf prompt)))
    (emacs-minibuffer--insert-initial buf initial)
    (emacs-minibuffer--push buf prompt prompt-end)
    (unwind-protect
        (funcall body)
      (emacs-minibuffer--pop))))

;;; A. core readers

;;;###autoload
(defun emacs-minibuffer-read-from-minibuffer
    (prompt &optional initial keymap read hist default _inherit-input-method)
  "Read a string from the user.
PROMPT is a string (mandatory).  INITIAL, if non-nil, is the initial
content (string or (STRING . POS)).  KEYMAP, READ, HIST, DEFAULT have
the same shape as the standard Emacs API; DEFAULT is normalized for
display via `emacs-minibuffer--default-as-string'.

If READ is non-nil, the resulting string is `read'-back into a Lisp
object before returning (matches Emacs precedent)."
  (unless (stringp prompt)
    (signal 'wrong-type-argument (list 'stringp prompt)))
  (let ((default-str (emacs-minibuffer--default-as-string default))
        (saved-table minibuffer-completion-table)
        (saved-confirm minibuffer-completion-confirm))
    (setq emacs-minibuffer-default default)
    (unwind-protect
        (emacs-minibuffer--with-frame
         prompt initial
         (lambda ()
           (let* ((line (emacs-minibuffer--read-line
                         prompt initial default hist keymap read))
                  (value (cond
                          ((and (string-empty-p line) default-str)
                           default-str)
                          (t line))))
             (emacs-minibuffer--push-history hist value)
             (if read (read value) value))))
      (setq minibuffer-completion-table saved-table
            minibuffer-completion-confirm saved-confirm))))

;;;###autoload
(defun emacs-minibuffer-read-string (prompt &optional initial hist default
                                            _inherit-input-method)
  "Convenience wrapper around `emacs-minibuffer-read-from-minibuffer'
that always returns a string and never `read's it back."
  (emacs-minibuffer-read-from-minibuffer
   prompt initial nil nil hist default))

;;;###autoload
(defun emacs-minibuffer-read-no-blanks-input (prompt &optional initial
                                                     _inherit-input-method)
  "Read a string with no whitespace allowed.
Whitespace is rejected by signalling `emacs-minibuffer-error'."
  (let ((s (emacs-minibuffer-read-from-minibuffer prompt initial)))
    (when (string-match-p "[ \t\n\r]" s)
      (signal 'emacs-minibuffer-error
              (list "Whitespace not allowed in input" s)))
    s))

;;;###autoload
(defun emacs-minibuffer-read-key (&optional prompt)
  "Read one event (= integer or symbol) and return it.
Routes through `emacs-minibuffer--key-fn' if non-nil; otherwise drains
`emacs-minibuffer--input-queue'.  Strings in the queue are interpreted
as a sequence of characters and the first char is returned (with the
remainder pushed back as one string)."
  (let ((fn emacs-minibuffer--key-fn))
    (cond
     (fn (funcall fn prompt))
     (t
      (when (null emacs-minibuffer--input-queue)
        (signal 'emacs-minibuffer-no-input (list prompt)))
      (let ((next (pop emacs-minibuffer--input-queue)))
        (cond
         ((eq next :abort) (signal 'quit nil))
         ((integerp next) next)
         ((symbolp next)  next)
         ((and (stringp next) (> (length next) 0))
          (let ((ev (aref next 0))
                (rest (substring next 1)))
            (when (> (length rest) 0)
              (push rest emacs-minibuffer--input-queue))
            ev))
         (t (signal 'emacs-minibuffer-error
                    (list "unrecognized event" next)))))))))

;;; B. typed readers

;;;###autoload
(defun emacs-minibuffer-read-buffer (prompt &optional default _require-match
                                            _predicate)
  "Read a buffer name.  DEFAULT, if non-nil, supplies the default value.
This is a thin wrapper — completion is offered via `nelisp-ec--buffers'
when `require-match' is non-nil (Phase 1 best-effort)."
  (let* ((default-str
          (cond
           ((null default) nil)
           ((stringp default) default)
           ((nelisp-ec-buffer-p default) (nelisp-ec-buffer-name default))
           (t (signal 'emacs-minibuffer-bad-default (list default)))))
         (table (mapcar #'car nelisp-ec--buffers))
         (minibuffer-completion-table table))
    (let ((s (emacs-minibuffer-read-from-minibuffer
              prompt nil nil nil nil default-str)))
      s)))

;;;###autoload
(defun emacs-minibuffer-read-file-name (prompt &optional dir default
                                               _mustmatch _initial _predicate)
  "Read a file name with prompt PROMPT.
DIR (string or nil) provides the implicit base directory; DEFAULT is
the fallback if the user enters an empty string.  MUSTMATCH / PREDICATE
are accepted for arity compatibility but are no-ops in Phase 1."
  (let* ((default-str
          (cond
           ((null default) nil)
           ((stringp default) default)
           (t (signal 'emacs-minibuffer-bad-default (list default)))))
         (s (emacs-minibuffer-read-from-minibuffer
             prompt nil nil nil nil default-str)))
    (if (and dir (not (string-match-p "\\`/" s)))
        (concat (file-name-as-directory dir) s)
      s)))

;;;###autoload
(defun emacs-minibuffer-read-directory-name (prompt &optional dir default
                                                    _mustmatch _initial)
  "Read a directory name.
Same shape as `emacs-minibuffer-read-file-name', but the returned path
always ends in a slash (= matches Emacs)."
  (let ((s (emacs-minibuffer-read-file-name prompt dir default)))
    (file-name-as-directory s)))

;;;###autoload
(defun emacs-minibuffer-read-passwd (prompt &optional confirm default)
  "Read a password (= a string) without echoing.
If CONFIRM is non-nil, the user is asked twice and the two entries
must match; mismatch signals `emacs-minibuffer-error'.  DEFAULT, when
non-nil, is returned for an empty input."
  (let* ((s1 (emacs-minibuffer-read-from-minibuffer prompt nil nil nil nil
                                                    default))
         (s2 (and confirm
                  (emacs-minibuffer-read-from-minibuffer
                   (concat prompt " (confirm) ") nil nil nil nil default))))
    (when (and confirm (not (string-equal s1 s2)))
      (signal 'emacs-minibuffer-error (list "Passwords do not match")))
    (cond
     ((and (string-empty-p s1) default) default)
     (t s1))))

;;;###autoload
(defun emacs-minibuffer-read-number (prompt &optional default _hist)
  "Read a number from the minibuffer.
DEFAULT, if non-nil, is returned for an empty input.  Non-numeric input
re-prompts up to `emacs-minibuffer--read-number-max-tries' times and
then signals `emacs-minibuffer-error'."
  (let ((tries 5)
        result)
    (catch 'done
      (while (> tries 0)
        (let* ((default-str (cond
                             ((numberp default) (number-to-string default))
                             ((stringp default) default)
                             (t nil)))
               (s (emacs-minibuffer-read-from-minibuffer
                   prompt nil nil nil nil default-str)))
          (cond
           ((and (string-empty-p s) (numberp default))
            (setq result default) (throw 'done nil))
           ((string-match-p "\\`-?[0-9]+\\(?:\\.[0-9]+\\)?\\'" s)
            (setq result (string-to-number s)) (throw 'done nil))
           (t
            (cl-decf tries)))))
      (signal 'emacs-minibuffer-error (list "Not a number" prompt)))
    result))

;;; C. confirmation

(defun emacs-minibuffer--y-or-n-default (prompt)
  "Built-in y-or-n-p reader: pops one entry from the input queue.
Accepts string \"y\"/\"yes\" or symbol `y' / `yes' as t; \"n\"/\"no\" or
symbol `n' / `no' as nil.  Anything else signals
`emacs-minibuffer-error'."
  (when (null emacs-minibuffer--input-queue)
    (signal 'emacs-minibuffer-no-input (list prompt)))
  (let* ((next (pop emacs-minibuffer--input-queue))
         (canon (cond
                 ((eq next :abort) (signal 'quit nil))
                 ((symbolp next) (symbol-name next))
                 ((stringp next) next)
                 (t (signal 'emacs-minibuffer-error
                            (list "unrecognized confirmation" next))))))
    (cond
     ((member (downcase canon) '("y" "yes")) t)
     ((member (downcase canon) '("n" "no")) nil)
     (t (signal 'emacs-minibuffer-error
                (list "Bad confirmation answer" canon))))))

;;;###autoload
(defun emacs-minibuffer-y-or-n-p (prompt)
  "Ask user a y-or-n question.  Return t or nil.
Routes through `emacs-minibuffer--y-or-n-fn' if non-nil."
  (let ((fn (or emacs-minibuffer--y-or-n-fn
                #'emacs-minibuffer--y-or-n-default)))
    (funcall fn prompt)))

;;;###autoload
(defun emacs-minibuffer-yes-or-no-p (prompt)
  "Ask user a yes/no question (full word).
Same plug-in as `emacs-minibuffer-y-or-n-p'; built-in reader requires
\"yes\" or \"no\" verbatim (case-insensitive)."
  (let ((fn emacs-minibuffer--y-or-n-fn))
    (cond
     (fn (funcall fn prompt))
     (t
      (when (null emacs-minibuffer--input-queue)
        (signal 'emacs-minibuffer-no-input (list prompt)))
      (let* ((next (pop emacs-minibuffer--input-queue))
             (canon (cond
                     ((eq next :abort) (signal 'quit nil))
                     ((symbolp next) (symbol-name next))
                     ((stringp next) next)
                     (t (signal 'emacs-minibuffer-error
                                (list "unrecognized confirmation" next))))))
        (cond
         ((string-equal (downcase canon) "yes") t)
         ((string-equal (downcase canon) "no") nil)
         (t (signal 'emacs-minibuffer-error
                    (list "Bad yes/no answer" canon)))))))))

;;; D. completion

(defun emacs-minibuffer--collection->list (collection)
  "Return COLLECTION as a list of strings.
Accepts list of strings, list of (STRING . _) pairs, an obarray
(=vector of symbols), or a function (= called with \"\" and
predicate nil to enumerate)."
  (cond
   ((null collection) nil)
   ((vectorp collection)
    (let (acc)
      (mapatoms (lambda (s) (push (symbol-name s) acc)) collection)
      acc))
   ((functionp collection)
    (let ((res (funcall collection "" nil t)))
      (cond
       ((listp res)
        (mapcar (lambda (e) (if (consp e) (car e) e)) res))
       (t nil))))
   ((listp collection)
    (mapcar (lambda (e) (cond ((stringp e) e)
                              ((consp e) (car e))
                              ((symbolp e) (symbol-name e))
                              (t (format "%S" e))))
            collection))
   (t (signal 'emacs-minibuffer-error
              (list "Bad collection" collection)))))

(defun emacs-minibuffer--prefix-match-p (prefix candidate)
  "Return non-nil iff CANDIDATE begins with PREFIX.
Honours `emacs-minibuffer-completion-ignore-case'."
  (let ((plen (length prefix)))
    (and (>= (length candidate) plen)
         (if emacs-minibuffer-completion-ignore-case
             (eq t (compare-strings prefix 0 plen candidate 0 plen t))
           (string-prefix-p prefix candidate)))))

(defun emacs-minibuffer--string-equal-cf (a b)
  "Case-aware string equality honouring `emacs-minibuffer-completion-ignore-case'."
  (if emacs-minibuffer-completion-ignore-case
      (eq t (compare-strings a 0 nil b 0 nil t))
    (string-equal a b)))

(defun emacs-minibuffer--filter-candidates (string table predicate)
  "Return entries of TABLE (list of strings) starting with STRING.
PREDICATE, when non-nil, further filters the result."
  (let ((cands (cl-remove-if-not
                (lambda (c) (emacs-minibuffer--prefix-match-p string c))
                table)))
    (if predicate
        (cl-remove-if-not predicate cands)
      cands)))

(defun emacs-minibuffer--common-prefix (cands)
  "Return the longest common prefix of CANDS (non-empty list of strings).
Case-fold honours `emacs-minibuffer-completion-ignore-case'."
  (let ((prefix (car cands)))
    (dolist (c (cdr cands))
      (let ((i 0)
            (lim (min (length prefix) (length c))))
        (while (and (< i lim)
                    (if emacs-minibuffer-completion-ignore-case
                        (eq t (compare-strings prefix i (1+ i)
                                               c i (1+ i) t))
                      (eq (aref prefix i) (aref c i))))
          (cl-incf i))
        (setq prefix (substring prefix 0 i))))
    prefix))

(defun emacs-minibuffer--try-completion (string table &optional predicate)
  "Return the longest common prefix in TABLE that begins with STRING,
t if STRING is itself a unique exact match, or nil when no candidate matches.
TABLE must already be a list of strings.  PREDICATE, when non-nil,
filters candidates after the prefix match.  Honours
`emacs-minibuffer-completion-ignore-case'."
  (let ((cands (emacs-minibuffer--filter-candidates string table predicate)))
    (cond
     ((null cands) nil)
     ((and (= (length cands) 1)
           (emacs-minibuffer--string-equal-cf (car cands) string))
      t)
     (t (emacs-minibuffer--common-prefix cands)))))

;;;###autoload
(defun emacs-minibuffer-completing-read
    (prompt collection &optional predicate require-match initial-input
            hist def _inherit-input-method)
  "Read a string in the minibuffer with completion.
Phase 1 — supports list / obarray / function COLLECTION.  PREDICATE is
applied as a filter when non-nil.  REQUIRE-MATCH non-nil insists the
final string be in the (filtered) table; otherwise free input is OK.
INITIAL-INPUT, HIST, DEF behave as in `read-from-minibuffer'."
  (let* ((table (emacs-minibuffer--collection->list collection))
         (table (if predicate
                    (cl-remove-if-not predicate table)
                  table))
         (default-str (emacs-minibuffer--default-as-string def))
         (minibuffer-completion-table table)
         (minibuffer-completion-confirm require-match))
    (let ((s (emacs-minibuffer-read-from-minibuffer
              prompt initial-input nil nil hist default-str)))
      (when require-match
        (unless (cl-some (lambda (c) (emacs-minibuffer--string-equal-cf c s))
                         table)
          (signal 'emacs-minibuffer-error
                  (list "Match required" s))))
      s)))

;;;###autoload
(defalias 'emacs-minibuffer-completing-read-default
  #'emacs-minibuffer-completing-read
  "Alias for `emacs-minibuffer-completing-read'.
Matches the host Emacs entry-point name used by libraries that bind
`completing-read-function' explicitly.")

;;;###autoload
(defun emacs-minibuffer-try-completion (string collection &optional predicate)
  "Public Phase 1 port of `try-completion'.
COLLECTION is normalised via `emacs-minibuffer--collection->list'
(= list of strings / alist / obarray / function).  Returns the longest
common prefix of (filtered) COLLECTION entries that begin with STRING,
or t when STRING is the unique exact match, or nil when nothing matches.
Honours `emacs-minibuffer-completion-ignore-case'."
  (emacs-minibuffer--try-completion
   string
   (emacs-minibuffer--collection->list collection)
   predicate))

;;;###autoload
(defun emacs-minibuffer-all-completions (string collection &optional predicate)
  "Public Phase 1 port of `all-completions'.
Return a list of every entry in COLLECTION that begins with STRING and
satisfies PREDICATE (when non-nil).  Order follows COLLECTION traversal
order (= post-`--collection->list').  Honours
`emacs-minibuffer-completion-ignore-case'."
  (let ((table (emacs-minibuffer--collection->list collection)))
    (emacs-minibuffer--filter-candidates string table predicate)))

;;;###autoload
(defun emacs-minibuffer-test-completion (string collection &optional predicate)
  "Public Phase 1 port of `test-completion'.
Return t iff STRING is an exact element of (filtered) COLLECTION.
Honours `emacs-minibuffer-completion-ignore-case'."
  (let* ((table (emacs-minibuffer--collection->list collection))
         (table (if predicate (cl-remove-if-not predicate table) table)))
    (and (cl-some (lambda (c) (emacs-minibuffer--string-equal-cf c string))
                  table)
         t)))

;;; E. minibuffer state / control

;;;###autoload
(defun emacs-minibuffer-minibufferp (&optional buffer)
  "Return t if BUFFER is a minibuffer.
BUFFER defaults to the current `nelisp-ec' buffer.  A buffer is a
minibuffer iff it appears in the active stack OR its name matches
\"^ \\*Minibuf-\\*[0-9]+\\*\"."
  (let ((b (or buffer (nelisp-ec-current-buffer))))
    (and (nelisp-ec-buffer-p b)
         (or (memq b emacs-minibuffer--buffers)
             (let ((name (nelisp-ec-buffer-name b)))
               (and (stringp name)
                    (string-match-p "\\` \\*Minibuf-[0-9]+\\*\\'" name)))))))

;;;###autoload
(defun emacs-minibuffer-active-minibuffer-window ()
  "Return the window of the currently active minibuffer, or nil.
Returns `:stub' if `emacs-window' tree is unavailable in the host."
  (when (> emacs-minibuffer--depth 0)
    emacs-minibuffer--window))

;;;###autoload
(defun emacs-minibuffer-minibuffer-window (&optional _frame)
  "Return the (single) minibuffer window if allocated, nil otherwise.
FRAME is accepted for arity compatibility (Phase 1 = single frame)."
  emacs-minibuffer--window)

;;;###autoload
(defun emacs-minibuffer-minibuffer-prompt ()
  "Return the prompt string of the active minibuffer, or nil."
  (car emacs-minibuffer--prompts))

;;;###autoload
(defun emacs-minibuffer-minibuffer-contents (&optional include-prompt)
  "Return the user-typed portion of the active minibuffer.
With INCLUDE-PROMPT non-nil the prompt is included.  Returns the empty
string if no read is in progress."
  (let ((buf (emacs-minibuffer--current-buffer))
        (pe  (emacs-minibuffer--current-prompt-end)))
    (cond
     ((null buf) "")
     (t
      (nelisp-ec-with-current-buffer buf
        (let ((max (nelisp-ec-point-max)))
          (cond
           ((or include-prompt (null pe))
            (nelisp-ec-buffer-substring 1 max))
           ((>= pe max) "")
           (t (nelisp-ec-buffer-substring pe max)))))))))

;;;###autoload
(defun emacs-minibuffer-minibuffer-prompt-end ()
  "Return the prompt-end position of the active minibuffer, or 1 if none."
  (or (emacs-minibuffer--current-prompt-end) 1))

;;;###autoload
(defun emacs-minibuffer-minibuffer-prompt-width ()
  "Return the width (in columns) of the active prompt, or 0 if none.
Phase 1 is character-count (= no display-width / multi-byte adjust)."
  (let ((p (emacs-minibuffer-minibuffer-prompt)))
    (if (stringp p) (length p) 0)))

;;;###autoload
(defun emacs-minibuffer-exit-minibuffer ()
  "Exit the active minibuffer reader normally.
Implementation: queues a sentinel `:exit' onto the input queue, so the
nearest pending built-in reader returns the empty string.  When the
plug-in `emacs-minibuffer--read-fn' is in use this signals
`emacs-minibuffer-error' (= caller is responsible)."
  (cond
   (emacs-minibuffer--read-fn
    (signal 'emacs-minibuffer-error
            '("exit-minibuffer requires the built-in reader"
              "or a plug-in that honours :exit")))
   (t
    (push :exit emacs-minibuffer--input-queue)
    nil)))

;;;###autoload
(defun emacs-minibuffer-abort-recursive-edit ()
  "Abort the current minibuffer read with `quit'.
With the built-in reader this queues an :abort sentinel; otherwise it
signals `quit' immediately."
  (cond
   (emacs-minibuffer--read-fn
    (signal 'quit nil))
   (t
    (push :abort emacs-minibuffer--input-queue)
    nil)))

;;;###autoload
(defun emacs-minibuffer-minibuffer-message (format-string &rest args)
  "Display a transient message in the minibuffer area.
Phase 1 — emit to `*Messages*' equivalent (= `message') and return nil.
ARGS are passed through to `format'."
  (let ((msg (apply #'format format-string args)))
    (message "%s" msg)
    nil))

;;; F. ERT helpers (not part of the public Emacs API)

(defun emacs-minibuffer-feed-input (&rest entries)
  "Append ENTRIES to the back of `emacs-minibuffer--input-queue'.
Each entry is either a string (= one line / event source) or one of
the sentinels `:abort' / `:exit' / a symbol for `read-key' / an int."
  (setq emacs-minibuffer--input-queue
        (append emacs-minibuffer--input-queue entries))
  emacs-minibuffer--input-queue)

(defun emacs-minibuffer-reset ()
  "Reset all module state to a fresh world.
Test-only convenience; not part of the public Emacs API surface."
  ;; Pop all live frames so any allocated buffers get killed.
  (while emacs-minibuffer--buffers
    (emacs-minibuffer--pop))
  (setq emacs-minibuffer--depth 0
        emacs-minibuffer--buffers nil
        emacs-minibuffer--prompts nil
        emacs-minibuffer--prompt-ends nil
        emacs-minibuffer--window nil
        emacs-minibuffer--saved-window nil
        emacs-minibuffer--input-queue nil
        emacs-minibuffer--read-fn nil
        emacs-minibuffer--key-fn nil
        emacs-minibuffer--y-or-n-fn nil
        minibuffer-completion-table nil
        minibuffer-completion-confirm nil
        emacs-minibuffer-default nil
        emacs-minibuffer-history nil)
  nil)

(provide 'emacs-minibuffer)
;;; emacs-minibuffer.el ends here
