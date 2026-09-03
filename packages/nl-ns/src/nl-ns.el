;;; nl-ns.el --- Namespace boundaries as a checkable declaration -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 169 language defect #6 (no namespaces), addressed as an opt-in
;; development-time library rather than as a reader extension.
;;
;; Elisp has one obarray, so `defun' is assignment to a global name and
;; a second definition silently wins.  The usual response is to invent
;; a namespace by rewriting code -- a macro that turns `foo' into
;; `mypkg-foo' inside a block.  This package deliberately does NOT do
;; that.  Rewriting buys shorter names and pays for them in the
;; debugger, the backtrace, `describe-function', and `M-.', all of
;; which start showing a name the author never wrote.
;;
;; What actually hurts is not that names are long.  It is that
;; collisions are silent, that "this file depends on that file" is
;; nowhere stated, and that the `--' private convention is a comment
;; rather than a rule.  Those three are checkable without touching a
;; single source file, which is what this package does.
;;
;; Public API:
;;
;;   Analysis:  `nl-ns-analyse' `nl-ns-analyse-files'
;;   Checking:  `nl-ns-check' `nl-ns-check-files'
;;   Reporting: `nl-ns-report' `nl-ns-summary'
;;              `nl-ns-findings-of-kind' `nl-ns-report-max-severity'
;;   Baseline:  `nl-ns-load-baseline'
;;   Overrides: `nl-ns-declare' `nl-ns-clear-declarations'
;;   Internals worth calling: `nl-ns-file-namespace' `nl-ns-read-file'
;;
;; Findings (same plist shape as nl-check):
;;
;;   ns-partial-override      partial shim can replace a host definition
;;   ns-unsafe-shim-guard     host shadow behind a non-fboundp guard
;;   ns-file-shadows-library  file basename masks a host library
;;   ns-collision             symbol defined in more than one file
;;   ns-collision-divergent   collision whose definitions differ
;;   ns-prefix-violation      definition outside its file's namespace
;;   ns-private-escape        another file's `--' name is referenced
;;   ns-undeclared-dependency cross-file reference with no `require'
;;   ns-shadows-host          tree definition also exists in host baseline
;;   ns-quoted-member         member name is literal inside `nl-ns-in'
;;   ns-unreadable            the file could not be read
;;
;; Zero configuration by design.  A file's namespace is inferred as the
;; longest hyphen-boundary prefix shared by a majority of the names it
;; defines, so a file that is deliberately global (a stdlib prelude
;; defining `car', `princ', ...) has no dominant prefix and opts itself
;; out of the prefix check.  `nl-ns-declare' overrides the inference
;; where it guesses wrong.
;;
;; Host-shadow findings are gated by a checked-in baseline, generated on
;; a real Emacs and read back as plain data.  With no baseline, nl-ns
;; emits nothing about host/runtime collisions; no baseline must never
;; mean guesses.
;;
;; Nothing here runs at load time and nothing depends on this package,
;; the same one-way rule `nl-check' follows (Doc 168 section 4.1).
;; Analysis functions take already-read forms so they work on the
;; standalone; only the `*-files' wrappers touch the filesystem.

;;; Code:

(require 'nl-prelude)

;;;; Declarations -----------------------------------------------------

(defvar nl-ns--declared (make-hash-table :test 'equal)
  "Map of FILE (string) -> namespace prefix string, overriding inference.")

(defconst nl-ns-definition-heads
  '(defun defmacro defsubst defalias defvar defconst defcustom
     define-error define-minor-mode define-derived-mode cl-defun
     cl-defmacro cl-defstruct cl-defgeneric cl-defmethod)
  "Heads whose second element names something this pass tracks.")

(defconst nl-ns-variable-definition-heads
  '(defvar defconst defcustom)
  "Definition heads that bind variables rather than functions.")

(defconst nl-ns-partial-markers
  '("does not recognise" "does not recognize" "does not handle"
    "only handles" "returns nil" "not supported" "unsupported"
    "approximation" "simplified" "minimal" "partial" "subset"
    "stub" "no-op" "todo" "fixme")
  "Case-insensitive markers that admit a partial implementation.")

(defconst nl-ns-finding-severities
  '((ns-partial-override . 1)
    (ns-unsafe-shim-guard . 2)
    (ns-file-shadows-library . 3)
    (ns-collision-divergent . 4)
    (ns-private-escape . 5)
    (ns-prefix-violation . 5)
    (ns-undeclared-dependency . 5)
    (ns-collision . 6)
    (ns-shadows-host . 6)
    (ns-quoted-member . 6)
    (ns-unreadable . 6))
  "Explicit severity ranking for nl-ns findings.")

(defun nl-ns-declare (file prefix)
  "Declare that definitions in FILE belong to namespace PREFIX.
PREFIX is the literal string every name in FILE should start with,
for example \"nl-safe-\".  Overrides `nl-ns-file-namespace' inference."
  (unless (stringp file)
    (error "nl-ns-declare: FILE must be a string, got %S" file))
  (unless (stringp prefix)
    (error "nl-ns-declare: PREFIX must be a string, got %S" prefix))
  (puthash file prefix nl-ns--declared)
  prefix)

(defun nl-ns-clear-declarations ()
  "Forget every `nl-ns-declare' override."
  (clrhash nl-ns--declared)
  nil)

;;;; Reading -----------------------------------------------------------

(defun nl-ns--string-blank-p (string)
  "Return non-nil when STRING is nil or only whitespace."
  (or (null string)
      (string= string "")
      (not (string-match "[^ \t\r\n]" string))))

(defun nl-ns--trim-string (string)
  "Return STRING without leading or trailing ASCII whitespace."
  (if (null string)
      nil
    (let ((start 0)
          (end (length string)))
      (while (and (< start end)
                  (memq (aref string start) '(?\s ?\t ?\r ?\n)))
        (setq start (1+ start)))
      (while (and (< start end)
                  (memq (aref string (1- end)) '(?\s ?\t ?\r ?\n)))
        (setq end (1- end)))
      (substring string start end))))

(defun nl-ns--basename (path)
  "Return PATH's final component."
  (let ((i (1- (length path))))
    (while (and (>= i 0)
                (not (memq (aref path i) '(?/ ?\\))))
      (setq i (1- i)))
    (substring path (1+ i))))

(defun nl-ns--library-basename (path)
  "Return PATH's basename without a final .el suffix."
  (let ((base (nl-ns--basename path)))
    (if (and (> (length base) 3)
             (string= (substring base (- (length base) 3)) ".el"))
        (substring base 0 (- (length base) 3))
      base)))

(defun nl-ns--read-file-entry (path)
  "Return plist metadata for PATH, or `nl-ns--unreadable' on read error."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path)
        (let ((forms nil)
              (metadata nil)
              (done nil)
              (prev-end (point-min)))
          (goto-char (point-min))
          (while (not done)
            (skip-chars-forward " \t\r\n\f")
            (let* ((start (point))
                   (leading (buffer-substring-no-properties prev-end start))
                   (form (condition-case nil
                             (read (current-buffer))
                           (end-of-file 'nl-ns--eof))))
              (if (eq form 'nl-ns--eof)
                  (setq done t)
                (let ((end (point)))
                  (setq forms (cons form forms))
                  (setq metadata
                        (cons (list form
                                    :source (buffer-substring-no-properties
                                             start end)
                                    :leading leading)
                              metadata))
                  (setq prev-end end)))))
          (list :forms (nreverse forms)
                :metadata (nreverse metadata))))
    (error 'nl-ns--unreadable)))

(defun nl-ns-read-file (path)
  "Return the top-level forms of PATH, or the symbol `nl-ns--unreadable'.
Reading only; nothing from PATH is evaluated."
  (let ((entry (nl-ns--read-file-entry path)))
    (if (eq entry 'nl-ns--unreadable)
        entry
      (plist-get entry :forms))))

(defun nl-ns--metadata-for-form (form metadata)
  "Return METADATA entry for FORM from file METADATA."
  (cdr (assq form metadata)))

(defun nl-ns--forward-declaration-p (form)
  "Return non-nil when FORM declares a special variable without defining it.
`(defvar NAME)' with no value is how a file says \"this special
variable lives somewhere else, do not warn about it\".  Counting it as
a definition manufactures a collision with the file that really does
define it, and worse, calls that collision divergent -- the two forms
are of course not equal, one of them has a value.  Six such phantoms
sat at the top of this tree's list."
  (and (consp form)
       (memq (car form) '(defvar))
       (null (cdr (cdr form)))))

(defun nl-ns--defined-symbol (form)
  "Return the symbol FORM defines, or nil when FORM defines nothing."
  (and (consp form)
       (memq (car form) nl-ns-definition-heads)
       (not (nl-ns--forward-declaration-p form))
       (let ((name (car (cdr form))))
         (cond
          ((symbolp name) name)
          ((and (consp name) (eq (car name) 'quote)
                (symbolp (car (cdr name))))
           (car (cdr name)))
          (t nil)))))

(defun nl-ns--definition-host-kind (form)
  "Return `variable' or `function' for definition FORM."
  (if (memq (car form) nl-ns-variable-definition-heads)
      'variable
    'function))

(defun nl-ns--definition-docstring (form)
  "Return FORM's docstring, or nil when FORM has none."
  (let ((head (car form)))
    (cond
     ((and (memq head '(defun defmacro defsubst cl-defun cl-defmacro))
           (stringp (car (cdr (cdr (cdr form))))))
      (car (cdr (cdr (cdr form)))))
     ((and (memq head '(defalias defvar defconst defcustom))
           (stringp (car (cdr (cdr (cdr form))))))
      (car (cdr (cdr (cdr form)))))
     ((and (eq head 'define-minor-mode) (stringp (car (cdr form))))
      (car (cdr form)))
     ((and (eq head 'define-derived-mode)
           (stringp (car (cdr (cdr (cdr (cdr form)))))))
      (car (cdr (cdr (cdr (cdr form))))))
     (t nil))))

(defun nl-ns--definition-without-docstring (form)
  "Return definition FORM without its own docstring, when it has one.
Only the docstring is excluded from divergent-collision equality; all
other parts of the definition remain significant."
  (let ((head (car form)))
    (cond
     ((and (memq head '(defun defmacro defsubst cl-defun cl-defmacro))
           (stringp (car (cdr (cdr (cdr form))))))
      (append (list (car form) (car (cdr form)) (car (cdr (cdr form))))
              (cdr (cdr (cdr (cdr form))))))
     ((and (memq head '(defalias defvar defconst defcustom))
           (stringp (car (cdr (cdr (cdr form))))))
      (append (list (car form) (car (cdr form)) (car (cdr (cdr form))))
              (cdr (cdr (cdr (cdr form))))))
     ((and (eq head 'define-minor-mode) (stringp (car (cdr form))))
      (cons head (cdr (cdr form))))
     ((and (eq head 'define-derived-mode)
           (stringp (car (cdr (cdr (cdr (cdr form)))))))
      (append (list (car form) (car (cdr form)) (car (cdr (cdr form)))
                    (car (cdr (cdr (cdr form)))))
              (cdr (cdr (cdr (cdr (cdr form)))))))
     (t form))))

(defun nl-ns--quoted-feature (form)
  "Return the feature symbol in FORM's second position, or nil."
  (let ((arg (car (cdr form))))
    (cond
     ((and (consp arg) (eq (car arg) 'quote) (symbolp (car (cdr arg))))
      (car (cdr arg)))
     ((symbolp arg) arg)
     (t nil))))

(defun nl-ns--collect-symbols (form table)
  "Add every symbol occurring in FORM to hash TABLE.
The list spine is walked iteratively: files in this tree hold quoted
forms thousands of elements long, and a recursive cdr walk overflows
on them."
  (let ((tail form))
    (while (consp tail)
      (nl-ns--collect-symbols (car tail) table)
      (setq tail (cdr tail)))
    (cond
     ((symbolp tail) (when tail (puthash tail t table)))
     ((vectorp tail)
      (let ((i 0) (n (length tail)))
        (while (< i n)
          (nl-ns--collect-symbols (aref tail i) table)
          (setq i (1+ i)))))))
  table)

(defun nl-ns--ns-members (form)
  "Return the `:members' list declared by an `nl-ns-define' FORM."
  (let ((properties (cdr (cdr form)))
        (members nil))
    (while properties
      (when (eq (car properties) :members)
        (setq members (car (cdr properties))))
      (setq properties (cdr (cdr properties))))
    (if (listp members) members nil)))

(defun nl-ns--ns-prefix (form name)
  "Return the prefix declared by `nl-ns-define' FORM for NAME."
  (let ((properties (cdr (cdr form)))
        (prefix nil))
    (while properties
      (when (eq (car properties) :prefix)
        (setq prefix (car (cdr properties))))
      (setq properties (cdr (cdr properties))))
    (if (stringp prefix) prefix (concat (symbol-name name) "-"))))

(defun nl-ns--quoted-members-in-template (form members depth)
  "Return MEMBERS appearing as literal data in backquote FORM at DEPTH."
  (cond
   ((symbolp form) (if (memq form members) (list form) nil))
   ((not (consp form)) nil)
   ((memq (car form) '(\` backquote))
    (nl-ns--quoted-members-in-template (car (cdr form)) members (1+ depth)))
   ((memq (car form) '(\, \,@ comma comma-at))
    (if (= depth 1)
        nil
      (nl-ns--quoted-members-in-template (car (cdr form)) members (1- depth))))
   (t (append (nl-ns--quoted-members-in-template (car form) members depth)
              (nl-ns--quoted-members-in-template (cdr form) members depth)))))

(defun nl-ns--quoted-members-in-form (form members)
  "Return MEMBERS that FORM uses in quote or backquote literal positions."
  (cond
   ((not (consp form)) nil)
   ((eq (car form) 'quote)
    (nl-ns--quoted-members-in-template (car (cdr form)) members 0))
   ((memq (car form) '(\` backquote))
    (nl-ns--quoted-members-in-template (car (cdr form)) members 1))
   (t (append (nl-ns--quoted-members-in-form (car form) members)
              (nl-ns--quoted-members-in-form (cdr form) members)))))

(defun nl-ns--ns-in-defines (form members prefix)
  "Return qualified definitions from `nl-ns-in' FORM for MEMBERS and PREFIX."
  (let ((out nil))
    (dolist (body-form (cdr (cdr form)))
      (let ((defined (nl-ns--defined-symbol body-form)))
        (when (and defined (memq defined members))
          (setq out (cons (intern (concat prefix (symbol-name defined))) out)))))
    (nreverse out)))

(defun nl-ns--definition-guard-test-p (predicate symbol variablep)
  "Return non-nil when PREDICATE checks for SYMBOL's presence.
VARIABLEP selects `boundp' instead of `fboundp'."
  (let ((probe (if variablep 'boundp 'fboundp)))
    (cond
     ((and (consp predicate)
           (eq (car predicate) probe)
           (consp (cdr predicate)))
      (let ((arg (car (cdr predicate))))
        (or (eq arg symbol)
            (and (consp arg)
                 (eq (car arg) 'quote)
                 (eq (car (cdr arg)) symbol)))))
     ((and (consp predicate) (eq (car predicate) 'or))
      (let ((tail (cdr predicate))
            (found nil))
        (while (and tail (not found))
          (when (nl-ns--definition-guard-test-p (car tail) symbol variablep)
            (setq found t))
          (setq tail (cdr tail)))
        found))
     (t nil))))

(defun nl-ns--guard-for-when (predicate symbol variablep)
  "Classify a `when' PREDICATE guarding SYMBOL."
  (if (and (consp predicate)
           (eq (car predicate) 'not)
           (nl-ns--definition-guard-test-p (car (cdr predicate)) symbol variablep))
      'fboundp
    'custom))

(defun nl-ns--guard-for-unless (predicate symbol variablep)
  "Classify an `unless' PREDICATE guarding SYMBOL."
  (if (nl-ns--definition-guard-test-p predicate symbol variablep)
      'fboundp
    'custom))

(defun nl-ns--make-guard (kind predicate)
  "Return guard plist for KIND and PREDICATE."
  (list :kind kind
        :predicate predicate
        :source (if predicate (prin1-to-string predicate) nil)))

(defun nl-ns--comment-line-p (line)
  "Return non-nil when LINE is an indented elisp comment."
  (let ((i 0)
        (n (length line)))
    (while (and (< i n)
                (memq (aref line i) '(?\s ?\t)))
      (setq i (1+ i)))
    (and (< i n) (eq (aref line i) ?\;))))

(defun nl-ns--line-start (text pos)
  "Return the index of the line start in TEXT containing POS."
  (while (and (> pos 0)
              (not (eq (aref text (1- pos)) ?\n)))
    (setq pos (1- pos)))
  pos)

(defun nl-ns--definition-source-comments (source symbol)
  "Return contiguous comment lines in SOURCE immediately before SYMBOL."
  (when (and (stringp source) (symbolp symbol))
    (let ((match
           (string-match
            (concat "(\\(defun\\|defmacro\\|defsubst\\|defalias\\|defvar\\|defconst\\|defcustom\\|"
                    "define-error\\|define-minor-mode\\|define-derived-mode\\|"
                    "cl-defun\\|cl-defmacro\\|cl-defstruct\\|cl-defgeneric\\|cl-defmethod\\)"
                    "[ \t\r\n]+'?"
                    (regexp-quote (symbol-name symbol))
                    "\\([ \t\r\n()]\\|$\\)")
            source)))
      (when match
        (let ((line-start (nl-ns--line-start source match))
              (comments nil)
              (done nil))
          (while (and (> line-start 0) (not done))
            (let* ((prev-end (1- line-start))
                   (prev-start (nl-ns--line-start source prev-end))
                   (line (substring source prev-start prev-end)))
              (cond
               ((nl-ns--string-blank-p line)
                (setq done t))
               ((nl-ns--comment-line-p line)
                (setq comments (cons line comments))
                (setq line-start prev-start))
               (t
                (setq done t)))))
          (when comments
            (mapconcat #'identity comments "\n")))))))

(defun nl-ns--definition-records-in-form (form guard leading-comments source)
  "Return definition plists nested in executable FORM.
GUARD is nil or a plist describing the enclosing conditional guard.
LEADING-COMMENTS comes from the enclosing top-level form metadata.
SOURCE is the unread top-level source text, used to recover comment
lines immediately above nested definitions inside conditionals."
  (cond
   ((not (consp form)) nil)
   ((nl-ns--defined-symbol form)
    (let ((symbol (nl-ns--defined-symbol form)))
      (list (list :symbol symbol
                :form form
                :guard-kind (or (plist-get guard :kind) 'none)
                :guard-source (plist-get guard :source)
                  :leading-comments
                  ;; A blank string is not "there are comments": an empty
                  ;; `:leading' is truthy in Elisp and would suppress the
                  ;; source scan that finds the comment a wrapper form
                  ;; pushed away from the top level.
                  (if (nl-ns--string-blank-p (or leading-comments ""))
                      (nl-ns--definition-source-comments source symbol)
                    leading-comments)))))
   ((or (eq (car form) 'quote) (eq (car form) 'function)
        (memq (car form) '(\` backquote)))
    nil)
   ((eq (car form) 'when)
    (let ((predicate (car (cdr form))))
      (apply #'append
             (mapcar
              (lambda (body-form)
                (let ((defined (nl-ns--defined-symbol body-form)))
                 (nl-ns--definition-records-in-form
                   body-form
                   (and defined
                        (nl-ns--make-guard
                         (nl-ns--guard-for-when
                          predicate defined
                          (eq (nl-ns--definition-host-kind body-form) 'variable))
                         predicate))
                   nil
                   source)))
              (cdr (cdr form))))))
   ((eq (car form) 'unless)
    (let ((predicate (car (cdr form))))
      (apply #'append
             (mapcar
              (lambda (body-form)
                (let ((defined (nl-ns--defined-symbol body-form)))
                 (nl-ns--definition-records-in-form
                   body-form
                   (and defined
                        (nl-ns--make-guard
                         (nl-ns--guard-for-unless
                          predicate defined
                          (eq (nl-ns--definition-host-kind body-form) 'variable))
                         predicate))
                   nil
                   source)))
              (cdr (cdr form))))))
   ((memq (car form) '(if cond))
    (apply #'append
           (mapcar (lambda (subform)
                     (nl-ns--definition-records-in-form
                      subform (nl-ns--make-guard 'custom (car (cdr form))) nil
                      source))
                   (cdr (cdr form)))))
   (t (append (nl-ns--definition-records-in-form (car form) guard nil source)
              (nl-ns--definition-records-in-form (cdr form) guard nil source)))))

(defun nl-ns--ns-in-definition-records (form members prefix)
  "Return qualified definition plists from `nl-ns-in' FORM."
  (let ((out nil))
    (dolist (body-form (cdr (cdr form)))
      (let ((defined (nl-ns--defined-symbol body-form)))
        (when (and defined (memq defined members))
          (setq out
                (cons (list :symbol (intern (concat prefix (symbol-name defined)))
                            :form body-form
                            :guard-kind 'none
                            :guard-source nil
                            :leading-comments nil)
                      out)))))
    (nreverse out)))

(defun nl-ns-scan-forms (forms &optional metadata)
  "Return a plist describing FORMS, the top-level forms of one file.
Keys: `:defines' (list of symbols, in order), `:definition-forms'
(symbol/form pairs), `:definition-records' (definition metadata plists),
`:requires' and `:provides' (lists of feature symbols), `:symbols' (hash
set of every symbol mentioned), and `:quoted-members' (namespace/member
pairs).  METADATA comes from `nl-ns--read-file-entry'."
  (let ((defines nil) (definition-forms nil) (definition-records nil)
        (requires nil) (provides nil) (quoted-members nil)
        (namespaces (make-hash-table :test 'eq))
        (symbols (make-hash-table :test 'eq)))
    (dolist (form forms)
      (let* ((info (nl-ns--metadata-for-form form metadata))
             (leading (plist-get info :leading))
             (source (plist-get info :source)))
        (cond
         ((and (consp form) (eq (car form) 'nl-ns-define)
               (symbolp (car (cdr form))))
          (let ((name (car (cdr form))))
            (puthash name (list :members (nl-ns--ns-members form)
                                :prefix (nl-ns--ns-prefix form name))
                     namespaces)))
         ((and (consp form) (eq (car form) 'nl-ns-in)
               (symbolp (car (cdr form))))
          (let* ((name (car (cdr form)))
                 (declaration (gethash name namespaces))
                 (members (plist-get declaration :members))
                 (prefix (plist-get declaration :prefix)))
            (dolist (record (nl-ns--ns-in-definition-records form members prefix))
              (setq defines (cons (plist-get record :symbol) defines))
              (setq definition-records (cons record definition-records))
              (setq definition-forms
                    (cons (cons (plist-get record :symbol)
                                (plist-get record :form))
                          definition-forms)))
            (dolist (member (nl-ns--quoted-members-in-form
                             (cdr (cdr form)) members))
              (setq quoted-members (cons (cons name member) quoted-members)))))
         (t
          (dolist (record (nl-ns--definition-records-in-form form nil leading source))
             (setq defines (cons (plist-get record :symbol) defines))
             (setq definition-records (cons record definition-records))
             (setq definition-forms
                   (cons (cons (plist-get record :symbol)
                               (plist-get record :form))
                        definition-forms)))))
        (when (and (consp form) (eq (car form) 'require))
          (let ((feature (nl-ns--quoted-feature form)))
            (when feature (setq requires (cons feature requires)))))
        (when (and (consp form) (eq (car form) 'provide))
          (let ((feature (nl-ns--quoted-feature form)))
            (when feature (setq provides (cons feature provides)))))
        (nl-ns--collect-symbols form symbols)))
    (list :defines (nreverse defines)
          :definition-forms (nreverse definition-forms)
          :definition-records (nreverse definition-records)
          :requires (nreverse requires)
          :provides (nreverse provides)
          :quoted-members (nreverse quoted-members)
          :symbols symbols)))

;;;; Namespace inference ------------------------------------------------

(defun nl-ns--prefixes (name)
  "Return NAME's hyphen-boundary prefixes, longest first.
\"nl-safe-foo\" yields (\"nl-safe-\" \"nl-\")."
  (let ((out nil) (i 0) (n (length name)))
    (while (< i n)
      (when (eq (aref name i) ?-)
        (setq out (cons (substring name 0 (1+ i)) out)))
      (setq i (1+ i)))
    out))

(defun nl-ns-file-namespace (file defines)
  "Return the namespace prefix string for FILE given its DEFINES.
An explicit `nl-ns-declare' wins.  Otherwise the answer is the longest
hyphen-boundary prefix shared by more than half of DEFINES, or nil when
no prefix reaches that share -- a file with no dominant prefix is
treated as deliberately global and skips the prefix check."
  (let ((declared (gethash file nl-ns--declared)))
    (if declared
        declared
      (let ((counts (make-hash-table :test 'equal))
            (total (length defines))
            (best nil)
            (best-len 0))
        (dolist (sym defines)
          (dolist (prefix (nl-ns--prefixes (symbol-name sym)))
            (puthash prefix (1+ (or (gethash prefix counts) 0)) counts)))
        (when (> total 0)
          (maphash
           (lambda (prefix count)
             (when (and (> (* 2 count) total)
                        (> (length prefix) best-len))
               (setq best prefix)
               (setq best-len (length prefix))))
           counts))
        best))))

;;;; Analysis -----------------------------------------------------------

(defun nl-ns-analyse (entries)
  "Analyse ENTRIES, a list of (FILE . FORMS), and return an analysis plist.
FORMS may be `nl-ns--unreadable', a plain list of forms, or a plist from
`nl-ns--read-file-entry'.  Keys of the result: `:files' (list of per-file
plists), `:owner' (hash symbol -> list of defining files, newest first),
`:feature-owner' (hash feature symbol -> file that provides it)."
  (let ((files nil)
        (owner (make-hash-table :test 'eq))
        (feature-owner (make-hash-table :test 'eq)))
    (dolist (entry entries)
      (let* ((file (car entry))
             (raw (cdr entry))
             (forms (if (and (listp raw) (plist-get raw :forms))
                        (plist-get raw :forms)
                      raw))
             (metadata (and (listp raw) (plist-get raw :metadata))))
        (if (eq forms 'nl-ns--unreadable)
            (setq files (cons (list :file file :unreadable t) files))
          (let* ((scan (nl-ns-scan-forms forms metadata))
                 (defines (plist-get scan :defines))
                 (namespace (nl-ns-file-namespace file defines)))
            (dolist (sym defines)
              (let ((seen (gethash sym owner)))
                (unless (member file seen)
                  (puthash sym (cons file seen) owner))))
            (dolist (feature (plist-get scan :provides))
              (puthash feature file feature-owner))
            (setq files
                  (cons (list :file file
                              :namespace namespace
                              :defines defines
                              :definition-forms (plist-get scan :definition-forms)
                              :definition-records
                              (plist-get scan :definition-records)
                              :requires (plist-get scan :requires)
                              :provides (plist-get scan :provides)
                              :quoted-members (plist-get scan :quoted-members)
                              :symbols (plist-get scan :symbols))
                        files))))))
    (list :files (nreverse files)
          :owner owner
          :feature-owner feature-owner)))

(defun nl-ns-analyse-files (paths)
  "Read each of PATHS and return the analysis, as `nl-ns-analyse' does."
  (let ((entries nil))
    (dolist (path paths)
      (setq entries (cons (cons path (nl-ns--read-file-entry path)) entries)))
    (nl-ns-analyse (nreverse entries))))

;;;; Baseline -----------------------------------------------------------

(defun nl-ns--list-to-set (items)
  "Return a hash set containing ITEMS."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (item items)
      (puthash item t table))
    table))

(defun nl-ns-load-baseline (path)
  "Read PATH and return a normalized nl-ns host baseline plist.
The file must contain one readable plist with keys `:emacs-version',
`:generated-at', `:functions', `:variables', and `:libraries'."
  (let ((raw (car (nl-ns-read-file path))))
    (unless (listp raw)
      (error "nl-ns-load-baseline: %s did not contain a plist" path))
    (let ((version (plist-get raw :emacs-version))
          (generated-at (plist-get raw :generated-at))
          (functions (plist-get raw :functions))
          (variables (plist-get raw :variables))
          (libraries (plist-get raw :libraries)))
      (unless (stringp version)
        (error "nl-ns-load-baseline: %s missing :emacs-version" path))
      (unless (stringp generated-at)
        (error "nl-ns-load-baseline: %s missing :generated-at" path))
      (list :source path
            :emacs-version version
            :generated-at generated-at
            :functions (nl-ns--list-to-set functions)
            :variables (nl-ns--list-to-set variables)
            :libraries (nl-ns--list-to-set libraries)))))

(defun nl-ns--coerce-baseline (baseline)
  "Return BASELINE as a normalized baseline plist."
  (cond
   ((null baseline) nil)
   ((stringp baseline) (nl-ns-load-baseline baseline))
   (t baseline)))

(defun nl-ns--baseline-host-kind (baseline symbol kind)
  "Return host kind for SYMBOL under BASELINE, or nil."
  (and baseline
       (if (eq kind 'variable)
           (and (gethash symbol (plist-get baseline :variables)) 'variable)
         (and (gethash symbol (plist-get baseline :functions)) 'function))))

;;;; Checking ------------------------------------------------------------

(defun nl-ns--private-name-p (name)
  "Return non-nil when NAME follows the `--' private convention."
  (let ((i 0) (n (length name)) (found nil))
    (while (and (< (1+ i) n) (not found))
      (when (and (eq (aref name i) ?-) (eq (aref name (1+ i)) ?-))
        (setq found t))
      (setq i (1+ i)))
    found))

(defun nl-ns--string-prefix-p (prefix string)
  "Return non-nil when STRING starts with PREFIX."
  (and (<= (length prefix) (length string))
       (string= prefix (substring string 0 (length prefix)))))

(defun nl-ns--requires-file-p (entry file analysis)
  "Return non-nil when ENTRY's file requires a feature provided by FILE."
  (let ((feature-owner (plist-get analysis :feature-owner))
        (found nil))
    (dolist (feature (plist-get entry :requires))
      (when (equal (gethash feature feature-owner) file)
        (setq found t)))
    found))

(defun nl-ns--normalise-definition-form (form)
  "Return FORM with equivalent backquote reader heads made identical."
  (cond
   ((consp form)
    (let ((head (car form)))
      (cons (cond
             ((memq head '(\` backquote)) 'backquote)
             ((memq head '(\, comma)) 'comma)
             ((memq head '(\,@ comma-at)) 'comma-at)
             (t (nl-ns--normalise-definition-form head)))
            (nl-ns--normalise-definition-form (cdr form)))))
   ((vectorp form)
    (let ((out (make-vector (length form) nil)) (i 0))
      (while (< i (length form))
        (aset out i (nl-ns--normalise-definition-form (aref form i)))
        (setq i (1+ i)))
      out))
   (t form)))

(defun nl-ns--definition-forms-equal-p (first second)
  "Return non-nil when definition forms FIRST and SECOND are equal."
  (equal (nl-ns--normalise-definition-form
          (nl-ns--definition-without-docstring first))
         (nl-ns--normalise-definition-form
          (nl-ns--definition-without-docstring second))))

(defun nl-ns--entry-definition (entry symbol)
  "Return ENTRY's first definition form for SYMBOL, or nil."
  (cdr (assq symbol (plist-get entry :definition-forms))))

(defun nl-ns--entry-definition-record (entry symbol)
  "Return ENTRY's first definition record for SYMBOL, or nil."
  (let ((records (plist-get entry :definition-records))
        (found nil))
    (while (and records (not found))
      (when (eq (plist-get (car records) :symbol) symbol)
        (setq found (car records)))
      (setq records (cdr records)))
    found))

(defun nl-ns--analysis-file-entry (analysis file)
  "Return ANALYSIS's entry for FILE, or nil."
  (let ((entries (plist-get analysis :files)) (found nil))
    (while (and entries (not found))
      (when (equal (plist-get (car entries) :file) file)
        (setq found (car entries)))
      (setq entries (cdr entries)))
    found))

(defun nl-ns--sentences (text)
  "Split TEXT into conservative sentences."
  (let ((i 0)
        (start 0)
        (out nil))
    (while (< i (length text))
      (when (memq (aref text i) '(?. ?! ?? ?\n))
        (let ((piece (nl-ns--trim-string (substring text start (1+ i)))))
          (unless (nl-ns--string-blank-p piece)
            (setq out (cons piece out))))
        (setq start (1+ i)))
      (setq i (1+ i)))
    (let ((tail (nl-ns--trim-string (substring text start))))
      (unless (nl-ns--string-blank-p tail)
        (setq out (cons tail out))))
    (nreverse out)))

(defun nl-ns--clean-comment-line (line)
  "Strip leading comment punctuation from LINE."
  (let ((i 0)
        (n (length line)))
    (while (and (< i n)
                (memq (aref line i) '(?\s ?\t ?\;)))
      (setq i (1+ i)))
    (substring line i)))

(defun nl-ns--comment-sentences (text)
  "Return cleaned comment sentences from TEXT."
  (let ((start 0)
        (i 0)
        (lines nil))
    (while (< i (length text))
      (when (eq (aref text i) ?\n)
        (setq lines (cons (substring text start i) lines))
        (setq start (1+ i)))
      (setq i (1+ i)))
    (when (< start (length text))
      (setq lines (cons (substring text start) lines)))
    (setq lines (nreverse lines))
    (let ((sentences nil))
      (dolist (line lines)
        (let ((clean (nl-ns--trim-string (nl-ns--clean-comment-line line))))
          (unless (nl-ns--string-blank-p clean)
            (dolist (sentence (nl-ns--sentences clean))
              (setq sentences (cons sentence sentences))))))
      (nreverse sentences))))

(defun nl-ns--marker-match (sentence)
  "Return the first partial marker found in SENTENCE, or nil."
  (let ((down (downcase sentence))
        (markers nl-ns-partial-markers)
        (found nil))
    (while (and markers (not found))
      (when (string-match (regexp-quote (car markers)) down)
        (setq found (car markers)))
      (setq markers (cdr markers)))
    found))

(defun nl-ns--definition-partial-evidence (record)
  "Return partiality evidence plist for RECORD, or nil."
  (let ((sources nil))
    (let ((docstring (nl-ns--definition-docstring (plist-get record :form))))
      (unless (nl-ns--string-blank-p docstring)
        (setq sources (cons docstring sources))))
    (let ((comments (plist-get record :leading-comments)))
      (unless (nl-ns--string-blank-p comments)
        (dolist (sentence (nl-ns--comment-sentences comments))
          (setq sources (cons sentence sources)))))
    (setq sources (nreverse sources))
    (let ((found nil))
      (dolist (source sources)
        (unless found
          (dolist (sentence (nl-ns--sentences source))
            (let ((marker (nl-ns--marker-match sentence)))
              (when (and marker (not found))
                (setq found (list :marker marker :sentence sentence)))))))
      found)))

(defun nl-ns--check-collisions (analysis findings)
  "Add one collision finding per multiply-defined symbol."
  (let ((owner (plist-get analysis :owner))
        (collisions nil))
    (maphash
     (lambda (sym files)
       (when (cdr files)
         (setq collisions (cons (cons sym (reverse files)) collisions))))
     owner)
    (setq collisions
          (sort collisions
                (lambda (a b) (string< (symbol-name (car a))
                                       (symbol-name (car b))))))
    (dolist (collision collisions)
      (let ((reference nil) (divergent nil) (heads nil))
        (dolist (file (cdr collision))
          (let* ((entry (nl-ns--analysis-file-entry analysis file))
                 (form (nl-ns--entry-definition entry (car collision))))
            (when form
              (setq heads (cons (cons file (car form)) heads))
              (if reference
                  (unless (nl-ns--definition-forms-equal-p reference form)
                    (setq divergent t))
                (setq reference form)))))
        (setq findings
              (cons (append (list :kind (if divergent
                                            'ns-collision-divergent
                                          'ns-collision)
                                  :subject (car collision)
                                  :files (cdr collision)
                                  :count (length (cdr collision)))
                            (if divergent
                                ;; `:shape' fingerprints WHAT diverges, not
                                ;; merely that something does.  Without it the
                                ;; accepted-set key is kind+subject+files, so
                                ;; once a divergence is accepted the two
                                ;; definitions may drift into any other shape
                                ;; and still match the same key.  Measured
                                ;; 2026-08-21 by `gate-mutation': injecting a
                                ;; real change into the prelude's `round' left
                                ;; ns-gate green, because `round' was already
                                ;; an accepted divergence.
                                (list :heads (nreverse heads)
                                      :shape (nl-ns--definition-shape
                                              (cdr collision) analysis
                                              (car collision)))
                              nil))
                    findings))))
    findings))

(defun nl-ns--check-prefixes (entry findings)
  "Add `ns-prefix-violation' findings for ENTRY's stray definitions."
  (let ((namespace (plist-get entry :namespace))
        (file (plist-get entry :file)))
    (when namespace
      (dolist (sym (plist-get entry :defines))
        (unless (nl-ns--string-prefix-p namespace (symbol-name sym))
          (setq findings
                (cons (list :kind 'ns-prefix-violation
                            :subject sym :file file :expected namespace)
                      findings)))))
    findings))

(defun nl-ns--check-references (entry analysis check-deps findings)
  "Add cross-file reference findings for ENTRY."
  (let* ((file (plist-get entry :file))
         (owner (plist-get analysis :owner))
         (symbols (plist-get entry :symbols))
         (escapes nil)
         (deps (make-hash-table :test 'equal)))
    (when symbols
      (maphash
       (lambda (sym _v)
         (let ((definers (gethash sym owner)))
           (when (and definers (null (cdr definers))
                      (not (equal (car definers) file)))
             (let ((other (car definers)))
               (when (nl-ns--private-name-p (symbol-name sym))
                 (setq escapes (cons (cons sym other) escapes)))
               (puthash other t deps)))))
       symbols))
    (setq escapes
          (sort escapes
                (lambda (a b) (string< (symbol-name (car a))
                                       (symbol-name (car b))))))
    (dolist (escape escapes)
      (setq findings
            (cons (list :kind 'ns-private-escape
                        :subject (car escape) :file file
                        :owner (cdr escape))
                  findings)))
    (when check-deps
      (let ((targets nil))
        (maphash (lambda (other _v) (setq targets (cons other targets))) deps)
        (setq targets (sort targets #'string<))
        (dolist (other targets)
          (unless (nl-ns--requires-file-p entry other analysis)
            (setq findings
                  (cons (list :kind 'ns-undeclared-dependency
                              :subject other :file file)
                        findings))))))
    findings))

(defun nl-ns--check-quoted-members (entry findings)
  "Add `ns-quoted-member' findings for literal members in ENTRY."
  (dolist (quoted (plist-get entry :quoted-members))
    (setq findings
          (cons (list :kind 'ns-quoted-member
                      :subject (cdr quoted) :file (plist-get entry :file)
                      :namespace (car quoted))
                findings)))
  findings)

(defun nl-ns--check-file-shadows-library (entry baseline findings)
  "Add `ns-file-shadows-library' for ENTRY when it masks BASELINE."
  (when (and baseline (not (plist-get entry :unreadable)))
    (let* ((file (plist-get entry :file))
           (library (nl-ns--library-basename file)))
      (when (gethash library (plist-get baseline :libraries))
        (setq findings
              (cons (list :kind 'ns-file-shadows-library
                          :subject library
                          :file file
                          :host-library library)
                    findings)))))
  findings)

(defun nl-ns--check-host-shadows (entry baseline findings)
  "Add host-shadow findings for ENTRY against BASELINE."
  (when (and baseline (not (plist-get entry :unreadable)))
    (dolist (record (plist-get entry :definition-records))
      (let* ((symbol (plist-get record :symbol))
             (file (plist-get entry :file))
             (kind (nl-ns--definition-host-kind (plist-get record :form)))
             (host-kind (nl-ns--baseline-host-kind baseline symbol kind)))
        (when host-kind
          (setq findings
                (cons (list :kind 'ns-shadows-host
                            :subject symbol
                            :file file
                            :host-kind host-kind)
                      findings))
          (let ((guard-kind (plist-get record :guard-kind))
                (guard-source (plist-get record :guard-source))
                (partial (nl-ns--definition-partial-evidence record)))
            (when (memq guard-kind '(none custom))
              (setq findings
                    (cons (append (list :kind 'ns-unsafe-shim-guard
                                        :subject symbol
                                        :file file
                                        :host-kind host-kind
                                        :guard-kind guard-kind)
                                  (if guard-source
                                      (list :guard-source guard-source)
                                    nil)
                                  (if (and guard-source
                                           (string-match "autoloadp" guard-source))
                                      (list :autoloadp t)
                                    nil))
                          findings)))
            (when (and partial (memq guard-kind '(none custom)))
              (setq findings
                    (cons (list :kind 'ns-partial-override
                                :subject symbol
                                :file file
                                :host-kind host-kind
                                :marker (plist-get partial :marker)
                                :sentence (plist-get partial :sentence)
                                :guard-kind guard-kind)
                          findings))))))))
  findings)

(defun nl-ns--finding-severity (finding)
  "Return FINDING's severity number."
  (or (cdr (assq (plist-get finding :kind) nl-ns-finding-severities)) 6))

(defun nl-ns--finding-sort-key (finding)
  "Return a stable string sort key for FINDING."
  (let ((subject (plist-get finding :subject)))
    (cond
     ((symbolp subject) (symbol-name subject))
     ((stringp subject) subject)
     (t (format "%S" subject)))))

(defun nl-ns--sort-findings (findings)
  "Return FINDINGS sorted by severity, kind, then subject."
  (sort findings
        (lambda (a b)
          (let ((sa (nl-ns--finding-severity a))
                (sb (nl-ns--finding-severity b)))
            (if (/= sa sb)
                (< sa sb)
              (let ((ka (symbol-name (plist-get a :kind)))
                    (kb (symbol-name (plist-get b :kind))))
                (if (string= ka kb)
                    (string< (nl-ns--finding-sort-key a)
                             (nl-ns--finding-sort-key b))
                  (string< ka kb))))))))

(defun nl-ns-check (analysis &optional check-dependencies baseline)
  "Return the findings for ANALYSIS, in severity order.
CHECK-DEPENDENCIES additionally reports `ns-undeclared-dependency'.
BASELINE is nil, a baseline plist, or a path accepted by
`nl-ns-load-baseline'.  With no baseline, host-shadow findings are
suppressed rather than guessed."
  (let ((findings nil)
        (baseline-data (nl-ns--coerce-baseline baseline)))
    (dolist (entry (plist-get analysis :files))
      (if (plist-get entry :unreadable)
          (setq findings
                (cons (list :kind 'ns-unreadable
                            :subject (plist-get entry :file)
                            :file (plist-get entry :file))
                      findings))
        (setq findings (nl-ns--check-file-shadows-library entry baseline-data findings))
        (setq findings (nl-ns--check-host-shadows entry baseline-data findings))
        (setq findings (nl-ns--check-prefixes entry findings))
        (setq findings (nl-ns--check-quoted-members entry findings))
        (setq findings
              (nl-ns--check-references entry analysis check-dependencies findings))))
    (setq findings (nl-ns--check-collisions analysis findings))
    (nl-ns--sort-findings findings)))

(defun nl-ns-check-files (paths &optional check-dependencies baseline)
  "Read PATHS and return their findings.  See `nl-ns-check'."
  (nl-ns-check (nl-ns-analyse-files paths) check-dependencies baseline))

;;;; Reporting -----------------------------------------------------------

(defun nl-ns-findings-of-kind (findings kind)
  "Return the elements of FINDINGS whose `:kind' is KIND."
  (let ((out nil))
    (dolist (finding findings)
      (when (eq (plist-get finding :kind) kind)
        (setq out (cons finding out))))
    (nreverse out)))

(defun nl-ns-report-max-severity (findings)
  "Return the highest-priority severity present in FINDINGS, or 0."
  (if (null findings)
      0
    (let ((best 99))
      (dolist (finding findings)
        (let ((severity (nl-ns--finding-severity finding)))
          (when (< severity best)
            (setq best severity))))
      best)))

(defun nl-ns--severity-summary (findings)
  "Return a compact severity-count string for FINDINGS."
  (let ((counts (make-hash-table :test 'eql))
        (levels '(1 2 3 4 5 6))
        (parts nil))
    (dolist (finding findings)
      (let* ((severity (nl-ns--finding-severity finding))
             (old (or (gethash severity counts) 0)))
        (puthash severity (1+ old) counts)))
    (dolist (level levels)
      (let ((count (gethash level counts)))
        (when count
          (setq parts (cons (format "%d=%d" level count) parts)))))
    (if parts
        (mapconcat #'identity (nreverse parts) " ")
      "none")))

(defun nl-ns--baseline-label (baseline)
  "Return a human-readable label for BASELINE."
  (if (null baseline)
      "baseline none"
    (format "baseline %s generated %s from %s"
            (plist-get baseline :emacs-version)
            (plist-get baseline :generated-at)
            (plist-get baseline :source))))

(defun nl-ns--describe (finding)
  "Return a one-line description of FINDING."
  (let ((kind (plist-get finding :kind))
        (subject (plist-get finding :subject)))
    (cond
     ((eq kind 'ns-partial-override)
      (format "ns-partial-override [sev %d]: %s defines `%s', replacing a host %s; marker %S in %S"
              (nl-ns--finding-severity finding)
              (plist-get finding :file) subject (plist-get finding :host-kind)
              (plist-get finding :marker) (plist-get finding :sentence)))
     ((eq kind 'ns-unsafe-shim-guard)
      (format "ns-unsafe-shim-guard [sev %d]: %s defines `%s' behind %s guard%s%s"
              (nl-ns--finding-severity finding)
              (plist-get finding :file) subject (plist-get finding :guard-kind)
              (if (plist-get finding :guard-source)
                  (format " %S" (plist-get finding :guard-source))
                "")
              (if (plist-get finding :autoloadp)
                  "; autoloads are real definitions, not absence"
                "")))
     ((eq kind 'ns-file-shadows-library)
      (format "ns-file-shadows-library [sev %d]: %s masks host library `%s' on load-path"
              (nl-ns--finding-severity finding)
              (plist-get finding :file) subject))
     ((eq kind 'ns-collision)
      (format "ns-collision [sev %d]: `%s' defined in %d files: %s"
              (nl-ns--finding-severity finding)
              subject (plist-get finding :count)
              (mapconcat #'identity (plist-get finding :files) ", ")))
     ((eq kind 'ns-collision-divergent)
      (format "ns-collision-divergent [sev %d]: `%s' differs in %d files: %s"
              (nl-ns--finding-severity finding)
              subject (plist-get finding :count)
              (mapconcat #'identity (plist-get finding :files) ", ")))
     ((eq kind 'ns-prefix-violation)
      (format "ns-prefix-violation [sev %d]: %s defines `%s', outside namespace `%s'"
              (nl-ns--finding-severity finding)
              (plist-get finding :file) subject
              (plist-get finding :expected)))
     ((eq kind 'ns-private-escape)
      (format "ns-private-escape [sev %d]: %s references `%s', private to %s"
              (nl-ns--finding-severity finding)
              (plist-get finding :file) subject (plist-get finding :owner)))
     ((eq kind 'ns-undeclared-dependency)
      (format "ns-undeclared-dependency [sev %d]: %s uses %s without requiring it"
              (nl-ns--finding-severity finding)
              (plist-get finding :file) subject))
     ((eq kind 'ns-shadows-host)
      (format "ns-shadows-host [sev %d]: %s defines `%s', also present in the host baseline as a %s"
              (nl-ns--finding-severity finding)
              (plist-get finding :file) subject (plist-get finding :host-kind)))
     ((eq kind 'ns-quoted-member)
      (format "ns-quoted-member [sev %d]: %s quotes member `%s' in namespace `%s'"
              (nl-ns--finding-severity finding)
              (plist-get finding :file) subject (plist-get finding :namespace)))
     ((eq kind 'ns-unreadable)
      (format "ns-unreadable [sev %d]: %s could not be read"
              (nl-ns--finding-severity finding) subject))
     (t (format "%s: %s" kind subject)))))

(defun nl-ns-report (findings &optional baseline)
  "Return a human-readable report string for FINDINGS.
BASELINE is nil, a baseline plist, or a path accepted by
`nl-ns-load-baseline'; the header reports which baseline was used."
  (let ((baseline-data (nl-ns--coerce-baseline baseline)))
    (if (null findings)
        (format "nl-ns: no findings (%s)\n"
                (nl-ns--baseline-label baseline-data))
      (let ((lines nil))
        (dolist (finding findings)
          (setq lines (cons (concat "  " (nl-ns--describe finding) "\n")
                            lines)))
        (apply #'concat
               (format "nl-ns: %d finding(s) | severity %s | %s\n"
                       (length findings)
                       (nl-ns--severity-summary findings)
                       (nl-ns--baseline-label baseline-data))
               (nreverse lines))))))

(defun nl-ns-summary (findings)
  "Return an alist of (KIND . COUNT) for FINDINGS, most frequent first."
  (let ((counts nil))
    (dolist (finding findings)
      (let* ((kind (plist-get finding :kind))
             (cell (assq kind counts)))
        (if cell
            (setcdr cell (1+ (cdr cell)))
          (setq counts (cons (cons kind 1) counts)))))
    (sort counts (lambda (a b) (> (cdr a) (cdr b))))))
;;;; Accepted-divergence ratchet ------------------------------------------

;; Some collisions are structural and will not go away.  A bootstrap
;; prelude has to define `when' and `cond' before the file that defines
;; them properly can be read, so its copies necessarily differ.  Telling
;; a reader "93 findings" every run trains them to ignore the number,
;; and the one new finding that matters arrives inside it unnoticed.
;;
;; So record the set that is known and accepted, and report only what is
;; not in it.  The accepted file is written once from a green tree and
;; reviewed like any other source; after that a gate can fail on anything
;; new.  Removing a divergence is also visible -- the entry goes stale --
;; so the list cannot quietly grow stale in the other direction either.

;; The 2026-08-23 Windows inventory saw 19-20 fingerprints move while the
;; collision set stayed fixed.  Reproduced 2026-09-03: `secure-hash' encodes
;; a multibyte string using the current buffer's file coding system.  An
;; Emacs child of MSYS make selected `japanese-shift-jis-dos', while direct
;; invocation selected UTF-8, so the same printed forms hashed differently.
;; Encode explicitly below; the accepted key is about the form, not the host.
(defun nl-ns--definition-shape (files analysis symbol)
  "Return a short digest of how SYMBOL is defined across FILES.

Two definitions that differ anywhere produce different digests, so an accepted
divergence stops matching the moment either side is edited.  That is the point:
accepting a divergence should accept THAT divergence, not the name."
  (let ((parts nil))
    (dolist (file (sort (copy-sequence files) #'string<))
      (let* ((entry (nl-ns--analysis-file-entry analysis file))
             (form (nl-ns--entry-definition entry symbol)))
        (setq parts (cons (if form (format "%S" form) "-") parts))))
    (secure-hash 'sha1
                 (encode-coding-string
                  (mapconcat #'identity (nreverse parts) "\0") 'utf-8))))

(defun nl-ns-finding-key (finding)
  "Return a stable string key for FINDING.
Built from kind, subject and the sorted file list, so it survives a
reordering of the scan and changes only when the finding itself does."
  (let ((files nil))
    (dolist (f (plist-get finding :files))
      (setq files (cons f files)))
    (format "%s\t%s\t%s%s"
            (plist-get finding :kind)
            (plist-get finding :subject)
            (mapconcat #'identity (sort files #'string<) " ")
            ;; The shape, when the finding carries one, so accepting a
            ;; divergence accepts THAT divergence rather than the name.
            (let ((shape (plist-get finding :shape)))
              (if shape (format "\t%s" (substring shape 0 12)) "")))))

(defun nl-ns-load-accepted (path)
  "Read the accepted-divergence file at PATH.
Return a plist with `:generated-at\=', `:reason\=', `:keys\=' (a hash table of
key -> t) and `:notes\=' (an alist of key -> reason string).  A missing file
yields an empty set rather than an error: a tree that has not adopted the
ratchet still reports normally.  A file with no `:notes\=' loads as before --
per-entry notes are additive.

Why per-entry notes exist.  The file-level `:reason\=' says one thing about
every entry, and on 2026-08-19 that one thing was \"package-local fallbacks
are fboundp-gated on purpose\" -- true of a fallback that DEFERS correctly,
and the justification under which two fallbacks that answered WRONGLY sat
unnoticed (`nelisp-sys-access\=' ignored its mode argument while every caller
passed 1).  A blanket reason cannot distinguish those two, so it stops being
a reason and becomes a place to put things."
  (let ((keys (make-hash-table :test 'equal))
        (generated nil)
        (reason nil)
        (notes nil))
    (when (and (stringp path) (file-readable-p path))
      (let ((entry (car (nl-ns-read-file path))))
        (when (consp entry)
          (setq generated (plist-get entry :generated-at))
          (setq reason (plist-get entry :reason))
          (setq notes (plist-get entry :notes))
          (dolist (key (plist-get entry :keys))
            (puthash key t keys)))))
    (list :generated-at generated :reason reason :keys keys :notes notes)))

(defun nl-ns-accepted-note (accepted key)
  "Return the per-entry note recorded for KEY in ACCEPTED, or nil."
  (cdr (assoc key (plist-get accepted :notes))))

(defun nl-ns-unnoted-accepted (accepted)
  "Return the accepted keys that carry no per-entry note, sorted.
An acceptance without a reason is a decision nobody wrote down."
  (let ((notes (plist-get accepted :notes))
        (out nil))
    (maphash (lambda (key _v)
               (unless (assoc key notes) (setq out (cons key out))))
             (plist-get accepted :keys))
    (sort out #'string<)))

(defun nl-ns-unaccepted (findings accepted)
  "Return the FINDINGS whose key is absent from ACCEPTED.
ACCEPTED is what `nl-ns-load-accepted' returned."
  (let ((keys (plist-get accepted :keys))
        (out nil))
    (dolist (finding findings)
      (unless (and keys (gethash (nl-ns-finding-key finding) keys))
        (setq out (cons finding out))))
    (nreverse out)))

(defun nl-ns-stale-accepted (findings accepted)
  "Return accepted keys that no longer match any of FINDINGS.
A key here means a divergence that has since been resolved, so the
entry should be dropped -- an accepted list that keeps stale entries
stops describing the tree."
  (let ((present (make-hash-table :test 'equal))
        (out nil))
    (dolist (finding findings)
      (puthash (nl-ns-finding-key finding) t present))
    (maphash (lambda (key _v)
               (unless (gethash key present)
                 (setq out (cons key out))))
             (plist-get accepted :keys))
    (sort out #'string<)))

(defun nl-ns-write-accepted (findings path &optional generated-at reason notes)
  "Write FINDINGS as the accepted-divergence set at PATH.
GENERATED-AT and REASON are recorded verbatim so the file says when it was
taken and why its contents are considered settled.

NOTES is an alist of key -> reason carried over from the file being
replaced.  Only notes whose key is still present survive; a note for a
divergence that has since been resolved goes with its key.  Regenerating
without passing NOTES silently discards every per-entry reason anyone
wrote, which is why the caller reads the old file first --
`nelisp-pkg-manifest-render\=' preserves author-written keys the same way and
for the same reason."
  (let ((keys nil)
        (kept nil))
    (dolist (finding findings)
      (setq keys (cons (nl-ns-finding-key finding) keys)))
    (setq keys (sort keys #'string<))
    (dolist (key keys)
      (let ((note (cdr (assoc key notes))))
        (when note (setq kept (cons (cons key note) kept)))))
    (setq kept (nreverse kept))
    (with-temp-buffer
      (insert ";; nl-ns accepted divergences -- generated, review before commit.\n")
      (insert ";; Regenerate with the make target that produced it; do not\n")
      (insert ";; hand-add keys to silence a finding.\n")
      (insert ";;\n")
      (insert ";; :notes is an alist of key -> why THIS entry is accepted, and it\n")
      (insert ";; survives regeneration.  The file-level :reason cannot tell a\n")
      (insert ";; fallback that defers correctly from one that answers wrongly;\n")
      (insert ";; two of the latter sat under it until 2026-08-19.\n")
      (prin1 (list :generated-at generated-at
                   :reason reason
                   :notes kept
                   :keys keys)
             (current-buffer))
      (insert "\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) path nil 'quiet)))
    (length keys)))


(provide 'nl-ns)

;;; nl-ns.el ends here
