;;; emacs-syntax-table.el --- Minimal syntax-table for font-lock pre-pass  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track R (2026-05-04) — minimum-viable syntax-table that the
;; font-lock pre-pass uses to identify *string* and *comment* regions.
;;
;; A "syntax table" here is just a hash from char → class symbol.
;; The class set is intentionally narrow (= what the pre-pass
;; needs):
;;
;;   word           default; alphanumeric / symbol-constituent
;;   open / close   ( )
;;   string-fence   "
;;   escape         \   (only meaningful inside a string)
;;   comment-start  ;   (line comment, single char)
;;   comment-end    \n  (line comment terminator)
;;   whitespace     space, tab
;;
;; The complete upstream Emacs syntax-table grammar (= 16+ classes,
;; flag bits, paired comment delimiters, syntactic-keyword
;; overlays) is OUT of scope for Track R.  When a major-mode needs
;; a different per-class char, it builds a fresh hash and binds
;; `font-lock-syntax-table' (or analogous).
;;
;; The integration point for font-lock is
;; `emacs-syntax-apply-faces-region' which walks a region and
;; faces every string + comment range with `font-lock-string-face'
;; / `font-lock-comment-face' respectively.  Run this AFTER the
;; keyword pass so syntactic faces win over keyword fontification
;; in string / comment text.

;;; Code:

(require 'emacs-buffer)
(require 'emacs-faces)
(require 'emacs-char-table)

;;;; --- standard table --------------------------------------------------------

(defvar emacs-syntax--standard-table
  (let ((tbl (make-hash-table :test 'eql)))
    (puthash ?\" 'string-fence tbl)
    (puthash ?\\ 'escape       tbl)
    (puthash ?\; 'comment-start tbl)
    (puthash ?\n 'comment-end  tbl)
    (puthash ?\( 'open         tbl)
    (puthash ?\) 'close        tbl)
    (puthash ?\s 'whitespace   tbl)
    (puthash ?\t 'whitespace   tbl)
    tbl)
  "Default syntax table used when no major-mode override is set.
A hash-table mapping integer CHAR → class symbol.  Chars not
present default to `word'.")

(defun emacs-syntax-class-of (char &optional table)
  "Return the syntax class symbol for CHAR in TABLE.
TABLE defaults to `emacs-syntax--standard-table'.  Chars not in
the table default to `word'."
  (or (gethash char (or table emacs-syntax--standard-table))
      'word))

(defun emacs-syntax-modify-entry (char class &optional table)
  "Set CHAR's syntax class to CLASS in TABLE (default = standard).
Returns CLASS.  If CLASS is nil, the entry is removed (= falls
back to `word')."
  (let ((tbl (or table emacs-syntax--standard-table)))
    (if class
        (puthash char class tbl)
      (remhash char tbl))
    class))

;;;; --- font-lock pre-pass ----------------------------------------------------

(defun emacs-syntax--char-at (buf pos)
  "Return the char at 1-based POS in BUF, or nil if out-of-range.
Implemented via `nelisp-ec-buffer-substring' which is the available
single-char accessor on the substrate (= no `char-after' yet)."
  (when (and (fboundp 'nelisp-ec-buffer-substring) buf)
    (let* ((nelisp-ec--current-buffer buf)
           (s (condition-case _
                  (nelisp-ec-buffer-substring pos (1+ pos))
                (error nil))))
      (and (stringp s) (> (length s) 0) (aref s 0)))))

(defun emacs-syntax-apply-faces-region (start end &optional buf table)
  "Walk BUF in [START, END) and face strings + line-comments.

Strings get `font-lock-string-face'; line comments get
`font-lock-comment-face'.  Use this AFTER the keyword pass so
syntactic faces overwrite any keyword face that fired inside a
string / comment.  No-op when neither buffer nor required
substrate is available (= host-driver fixture mode)."
  (when (and (fboundp 'nelisp-ec-buffer-substring)
             (fboundp 'emacs-buffer-put-text-property))
    (let* ((tbl (or table emacs-syntax--standard-table))
           (state 'code)
           (range-start nil)
           (escape nil)
           ;; Snapshot the region in one substrate call instead of
           ;; per-char (= O(n) substrate hops dropped to O(1)).
           (region (let ((nelisp-ec--current-buffer
                          (or buf (and (boundp 'nelisp-ec--current-buffer)
                                       nelisp-ec--current-buffer))))
                     (condition-case _
                         (nelisp-ec-buffer-substring start end)
                       (error nil))))
           (rlen (and region (length region)))
           (i 0))
      (while (and rlen (< i rlen))
        (let* ((ch (aref region i))
               (cls (emacs-syntax-class-of ch tbl))
               (abs-pos (+ start i)))
          (cond
           ;; In code: maybe enter string / comment.
           ((eq state 'code)
            (cond
             ((eq cls 'string-fence)
              (setq state 'string range-start abs-pos escape nil))
             ((eq cls 'comment-start)
              (setq state 'comment range-start abs-pos))))
           ;; In string: handle escape + closing fence.
           ((eq state 'string)
            (cond
             (escape (setq escape nil))
             ((eq cls 'escape) (setq escape t))
             ((eq cls 'string-fence)
              (emacs-buffer-put-text-property
               range-start (1+ abs-pos) 'face 'font-lock-string-face buf)
              (setq state 'code range-start nil))))
           ;; In comment: end-of-line closes it.
           ((eq state 'comment)
            (when (eq cls 'comment-end)
              (emacs-buffer-put-text-property
               range-start (1+ abs-pos) 'face 'font-lock-comment-face buf)
              (setq state 'code range-start nil)))))
        (setq i (1+ i)))
      ;; Unterminated open at end-of-region: face to end.
      (when range-start
        (emacs-buffer-put-text-property
         range-start end 'face
         (if (eq state 'string) 'font-lock-string-face
           'font-lock-comment-face)
         buf))
      nil)))

(defun emacs-syntax-state-at (pos &optional buf table)
  "Walk BUF from BOB to POS, returning the current syntactic state.
One of `code', `string', `comment'.  Used by syntactic-aware
matchers (= e.g. a keyword that should only fire in code)."
  (let* ((tbl (or table emacs-syntax--standard-table))
         (state 'code)
         (escape nil)
         (region (let ((nelisp-ec--current-buffer
                        (or buf (and (boundp 'nelisp-ec--current-buffer)
                                     nelisp-ec--current-buffer))))
                   (condition-case _
                       (nelisp-ec-buffer-substring 1 pos)
                     (error nil))))
         (rlen (and region (length region)))
         (i 0))
    (while (and rlen (< i rlen))
      (let* ((ch (aref region i))
             (cls (emacs-syntax-class-of ch tbl)))
        (cond
         ((eq state 'comment)
          (when (eq cls 'comment-end) (setq state 'code)))
         ((eq state 'string)
          (cond
           (escape (setq escape nil))
           ((eq cls 'escape) (setq escape t))
           ((eq cls 'string-fence) (setq state 'code))))
         (t
          (cond
           ((eq cls 'string-fence) (setq state 'string escape nil))
           ((eq cls 'comment-start) (setq state 'comment))))))
      (setq i (1+ i)))
    state))

;;;; --- upstream-compatible syntax-table layer --------------------------------

;; A second, upstream-faithful syntax-table API built on the real
;; `emacs-char-table' substrate, providing `char-syntax' /
;; `make-syntax-table' / `standard-syntax-table' / `modify-syntax-entry' /
;; `string-to-syntax' (Doc 06 MISSING ranks; `char-syntax' was a nil stub).
;; Entries are upstream raw descriptors (CLASS-CODE . MATCH); `char-syntax'
;; returns the class designator character.  This coexists with the narrow
;; hash-based Track R helpers above (different `emacs-syntax-table-' prefix).
;;
;; The current table is a dynamic value (default standard).  Full buffer-local
;; syntax tables and class flag bits are not modeled.

(defconst emacs-syntax-table--code-spec " .w_()'\"$\\/<>@!|"
  "Syntax class designator characters indexed by Emacs syntax class code.")

(defvar emacs-syntax-table--standard-char-table nil
  "Cached standard syntax char-table (built lazily).")

(defvar emacs-syntax-table--current nil
  "Dynamic current syntax char-table; nil means use the standard table.")

(defun emacs-syntax-table--install-function-p (symbol)
  "Return non-nil when SYMBOL's unprefixed shim should be installed.
Always installs under standalone NeLisp (overriding nil stubs); under host
Emacs only when SYMBOL is not already bound (host C builtin wins)."
  (if (not (boundp 'emacs-version))
      t
    (not (fboundp symbol))))

(defun emacs-syntax-table--build-standard ()
  "Build a fresh standard syntax char-table matching Emacs ASCII classes.
Non-ASCII defaults to word; ASCII classes are baked from the host standard
syntax table."
  (let ((tbl (emacs-char-table-make 'syntax-table (cons 2 nil))))
    (dolist (c '(9 10 12 13 32))
      (emacs-char-table-set tbl c (cons 0 nil)))
    (dolist (c '(0 1 2 3 4 5 6 7 8 11 14 15 16 17 18 19 20 21 22 23 24 25 26 27
                 28 29 30 31 33 35 39 44 46 58 59 63 64 94 96 126 127))
      (emacs-char-table-set tbl c (cons 1 nil)))
    (dolist (c '(38 42 43 45 47 60 61 62 95 124))
      (emacs-char-table-set tbl c (cons 3 nil)))
    (emacs-char-table-set tbl 34 (cons 7 nil))
    (emacs-char-table-set tbl 92 (cons 9 nil))
    (emacs-char-table-set tbl 40 (cons 4 41))
    (emacs-char-table-set tbl 41 (cons 5 40))
    (emacs-char-table-set tbl 91 (cons 4 93))
    (emacs-char-table-set tbl 93 (cons 5 91))
    (emacs-char-table-set tbl 123 (cons 4 125))
    (emacs-char-table-set tbl 125 (cons 5 123))
    tbl))

(defun emacs-syntax-table-standard ()
  "Return the cached standard syntax char-table, building it on first use."
  (or emacs-syntax-table--standard-char-table
      (setq emacs-syntax-table--standard-char-table
            (emacs-syntax-table--build-standard))))

(defconst emacs-syntax-table--local-key 'emacs-syntax-table--buffer-local
  "Buffer-local variable key holding a buffer's syntax char-table.")

(defun emacs-syntax-table--buffer ()
  "Return the current nelisp-ec buffer, or nil."
  (and (boundp 'nelisp-ec--current-buffer) nelisp-ec--current-buffer))

(defun emacs-syntax-table-current ()
  "Return the active syntax char-table.
Precedence: a `with-syntax-table' dynamic binding, then the current buffer's
buffer-local table, then the standard table."
  (or emacs-syntax-table--current
      (let ((buf (emacs-syntax-table--buffer)))
        (and buf
             (fboundp 'emacs-buffer-local-variable-p)
             (emacs-buffer-local-variable-p emacs-syntax-table--local-key buf)
             (emacs-buffer-buffer-local-value emacs-syntax-table--local-key buf)))
      (emacs-syntax-table-standard)))

(defun emacs-syntax-table-set-current (table)
  "Make TABLE the syntax char-table of the current buffer (buffer-local).
Falls back to a global setting when there is no current buffer."
  (let ((buf (emacs-syntax-table--buffer)))
    (if (and buf (fboundp 'emacs-buffer-set-buffer-local-value))
        (emacs-buffer-set-buffer-local-value emacs-syntax-table--local-key buf table)
      (setq emacs-syntax-table--current table)))
  table)

(defun emacs-syntax-table-make (&optional parent)
  "Return a fresh syntax char-table inheriting from PARENT (default standard)."
  (let ((tbl (emacs-char-table-make 'syntax-table nil)))
    (emacs-char-table-set-parent tbl (or parent (emacs-syntax-table-standard)))
    tbl))

(defun emacs-syntax-table--designator-code (designator)
  "Return the syntax class code for a DESIGNATOR character.
`-' and space both denote whitespace; unknown designators fall back to
punctuation (1)."
  (cond
   ((or (eq designator ?-) (eq designator ?\s)) 0)
   (t (let ((i 0) (n (length emacs-syntax-table--code-spec)) (res 1))
        (while (< i n)
          (when (eq (aref emacs-syntax-table--code-spec i) designator)
            (setq res i i n))
          (setq i (1+ i)))
        res))))

(defun emacs-syntax-table-string-to-syntax (descriptor)
  "Parse a syntax DESCRIPTOR string into a raw (CLASS-CODE . MATCH) cons.
Only the class designator and the optional matching character are modeled;
trailing flag characters are ignored."
  (let* ((code (emacs-syntax-table--designator-code (aref descriptor 0)))
         (match (and (> (length descriptor) 1)
                     (not (eq (aref descriptor 1) ?\s))
                     (aref descriptor 1))))
    (cons code match)))

(defun emacs-syntax-table-modify-entry (char descriptor &optional table)
  "Set CHAR's raw syntax to DESCRIPTOR in TABLE (default current).
CHAR may be a single character or a (MIN . MAX) range cons.  Returns nil."
  (let ((tbl (or table (emacs-syntax-table-current)))
        (syn (emacs-syntax-table-string-to-syntax descriptor)))
    (if (consp char)
        (let ((c (car char)) (hi (cdr char)))
          (while (<= c hi)
            (emacs-char-table-set tbl c syn)
            (setq c (1+ c))))
      (emacs-char-table-set tbl char syn))
    nil))

(defun emacs-syntax-table-char-syntax (char &optional table)
  "Return CHAR's syntax class designator character via TABLE (default current)."
  (let* ((entry (emacs-char-table-ref (or table (emacs-syntax-table-current))
                                      char))
         (code (cond ((consp entry) (car entry))
                     ((integerp entry) entry)
                     (t 2))))
    (aref emacs-syntax-table--code-spec code)))

(defun emacs-syntax-table-parse-partial-sexp
    (from to &optional buffer table state targetdepth stopbefore)
  "Scan BUFFER from FROM to TO and return an Emacs parse-partial-sexp state.

The returned list mirrors the upstream 11-element state: (DEPTH
INNERMOST-START LAST-SEXP-START IN-STRING IN-COMMENT AFTER-QUOTE MIN-DEPTH
COMMENT-STYLE COMMENT-OR-STRING-START OPEN-PAREN-POSITIONS INTERNAL).  IN-STRING
is the opening quote character (or nil); OPEN-PAREN-POSITIONS is outermost
first.  Classification uses TABLE (default the current syntax table).  When
STATE (a value from a previous call) is given, parsing resumes from it.  When
TARGETDEPTH is non-nil scanning stops once the paren depth becomes equal to it;
when STOPBEFORE is non-nil scanning stops before the start of the next sexp.
Point in BUFFER is moved to the stop position (TO when neither limit fires).
Comment styles, generic comments/strings, two-character comment delimiters, and
syntax flag bits are not modeled; the INTERNAL slot is nil."
  (let* ((tbl (or table (emacs-syntax-table-current)))
         (buf (or buffer (and (boundp 'nelisp-ec--current-buffer)
                              nelisp-ec--current-buffer)))
         (region (let ((nelisp-ec--current-buffer buf))
                   (condition-case _
                       (nelisp-ec-buffer-substring from to)
                     (error ""))))
         (n (length region))
         (i 0)
         (depth (or (nth 0 state) 0))
         (mindepth (or (nth 6 state) (or (nth 0 state) 0)))
         (open (reverse (nth 9 state)))
         (instr (nth 3 state))
         (incomment (nth 4 state))
         (afterq (nth 5 state))
         (last-sexp (nth 2 state))
         (scstart (nth 8 state))
         (tok-start nil)
         (stop-pos to)
         (done nil))
    (while (and (< i n) (not done))
      (let* ((ch (aref region i))
             (abs (+ from i))
             (syn (emacs-syntax-table-char-syntax ch tbl)))
        (cond
         (afterq (setq afterq nil))
         (instr
          (cond
           ((eq syn ?\\) (setq afterq t))
           ((and (eq syn ?\") (eq ch instr))
            (setq instr nil last-sexp scstart scstart nil))))
         (incomment
          (when (eq syn ?>)
            (setq incomment nil scstart nil)))
         ((and stopbefore
               (or (eq syn ?\() (eq syn ?\") (eq syn ?<)
                   (and (or (eq syn ?w) (eq syn ?_)) (not tok-start))))
          (setq stop-pos abs done t))
         (t
          (cond
           ((eq syn ?\\) (setq afterq t tok-start nil))
           ((eq syn ?\") (setq instr ch scstart abs tok-start nil))
           ((eq syn ?<) (setq incomment t scstart abs tok-start nil))
           ((eq syn ?\()
            (setq depth (1+ depth) open (cons abs open)
                  last-sexp nil tok-start nil)
            (when (and targetdepth (= depth targetdepth))
              (setq stop-pos (1+ abs) done t)))
           ((eq syn ?\))
            (when tok-start (setq last-sexp tok-start tok-start nil))
            (setq depth (1- depth))
            (when (< depth mindepth) (setq mindepth depth))
            (setq last-sexp (car open) open (cdr open))
            (when (and targetdepth (= depth targetdepth))
              (setq stop-pos (1+ abs) done t)))
           ((or (eq syn ?w) (eq syn ?_))
            (unless tok-start (setq tok-start abs)))
           (t
            (when tok-start (setq last-sexp tok-start tok-start nil)))))))
      (unless done (setq i (1+ i))))
    (when (and tok-start (not instr) (not incomment) (not done))
      (setq last-sexp tok-start))
    (when buf
      (let ((nelisp-ec--current-buffer buf))
        (ignore-errors (nelisp-ec-goto-char stop-pos))))
    (list depth (car open) last-sexp instr incomment afterq mindepth
          nil scstart (reverse open) nil)))

(when (emacs-syntax-table--install-function-p 'char-syntax)
  (defun char-syntax (char)
    "Return the syntax class designator character for CHAR (current table)."
    (emacs-syntax-table-char-syntax char)))

(when (emacs-syntax-table--install-function-p 'standard-syntax-table)
  (defun standard-syntax-table ()
    "Return the standard syntax char-table."
    (emacs-syntax-table-standard)))

(when (emacs-syntax-table--install-function-p 'make-syntax-table)
  (defun make-syntax-table (&optional parent)
    "Return a new syntax char-table inheriting from PARENT or the standard."
    (emacs-syntax-table-make parent)))

(when (emacs-syntax-table--install-function-p 'string-to-syntax)
  (defun string-to-syntax (descriptor)
    "Parse DESCRIPTOR into a raw (CLASS-CODE . MATCH) syntax cons."
    (emacs-syntax-table-string-to-syntax descriptor)))

(when (emacs-syntax-table--install-function-p 'modify-syntax-entry)
  (defun modify-syntax-entry (char descriptor &optional table)
    "Set CHAR's syntax to DESCRIPTOR in TABLE (default current)."
    (emacs-syntax-table-modify-entry char descriptor table)))

(when (emacs-syntax-table--install-function-p 'syntax-table)
  (defun syntax-table ()
    "Return the current syntax char-table."
    (emacs-syntax-table-current)))

(when (emacs-syntax-table--install-function-p 'set-syntax-table)
  (defun set-syntax-table (table)
    "Make TABLE the current syntax char-table."
    (emacs-syntax-table-set-current table)))

(when (emacs-syntax-table--install-function-p 'with-syntax-table)
  (defmacro with-syntax-table (table &rest body)
    "Evaluate BODY with TABLE as the current syntax char-table."
    (declare (indent 1) (debug t))
    `(let ((emacs-syntax-table--current ,table)) ,@body)))

(when (emacs-syntax-table--install-function-p 'parse-partial-sexp)
  (defun parse-partial-sexp (from to &optional targetdepth stopbefore
                                  state _commentstop)
    "Parse the current buffer from FROM to TO, returning a syntactic state.
STATE resumes from a previous call; TARGETDEPTH and STOPBEFORE bound the scan
and move point to the stop position.  COMMENTSTOP is accepted for call
compatibility but not yet honored."
    (emacs-syntax-table-parse-partial-sexp
     from to nil nil state targetdepth stopbefore)))

(provide 'emacs-syntax-table)

;;; emacs-syntax-table.el ends here
