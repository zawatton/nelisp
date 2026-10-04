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

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- syntax tables and parse-partial-sexp work on ec-buffers.
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
;; syntax tables are stored in the substrate's buffer-local variables.

(defconst emacs-syntax-table--code-spec " .w_()'\"$\\/<>@!|"
  "Syntax class designator characters indexed by Emacs syntax class code.")

(defvar emacs-syntax-table--standard-char-table nil
  "Cached standard syntax char-table (built lazily).")

(defvar emacs-syntax-table--current nil
  "Dynamic current syntax char-table; nil means use the standard table.")

(defun emacs-syntax-table--standalone-p ()
  "Return non-nil under standalone NeLisp.
The NeLisp reader binds `emacs-version' just like host Emacs, so a bare
`(not (boundp 'emacs-version))' test misfires there.  Detect the
standalone path by a NeLisp-only primitive (`nl-write-file'), matching
the standalone predicate in `emacs-char-table.el'."
  (or (fboundp 'nl-write-file)
      (not (boundp 'emacs-version))))

(defun emacs-syntax-table--install-function-p (symbol)
  "Return non-nil when SYMBOL's unprefixed shim should be installed.
Always installs under standalone NeLisp — overriding both the
`emacs-stub-bulk.el' nil-stubs and the (also-standalone) plain-list/
plain-vector `emacs-stub.el' / `emacs-stub-bulk.el' syntax-table
fallbacks, which load earlier in the bootstrap bundle and would
otherwise stay `fboundp' and shadow this module's real char-table-backed
implementations for the rest of the session (Doc 51 defect: callers of
e.g. `(make-syntax-table)' got back the stub's `(syntax-table nil)'
list, and any later `aref' on it signalled `wrong-type-argument
arrayp').  Under host Emacs, only installs when SYMBOL is not already
bound (host C builtin wins)."
  (if (emacs-syntax-table--standalone-p)
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
Normalize bootstrap records to their shared sparse view so array access
uses the native bridge.  Falls back to a global setting without a buffer."
  (setq table (emacs-char-table--storage table))
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
  "Return the syntax class code for DESIGNATOR, signaling invalid letters."
  (if (eq designator ?-)
      0
    (let ((i 0) (n (length emacs-syntax-table--code-spec)) result)
      (while (< i n)
        (when (eq (aref emacs-syntax-table--code-spec i) designator)
          (setq result i i n))
        (setq i (1+ i)))
      (or result (error "Invalid syntax description letter: %c" designator)))))

(defun emacs-syntax-table-string-to-syntax (descriptor)
  "Parse DESCRIPTOR's class, matching character and syntax flags."
  (unless (stringp descriptor)
    (signal 'wrong-type-argument (list 'stringp descriptor)))
  (let* ((code (emacs-syntax-table--designator-code
                (if (> (length descriptor) 0) (aref descriptor 0) 0)))
         (match (and (> (length descriptor) 1)
                     (not (eq (aref descriptor 1) ?\s))
                     (aref descriptor 1)))
         (i 2))
    (while (< i (length descriptor))
      (let ((flag (cdr (assq (aref descriptor i)
                            '((?1 . 65536) (?2 . 131072) (?3 . 262144)
                              (?4 . 524288) (?p . 1048576) (?b . 2097152)
                              (?n . 4194304) (?c . 8388608))))))
        (when flag (setq code (logior code flag))))
      (setq i (1+ i)))
    ;; Inherit syntax is represented by a nil entry, rather than class 13.
    (unless (= (logand code 255) 13) (cons code match))))

(defun emacs-syntax-table--check-character (char)
  "Signal a character type error for an invalid CHAR."
  (unless (and (integerp char) (<= 0 char) (<= char #x3fffff))
    (signal 'wrong-type-argument (list 'characterp char))))

(defun emacs-syntax-table--check-table (table)
  "Require a char-table with syntax-table subtype."
  (unless (and (emacs-char-table-p table)
               (eq (emacs-char-table-subtype table) 'syntax-table))
    (signal 'wrong-type-argument (list 'syntax-table-p table))))

(defun emacs-syntax-table-modify-entry (char descriptor &optional table)
  "Set CHAR or its inclusive range to DESCRIPTOR in TABLE; return nil."
  (if (consp char)
      (progn (emacs-syntax-table--check-character (car char))
             (emacs-syntax-table--check-character (cdr char)))
    (emacs-syntax-table--check-character char))
  (let ((tbl (or table (emacs-syntax-table-current))))
    (emacs-syntax-table--check-table tbl)
    ;; Keep large ranges sparse and ignore reversed ranges, as Emacs does.
    (emacs-char-table-set-range
     tbl char (emacs-syntax-table-string-to-syntax descriptor))
    nil))

(defun emacs-syntax-table-char-syntax (char &optional table)
  "Return CHAR's syntax class designator via TABLE (default current)."
  (emacs-syntax-table--check-character char)
  (let* ((entry (emacs-char-table-ref (or table (emacs-syntax-table-current))
                                      char))
         (code (cond ((consp entry) (car entry))
                     ((integerp entry) entry)
                     (t 2))))
    (aref emacs-syntax-table--code-spec (logand code 255))))

(defun emacs-syntax-table-parse-partial-sexp
    (from to &optional buffer table state targetdepth stopbefore commentstop)
  "Scan BUFFER from FROM to TO and return an Emacs parse-partial-sexp state.

The returned list mirrors the upstream 11-element state: (DEPTH
INNERMOST-START LAST-SEXP-START IN-STRING IN-COMMENT AFTER-QUOTE MIN-DEPTH
COMMENT-STYLE COMMENT-OR-STRING-START OPEN-PAREN-POSITIONS INTERNAL).  IN-STRING
is the opening quote character (or nil); OPEN-PAREN-POSITIONS is outermost
first.  Classification uses TABLE (default the current syntax table).  When
STATE (a value from a previous call) is given, parsing resumes from it.  When
TARGETDEPTH is non-nil scanning stops once the paren depth becomes equal to it;
when STOPBEFORE is non-nil scanning stops before the start of the next sexp.
Point is moved to the stop position (TO when neither limit fires).
Single and two-character comments, generic delimiters, comment styles,
nesting, escapes and continuation states use the raw syntax flag bits."
  (dolist (pos (list from to))
    (unless (or (integerp pos) (markerp pos))
      (signal 'wrong-type-argument (list 'integer-or-marker-p pos))))
  (when (markerp from) (setq from (marker-position from)))
  (when (markerp to) (setq to (marker-position to)))
  (when (and targetdepth (not (integerp targetdepth)))
    (signal 'wrong-type-argument (list 'fixnump targetdepth)))
  (unless (listp state)
    (signal 'wrong-type-argument (list 'listp state)))
  (let ((lo (if buffer
                (let ((nelisp-ec--current-buffer buffer)) (nelisp-ec-point-min))
              (point-min)))
        (hi (if buffer
                (let ((nelisp-ec--current-buffer buffer)) (nelisp-ec-point-max))
              (point-max))))
    (when (or (< from lo) (> from hi) (< to lo) (> to hi))
      (signal 'args-out-of-range (list (or buffer (current-buffer)) from to))))
  (let* ((tbl (or table (emacs-syntax-table-current)))
         (region (if buffer
                     (let ((nelisp-ec--current-buffer buffer))
                       (nelisp-ec-buffer-substring from (max from to)))
                   (buffer-substring-no-properties from (max from to))))
         (n (length region))
         (i 0)
         (depth (if (integerp (nth 0 state)) (nth 0 state) 0))
         (mindepth depth)
         (open (reverse (nth 9 state)))
         (instr (nth 3 state))
         (incomment (nth 4 state))
         (comment-style (nth 7 state))
         (afterq (nth 5 state))
         (last-sexp nil)
         (scstart (nth 8 state))
         (tok-start (and afterq (not instr) 'continuation))
         (string-sexp-start nil)
         (quoted-syntax (nth 10 state))
         (stop-pos (max from to))
         (done nil))
    (while (and (< i n) (not done))
      (let* ((ch (aref region i))
             (abs (+ from i))
             (entry (emacs-char-table-ref tbl ch))
             (raw (if (consp entry) (car entry) 2))
             (code (logand raw 255))
             (syn (aref emacs-syntax-table--code-spec code))
             (pending quoted-syntax)
             (style (if (/= (logand raw 2097152) 0) 1 nil))
             (start-pair (and (integerp pending)
                              (/= (logand pending 65536) 0)
                              (/= (logand raw 131072) 0)))
             (end-pair (and (integerp pending)
                            (/= (logand pending 262144) 0)
                            (/= (logand raw 524288) 0))))
        (setq quoted-syntax nil)
        (cond
         (afterq
          (setq afterq nil quoted-syntax nil)
          (unless (or instr incomment tok-start) (setq tok-start abs)))
         (instr
          (cond
           ((memq syn '(?\\ ?/)) (setq afterq t quoted-syntax
                                           (if (eq syn ?\\) 9 10)))
           ((if (eq instr t) (= code 15) (and (= code 7) (eq ch instr)))
            (setq instr nil scstart nil)
            (if (eq commentstop 'syntax-table)
                (setq stop-pos (1+ abs) done t)
              (setq last-sexp string-sexp-start)))))
         (incomment
          (cond
           ((or (and (eq comment-style 'syntax-table) (= code 14))
                (and (equal style comment-style)
                     (or (= code 12) end-pair)))
            (if (and (integerp incomment) (> incomment 1))
                (setq incomment (1- incomment))
              (setq incomment nil scstart nil comment-style nil)
              (when (eq commentstop 'syntax-table)
                (setq stop-pos (1+ abs) done t))))
           ((and (integerp incomment) (equal style comment-style)
                 (or (= code 11) start-pair))
            (setq incomment (1+ incomment)))
           ((or (/= (logand raw 262144) 0)
                (and (integerp incomment) (/= (logand raw 65536) 0)))
            (setq quoted-syntax raw))))
         (start-pair
          (when (integerp tok-start) (setq last-sexp tok-start))
          (setq tok-start nil scstart (1- abs) comment-style style
                incomment (if (/= (logand raw 4194304) 0) 1 t))
          (when commentstop (setq stop-pos (1+ abs) done t)))
         ((and stopbefore
               (or (eq syn ?\() (memq code '(7 15)) (memq syn '(?\\ ?/))
                   (and (or (eq syn ?w) (eq syn ?_)) (not tok-start))))
          (setq stop-pos abs done t))
         (t
          (cond
           ((memq syn '(?\\ ?/))
            (setq afterq t quoted-syntax (if (eq syn ?\\) 9 10))
            (unless tok-start (setq tok-start abs)))
           ((memq code '(7 15))
            (when (integerp tok-start) (setq last-sexp tok-start))
            (setq instr (if (= code 15) t ch)
                  scstart abs string-sexp-start abs tok-start nil)
            (when (eq commentstop 'syntax-table)
              (setq stop-pos (1+ abs) done t)))
           ((memq code '(11 14))
            (when (integerp tok-start) (setq last-sexp tok-start))
            (setq incomment (if (/= (logand raw 4194304) 0) 1 t)
                  comment-style (if (= code 14) 'syntax-table style)
                  scstart abs tok-start nil)
            (when commentstop (setq stop-pos (1+ abs) done t)))
           ((eq syn ?\()
            (setq depth (1+ depth) open (cons abs open)
                  last-sexp nil tok-start nil)
            (when (and targetdepth (= depth targetdepth))
              (setq stop-pos (1+ abs) done t)))
           ((eq syn ?\))
            (when (integerp tok-start) (setq last-sexp tok-start))
            (setq tok-start nil)
            (setq depth (1- depth))
            (when (< depth mindepth) (setq mindepth depth))
            (setq last-sexp (car open) open (cdr open))
            (when (and targetdepth (= depth targetdepth))
              (setq stop-pos (1+ abs) done t)))
           ((or (eq syn ?w) (eq syn ?_))
            (unless tok-start (setq tok-start abs)))
           (t
            (when (integerp tok-start) (setq last-sexp tok-start))
            (setq tok-start nil)))))
        (when (and (not instr) (not incomment) (not afterq) (not end-pair)
                   (/= (logand raw 65536) 0))
          (setq quoted-syntax raw)))
      (unless done (setq i (1+ i))))
    (when (and (integerp tok-start) (not instr) (not incomment)
               (not afterq) (not done))
      (setq last-sexp tok-start))
    (if buffer
        (let ((nelisp-ec--current-buffer buffer)) (nelisp-ec-goto-char stop-pos))
      (goto-char stop-pos))
    (list depth (car open) last-sexp instr incomment afterq mindepth
          comment-style scstart (reverse open) quoted-syntax)))

(defun emacs-syntax-table--entry-at (pos)
  "Return the raw syntax code at POS in the current buffer."
  (let ((entry (emacs-char-table-ref (emacs-syntax-table-current)
                                     (char-after pos))))
    (if (consp entry) (car entry) 2)))

(defun emacs-syntax-table--comment-start (pos limit)
  "Return (WIDTH STYLE NESTED) for a comment beginning at POS, or nil."
  (when (< pos limit)
    (let* ((raw (emacs-syntax-table--entry-at pos))
           (code (logand raw 255))
           (second (and (< (1+ pos) limit)
                        (emacs-syntax-table--entry-at (1+ pos))))
           (width (cond ((memq code '(11 14)) 1)
                        ((and (/= (logand raw 65536) 0) second
                              (/= (logand second 131072) 0)) 2))))
      (when width
        (let ((flags (if (= width 2) second raw)))
          (list width (if (= code 14) 'syntax-table
                        (/= (logand flags 2097152) 0))
                (/= (logand flags 4194304) 0)))))))

(defun emacs-syntax-table--comment-end (pos limit style)
  "Return the width of the comment terminator at POS for STYLE, or nil."
  (when (< pos limit)
    (let* ((raw (emacs-syntax-table--entry-at pos))
           (code (logand raw 255))
           (second (and (< (1+ pos) limit)
                        (emacs-syntax-table--entry-at (1+ pos)))))
      (if (eq style 'syntax-table)
          (and (= code 14) 1)
        (cond
         ((and (= code 12) (eq style (/= (logand raw 2097152) 0))) 1)
         ((and (/= (logand raw 262144) 0) second
               (/= (logand second 524288) 0)
               (eq style (/= (logand raw 2097152) 0))) 2))))))

(defun emacs-syntax-table--quoted-at-p (pos)
  "Return non-nil if POS follows an odd run of escape or character quotes."
  (let ((cursor (1- pos)) (quoted nil))
    (while (and (>= cursor (point-min))
                (memq (logand (emacs-syntax-table--entry-at cursor) 255) '(9 10)))
      (setq quoted (not quoted) cursor (1- cursor)))
    quoted))

(defun emacs-syntax-table--skip-comment (pos limit start &optional backward)
  "Scan the comment START at POS; return (END . COMPLETE).
BACKWARD supplies the target point for GNU's backward fence matching."
  (let ((cursor (+ pos (car start))) (depth 1) (style (nth 1 start))
        (nested (nth 2 start)))
    (while (and (< cursor limit) (> depth 0))
      (let* ((quoted-fence
              (and backward (eq style 'syntax-table)
                   (/= (1+ cursor) backward)
                   (emacs-syntax-table--quoted-at-p cursor)))
             (end (and (not quoted-fence)
                       (emacs-syntax-table--comment-end cursor limit style)))
             (inner (and nested
                         (emacs-syntax-table--comment-start cursor limit))))
        (cond
         (end (setq depth (1- depth) cursor (+ cursor end)))
         ((and inner (eq style (nth 1 inner)) (nth 2 inner))
          (setq depth (1+ depth) cursor (+ cursor (car inner))))
         (t (setq cursor (1+ cursor))))))
    (cons cursor (= depth 0))))

(defun emacs-syntax-table-forward-comment (count)
  "Skip COUNT comments in the current buffer, returning t on completion.
Use the current syntax table, including paired, fenced, styled and nested
comments.  Whitespace is skipped only while looking for the next comment."
  (unless (integerp count)
    (signal 'wrong-type-argument (list 'fixnump count)))
  (let ((remaining (abs count)) (pos (point)) (lo (point-min))
        (hi (point-max)) (done nil))
    (while (and (> remaining 0) (not done))
      (if (>= count 0)
          (progn
            (while (and (< pos hi)
                        (memq (logand (emacs-syntax-table--entry-at pos) 255)
                              '(0 12)))
              (setq pos (1+ pos)))
            (let ((start (emacs-syntax-table--comment-start pos hi)))
              (if (not start) (setq done t)
                (let ((result (emacs-syntax-table--skip-comment pos hi start)))
                  (setq pos (car result))
                  (if (cdr result) (setq remaining (1- remaining))
                    (setq done t))))))
        ;; Find a completed comment ending at point or in its trailing
        ;; whitespace.  GNU backward motion can begin inside a string or a
        ;; nested comment, so do not exclude candidates based on outer parse
        ;; state.  Quoted delimiters are skipped during the scan.
        (let ((right pos) (cursor lo) candidate)
          (while (and (> pos lo)
                      (memq (logand (emacs-syntax-table--entry-at (1- pos)) 255)
                            '(0 12))
                      (or (= (logand (emacs-syntax-table--entry-at (1- pos)) 255) 12)
                          (not (emacs-syntax-table--quoted-at-p (1- pos)))))
            (setq pos (1- pos)))
          (while (< cursor right)
            (let ((start (emacs-syntax-table--comment-start cursor hi))
                  (code (logand (emacs-syntax-table--entry-at cursor) 255)))
              (cond
               (start
                (let ((result (emacs-syntax-table--skip-comment cursor hi start right)))
                  (when (and (cdr result) (>= (car result) pos)
                             (<= (car result) right)
                             (not (and (>= (- (car result) 2) cursor)
                                       (eq (emacs-syntax-table--comment-end
                                            (- (car result) 2) hi (nth 1 start)) 2)
                                       (emacs-syntax-table--quoted-at-p
                                        (- (car result) 2)))))
                    (setq candidate cursor))
                  (setq cursor (if (or (not (cdr result)) (> (car result) right))
                                   (1+ cursor) (car result)))))
               ((memq code '(9 10)) (setq cursor (+ cursor 2)))
               ((memq code '(7 15))
                (let ((end (1+ cursor)) (fence (char-after cursor)))
                  (while (and (< end right)
                              (not (if (= code 15)
                                       (= (logand (emacs-syntax-table--entry-at end) 255) 15)
                                     (eq (char-after end) fence))))
                    (setq end (+ end
                                 (if (memq (logand (emacs-syntax-table--entry-at end) 255)
                                           '(9 10)) 2 1))))
                  (setq cursor (if (< end right) (1+ end) (1+ cursor)))))
               (t (setq cursor (1+ cursor))))))
          (if candidate (setq pos candidate remaining (1- remaining))
            (setq done t)))))
    (goto-char pos)
    (= remaining 0)))

(when (emacs-syntax-table--install-function-p 'forward-comment)
  (defalias 'forward-comment #'emacs-syntax-table-forward-comment))

(when (emacs-syntax-table--install-function-p 'syntax-table-p)
  (defun syntax-table-p (object)
    "Return t for a syntax char-table, including bootstrap syntax records."
    (and (emacs-char-table-p object)
         (eq (emacs-char-table-subtype object) 'syntax-table))))

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

(when (emacs-syntax-table--install-function-p 'copy-syntax-table)
  (defun copy-syntax-table (&optional table)
    "Return a copy of TABLE, or of the standard syntax table."
    (let ((source (or table (emacs-syntax-table-standard))))
      (emacs-syntax-table--check-table source)
      (emacs-char-table-copy source))))

(unless (boundp 'emacs-lisp-mode-syntax-table)
  ;; Approximation sufficient for url.el consumers until a vendored
  ;; elisp-mode syntax table is baked into the standalone runtime.
  (defvar emacs-lisp-mode-syntax-table
    (let ((table (make-syntax-table)))
      (modify-syntax-entry ?- "_" table)
      (modify-syntax-entry ?+ "_" table)
      (modify-syntax-entry ?* "_" table)
      (modify-syntax-entry ?/ "_" table)
      (modify-syntax-entry ?_ "_" table)
      (modify-syntax-entry ?~ "_" table)
      (modify-syntax-entry ?! "_" table)
      (modify-syntax-entry ?$ "_" table)
      (modify-syntax-entry ?% "_" table)
      (modify-syntax-entry ?^ "_" table)
      (modify-syntax-entry ?& "_" table)
      (modify-syntax-entry ?= "_" table)
      (modify-syntax-entry ?< "_" table)
      (modify-syntax-entry ?> "_" table)
      (modify-syntax-entry ?? "_" table)
      (modify-syntax-entry ?@ "_" table)
      (modify-syntax-entry ?\; "<" table)
      (modify-syntax-entry ?\n ">" table)
      (modify-syntax-entry ?` "'" table)
      (modify-syntax-entry ?' "'" table)
      (modify-syntax-entry ?, "'" table)
      (modify-syntax-entry ?# "'" table)
      table)
    "Approximate syntax table for Emacs Lisp mode in standalone runtime."))

(when (and (emacs-syntax-table--standalone-p)
           (emacs-char-table--native-syntax-p emacs-lisp-mode-syntax-table))
  ;; The mode retains a pre-bundle table.  Expose its sparse view so ordinary
  ;; array access uses the native vector bridge, preserving all local entries.
  (setq emacs-lisp-mode-syntax-table
        (emacs-char-table--storage emacs-lisp-mode-syntax-table)))

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
    "Make TABLE the current buffer's syntax char-table and return TABLE.
Signal `wrong-type-argument' unless TABLE has syntax-table subtype."
    (emacs-syntax-table--check-table table)
    (emacs-syntax-table-set-current table)))

(when (emacs-syntax-table--install-function-p 'with-syntax-table)
  (defmacro with-syntax-table (table &rest body)
    "Evaluate BODY with TABLE as the current syntax char-table."
    (declare (indent 1) (debug t))
    `(let ((emacs-syntax-table--current ,table)) ,@body)))

(when (emacs-syntax-table--install-function-p 'parse-partial-sexp)
  (defun parse-partial-sexp (from to &optional targetdepth stopbefore
                                  state commentstop)
    "Parse the current buffer from FROM to TO, returning a syntactic state.
STATE resumes from a previous call; TARGETDEPTH and STOPBEFORE bound the scan
and move point to the stop position.  COMMENTSTOP is accepted for call
compatibility and stops at comment boundaries."
    (emacs-syntax-table-parse-partial-sexp
     from to nil nil state targetdepth stopbefore commentstop)))

(provide 'emacs-syntax-table)

;;; emacs-syntax-table.el ends here
