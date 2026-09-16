;;; nelisp-toml.el --- Reader for the project TOML subset -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A pure Lisp TOML reader for the subset of TOML v1.0 that this
;; repository's own project files (`nelisp.toml', `nelisp.lock', and
;; their test fixtures under `test/') actually use.  See README.org for
;; exactly which constructs that subset contains and which do not, and
;; why: this package is deliberately not a general TOML implementation.
;;
;; Everything outside the supported subset -- floats, booleans, dates
;; and times, inline tables, multi-line strings, hex/octal/binary
;; integers, underscore digit separators, dotted keys outside a table
;; header -- is refused with `nelisp-toml-unsupported-error', naming the
;; line and the reason.  Anything else that is not valid TOML at all is
;; refused with `nelisp-toml-parse-error', same shape.  Neither ever
;; returns a silent nil or a partially built table: parsing a document
;; either fully succeeds or signals before returning anything.
;;
;; No writer/serializer: see README.org "Writer/serializer: out of
;; scope" for why. `tools/nelisp_project_manifest.py' keeps doing
;; surgical manifest edits in Python; this package does not replace it.
;;
;; A parsed document is a tree of hash tables (`equal' keys, matching
;; `nelisp-json''s "object default = hash-table" convention) and lists
;; (TOML arrays, and array-of-tables values).  See `nelisp-toml-get' for
;; a small nested-lookup convenience.

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-toml-error "NeLisp TOML error")
(define-error 'nelisp-toml-parse-error "NeLisp TOML parse error" 'nelisp-toml-error)
(define-error 'nelisp-toml-unsupported-error
  "NeLisp TOML construct outside the supported subset" 'nelisp-toml-error)

(defconst nelisp-toml--absent (make-symbol "nelisp-toml--absent")
  "Sentinel distinguishing \"key absent\" from \"key present with value nil\".")

;;;; Errors ----------------------------------------------------------------

(defun nelisp-toml--line-at (string pos)
  "Return the 1-based line number of POS within STRING."
  (let ((line 1) (i 0) (n (min pos (length string))))
    (while (< i n)
      (when (= (aref string i) ?\n) (setq line (1+ line)))
      (setq i (1+ i)))
    line))

(defun nelisp-toml--signal (kind string pos format-string args)
  "Signal KIND with a \"line N: reason\" message built from POS in STRING."
  (let ((line (nelisp-toml--line-at string pos)))
    (signal kind (list (format "line %d: %s" line (apply #'format format-string args))
                        line pos))))

(defun nelisp-toml--parse-error (string pos format-string &rest args)
  "Signal `nelisp-toml-parse-error' at POS in STRING."
  (nelisp-toml--signal 'nelisp-toml-parse-error string pos format-string args))

(defun nelisp-toml--unsupported (string pos format-string &rest args)
  "Signal `nelisp-toml-unsupported-error' at POS in STRING."
  (nelisp-toml--signal 'nelisp-toml-unsupported-error string pos format-string args))

;;;; Character classes -------------------------------------------------------

(defun nelisp-toml--line-ws-p (ch)
  "Return non-nil if CH is TOML intra-line whitespace (space or tab)."
  (memq ch '(?\s ?\t)))

(defun nelisp-toml--bare-key-char-p (ch)
  "Return non-nil if CH may appear in a bare (unquoted) key."
  (or (and (>= ch ?a) (<= ch ?z))
      (and (>= ch ?A) (<= ch ?Z))
      (and (>= ch ?0) (<= ch ?9))
      (= ch ?_) (= ch ?-)))

(defun nelisp-toml--bare-value-char-p (ch)
  "Return non-nil if CH may appear in a bare (unquoted) value token.
Covers every character this subset's bare values use: digits, sign,
decimal point, and the letters/`:'/`-' that distinguish -- and let us
name -- the unsupported float/bool/datetime shapes."
  (or (and (>= ch ?a) (<= ch ?z))
      (and (>= ch ?A) (<= ch ?Z))
      (and (>= ch ?0) (<= ch ?9))
      (memq ch '(?+ ?- ?. ?:))))

;;;; Whitespace and comments -------------------------------------------------

(defun nelisp-toml--skip-line-ws (string pos)
  "Skip spaces and tabs in STRING from POS.  Return the new position."
  (let ((len (length string)))
    (while (and (< pos len) (nelisp-toml--line-ws-p (aref string pos)))
      (setq pos (1+ pos)))
    pos))

(defun nelisp-toml--skip-to-eol (string pos)
  "Skip trailing whitespace and an optional comment, then require EOL/EOF.
Accepts a bare LF or a CRLF pair as the line ending -- CRLF-saved
manifests are one of the shapes `tools/nelisp_project_manifest.py'
explicitly preserves (its own CRLF round-trip test).  Return the
position after the newline, or at end of string."
  (setq pos (nelisp-toml--skip-line-ws string pos))
  (let ((len (length string)))
    (when (and (< pos len) (= (aref string pos) ?#))
      (while (and (< pos len) (/= (aref string pos) ?\n))
        (setq pos (1+ pos))))
    (cond
     ((>= pos len) pos)
     ((and (= (aref string pos) ?\r) (< (1+ pos) len) (= (aref string (1+ pos)) ?\n))
      (+ pos 2))
     ((= (aref string pos) ?\n) (1+ pos))
     (t (nelisp-toml--parse-error string pos "expected end of line")))))

(defun nelisp-toml--skip-blank (string pos)
  "Skip whitespace, blank lines, and whole-line comments between entries."
  (let ((len (length string)))
    (catch 'done
      (while t
        (setq pos (nelisp-toml--skip-line-ws string pos))
        (cond
         ((>= pos len) (throw 'done pos))
         ((memq (aref string pos) '(?\n ?\r)) (setq pos (1+ pos)))
         ((= (aref string pos) ?#)
          (while (and (< pos len) (/= (aref string pos) ?\n))
            (setq pos (1+ pos))))
         (t (throw 'done pos)))))))

(defun nelisp-toml--skip-array-ws (string pos)
  "Skip whitespace, newlines, and comments inside an array's brackets."
  (let ((len (length string)))
    (catch 'done
      (while t
        (cond
         ((and (< pos len) (memq (aref string pos) '(?\s ?\t ?\n ?\r)))
          (setq pos (1+ pos)))
         ((and (< pos len) (= (aref string pos) ?#))
          (while (and (< pos len) (/= (aref string pos) ?\n))
            (setq pos (1+ pos))))
         (t (throw 'done pos)))))))

;;;; Strings (basic and literal; also used for quoted keys) ------------------

(defun nelisp-toml--hex-digit-value (char)
  "Return the numeric value of hexadecimal CHAR, or nil if invalid."
  (cond
   ((and (>= char ?0) (<= char ?9)) (- char ?0))
   ((and (>= char ?a) (<= char ?f)) (+ 10 (- char ?a)))
   ((and (>= char ?A) (<= char ?F)) (+ 10 (- char ?A)))
   (t nil)))

(defun nelisp-toml--parse-hex-digits (string pos count)
  "Parse COUNT hex digits from STRING at POS.  Return (CODEPOINT . NEXT-POS)."
  (let ((value 0) (idx pos) (len (length string)) digit)
    (dotimes (_ count)
      (when (>= idx len)
        (nelisp-toml--parse-error string idx "truncated unicode escape"))
      (setq digit (nelisp-toml--hex-digit-value (aref string idx)))
      (unless digit
        (nelisp-toml--parse-error string idx "invalid unicode escape"))
      (setq value (+ (* value 16) digit))
      (setq idx (1+ idx)))
    (cons value idx)))

(defun nelisp-toml--parse-basic-string (string pos)
  "Parse a TOML basic string in STRING at POS (the opening quote).
Return (VALUE . NEXT-POS)."
  (let ((len (length string)))
    (when (and (< (+ pos 2) len)
               (eql (aref string (1+ pos)) ?\")
               (eql (aref string (+ pos 2)) ?\"))
      (nelisp-toml--unsupported
       string pos "multi-line basic strings (triple-quoted) are not supported"))
    (let ((idx (1+ pos)) (chunks nil) start ch escape info)
      (catch 'done
        (while t
          (setq start idx)
          (while (and (< idx len)
                      (not (memq (aref string idx) '(?\" ?\\ ?\n ?\r))))
            (setq idx (1+ idx)))
          (when (> idx start)
            (push (substring string start idx) chunks))
          (when (>= idx len)
            (nelisp-toml--parse-error string idx "unterminated string"))
          (setq ch (aref string idx))
          (cond
           ((eql ch ?\")
            (throw 'done (cons (apply #'concat (nreverse chunks)) (1+ idx))))
           ((memq ch '(?\n ?\r))
            (nelisp-toml--parse-error
             string idx "basic strings cannot contain a literal newline"))
           (t ;; backslash escape
            (setq idx (1+ idx))
            (when (>= idx len)
              (nelisp-toml--parse-error string idx "truncated escape sequence"))
            (setq escape (aref string idx))
            (push
             (pcase escape
               (?\" "\"") (?\\ "\\") (?b "\b") (?f "\f")
               (?n "\n") (?r "\r") (?t "\t")
               (?u (setq info (nelisp-toml--parse-hex-digits string (1+ idx) 4))
                   (setq idx (1- (cdr info)))
                   (char-to-string (car info)))
               (?U (setq info (nelisp-toml--parse-hex-digits string (1+ idx) 8))
                   (setq idx (1- (cdr info)))
                   (char-to-string (car info)))
               (_ (nelisp-toml--parse-error string idx
                                            "invalid escape character %c" escape)))
             chunks)
            (setq idx (1+ idx)))))))))

(defun nelisp-toml--parse-literal-string (string pos)
  "Parse a TOML literal string in STRING at POS (the opening quote).
Return (VALUE . NEXT-POS)."
  (let ((len (length string)))
    (when (and (< (+ pos 2) len)
               (eql (aref string (1+ pos)) ?\')
               (eql (aref string (+ pos 2)) ?\'))
      (nelisp-toml--unsupported
       string pos "multi-line literal strings (triple-quoted) are not supported"))
    (let ((idx (1+ pos)))
      (while (and (< idx len) (not (memq (aref string idx) '(?\' ?\n ?\r))))
        (setq idx (1+ idx)))
      (when (or (>= idx len) (not (eql (aref string idx) ?\')))
        (nelisp-toml--parse-error string idx "unterminated literal string"))
      (cons (substring string (1+ pos) idx) (1+ idx)))))

;;;; Keys ---------------------------------------------------------------------

(defun nelisp-toml--parse-key (string pos)
  "Parse one bare or quoted key in STRING at POS.  Return (KEY . NEXT-POS)."
  (let ((len (length string)))
    (when (>= pos len)
      (nelisp-toml--parse-error string pos "expected a key"))
    (cond
     ((= (aref string pos) ?\") (nelisp-toml--parse-basic-string string pos))
     ((= (aref string pos) ?\') (nelisp-toml--parse-literal-string string pos))
     ((nelisp-toml--bare-key-char-p (aref string pos))
      (let ((start pos))
        (while (and (< pos len) (nelisp-toml--bare-key-char-p (aref string pos)))
          (setq pos (1+ pos)))
        (cons (substring string start pos) pos)))
     (t (nelisp-toml--parse-error string pos "invalid key")))))

(defun nelisp-toml--parse-header-path (string pos)
  "Parse a dotted key path (for a `[...]'/`[[...]]' header) at POS.
Return (PATH . NEXT-POS), PATH a list of key strings, NEXT-POS just
before the closing `]'."
  (let (path parsed)
    (setq pos (nelisp-toml--skip-line-ws string pos))
    (setq parsed (nelisp-toml--parse-key string pos))
    (push (car parsed) path)
    (setq pos (nelisp-toml--skip-line-ws string (cdr parsed)))
    (while (and (< pos (length string)) (= (aref string pos) ?.))
      (setq pos (nelisp-toml--skip-line-ws string (1+ pos)))
      (setq parsed (nelisp-toml--parse-key string pos))
      (push (car parsed) path)
      (setq pos (nelisp-toml--skip-line-ws string (cdr parsed))))
    (cons (nreverse path) pos)))

;;;; Values ---------------------------------------------------------------

(defconst nelisp-toml--integer-regexp "\\`[+-]?[0-9]+\\'")
(defconst nelisp-toml--float-regexp
  "\\`[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?\\'")

(defun nelisp-toml--parse-bare-token (string pos)
  "Consume one bare value token from STRING at POS.  Return (TEXT . NEXT-POS)."
  (let ((len (length string)) (start pos))
    (while (and (< pos len) (nelisp-toml--bare-value-char-p (aref string pos)))
      (setq pos (1+ pos)))
    (cons (substring string start pos) pos)))

(defun nelisp-toml--parse-bare-value (string pos)
  "Parse the unquoted, unbracketed value at POS: an integer, or a named
refusal for every other bare TOML value shape this subset excludes."
  (let* ((token-info (nelisp-toml--parse-bare-token string pos))
         (token (car token-info))
         (next (cdr token-info)))
    (when (= (length token) 0)
      (nelisp-toml--parse-error string pos "unexpected character %c in value position"
                                (aref string pos)))
    (when (and (< next (length string)) (= (aref string next) ?_))
      (nelisp-toml--unsupported
       string next "underscore digit separators in numbers are not supported"))
    (cond
     ((string-match-p nelisp-toml--integer-regexp token)
      (let* ((signed (memq (aref token 0) '(?+ ?-)))
             (digits (if signed (substring token 1) token)))
        (when (= (length digits) 0)
          (nelisp-toml--parse-error string pos "invalid integer %S" token))
        (when (and (> (length digits) 1) (= (aref digits 0) ?0))
          (nelisp-toml--parse-error string pos "leading zeros are not allowed in integers"))
        (cons (string-to-number token) next)))
     ((member token '("true" "false"))
      (nelisp-toml--unsupported string pos "booleans are not supported"))
     ((member token '("inf" "+inf" "-inf" "nan" "+nan" "-nan"))
      (nelisp-toml--unsupported string pos "special float values are not supported"))
     ((string-match-p nelisp-toml--float-regexp token)
      (nelisp-toml--unsupported string pos "floating-point numbers are not supported"))
     ((string-match-p "\\`0[xXoObB]" token)
      (nelisp-toml--unsupported
       string pos "hexadecimal, octal, and binary integers are not supported"))
     ((string-match-p "\\`[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\|:" token)
      (nelisp-toml--unsupported string pos "dates and times are not supported"))
     (t (nelisp-toml--parse-error string pos "unsupported or invalid value %S" token)))))

(defun nelisp-toml--parse-array (string pos)
  "Parse a TOML array in STRING at POS (the opening `[').
Return (VALUE . NEXT-POS), VALUE a Lisp list in source order."
  (let ((idx (nelisp-toml--skip-array-ws string (1+ pos)))
        (len (length string))
        (items nil) parsed)
    (if (and (< idx len) (= (aref string idx) ?\]))
        (cons nil (1+ idx))
      (catch 'done
        (while t
          (setq parsed (nelisp-toml--parse-value string idx))
          (push (car parsed) items)
          (setq idx (nelisp-toml--skip-array-ws string (cdr parsed)))
          (when (>= idx len)
            (nelisp-toml--parse-error string idx "unterminated array"))
          (cond
           ((= (aref string idx) ?,)
            (setq idx (nelisp-toml--skip-array-ws string (1+ idx)))
            (when (and (< idx len) (= (aref string idx) ?\]))
              (throw 'done (cons (nreverse items) (1+ idx)))))
           ((= (aref string idx) ?\])
            (throw 'done (cons (nreverse items) (1+ idx))))
           (t (nelisp-toml--parse-error string idx "expected ',' or ']' in array"))))))))

(defun nelisp-toml--parse-value (string pos)
  "Parse one TOML value in STRING at POS.  Return (VALUE . NEXT-POS)."
  (when (>= pos (length string))
    (nelisp-toml--parse-error string pos "expected a value"))
  (let ((ch (aref string pos)))
    (cond
     ((= ch ?\") (nelisp-toml--parse-basic-string string pos))
     ((= ch ?\') (nelisp-toml--parse-literal-string string pos))
     ((= ch ?\[) (nelisp-toml--parse-array string pos))
     ((= ch ?\{) (nelisp-toml--unsupported string pos "inline tables are not supported"))
     (t (nelisp-toml--parse-bare-value string pos)))))

;;;; Tables and headers ---------------------------------------------------

(defun nelisp-toml--table-navigate (table key string pos)
  "Return the table reached from TABLE via KEY, an intermediate header
segment.  Creates an empty table when KEY is absent; descends into the
last element when KEY names an array of tables (dotted-header
semantics); signals on anything else."
  (let ((existing (gethash key table nelisp-toml--absent)))
    (cond
     ((eq existing nelisp-toml--absent)
      (let ((new (make-hash-table :test 'equal)))
        (puthash key new table)
        new))
     ((hash-table-p existing) existing)
     ((and (consp existing) (hash-table-p (car (last existing))))
      (car (last existing)))
     (t (nelisp-toml--parse-error string pos "%s is not a table" key)))))

(defun nelisp-toml--resolve-header (root path string pos is-array)
  "Return the table a `[PATH]' or `[[PATH]]' header (IS-ARRAY) selects,
creating tables (and, for an array header, appending one) as needed."
  (let ((table root) (n (length path)) (i 0))
    (while (< i (1- n))
      (setq table (nelisp-toml--table-navigate table (nth i path) string pos))
      (setq i (1+ i)))
    (let* ((key (nth (1- n) path))
           (existing (gethash key table nelisp-toml--absent)))
      (if is-array
          (cond
           ((eq existing nelisp-toml--absent)
            (let ((new (make-hash-table :test 'equal)))
              (puthash key (list new) table)
              new))
           ((and (consp existing) (hash-table-p (car existing)))
            (let ((new (make-hash-table :test 'equal)))
              (puthash key (append existing (list new)) table)
              new))
           (t (nelisp-toml--parse-error
               string pos "%s is already defined and is not an array of tables" key)))
        (if (eq existing nelisp-toml--absent)
            (let ((new (make-hash-table :test 'equal)))
              (puthash key new table)
              new)
          (nelisp-toml--parse-error string pos "%s is already defined" key))))))

;;;; Top-level document -----------------------------------------------------

;;;###autoload
(defun nelisp-toml-parse-string (string)
  "Parse STRING as TOML and return a nested hash-table/list document.

Supported subset only; see README.org.  Signals
`nelisp-toml-parse-error' on malformed input and
`nelisp-toml-unsupported-error' on a valid TOML construct outside the
supported subset.  Both carry a \"line N: reason\" message, the line
number, and the character position; nothing is ever silently dropped
or partially built."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (let ((root (make-hash-table :test 'equal))
        (current nil)
        (pos 0)
        (len (length string)))
    (setq current root)
    (setq pos (nelisp-toml--skip-blank string pos))
    (while (< pos len)
      (if (= (aref string pos) ?\[)
          (let* ((is-array (and (< (1+ pos) len) (= (aref string (1+ pos)) ?\[)))
                 (start (if is-array (+ pos 2) (1+ pos)))
                 (path-info (nelisp-toml--parse-header-path string start))
                 (path (car path-info))
                 (after (cdr path-info)))
            (when (= (length path) 0)
              (nelisp-toml--parse-error string pos "empty table header"))
            (unless (and (< after len) (= (aref string after) ?\]))
              (nelisp-toml--parse-error string after "expected ']'"))
            (setq after (1+ after))
            (when is-array
              (unless (and (< after len) (= (aref string after) ?\]))
                (nelisp-toml--parse-error string after "expected ']]'"))
              (setq after (1+ after)))
            (setq current (nelisp-toml--resolve-header root path string pos is-array))
            (setq pos (nelisp-toml--skip-to-eol string after)))
        (let* ((key-info (nelisp-toml--parse-key string pos))
               (key (car key-info))
               (after (nelisp-toml--skip-line-ws string (cdr key-info))))
          (when (and (< after len) (= (aref string after) ?.))
            (nelisp-toml--unsupported
             string after "dotted keys are not supported outside table headers"))
          (unless (and (< after len) (= (aref string after) ?=))
            (nelisp-toml--parse-error string after "expected '=' after key"))
          (setq after (nelisp-toml--skip-line-ws string (1+ after)))
          (let* ((value-info (nelisp-toml--parse-value string after))
                 (value (car value-info))
                 (next (cdr value-info)))
            (unless (eq (gethash key current nelisp-toml--absent) nelisp-toml--absent)
              (nelisp-toml--parse-error string pos "duplicate key %s" key))
            (puthash key value current)
            (setq pos (nelisp-toml--skip-to-eol string next)))))
      (setq pos (nelisp-toml--skip-blank string pos)))
    root))

;;;###autoload
(defun nelisp-toml-parse-file (file)
  "Read FILE and parse it as TOML.  See `nelisp-toml-parse-string'."
  (with-temp-buffer
    (insert-file-contents file)
    (nelisp-toml-parse-string (buffer-string))))

;;;###autoload
(defun nelisp-toml-get (table &rest keys)
  "Look up nested KEYS in TABLE, a document from `nelisp-toml-parse-string'.
Return nil if any component of the path is missing or is not a table
to descend into -- this never signals, unlike `nelisp-toml-parse-string'
itself, so it can be used for optional keys without extra guarding."
  (let ((value table))
    (dolist (key keys value)
      (setq value (if (hash-table-p value) (gethash key value) nil)))))

(provide 'nelisp-toml)

;;; nelisp-toml.el ends here
