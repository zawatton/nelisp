;;; json.el --- Native JSON encode/decode for NeLisp standalone -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Minimal native JSON support that shadows the vendored upstream
;; `json.el' on NeLisp standalone load-path.  The upstream file pulls
;; `map.el' / `subr-x' / `macroexp' / top-level `throw' chains that
;; the standalone runtime cannot satisfy at bootstrap, so we provide
;; just the surface anvil.el modules use:
;;
;;   `json-encode'                — main serializer
;;   `json-read-from-string'      — recursive-descent parser, alist
;;                                  with symbol keys (= upstream
;;                                  default), int / float / string /
;;                                  list / nested object support
;;   `json-encoding-pretty-print' — defvar, controls indentation (no-op
;;                                  in this minimal port; emit compact)
;;   `json-false' / `json-null'   — sentinels (`:json-false' / `:json-null'
;;                                  to distinguish JSON `false' / `null'
;;                                  from elisp `nil')
;;
;; The parser is a single-pass recursive-descent reader operating on
;; the input string with a fluid-bound position cursor.  Errors
;; signal `json-error' so callers (e.g. `anvil-server-process-jsonrpc')
;; can `condition-case' on parse failures.

;;; Code:

(defvar json-encoding-pretty-print nil
  "Non-nil indents encoded JSON across multiple lines.
Currently a no-op in the standalone port — output is always compact.")

(defvar json-encoding-default-indentation "  "
  "String used for one indentation level when pretty-printing.")

(defvar json-false :json-false
  "Value used to represent JSON `false' on input/output.")

(defvar json-null :json-null
  "Value used to represent JSON `null' on input/output (default = nil).")

(defun emacs-json--escape-string (s)
  "Return JSON-escaped representation of string S (no surrounding quotes)."
  (let ((out "")
        (i 0)
        (len (length s)))
    (while (< i len)
      (let ((c (aref s i)))
        (cond
         ((eq c ?\")  (setq out (concat out "\\\"")))
         ((eq c ?\\)  (setq out (concat out "\\\\")))
         ((eq c ?\n)  (setq out (concat out "\\n")))
         ((eq c ?\r)  (setq out (concat out "\\r")))
         ((eq c ?\t)  (setq out (concat out "\\t")))
         ((eq c ?\b)  (setq out (concat out "\\b")))
         ((eq c ?\f)  (setq out (concat out "\\f")))
         ((< c 32)    (setq out (concat out (format "\\u%04x" c))))
         (t           (setq out (concat out (char-to-string c))))))
      (setq i (1+ i)))
    out))

(defun emacs-json--encode-string (s)
  (concat "\"" (emacs-json--escape-string s) "\""))

(defun emacs-json--alist-p (lst)
  "Non-nil if LST is a proper alist (= every element is a cons cell).

Phase B5 fix (= 2026-05-09): the earlier check additionally required
`(not (consp (cdr (car cur))))', i.e. that no entry's value was itself
a list.  That rejected nested-object alists like
`((result . ((protocolVersion . \"...\"))))', forcing them through the
generic `listp' encoder which mapconcat-ed the dotted pair and tripped
with `(wrong-type-argument listp \"2.0\")' deep inside JSON-RPC
responses.  An alist may legitimately have list-valued cdrs."
  (and (listp lst) lst
       (let ((all-cons t)
             (cur lst))
         (while (and cur all-cons)
           (unless (consp (car cur))
             (setq all-cons nil))
           (setq cur (cdr cur)))
         all-cons)))

(defun emacs-json--plist-p (lst)
  "Non-nil if LST is a plist with keyword keys."
  (and (listp lst) lst
       (let ((ok t)
             (cur lst))
         (while (and cur ok)
           (unless (and (keywordp (car cur)) (consp (cdr cur)))
             (setq ok nil))
           (setq cur (and ok (cddr cur))))
         ok)))

(defun emacs-json--key-string (k)
  (cond ((stringp k) k)
        ((symbolp k) (let ((n (symbol-name k)))
                       (if (and (> (length n) 0) (eq (aref n 0) ?:))
                           (substring n 1)
                         n)))
        (t (format "%s" k))))

(defun emacs-json--encode-pairs (pairs)
  "Encode list of (key . value) PAIRS as a JSON object."
  (let ((parts nil))
    (dolist (p pairs)
      (push (concat (emacs-json--encode-string
                     (emacs-json--key-string (car p)))
                    ":"
                    (json-encode (cdr p)))
            parts))
    (concat "{" (mapconcat 'identity (nreverse parts) ",") "}")))

(defun emacs-json--plist-to-pairs (plist)
  (let ((acc nil))
    (while plist
      (push (cons (car plist) (cadr plist)) acc)
      (setq plist (cddr plist)))
    (nreverse acc)))

(defun emacs-json--encode-array (vec-or-list)
  (let ((items (if (vectorp vec-or-list)
                   (append vec-or-list nil)
                 vec-or-list)))
    (concat "["
            (mapconcat (lambda (x) (json-encode x)) items ",")
            "]")))

(defun emacs-json--encode-hash-table (h)
  "Encode hash-table H as a JSON object.
Empty hash → `{}'.  Iteration order follows `maphash' (= insertion
order on standalone NeLisp).  This is the canonical path for the
empty `(make-hash-table)' values that anvil-server uses to mean
empty JSON object in initialize / capabilities responses."
  (let ((pairs nil))
    (maphash (lambda (k v) (push (cons k v) pairs)) h)
    (emacs-json--encode-pairs (nreverse pairs))))

(defun json-encode (object)
  "Return a JSON representation of OBJECT as a string."
  (cond
   ((null object) "null")
   ((eq object t) "true")
   ((eq object json-false) "false")
   ((eq object json-null) "null")
   ((numberp object) (number-to-string object))
   ((stringp object) (emacs-json--encode-string object))
   ((keywordp object)
    (emacs-json--encode-string (substring (symbol-name object) 1)))
   ((symbolp object) (emacs-json--encode-string (symbol-name object)))
   ((vectorp object) (emacs-json--encode-array object))
   ((hash-table-p object) (emacs-json--encode-hash-table object))
   ((emacs-json--alist-p object)
    (emacs-json--encode-pairs object))
   ((emacs-json--plist-p object)
    (emacs-json--encode-pairs (emacs-json--plist-to-pairs object)))
   ((listp object) (emacs-json--encode-array object))
   (t (error "json-encode: unsupported type for %S" object))))

;;; ---- JSON reader (Phase B2) ------------------------------------

(defvar emacs-json--read-pos 0
  "Current position in the input being parsed.  Fluid-bound by
`json-read-from-string'; set to 0 before each parse.")

(defvar emacs-json--read-src ""
  "Input string currently being parsed.  Fluid-bound by
`json-read-from-string'.")

(define-error 'json-error "JSON parsing error")

(defun emacs-json--read-error (fmt &rest args)
  (signal 'json-error
          (list (apply #'format fmt args)
                emacs-json--read-pos)))

(defun emacs-json--read-peek ()
  (when (< emacs-json--read-pos (length emacs-json--read-src))
    (aref emacs-json--read-src emacs-json--read-pos)))

(defun emacs-json--read-bump ()
  (setq emacs-json--read-pos (1+ emacs-json--read-pos)))

(defun emacs-json--read-eof-p ()
  (>= emacs-json--read-pos (length emacs-json--read-src)))

(defun emacs-json--read-skip-ws ()
  (while (and (not (emacs-json--read-eof-p))
              (let ((c (emacs-json--read-peek)))
                (or (eq c ?\s) (eq c ?\t) (eq c ?\n) (eq c ?\r))))
    (emacs-json--read-bump)))

(defun emacs-json--read-expect (ch)
  (unless (eq (emacs-json--read-peek) ch)
    (emacs-json--read-error "expected %c, got %S" ch (emacs-json--read-peek)))
  (emacs-json--read-bump))

(defun emacs-json--read-keyword (word value)
  "Match literal WORD at cursor; return VALUE on success."
  (let ((len (length word))
        (start emacs-json--read-pos))
    (when (> (+ start len) (length emacs-json--read-src))
      (emacs-json--read-error "unexpected EOF reading %s" word))
    (unless (string= word
                     (substring emacs-json--read-src start (+ start len)))
      (emacs-json--read-error "expected %s" word))
    (setq emacs-json--read-pos (+ start len))
    value))

(defun emacs-json--read-string ()
  "Read a JSON string at cursor, return elisp string."
  (emacs-json--read-expect ?\")
  (let ((acc "")
        (done nil))
    (while (not done)
      (when (emacs-json--read-eof-p)
        (emacs-json--read-error "unterminated string"))
      (let ((c (emacs-json--read-peek)))
        (cond
         ((eq c ?\") (emacs-json--read-bump) (setq done t))
         ((eq c ?\\)
          (emacs-json--read-bump)
          (when (emacs-json--read-eof-p)
            (emacs-json--read-error "trailing backslash in string"))
          (let ((esc (emacs-json--read-peek)))
            (emacs-json--read-bump)
            (setq acc
                  (concat acc
                          (cond
                           ((eq esc ?\") "\"")
                           ((eq esc ?\\) "\\")
                           ((eq esc ?/)  "/")
                           ((eq esc ?n)  "\n")
                           ((eq esc ?t)  "\t")
                           ((eq esc ?r)  "\r")
                           ((eq esc ?b)  "\b")
                           ((eq esc ?f)  "\f")
                           ((eq esc ?u)
                            (when (> (+ emacs-json--read-pos 4)
                                     (length emacs-json--read-src))
                              (emacs-json--read-error "short \\u escape"))
                            (let ((hex (substring emacs-json--read-src
                                                  emacs-json--read-pos
                                                  (+ emacs-json--read-pos 4))))
                              (setq emacs-json--read-pos
                                    (+ emacs-json--read-pos 4))
                              (char-to-string
                               (string-to-number hex 16))))
                           (t (emacs-json--read-error
                               "invalid escape \\%c" esc)))))))
         (t
          (emacs-json--read-bump)
          (setq acc (concat acc (char-to-string c)))))))
    acc))

(defun emacs-json--read-number ()
  "Read a JSON number at cursor.  Return int or float."
  (let ((start emacs-json--read-pos)
        (has-frac nil)
        (has-exp nil))
    (when (eq (emacs-json--read-peek) ?-)
      (emacs-json--read-bump))
    (while (and (not (emacs-json--read-eof-p))
                (let ((c (emacs-json--read-peek)))
                  (and (>= c ?0) (<= c ?9))))
      (emacs-json--read-bump))
    (when (eq (emacs-json--read-peek) ?.)
      (setq has-frac t)
      (emacs-json--read-bump)
      (while (and (not (emacs-json--read-eof-p))
                  (let ((c (emacs-json--read-peek)))
                    (and (>= c ?0) (<= c ?9))))
        (emacs-json--read-bump)))
    (let ((c (emacs-json--read-peek)))
      (when (or (eq c ?e) (eq c ?E))
        (setq has-exp t)
        (emacs-json--read-bump)
        (let ((s (emacs-json--read-peek)))
          (when (or (eq s ?+) (eq s ?-))
            (emacs-json--read-bump)))
        (while (and (not (emacs-json--read-eof-p))
                    (let ((d (emacs-json--read-peek)))
                      (and (>= d ?0) (<= d ?9))))
          (emacs-json--read-bump))))
    (let ((text (substring emacs-json--read-src start emacs-json--read-pos)))
      (if (or has-frac has-exp)
          (string-to-number text)
        (string-to-number text)))))

(defun emacs-json--read-array ()
  "Read a JSON array at cursor.  Return list."
  (emacs-json--read-expect ?\[)
  (emacs-json--read-skip-ws)
  (if (eq (emacs-json--read-peek) ?\])
      (progn (emacs-json--read-bump) nil)
    (let ((items nil)
          (done nil))
      (while (not done)
        (push (emacs-json--read-value) items)
        (emacs-json--read-skip-ws)
        (let ((c (emacs-json--read-peek)))
          (cond
           ((eq c ?,) (emacs-json--read-bump) (emacs-json--read-skip-ws))
           ((eq c ?\]) (emacs-json--read-bump) (setq done t))
           (t (emacs-json--read-error "expected , or ] in array")))))
      (nreverse items))))

(defun emacs-json--read-object ()
  "Read a JSON object at cursor.  Return alist with symbol keys."
  (emacs-json--read-expect ?\{)
  (emacs-json--read-skip-ws)
  (if (eq (emacs-json--read-peek) ?\})
      (progn (emacs-json--read-bump) nil)
    (let ((pairs nil)
          (done nil))
      (while (not done)
        (emacs-json--read-skip-ws)
        (let ((key-str (emacs-json--read-string)))
          (emacs-json--read-skip-ws)
          (emacs-json--read-expect ?:)
          (emacs-json--read-skip-ws)
          (let ((val (emacs-json--read-value)))
            (push (cons (intern key-str) val) pairs)))
        (emacs-json--read-skip-ws)
        (let ((c (emacs-json--read-peek)))
          (cond
           ((eq c ?,) (emacs-json--read-bump))
           ((eq c ?\}) (emacs-json--read-bump) (setq done t))
           (t (emacs-json--read-error "expected , or } in object")))))
      (nreverse pairs))))

(defun emacs-json--read-value ()
  "Read any JSON value at cursor."
  (emacs-json--read-skip-ws)
  (let ((c (emacs-json--read-peek)))
    (cond
     ((null c) (emacs-json--read-error "unexpected EOF"))
     ((eq c ?\") (emacs-json--read-string))
     ((eq c ?\{) (emacs-json--read-object))
     ((eq c ?\[) (emacs-json--read-array))
     ((eq c ?t)  (emacs-json--read-keyword "true" t))
     ((eq c ?f)  (emacs-json--read-keyword "false" json-false))
     ((eq c ?n)  (emacs-json--read-keyword "null" json-null))
     ((or (eq c ?-) (and (>= c ?0) (<= c ?9)))
      (emacs-json--read-number))
     (t (emacs-json--read-error "unexpected character %c" c)))))

(defun json-read-from-string (string)
  "Parse JSON STRING into an elisp value.
Objects are returned as alists with symbol keys; arrays as lists;
numbers as int or float; `true' / `false' / `null' as t /
`json-false' / `json-null' respectively.  Signals `json-error' on
malformed input."
  (let ((emacs-json--read-pos 0)
        (emacs-json--read-src string))
    (let ((value (emacs-json--read-value)))
      (emacs-json--read-skip-ws)
      (unless (emacs-json--read-eof-p)
        (emacs-json--read-error "trailing garbage after JSON value"))
      value)))

;; Emacs 27+ native JSON entry points.  The reader has no native json_*
;; C functions, so alias them onto the elisp reader/encoder.  anvil and
;; other callers use (json-parse-string S :object-type 'alist :array-type
;; 'list), which matches json-read-from-string's defaults (alists + lists).
(defun emacs-json--object-alist-p (x)
  "Non-nil if X is a JSON-object alist ((SYM . VAL) ...), not a JSON array."
  (and (consp x)
       (let ((ok t) (cur x))
         (while (and ok (consp cur))
           (unless (and (consp (car cur)) (symbolp (car (car cur)))) (setq ok nil))
           (setq cur (cdr cur)))
         (and ok (null cur)))))

(defun emacs-json--reshape (x object-type array-type null-object false-object)
  "Reshape json-read-from-string's alist/list tree to OBJECT-TYPE / ARRAY-TYPE
and replace the :json-null / :json-false sentinels with NULL-OBJECT / FALSE-OBJECT."
  (cond
   ((eq x :json-null) null-object)
   ((eq x :json-false) false-object)
   ((emacs-json--object-alist-p x)
    (cond
     ((eq object-type 'plist)
      (let ((out nil) (cur x))
        (while cur
          (let ((pair (car cur)))
            (setq out (cons (intern (concat ":" (symbol-name (car pair)))) out))
            (setq out (cons (emacs-json--reshape (cdr pair) object-type array-type
                                                 null-object false-object)
                            out)))
          (setq cur (cdr cur)))
        (nreverse out)))
     ((eq object-type 'hash-table)
      (let ((h (make-hash-table :test 'equal)) (cur x))
        (while cur
          (puthash (symbol-name (car (car cur)))
                   (emacs-json--reshape (cdr (car cur)) object-type array-type
                                        null-object false-object)
                   h)
          (setq cur (cdr cur)))
        h))
     (t
      (mapcar (lambda (pair)
                (cons (car pair)
                      (emacs-json--reshape (cdr pair) object-type array-type
                                           null-object false-object)))
              x))))
   ((consp x)
    (let ((lst (mapcar (lambda (e)
                         (emacs-json--reshape e object-type array-type
                                              null-object false-object))
                       x)))
      (if (eq array-type 'array) (apply #'vector lst) lst)))
   (t x)))

(unless (fboundp 'json-parse-string)
  (defun json-parse-string (string &rest args)
    "Parse JSON STRING honoring :object-type (alist / plist / hash-table),
:array-type (list / array), :null-object and :false-object (Emacs 27+ native
semantics) on top of the alist/list `json-read-from-string'."
    (let ((object-type (or (plist-get args :object-type) 'alist))
          (array-type  (or (plist-get args :array-type) 'list))
          (null-object (if (plist-member args :null-object)
                           (plist-get args :null-object) :null))
          (false-object (if (plist-member args :false-object)
                            (plist-get args :false-object) :false)))
      (emacs-json--reshape (json-read-from-string string)
                           object-type array-type null-object false-object))))

(defun emacs-json--to-encodable (x null-object false-object)
  "Convert a plist / vector / :null / :false tree (the shape json-parse-string
with :object-type 'plist produces) into the alist / json-null / json-false form
that `json-encode' renders."
  (cond
   ((eq x null-object) json-null)
   ((eq x false-object) json-false)
   ((and (consp x) (keywordp (car x))) ;; plist -> object alist
    (let ((out nil) (cur x))
      (while (and (consp cur) (consp (cdr cur)))
        (setq out (cons (cons (intern (substring (symbol-name (car cur)) 1))
                              (emacs-json--to-encodable (car (cdr cur))
                                                        null-object false-object))
                        out))
        (setq cur (cdr (cdr cur))))
      (nreverse out)))
   ((vectorp x)
    (apply #'vector
           (mapcar (lambda (e) (emacs-json--to-encodable e null-object false-object))
                   (append x nil))))
   ((consp x)
    (mapcar (lambda (e) (emacs-json--to-encodable e null-object false-object)) x))
   (t x)))

(unless (fboundp 'json-serialize)
  (defun json-serialize (object &rest args)
    "Serialize OBJECT to a JSON string, honoring :null-object / :false-object
(Emacs 27+ native semantics) and plist objects + vector arrays, on top of
`json-encode'."
    (let ((null-object (if (plist-member args :null-object)
                           (plist-get args :null-object) :null))
          (false-object (if (plist-member args :false-object)
                            (plist-get args :false-object) :false)))
      (json-encode (emacs-json--to-encodable object null-object false-object)))))

(unless (fboundp 'json-parse-buffer)
  (defun json-parse-buffer (&rest args)
    "Parse JSON from the current buffer with `json-parse-string' keyword handling."
    (apply #'json-parse-string (buffer-string) args)))

(unless (fboundp 'json-pretty-print-buffer)
  (defun json-pretty-print-buffer (&rest _args)
    "No-op pretty printer for the standalone reader: the buffer already holds
valid (compact) JSON, which is what callers ultimately write."
    nil))
(unless (fboundp 'json-pretty-print)
  (defun json-pretty-print (_begin _end &rest _args) nil))

(provide 'json)
;;; json.el ends here
