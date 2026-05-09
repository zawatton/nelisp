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
  "Non-nil if LST is a proper alist of cons cells."
  (and (listp lst) lst
       (let ((all-cons t)
             (cur lst))
         (while (and cur all-cons)
           (unless (and (consp (car cur))
                        (not (consp (cdr (car cur)))))
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

(provide 'json)
;;; json.el ends here
