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
;;   `json-encoding-pretty-print' — defvar, controls indentation (no-op
;;                                  in this minimal port; emit compact)
;;   `json-false' / `json-null'   — sentinels
;;   `json-read-from-string'      — stub (unused on standalone path)
;;
;; If a future module needs JSON parsing, replace the stub with a
;; real reader (~150 LoC).

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

(defun json-read-from-string (_string)
  "Stub: NeLisp standalone does not yet support JSON parsing.
Replace with a real parser when needed."
  (error "json-read-from-string not implemented in NeLisp standalone"))

(provide 'json)
;;; json.el ends here
