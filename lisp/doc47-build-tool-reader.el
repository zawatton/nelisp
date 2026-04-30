;;; doc47-build-tool-reader.el --- Elisp port of build-tool reader -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; Thin Elisp wrapper for the Doc 44 §3.2 LOCKED reader subset.  The
;; subset is identical to what Emacs `read' already parses natively, so
;; this module mainly adds:
;; - exact-one-form / read-all helpers
;; - explicit deferred-feature errors for Rust parity
;; - formatter parity with build-tool/src/reader/fmt_sexp

;;; Code:

(require 'cl-lib)

(defconst doc47-build-tool-reader--prefix "doc47-build-tool-reader")

(defun doc47-build-tool-reader--nyi (feature)
  (error "%s: not yet implemented: %s"
         doc47-build-tool-reader--prefix
         feature))

(defun doc47-build-tool-reader--skip-ws-comments (string pos)
  (let ((len (length string))
        (i pos)
        done)
    (while (not done)
      (setq done t)
      (while (and (< i len)
                  (memq (aref string i) '(32 ?\t ?\n ?\r ?\f)))
        (setq i (1+ i)))
      (when (and (< i len) (eq (aref string i) ?\;))
        (setq done nil)
        (while (and (< i len) (not (eq (aref string i) ?\n)))
          (setq i (1+ i)))))
    i))

(defun doc47-build-tool-reader--check-deferred (string pos)
  (let* ((len (length string))
         (i (doc47-build-tool-reader--skip-ws-comments string pos)))
    (when (< i len)
      (cond
       ((and (eq (aref string i) ?\?)
             (< (1+ i) len)
             (eq (aref string (1+ i)) ?\\)
             (< (+ i 2) len)
             (memq (aref string (+ i 2)) '(?M ?m)))
        (doc47-build-tool-reader--nyi
         "meta char literal modifiers (?\\M-a, multi-modifier variants)"))
       ((and (eq (aref string i) ?#)
             (< (1+ i) len)
             (eq (aref string (1+ i)) ?\[))
        (doc47-build-tool-reader--nyi
         "byte-code literal #[...]"))
       ((and (eq (aref string i) ?#)
             (< (1+ i) len)
             (memq (aref string (1+ i)) '(?s ?S)))
        (doc47-build-tool-reader--nyi
         "structure literal #s(...)"))))))

(defun doc47-build-tool-reader-read (string)
  "Read exactly one form from STRING."
  (doc47-build-tool-reader--check-deferred string 0)
  (pcase-let ((`(,form . ,pos) (read-from-string string)))
    (setq pos (doc47-build-tool-reader--skip-ws-comments string pos))
    (doc47-build-tool-reader--check-deferred string pos)
    (unless (= pos (length string))
      (error "%s: trailing token after first form" doc47-build-tool-reader--prefix))
    form))

(defun doc47-build-tool-reader-read-all (string)
  "Read every top-level form from STRING."
  (let ((pos 0)
        (len (length string))
        forms)
    (while (< (setq pos (doc47-build-tool-reader--skip-ws-comments string pos)) len)
      (doc47-build-tool-reader--check-deferred string pos)
      (pcase-let ((`(,form . ,next) (read-from-string string pos)))
        (push form forms)
        (setq pos next)))
    (nreverse forms)))

(defun doc47-build-tool-reader--list-tag-and-arg (sexp)
  (when (and (consp sexp)
             (symbolp (car sexp))
             (consp (cdr sexp))
             (null (cddr sexp)))
    (cons (car sexp) (cadr sexp))))

(defun doc47-build-tool-reader--macro-prefix (tag)
  (pcase (symbol-name tag)
    ("quote" "'")
    ("backquote" "`")
    ("comma" ",")
    ("comma-at" ",@")
    ("function" "#'")
    ("`" "`")
    ("," ",")
    (",@" ",@")
    (_ nil)))

(defun doc47-build-tool-reader--float-string (sexp)
  (let ((text (prin1-to-string sexp)))
    (if (string-match-p "\\(?:\\.\\|[eE]\\|inf\\|NaN\\)" text)
        text
      (concat text ".0"))))

(defun doc47-build-tool-reader--write-list-body (sexp)
  (let ((parts nil)
        (cur sexp))
    (while (consp cur)
      (push (doc47-build-tool-reader-fmt (car cur)) parts)
      (setq cur (cdr cur)))
    (setq parts (nreverse parts))
    (if (null cur)
        (mapconcat #'identity parts " ")
      (concat (mapconcat #'identity parts " ")
              " . "
              (doc47-build-tool-reader-fmt cur)))))

(defun doc47-build-tool-reader--string-body (sexp)
  (mapconcat
   (lambda (ch)
     (pcase ch
       (?\" "\\\"")
       (?\\ "\\\\")
       (?\n "\\n")
       (?\t "\\t")
       (?\r "\\r")
       (_ (string ch))))
   sexp
   ""))

(defun doc47-build-tool-reader-fmt (sexp)
  "Format SEXP to match the Rust reader's `fmt_sexp' output."
  (let* ((macro-form (doc47-build-tool-reader--list-tag-and-arg sexp))
         (macro-prefix (and macro-form
                            (doc47-build-tool-reader--macro-prefix
                             (car macro-form)))))
    (cond
     ((eq sexp nil) "nil")
     ((eq sexp t) "t")
     ((integerp sexp) (number-to-string sexp))
     ((floatp sexp) (doc47-build-tool-reader--float-string sexp))
     ((stringp sexp) (concat "\"" (doc47-build-tool-reader--string-body sexp) "\""))
     ((vectorp sexp)
      (concat "["
              (mapconcat #'doc47-build-tool-reader-fmt sexp " ")
              "]"))
     (macro-prefix
      (concat macro-prefix
              (doc47-build-tool-reader-fmt (cdr macro-form))))
     ((symbolp sexp) (symbol-name sexp))
     ((consp sexp) (concat "(" (doc47-build-tool-reader--write-list-body sexp) ")"))
     (t (prin1-to-string sexp)))))

(provide 'doc47-build-tool-reader)

;;; doc47-build-tool-reader.el ends here
