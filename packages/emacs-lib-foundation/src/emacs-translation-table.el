;;; emacs-translation-table.el --- lightweight translation table substrate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Standalone NeLisp does not yet expose Emacs' full char-table and Mule
;; translation-table substrate.  This file provides the small public
;; surface needed by lightweight vendor facades such as cp51932.el and
;; eucjp-ms.el.  Host Emacs keeps its native implementation.

;;; Code:

(defvar translation-table-vector []
  "Vector of registered lightweight translation tables.")

(defvar translation-table-for-input nil
  "Input translation table placeholder for standalone NeLisp.")

(unless (fboundp 'decode-char)
  (defun decode-char (_charset code-point &optional _restriction)
    "Return CODE-POINT as a lightweight decoded character.
This is a compatibility fallback for standalone NeLisp.  It preserves
stable integer identity for generated table keys until full charset
decoding is available in Layer 1."
    code-point))

(defun emacs-translation-table--alist->hash (alist)
  "Return a hash-table translation table populated from ALIST."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry alist)
      (when (consp entry)
        (puthash (car entry) (cdr entry) table)))
    table))

(unless (fboundp 'make-translation-table-from-alist)
  (defun make-translation-table-from-alist (alist)
    "Create a lightweight translation table from ALIST."
    (emacs-translation-table--alist->hash alist)))

(unless (fboundp 'make-translation-table)
  (defun make-translation-table (&rest args)
    "Create a lightweight translation table from ARGS.
Only the common alist form is modeled in standalone NeLisp."
    (emacs-translation-table--alist->hash
     (if (and (= (length args) 1) (listp (car args)))
         (car args)
       args))))

(unless (fboundp 'define-translation-table)
  (defun define-translation-table (symbol &rest args)
    "Register SYMBOL as a lightweight translation table."
    (let* ((table (if (and (= (length args) 1)
                           (hash-table-p (car args)))
                      (car args)
                    (apply #'make-translation-table args)))
           (id (length translation-table-vector)))
      (put symbol 'translation-table table)
      (put symbol 'translation-table-id id)
      (setq translation-table-vector
            (vconcat translation-table-vector (vector (cons symbol table))))
      symbol)))

(defun emacs-translation-table-get (table key)
  "Return TABLE's translation for KEY, or nil."
  (let ((object (if (symbolp table)
                    (get table 'translation-table)
                  table)))
    (cond
     ((hash-table-p object)
      (gethash key object))
     ((and (fboundp 'char-table-p) (char-table-p object))
      (aref object key))
     ((vectorp object)
      (and (integerp key)
           (>= key 0)
           (< key (length object))
           (aref object key)))
     ((listp object)
      (cdr (assoc key object)))
     (t nil))))

(provide 'emacs-translation-table)

;;; emacs-translation-table.el ends here
