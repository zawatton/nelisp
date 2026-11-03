;;; emacs-translation-table.el --- lightweight translation table substrate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Standalone NeLisp does not yet expose Emacs' full char-table and Mule
;; translation-table substrate.  This file provides the small public
;; surface needed by lightweight vendor facades such as cp51932.el and
;; eucjp-ms.el.  Host Emacs keeps its native implementation.

;;; Code:

(unless (boundp 'translation-table-vector)
  (setq translation-table-vector nil))

(unless (boundp 'translation-table-for-input)
  (setq translation-table-for-input nil))

(setq emacs-translation-table--max-vector-key #x10ffff)
(setq emacs-translation-table--vector-density-factor 8)

(unless (fboundp 'decode-char)
  (defun decode-char (_charset code-point &optional _restriction)
    code-point))

(defun emacs-translation-table--integer-alist-max-key (alist)
  (let ((max-key -1)
        (ok t))
    (dolist (entry alist)
      (cond
       ((not (consp entry))
        (setq ok nil))
       ((and (integerp (car entry))
             (>= (car entry) 0)
             (<= (car entry) emacs-translation-table--max-vector-key))
        (when (> (car entry) max-key)
          (setq max-key (car entry))))
       (t
        (setq ok nil))))
    (and ok max-key)))

(defun emacs-translation-table--alist-vector (alist max-key)
  (let ((table (make-vector (1+ max-key) nil)))
    (dolist (entry alist)
      (aset table (car entry) (cdr entry)))
    table))

(defun emacs-translation-table--copy-alist (alist)
  (let (out)
    (dolist (entry alist)
      (when (consp entry)
        (push (cons (car entry) (cdr entry)) out)))
    (nreverse out)))

(defun emacs-translation-table--alist-table (alist)
  (let ((max-key (emacs-translation-table--integer-alist-max-key alist)))
    (if (and max-key
             (<= (1+ max-key)
                 (* (length alist)
                    emacs-translation-table--vector-density-factor)))
        (emacs-translation-table--alist-vector alist max-key)
      (emacs-translation-table--copy-alist alist))))

(defun emacs-translation-table--retain-source-alist-p (symbol)
  (eq symbol 'eucjp-ms-encode))

(unless (fboundp 'make-translation-table-from-alist)
  (defun make-translation-table-from-alist (alist)
    (emacs-translation-table--alist-table alist)))

(unless (fboundp 'make-translation-table)
  (defun make-translation-table (&rest args)
    (emacs-translation-table--alist-table
     (if (and (= (length args) 1) (listp (car args)))
         (car args)
       args))))

(unless (fboundp 'define-translation-table)
  (defun define-translation-table (symbol &rest args)
    (let* ((single-arg-p (= (length args) 1))
           (source (and single-arg-p (car args)))
           (table (cond
                   ((and single-arg-p (hash-table-p source))
                    source)
                   ((and single-arg-p
                         (listp source)
                         (emacs-translation-table--retain-source-alist-p
                          symbol))
                    ;; Vendored generated tables reverse the decode alist in
                    ;; place and immediately register the encode table.  The
                    ;; reversed list is no longer mutated, so retaining it
                    ;; avoids a second large traversal in persistent REPLs.
                    source)
                   (t
                    (apply #'make-translation-table args))))
           (id (length translation-table-vector)))
      (put symbol 'translation-table table)
      (put symbol 'translation-table-id id)
      (setq translation-table-vector
            (vconcat translation-table-vector (vector (cons symbol table))))
      symbol)))

(defun emacs-translation-table-get (table key)
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
