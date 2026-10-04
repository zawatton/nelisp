;;; nelisp-stdlib-defvaralias-reader-callable.el --- callable reader alias provider -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Bridge the standalone VM's ordinary function call to the existing
;; `defvaralias' special-form implementation.  The generated form quotes its
;; symbol arguments so the special-form path owns alias validation, chain
;; migration, special-variable metadata, and mutation ordering.  After that
;; succeeds, this callable stores the documentation value in the symbol plist.

;;; Code:

(defun nelisp-stdlib-defvaralias-reader-callable-install ()
  "Install a Lisp callable for the reader's `defvaralias' special form."
  (unless (fboundp 'defvaralias)
    (fset 'defvaralias
          (eval
           '(lambda (symbol target &optional docstring)
              (if (symbolp symbol)
                  nil
                (signal 'wrong-type-argument (list 'symbolp symbol)))
              (if (symbolp target)
                  nil
                (signal 'wrong-type-argument (list 'symbolp target)))
              (let ((result
                     (eval (list 'defvaralias
                                 (list 'quote symbol)
                                 (list 'quote target)
                                 docstring)
                           nil)))
                (put symbol 'variable-documentation docstring)
                result))
           nil)))
  'defvaralias)

(provide 'nelisp-stdlib-defvaralias-reader-callable)
;;; nelisp-stdlib-defvaralias-reader-callable.el ends here
