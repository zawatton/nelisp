;;; case-table.el --- GNU case-table loader and NeLisp substrate -*- lexical-binding: t; -*-

;; The core owns the casetab.c port and its Unicode data.  Public Lisp
;; definitions come from GNU's case-table.el.

;;; Code:

(require 'nelisp-stdlib-casetab)

(nelisp-casetab--register-standard-data)

(unless (featurep 'case-table)
  (load (expand-file-name "emacs-lisp/case-table.el"
                          (or (and (boundp 'nelisp-emacs-vendor-root)
                                   nelisp-emacs-vendor-root)
                              (expand-file-name "../../../vendor"
                                                (file-name-directory load-file-name))))
        nil 'no-message t t))
;;; case-table.el ends here
