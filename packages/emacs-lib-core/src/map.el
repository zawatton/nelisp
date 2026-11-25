;;; map.el --- Load the GNU map library  -*- lexical-binding: t; -*-

(load (expand-file-name
       "emacs-lisp/emacs-lisp/map.el"
       (or (and (boundp 'nelisp-emacs-vendor-root) nelisp-emacs-vendor-root)
           (expand-file-name "../../../vendor"
                             (file-name-directory load-file-name))))
      nil 'no-message t t)

;;; map.el ends here
