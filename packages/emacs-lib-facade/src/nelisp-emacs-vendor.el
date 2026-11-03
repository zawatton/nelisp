;;; nelisp-emacs-vendor.el --- Resolve GNU vendor files -*- lexical-binding: t; -*-

(defvar nelisp-emacs-vendor-root nil)

(defun nelisp-emacs-vendor-file (relative)
  "Find RELATIVE GNU Lisp file in the single GNU vendor tree.
RELATIVE is relative to `vendor/emacs-lisp'.  `nelisp-emacs-vendor-root'
may point at a staging root containing that tree."
  (let* ((root (or (and (boundp 'nelisp-emacs-vendor-root)
                        nelisp-emacs-vendor-root)
                   (let ((here (or load-file-name buffer-file-name
                                   (and (fboundp 'locate-library)
                                        (locate-library "nelisp-emacs-vendor")))))
                     (and here (expand-file-name "../vendor"
                                                 (file-name-directory here))))))
         (file (and root
                    (let ((path (expand-file-name
                                 (concat "emacs-lisp/" relative) root)))
                      (and (file-readable-p path) path)))))
    (or file (error "Missing GNU vendor file %s (root %s)" relative root))))

(provide 'nelisp-emacs-vendor)
;;; nelisp-emacs-vendor.el ends here
