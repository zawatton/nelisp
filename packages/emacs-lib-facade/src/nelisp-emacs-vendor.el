;;; nelisp-emacs-vendor.el --- Resolve GNU vendor files -*- lexical-binding: t; -*-

(defvar nelisp-emacs-vendor-root nil)

(defun nelisp-emacs-vendor--find-root (start)
  "Return the nearest ancestor of START holding `vendor/emacs-lisp', or nil."
  (let ((dir (file-name-as-directory (expand-file-name start)))
        (found nil))
    (while (and dir (not found))
      (if (file-directory-p (expand-file-name "vendor/emacs-lisp" dir))
          (setq found (expand-file-name "vendor" dir))
        (let ((parent (file-name-directory (directory-file-name dir))))
          (setq dir (and parent (not (equal parent dir)) parent)))))
    found))

(defun nelisp-emacs-vendor-file (relative)
  "Find RELATIVE GNU Lisp file in the single GNU vendor tree.
RELATIVE is relative to `vendor/emacs-lisp'.  `nelisp-emacs-vendor-root'
overrides the default, which is the repository `vendor/' found by walking
up from this file's directory."
  (let* ((root (or (and (boundp 'nelisp-emacs-vendor-root)
                        nelisp-emacs-vendor-root)
                   (let ((here (or load-file-name buffer-file-name
                                   (and (fboundp 'locate-library)
                                        (locate-library "nelisp-emacs-vendor")))))
                     (and here (nelisp-emacs-vendor--find-root
                                (file-name-directory here))))))
         (file (and root
                    (let ((path (expand-file-name
                                 (concat "emacs-lisp/" relative) root)))
                      (and (file-readable-p path) path)))))
    (or file (error "Missing GNU vendor file %s (root %s)" relative root))))

(provide 'nelisp-emacs-vendor)
;;; nelisp-emacs-vendor.el ends here
