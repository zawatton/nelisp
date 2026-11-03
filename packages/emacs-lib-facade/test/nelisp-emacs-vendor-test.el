;;; nelisp-emacs-vendor-test.el --- GNU vendor path resolution -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-emacs-vendor)

(ert-deftest nelisp-emacs-vendor-resolves-from-single-tree ()
  (let* ((root (make-temp-file "nelisp-vendor-" t))
         (nelisp-emacs-vendor-root root)
         (core (expand-file-name "emacs-lisp/foo.el" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory core) t)
          (with-temp-file core (insert "; core"))
          (should (equal (nelisp-emacs-vendor-file "foo.el") core)))
      (delete-directory root t))))

(ert-deftest nelisp-emacs-vendor-errors-on-missing-file ()
  (let ((nelisp-emacs-vendor-root (make-temp-file "nelisp-vendor-" t)))
    (unwind-protect
        (should-error (nelisp-emacs-vendor-file "absent.el"))
      (delete-directory nelisp-emacs-vendor-root t))))

(ert-deftest nelisp-emacs-vendor-default-root-walks-up ()
  (let* ((root (make-temp-file "nelisp-vendor-" t))
         (deep (expand-file-name "packages/x/src" root)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "vendor/emacs-lisp" root) t)
          (make-directory deep t)
          (should (equal (nelisp-emacs-vendor--find-root deep)
                         (expand-file-name "vendor" root))))
      (delete-directory root t))))

(ert-deftest nelisp-emacs-vendor-default-root-in-repo ()
  (let ((nelisp-emacs-vendor-root nil))
    (should (file-readable-p (nelisp-emacs-vendor-file "ansi-color.el")))))

(provide 'nelisp-emacs-vendor-test)
;;; nelisp-emacs-vendor-test.el ends here
