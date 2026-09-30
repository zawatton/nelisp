;;; nelisp-emacs-vendor-test.el --- GNU vendor path resolution -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-emacs-vendor)

(ert-deftest nelisp-emacs-vendor-prefers-api-tree ()
  (let* ((root (make-temp-file "nelisp-vendor-" t))
         (nelisp-emacs-vendor-root root)
         (api (expand-file-name "emacs-lisp-api/foo.el" root))
         (core (expand-file-name "emacs-lisp/foo.el" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory api) t)
          (make-directory (file-name-directory core) t)
          (with-temp-file api (insert "; api"))
          (with-temp-file core (insert "; core"))
          (should (equal (nelisp-emacs-vendor-file "foo.el") api)))
      (delete-directory root t))))

(ert-deftest nelisp-emacs-vendor-falls-back-to-core-tree ()
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

(provide 'nelisp-emacs-vendor-test)
;;; nelisp-emacs-vendor-test.el ends here
