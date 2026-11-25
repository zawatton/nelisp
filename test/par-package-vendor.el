;;; par-package-vendor.el --- GNU package activation parity -*- lexical-binding: t; -*-

(defmacro par-package-vendor--row (name &rest body)
  `(progn
     (prin1 (cons ',name (condition-case err (progn ,@body)
                           (error (list 'error (car err))))))
     (terpri)))

(defun par-package-vendor--owner (name library)
  (let ((file (symbol-file name 'defun)))
    (and file (equal (file-name-base file) library)
         (or (not (fboundp 'nemacs-runtime-dump-reinit))
             (string-match-p "/vendor/emacs-lisp/" file))
         t)))

(par-package-vendor--row archive-autoloads
  (mapcar (lambda (name) (par-package-vendor--owner name "package"))
          '(package-initialize package-install package-refresh-contents
            package-delete package-recompile-all package-upgrade-all)))

(par-package-vendor--row managers
  (require 'package)
  (list (featurep 'package-activate)
        (par-package-vendor--owner 'package-upgrade-all "package")
        (par-package-vendor--owner 'package-activate-all "package-activate")
        (par-package-vendor--owner 'package-initialize "package")))

(defvar par-package-vendor-trace nil)

(defun par-package-vendor--make-fixture (root name dependencies)
  (let ((dir (expand-file-name (concat name "-1.0") root)))
    (make-directory dir t)
    (with-temp-file (expand-file-name (concat name "-pkg.el") dir)
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (prin1 (list 'define-package name "1.0" "Parity fixture"
                   (list 'quote dependencies))
             (current-buffer)))
    (with-temp-file (expand-file-name (concat name "-autoloads.el") dir)
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (prin1 (list 'push (list 'quote (intern name)) 'par-package-vendor-trace)
             (current-buffer)))))

(par-package-vendor--row activate-dependencies
  (let* ((root (make-temp-file "par-package-vendor-" t))
         (package-user-dir root)
         (package-directory-list nil)
         (package-alist nil)
         (package-activated-list nil)
         (package-load-list '(all))
         (package-quickstart nil)
         (package--initialized nil)
         (par-package-vendor-trace nil)
         (load-path (copy-sequence load-path)))
    (unwind-protect
        (progn
          (par-package-vendor--make-fixture root "par-package-dep" nil)
          (par-package-vendor--make-fixture
           root "par-package-main" '((par-package-dep "1.0")))
          (package-initialize t)
          (list (package-desc-full-name (cadr (assq 'par-package-main package-alist)))
                (null par-package-vendor-trace)
                (package-activate 'par-package-main)
                (reverse par-package-vendor-trace)))
      (delete-directory root t))))
