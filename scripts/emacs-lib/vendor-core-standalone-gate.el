;;; vendor-core-standalone-gate.el --- standalone-reader vendor-core gate  -*- lexical-binding: t; -*-

;;; Code:

(defvar vendor-core-standalone-gate-module-spec nil
  "Comma/whitespace-separated module override for the standalone gate.")

(defvar vendor-core-standalone-gate-default-limit nil
  "Optional numeric default module limit for the standalone gate.")

(defvar vendor-core-standalone-gate-strict nil
  "Optional strict flag for the standalone gate.")

(defun vendor-core-standalone-gate--repo-root ()
  "Return the nelisp-emacs repository root."
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun vendor-core-standalone-gate--add-load-paths (repo)
  "Install the load paths needed to smoke vendor core modules under REPO."
  (unless (boundp 'load-path)
    (defvar load-path nil))
  (setq nelisp-emacs-vendor-root (expand-file-name "vendor" repo))
  (setq load-path
        (append (list (expand-file-name "src" repo)
                      (expand-file-name "scripts" repo)
                      (expand-file-name "vendor/emacs-lisp" repo)
                      (expand-file-name "vendor/emacs-lisp/emacs-lisp" repo)
                      (expand-file-name "vendor/emacs-lisp/vc" repo))
                load-path)))

(defun vendor-core-standalone-gate-run ()
  "Run `vendor-core-smoke-batch' and return a standalone-reader exit code."
  (condition-case _err
      (let* ((repo (vendor-core-standalone-gate--repo-root))
             (bootstrap (expand-file-name "build/nemacs-bootstrap.el" repo)))
        (vendor-core-standalone-gate--add-load-paths repo)
        (load bootstrap nil 'no-message t t)
        (load (expand-file-name "scripts/vendor-core-smoke.el" repo)
              nil 'no-message t t)
        (when vendor-core-standalone-gate-module-spec
          (setq vendor-core-smoke-module-spec
                vendor-core-standalone-gate-module-spec))
        (when vendor-core-standalone-gate-default-limit
          (setq vendor-core-smoke-default-limit
                vendor-core-standalone-gate-default-limit))
        (when vendor-core-standalone-gate-strict
          (setq vendor-core-smoke-strict
                vendor-core-standalone-gate-strict))
        (vendor-core-smoke-batch)
        0)
    (error 1)))

(vendor-core-standalone-gate-run)

;;; vendor-core-standalone-gate.el ends here
