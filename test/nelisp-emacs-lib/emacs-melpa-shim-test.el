;;; emacs-melpa-shim-test.el --- ERT tests for Phase 4 shim  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-melpa-shim)

(defconst emacs-melpa-shim-test--pilot-dir
  (expand-file-name "nelisp-emacs-phase4-pilot" temporary-file-directory))

(defconst emacs-melpa-shim-test--pilot-file
  (expand-file-name "phase4-pilot.el" emacs-melpa-shim-test--pilot-dir))

(defconst emacs-melpa-shim-test--pilot-source
  ";;; phase4-pilot.el --- Phase 4 pilot synthetic package -*- lexical-binding: t; -*-\n\n(defun phase4-pilot-transform (input)\n  (let ((buf (generate-new-buffer \" *phase4-pilot*\")))\n    (unwind-protect\n        (progn\n          (set-buffer buf)\n          (insert input)\n          (goto-char (point-min))\n          (search-forward \"-\")\n          (delete-region (point-min) (point))\n          (insert \"compat:\")\n          (buffer-string))\n      (kill-buffer buf))))\n\n(provide 'phase4-pilot)\n")

(defun emacs-melpa-shim-test--ensure-pilot-package ()
  "Materialize the synthetic package under /tmp for the pilot ERT."
  (unless (file-directory-p emacs-melpa-shim-test--pilot-dir)
    (make-directory emacs-melpa-shim-test--pilot-dir t))
  (with-temp-file emacs-melpa-shim-test--pilot-file
    (insert emacs-melpa-shim-test--pilot-source))
  emacs-melpa-shim-test--pilot-file)

(ert-deftest emacs-melpa-shim-loads-synthetic-package-end-to-end ()
  (let* ((pilot-file (emacs-melpa-shim-test--ensure-pilot-package))
         (load-path (cons emacs-melpa-shim-test--pilot-dir load-path))
         (nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil)
         (emacs-buffer--state (make-hash-table :test 'eq))
         (emacs-buffer--variable-buffer-local nil)
         (emacs-buffer--default-values (make-hash-table :test 'eq)))
    (skip-unless (file-readable-p pilot-file))
    (ignore-errors (unload-feature 'phase4-pilot t))
    (unwind-protect
        (progn
          (should (require 'phase4-pilot nil t))
          (emacs-melpa-shim-with-installed
          (should (equal "compat:world"
                         (phase4-pilot-transform "hello-world")))))
      (ignore-errors (unload-feature 'phase4-pilot t))
      (emacs-melpa-shim-uninstall))))

(provide 'emacs-melpa-shim-test)

;;; emacs-melpa-shim-test.el ends here
