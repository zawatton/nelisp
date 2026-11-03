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

;;;; Phase 4 protocol harmonisation (2026-05-06): runtime-dispatch shims

(ert-deftest emacs-melpa-shim-test/set-buffer-dispatch-routes-nelisp ()
  "`set-buffer-dispatch' must route a NeLisp buffer to `nelisp-ec-set-buffer'."
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil))
    (let ((b (nelisp-ec-generate-new-buffer "dispatch-test")))
      (emacs-melpa-shim-set-buffer-dispatch b)
      (should (eq b nelisp-ec--current-buffer)))))

(ert-deftest emacs-melpa-shim-test/set-buffer-dispatch-routes-nelisp-by-name ()
  "`set-buffer-dispatch' must look up a NeLisp buffer by name string."
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil))
    (let ((b (nelisp-ec-generate-new-buffer "dispatch-name")))
      (emacs-melpa-shim-set-buffer-dispatch "dispatch-name")
      (should (eq b nelisp-ec--current-buffer)))))

(ert-deftest emacs-melpa-shim-test/set-buffer-dispatch-falls-through-to-host ()
  "`set-buffer-dispatch' must call the captured original for host buffers."
  (let* ((host-buf (generate-new-buffer " *dispatch-host*"))
         (called-with nil)
         (emacs-melpa-shim--originals
          (list (cons 'set-buffer
                      (lambda (b) (setq called-with b) b)))))
    (unwind-protect
        (progn
          (emacs-melpa-shim-set-buffer-dispatch host-buf)
          (should (eq host-buf called-with)))
      (kill-buffer host-buf))))

(ert-deftest emacs-melpa-shim-test/current-buffer-dispatch-prefers-nelisp ()
  "`current-buffer-dispatch' must return the NeLisp current buffer when set."
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil))
    (let ((b (nelisp-ec-generate-new-buffer "current-test")))
      (nelisp-ec-set-buffer b)
      (should (eq b (emacs-melpa-shim-current-buffer-dispatch))))))

(ert-deftest emacs-melpa-shim-test/current-buffer-dispatch-falls-through ()
  "When NeLisp current buffer is nil, fall through to host current-buffer."
  (let* ((nelisp-ec--current-buffer nil)
         (called-p nil)
         (mock-fn (lambda () (setq called-p t) 'host-result))
         (emacs-melpa-shim--originals
          (list (cons 'current-buffer mock-fn))))
    (let ((r (emacs-melpa-shim-current-buffer-dispatch)))
      (should called-p)
      (should (eq 'host-result r)))))

(ert-deftest emacs-melpa-shim-test/text-properties-at-accepts-2-args ()
  "Phase 4 protocol harmonisation: 2-arg `(text-properties-at POS OBJECT)'
must not raise wrong-number-of-arguments — host emacs's load machinery
calls it with 2 args during source reading."
  (should (functionp #'emacs-melpa-shim-text-properties-at))
  (let ((emacs-melpa-shim--originals
         (list (cons 'text-properties-at
                     (lambda (_pos _obj) nil)))))
    ;; nil OBJECT → routes to NeLisp path; should not raise
    (let ((nelisp-ec--buffers nil)
          (nelisp-ec--current-buffer nil))
      (let ((b (nelisp-ec-generate-new-buffer "tpat-test")))
        (nelisp-ec-set-buffer b)
        (nelisp-ec-insert "x")
        (should-not
         (emacs-melpa-shim-text-properties-at 1))))
    ;; string OBJECT → falls through to original
    (should-not (emacs-melpa-shim-text-properties-at 0 "abc"))))

(ert-deftest emacs-melpa-shim-test/with-installed-captures-originals ()
  "`emacs-melpa-shim-with-installed' must populate `--originals' so the
dispatch shims can call back to host primitives."
  (let ((capture-set-buffer nil)
        (capture-current-buffer nil))
    (emacs-melpa-shim-with-installed
      (setq capture-set-buffer
            (cdr (assq 'set-buffer emacs-melpa-shim--originals)))
      (setq capture-current-buffer
            (cdr (assq 'current-buffer emacs-melpa-shim--originals))))
    (should (functionp capture-set-buffer))
    (should (functionp capture-current-buffer))))

(provide 'emacs-melpa-shim-test)

;;; emacs-melpa-shim-test.el ends here
