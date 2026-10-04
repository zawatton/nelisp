;;; emacs-cc-frame-3-test.el --- focused frame fallback checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defconst emacs-cc-frame-3-test--root
  (expand-file-name "../.."
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst emacs-cc-frame-3-test--foundation
  (expand-file-name "packages/nelisp-emacs-foundation/src"
                    emacs-cc-frame-3-test--root))

(defconst emacs-cc-frame-3-test--core
  (expand-file-name "packages/nelisp-emacs-core/src"
                    emacs-cc-frame-3-test--root))

(add-to-list 'load-path emacs-cc-frame-3-test--foundation)
(add-to-list 'load-path emacs-cc-frame-3-test--core)

(defconst emacs-cc-frame-3-test--source
  (expand-file-name
   "emacs-cc-frame-3.el" emacs-cc-frame-3-test--foundation))

;; Load the real frame dependencies; their own guards preserve host GNU
;; primitive bindings, while the standalone runtime supplies its stubs.
(load (expand-file-name "emacs-stub.el"
                        emacs-cc-frame-3-test--foundation) nil t)
(load (expand-file-name "emacs-frame.el" emacs-cc-frame-3-test--core) nil t)
(load (expand-file-name "emacs-frame-builtins.el"
                        emacs-cc-frame-3-test--core) nil t)

(ert-deftest emacs-cc-frame-3-test/preserves-existing-gnu-binding ()
  "Loading the compatibility file must leave GNU's primitive binding intact."
  (let ((before (and (fboundp 'handle-switch-frame)
                     (symbol-function 'handle-switch-frame))))
    (load emacs-cc-frame-3-test--source nil t)
    (when before
      (should (eq before (symbol-function 'handle-switch-frame))))))

(ert-deftest emacs-cc-frame-3-test/handler-selects-and-returns-live-frame ()
  "The fallback accepts frames and switch-frame events and returns the target."
  (let* ((symbol 'handle-switch-frame)
         (had-function (fboundp symbol))
         (old-function (and had-function (symbol-function symbol))))
    (unwind-protect
        (progn
          (when had-function (fmakunbound symbol))
          (load emacs-cc-frame-3-test--source nil t)
          (let ((mouse-leave-buffer-hook nil)
                (frame (selected-frame)))
            (should (eq frame (handle-switch-frame frame)))
            (should (eq frame
                        (handle-switch-frame (list 'switch-frame frame))))
            ;; The C path selects a different live target before returning it.
            ;; A spy keeps the headless test independent of window-system frames.
            (let (selected)
              (cl-letf (((symbol-function 'selected-frame) (lambda () nil))
                        ((symbol-function 'select-frame)
                         (lambda (target &optional _norecord)
                           (setq selected target)
                           target)))
                (should (eq frame
                            (handle-switch-frame (list 'switch-frame frame))))
                (should (eq selected frame)))
            (cl-letf (((symbol-function 'frame-live-p) (lambda (_frame) nil)))
              (should-not (handle-switch-frame frame)))
            (should-error (handle-switch-frame 'not-a-frame)
                          :type 'wrong-type-argument)))
      (if had-function
          (fset symbol old-function)
        (when (fboundp symbol) (fmakunbound symbol)))))))

(provide 'emacs-cc-frame-3-test)
;;; emacs-cc-frame-3-test.el ends here
