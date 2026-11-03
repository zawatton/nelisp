;;; emacs-mark-state-test.el --- Interactive mark defaults -*- lexical-binding: t; -*-
(require 'ert)
(require 'emacs-mark-state)

(ert-deftest emacs-mark-state-interactive-default-preserves-buffer-local-override ()
  (let ((saved (default-value 'transient-mark-mode)))
    (unwind-protect
        (with-temp-buffer
          (setq-default transient-mark-mode nil)
          (setq-local transient-mark-mode nil)
          (emacs-mark-state-initialize-interactive)
          (should (default-value 'transient-mark-mode))
          (should-not transient-mark-mode))
      (setq-default transient-mark-mode saved))))

(provide 'emacs-mark-state-test)
