;;; emacs-cc-toolbar-width-1.el --- Batch frame toolbar width -*- lexical-binding: t; -*-

(unless (fboundp 'tool-bar-pixel-width)
  (defun tool-bar-pixel-width (&optional frame)
    "Return FRAME's side toolbar width in pixels."
    (setq frame (or frame (selected-frame)))
    (unless (framep frame)
      (signal 'wrong-type-argument (list 'framep frame)))
    ;; The batch frame has no left or right toolbar.
    0))

(provide 'emacs-cc-toolbar-width-1)
;;; emacs-cc-toolbar-width-1.el ends here
