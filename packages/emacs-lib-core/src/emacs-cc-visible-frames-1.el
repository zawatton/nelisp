;;; emacs-cc-visible-frames-1.el --- Visible frame inventory -*- lexical-binding: t; -*-

(unless (fboundp 'visible-frame-list)
  (defun visible-frame-list (&rest arguments)
    "Return all live frames being updated, excluding iconified frames."
    (when arguments
      (signal 'wrong-number-of-arguments
              (list 'visible-frame-list (length arguments))))
    (let (visible)
      (dolist (frame (frame-list))
        (when (eq (frame-visible-p frame) t)
          (push frame visible)))
      (nreverse visible))))

(provide 'emacs-cc-visible-frames-1)
;;; emacs-cc-visible-frames-1.el ends here
