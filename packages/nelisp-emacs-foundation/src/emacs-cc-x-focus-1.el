;;; emacs-cc-x-focus-1.el --- batch X focus primitive -*- lexical-binding: t; -*-

(unless (fboundp 'x-focus-frame)
  (defun x-focus-frame (frame &optional noactivate)
    "Set input focus to FRAME; NOACTIVATE requests no activation.

The standalone batch runtime only has terminal frames.  Match GNU Emacs'
error for attempting to focus one as an X window-system frame."
    (ignore noactivate)
    (let ((target (or frame (selected-frame))))
      (unless (or (and (fboundp 'frame-live-p) (frame-live-p target))
                  (and (not (fboundp 'frame-live-p))
                       (fboundp 'framep) (framep target)))
        (signal 'wrong-type-argument (list 'frame-live-p frame)))
      (signal 'error (list "Window system frame should be used")))))

(provide 'emacs-cc-x-focus-1)
;;; emacs-cc-x-focus-1.el ends here
