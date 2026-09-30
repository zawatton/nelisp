;;; emacs-cc-pgtkfns-4.el --- pgtkfns.c color primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-pgtkfns-4--check-frame (frame)
  "Signal the GNU-compatible error for an invalid FRAME."
  (unless (and (fboundp 'frame-live-p) (frame-live-p frame))
    (signal 'wrong-type-argument (list 'frame-live-p frame))))

(unless (fboundp 'xw-color-defined-p)
  (defun xw-color-defined-p (color &optional frame)
    "Internal function called by `color-defined-p'.
(Note that the Nextstep version of this function ignores FRAME.)"
    (ignore color)
    (emacs-cc-pgtkfns-4--check-frame
     (or frame (and (fboundp 'selected-frame) (selected-frame))))
    (signal 'error (list "Window system frame should be used"))))

(unless (fboundp 'xw-color-values)
  (defun xw-color-values (color &optional frame)
    "Internal function called by `color-values'.
(Note that the Nextstep version of this function ignores FRAME.)"
    (ignore color)
    (emacs-cc-pgtkfns-4--check-frame
     (or frame (and (fboundp 'selected-frame) (selected-frame))))
    (signal 'error (list "Window system frame should be used"))))

(unless (fboundp 'xw-display-color-p)
  (defun xw-display-color-p (&optional terminal)
    "Internal function called by `display-color-p'."
    (if terminal
        (progn
          (emacs-cc-pgtkfns-4--check-frame terminal)
          (signal 'error (list "Window system frame should be used")))
      (signal 'error (list "Frames are not in use or not initialized")))))

(provide 'emacs-cc-pgtkfns-4)

;;; emacs-cc-pgtkfns-4.el ends here
