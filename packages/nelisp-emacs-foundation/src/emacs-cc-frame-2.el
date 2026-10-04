;;; emacs-cc-frame-2.el --- frame.c terminal primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-frame-2--check-framep (frame)
  "Signal the framep type error used by terminal frame queries."
  (unless (framep frame)
    (signal 'wrong-type-argument (list 'framep frame))))

(defun emacs-cc-frame-2--check-live (frame)
  "Signal the frame-live-p type error used by live frame queries."
  (unless (frame-live-p frame)
    (signal 'wrong-type-argument (list 'frame-live-p frame))))

(unless (fboundp 'frame-right-divider-width)
  (defun frame-right-divider-width (&optional frame)
    "Return width (in pixels) of vertical window dividers on FRAME."
    (setq frame (or frame (selected-frame)))
    (emacs-cc-frame-2--check-framep frame)
    0))

(unless (fboundp 'frame-root-frame)
  (defun frame-root-frame (&optional frame)
    "Return root frame of specified FRAME.
FRAME must be a live frame and defaults to the selected one.  The root
frame of FRAME is the frame obtained by following the chain of parent
frames starting with FRAME until a frame is reached that has no parent.
If FRAME has no parent, its root frame is FRAME."
    (setq frame (or frame (selected-frame)))
    (emacs-cc-frame-2--check-live frame)
    frame))

(unless (fboundp 'frame-scale-factor)
  (defun frame-scale-factor (&optional frame)
    "Return FRAMEs scale factor.
If FRAME is omitted or nil, the selected frame is used.
The scale factor is the amount by which a logical pixel size must be
multiplied to find the real number of pixels."
    (setq frame (or frame (selected-frame)))
    (emacs-cc-frame-2--check-live frame)
    1.0))

(unless (fboundp 'frame-scroll-bar-height)
  (defun frame-scroll-bar-height (&optional frame)
    "Return scroll bar height of FRAME in pixels."
    (setq frame (or frame (selected-frame)))
    (emacs-cc-frame-2--check-framep frame)
    0))

(unless (fboundp 'frame-scroll-bar-width)
  (defun frame-scroll-bar-width (&optional frame)
    "Return scroll bar width of FRAME in pixels."
    (setq frame (or frame (selected-frame)))
    (emacs-cc-frame-2--check-framep frame)
    0))

(unless (fboundp 'frame--set-was-invisible)
  (defun frame--set-was-invisible (frame was-invisible)
    "Set FRAME's was-invisible flag if WAS-INVISIBLE is non-nil.
This function is for internal use only."
    (emacs-cc-frame-2--check-live frame)
    (when was-invisible
      (set-frame-parameter frame 'was-invisible was-invisible))
    was-invisible))

(unless (fboundp 'frame-text-cols)
  (defun frame-text-cols (&optional frame)
    "Return width in columns of FRAME's text area."
    (setq frame (or frame (selected-frame)))
    (emacs-cc-frame-2--check-framep frame)
    (frame-text-width frame)))

(unless (fboundp 'frame-text-height)
  (defun frame-text-height (&optional frame)
    "Return text area height of FRAME in pixels."
    (setq frame (or frame (selected-frame)))
    (emacs-cc-frame-2--check-framep frame)
    (* (frame-text-lines frame) (frame-char-height frame))))

(unless (fboundp 'frame-text-lines)
  (defun frame-text-lines (&optional frame)
    "Return height in lines of FRAME's text area."
    (setq frame (or frame (selected-frame)))
    (emacs-cc-frame-2--check-framep frame)
    (if (and (fboundp 'emacs-frame-p) (emacs-frame-p frame))
        (- (/ (emacs-frame-pixel-height frame) emacs-frame--char-height)
           (emacs-frame-menu-bar-lines frame))
      (frame-height frame))))

(unless (fboundp 'frame-text-width)
  (defun frame-text-width (&optional frame)
    "Return text area width of FRAME in pixels."
    (setq frame (or frame (selected-frame)))
    (emacs-cc-frame-2--check-framep frame)
    (if (and (fboundp 'emacs-frame-p) (emacs-frame-p frame))
        (/ (emacs-frame-pixel-width frame) emacs-frame--char-width)
      (frame-width frame))))

(unless (fboundp 'frame-total-cols)
  (defun frame-total-cols (&optional frame)
    "Return number of total columns of FRAME."
    (setq frame (or frame (selected-frame)))
    (emacs-cc-frame-2--check-framep frame)
    80))

(unless (fboundp 'frame-total-lines)
  (defun frame-total-lines (&optional frame)
    "Return number of total lines of FRAME."
    (setq frame (or frame (selected-frame)))
    (emacs-cc-frame-2--check-framep frame)
    25))

(provide 'emacs-cc-frame-2)
;;; emacs-cc-frame-2.el ends here
