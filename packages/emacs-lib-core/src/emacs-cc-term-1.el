;;; emacs-cc-term-1.el --- term.c batch primitives -*- lexical-binding: t; -*-

(defun emacs-cc-term-1--terminal (terminal)
  (let ((term (if (null terminal) nil
                (if (and (fboundp 'framep) (framep terminal))
                    (frame-terminal terminal) terminal))))
    (unless (or (null terminal)
                (and (fboundp 'terminal-live-p) (terminal-live-p term)))
      (signal 'wrong-type-argument (list 'terminal-live-p terminal)))
    term))

(defun emacs-cc-term-1--frame (frame)
  (let ((f (or frame (selected-frame))))
    (unless (and (fboundp 'frame-live-p) (frame-live-p f))
      (signal 'wrong-type-argument (list 'frame-live-p f)))
    f))

(unless (fboundp 'controlling-tty-p)
  (defun controlling-tty-p (&optional terminal)
    "Return non-nil if TERMINAL is the controlling tty of the Emacs process."
    (emacs-cc-term-1--terminal terminal)
    nil))
(unless (fboundp 'tty-display-color-cells)
  (defun tty-display-color-cells (&optional terminal)
    "Return the number of colors supported by the tty device TERMINAL."
    (emacs-cc-term-1--terminal terminal)
    0))
(unless (fboundp 'tty-display-pixel-height)
  (defun tty-display-pixel-height (&optional display)
    "Return the height of DISPLAY's screen in pixels."
    (if (fboundp 'emacs-frame-display-pixel-height)
        (emacs-frame-display-pixel-height display) 25)))
(unless (fboundp 'tty-display-pixel-width)
  (defun tty-display-pixel-width (&optional display)
    "Return the width of DISPLAY's screen in pixels."
    (if (fboundp 'emacs-frame-display-pixel-width)
        (emacs-frame-display-pixel-width display) 80)))
(unless (fboundp 'tty-frame-at)
  (defun tty-frame-at (x y)
    "Return tty frame containing absolute pixel position (X, Y)."
    (unless (and (integerp x) (integerp y)) (setq x -1 y -1))
    (let ((frames (frame-list)))
      (when (and (>= x 0) (< x (tty-display-pixel-width))
                 (>= y 0) (< y (tty-display-pixel-height)) frames)
        (list (car frames) x y)))))
(unless (fboundp 'tty-frame-edges)
  (defun tty-frame-edges (&optional frame type)
    "Return coordinates of FRAME's edges."
    (ignore type)
    (emacs-cc-term-1--frame frame)
    nil))
(unless (fboundp 'tty-frame-geometry)
  (defun tty-frame-geometry (&optional frame)
    "Return geometric attributes of terminal frame FRAME."
    (emacs-cc-term-1--frame frame)
    nil))
(unless (fboundp 'tty-frame-list-z-order)
  (defun tty-frame-list-z-order (&optional frame)
    "Return list of Emacs's frames, in Z (stacking) order."
    (when frame (emacs-cc-term-1--frame frame))
    (frame-list)))
(unless (fboundp 'tty-frame-restack)
  (defun tty-frame-restack (frame1 frame2 &optional above)
    "Restack FRAME1 below FRAME2 on terminals."
    (ignore above)
    (error "tty-frame-restack is not implemented")))
(unless (fboundp 'tty-no-underline)
  (defun tty-no-underline (&optional terminal)
    "Declare that the tty used by TERMINAL does not handle underlining."
    (emacs-cc-term-1--terminal terminal)
    nil))
(unless (fboundp 'tty--output-buffer-size)
  (defun tty--output-buffer-size (&optional tty)
    "Return the output buffer size of TTY."
    (emacs-cc-term-1--terminal tty)
    (error "Not a tty terminal")))
(unless (fboundp 'tty--set-output-buffer-size)
  (defun tty--set-output-buffer-size (size &optional tty)
    "Set the output buffer size for a TTY."
    (unless (and (integerp size) (>= size 0)) (error "Invalid output buffer size"))
    (emacs-cc-term-1--terminal tty)
    (error "Attempt to suspend a non-text terminal device")))

(provide 'emacs-cc-term-1)
