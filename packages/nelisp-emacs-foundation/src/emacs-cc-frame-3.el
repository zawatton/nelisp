;;; emacs-cc-frame-3.el --- frame.c batch primitives -*- lexical-binding: t; -*-

;;; Code:

(defvar emacs-cc-frame-3--window-state-change nil)
(defvar emacs-cc-frame-3--old-selected-frame nil)

(defun emacs-cc-frame-3--frame (frame)
  (let ((f (or frame (selected-frame))))
    (unless (and (fboundp 'frame-live-p) (frame-live-p f))
      (signal 'wrong-type-argument (list 'frame-live-p f)))
    f))

(unless (fboundp 'frame-window-state-change)
  (defun frame-window-state-change (&optional frame)
    "Return t if FRAME's window state change flag is set, nil otherwise."
    (let ((f (emacs-cc-frame-3--frame frame)))
      (and (memq f emacs-cc-frame-3--window-state-change) t))))

(unless (fboundp 'handle-switch-frame)
  (defun handle-switch-frame (event)
    "Handle a switch-frame event EVENT."
    ;; GNU's do_switch_frame accepts either a frame or (switch-frame FRAME).
    ;; Run the leave-buffer hook before validating the event, as the C entry
    ;; point does, and return the live target frame after selecting it.
    (when (fboundp 'run-hooks)
      (run-hooks 'mouse-leave-buffer-hook))
    (let ((frame (if (and (consp event)
                          (eq (car event) 'switch-frame)
                          (consp (cdr event)))
                     (cadr event)
                   event)))
      (unless (and (fboundp 'framep) (framep frame))
        (signal 'wrong-type-argument (list 'framep frame)))
      (if (or (not (frame-live-p frame))
              (and (fboundp 'frame-parameter)
                   (frame-parameter frame 'tooltip)))
        nil
        (if (eq frame (selected-frame))
            frame
          (progn
            (select-frame frame)
            frame))))))

(unless (fboundp 'last-nonminibuffer-frame)
  (defun last-nonminibuffer-frame ()
    "Return last non-minibuffer frame selected."
    (or (and (frame-live-p emacs-cc-frame-3--old-selected-frame)
             emacs-cc-frame-3--old-selected-frame)
        (selected-frame))))

(unless (fboundp 'make-terminal-frame)
  (defun make-terminal-frame (parms)
    "Create an additional terminal frame, possibly on another terminal."
    (unless (listp parms)
      (signal 'wrong-type-argument (list 'listp parms)))
    (let ((tty (cdr (assq 'tty parms)))
          (type (cdr (assq 'tty-type parms))))
      (cond (tty (error "Could not open file: %s" tty))
            (type (error "Could not open file: /dev/tty"))
            (t (error "Unknown terminal type"))))))

(unless (fboundp 'mouse-pixel-position)
  (defun mouse-pixel-position ()
    "Return the current mouse frame and position in pixel units."
    (list (selected-frame) nil)))

(unless (fboundp 'mouse-position-in-root-frame)
  (defun mouse-position-in-root-frame ()
    "Return mouse position in selected frame's root frame."
    '(0 . 0)))

(unless (fboundp 'old-selected-frame)
  (defun old-selected-frame ()
    "Return the old selected FRAME."
    (or (and (frame-live-p emacs-cc-frame-3--old-selected-frame)
             emacs-cc-frame-3--old-selected-frame)
        (selected-frame))))

(unless (fboundp 'previous-frame)
  (defun previous-frame (&optional frame miniframe)
    "Return the previous frame in the frame list before FRAME."
    (let* ((f (emacs-cc-frame-3--frame frame))
           (frames (frame-list))
           (before (memq f frames))
           (candidates (cl-remove-if
                        (lambda (candidate)
                          (or (and (null miniframe)
                                   (frame-parameter candidate 'minibuffer) )
                              (and (eq miniframe 'visible)
                                   (not (frame-visible-p candidate)))
                              (and (equal miniframe 0)
                                   (not (frame-visible-p candidate)))))
                        frames))
           (tail (memq f candidates)))
      (or (and tail (car (last (butlast candidates (length tail))))) f))))

(unless (fboundp 'reconsider-frame-fonts)
  (defun reconsider-frame-fonts (frame)
    "Recreate FRAME's default font using updated font parameters."
    (unless (frame-live-p frame)
      (signal 'wrong-type-argument (list 'frame-live-p frame)))
    (error "Window system frame should be used")))

(unless (fboundp 'set-frame-size-and-position-pixelwise)
  (defun set-frame-size-and-position-pixelwise (frame width height x y &optional gravity)
    "Set FRAME's size to WIDTH and HEIGHT and its position to (X, Y)."
    (let ((f (emacs-cc-frame-3--frame frame)))
      (unless (integerp width)
        (signal 'wrong-type-argument (list 'integerp width)))
      (unless (integerp height)
        (signal 'wrong-type-argument (list 'integerp height)))
      (unless (and (integerp x) (integerp y))
        (signal 'wrong-type-argument (list 'integerp (if (integerp x) y x))))
      (when gravity
        (unless (and (integerp gravity) (<= 0 gravity 10))
          (signal 'args-out-of-range (list gravity 0 10))))
      (set-frame-size f width height t)
      (set-frame-parameter f 'left x)
      (set-frame-parameter f 'top y)
      nil)))

(unless (fboundp 'set-frame-window-state-change)
  (defun set-frame-window-state-change (&optional frame arg)
    "Set FRAME's window state change flag according to ARG."
    (let ((f (emacs-cc-frame-3--frame frame)))
      (if arg
          (unless (memq f emacs-cc-frame-3--window-state-change)
            (push f emacs-cc-frame-3--window-state-change))
        (setq emacs-cc-frame-3--window-state-change
              (delq f emacs-cc-frame-3--window-state-change)))
      nil)))

(unless (fboundp 'set-mouse-pixel-position)
  (defun set-mouse-pixel-position (frame x y)
    "Move the mouse pointer to pixel position (X,Y) in FRAME."
    (let ((f (emacs-cc-frame-3--frame frame)))
      (unless (and (integerp x) (integerp y))
        (signal 'wrong-type-argument (list 'integerp (if (integerp x) y x))))
      (if (display-graphic-p f)
          (error "Moving mouse position not supported")
        nil))))

(require 'cl-lib)
(require 'emacs-frame)

(provide 'emacs-cc-frame-3)
