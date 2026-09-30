;;; emacs-cc-frame-1.el --- frame.c primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-frame-1--selected ()
  (selected-frame))

(defun emacs-cc-frame-1--framep (frame)
  (and (fboundp 'framep) (framep frame)))

(defun emacs-cc-frame-1--require-framep (frame)
  (unless (emacs-cc-frame-1--framep frame)
    (signal 'wrong-type-argument (list 'framep frame)))
  frame)

(defun emacs-cc-frame-1--require-live (frame)
  (unless (and (emacs-cc-frame-1--framep frame)
               (frame-live-p frame))
    (signal 'wrong-type-argument (list 'frame-live-p frame)))
  frame)

(unless (fboundp 'frame-after-make-frame)
  (defun frame-after-make-frame (frame made)
    "Mark FRAME as made, optionally notifying configuration hooks."
    (ignore frame)
    made))

(unless (fboundp 'frame-ancestor-p)
  (defun frame-ancestor-p (ancestor descendant)
    "Return non-nil if ANCESTOR is an ancestor of DESCENDANT."
    (let ((a (or ancestor (emacs-cc-frame-1--selected)))
          (d (or descendant (emacs-cc-frame-1--selected))))
      (emacs-cc-frame-1--require-live a)
      (emacs-cc-frame-1--require-live d)
      (let ((parent (frame-parent d)) found)
        (while (and parent (not found))
          (if (eq parent a) (setq found t) (setq parent (frame-parent parent))))
        found))))

(unless (fboundp 'frame-bottom-divider-width)
  (defun frame-bottom-divider-width (&optional frame)
    "Return width (in pixels) of horizontal window dividers on FRAME."
    (emacs-cc-frame-1--require-framep (or frame (emacs-cc-frame-1--selected)))
    0))

(unless (fboundp 'frame-child-frame-border-width)
  (defun frame-child-frame-border-width (&optional frame)
    "Return width of FRAME's child-frame border in pixels."
    (let ((f (emacs-cc-frame-1--require-framep
              (or frame (emacs-cc-frame-1--selected)))))
      (or (frame-parameter f 'child-frame-border-width)
          (frame-internal-border-width f)))))

(unless (fboundp 'frame-fringe-width)
  (defun frame-fringe-width (&optional frame)
    "Return fringe width of FRAME in pixels."
    (emacs-cc-frame-1--require-framep (or frame (emacs-cc-frame-1--selected)))
    0))

(unless (fboundp 'frame-id)
  (defun frame-id (&optional frame)
    "Return FRAME's id, or nil if its id has not been set."
    (let ((f (emacs-cc-frame-1--require-live
              (or frame (emacs-cc-frame-1--selected)))))
      (cond ((and (fboundp 'emacs-frame-p) (emacs-frame-p f))
             (emacs-frame-id f))
            ((frame-parameter f 'outer-window-id))
            ;; NeLisp's batch terminal frame is represented by the singleton
            ;; value in `frame-list'; GNU assigns that initial frame id 1.
            ((and (eq f (car (frame-list))) (= (length (frame-list)) 1)) 1)
            (t nil)))))

(unless (fboundp 'frame-internal-border-width)
  (defun frame-internal-border-width (&optional frame)
    "Return width of FRAME's internal border in pixels."
    (emacs-cc-frame-1--require-framep (or frame (emacs-cc-frame-1--selected)))
    0))

(unless (fboundp 'frame-native-height)
  (defun frame-native-height (&optional frame)
    "Return FRAME's native height in pixels (or characters on a terminal)."
    (let ((f (emacs-cc-frame-1--require-framep
              (or frame (emacs-cc-frame-1--selected)))))
      (frame-height f))))

(unless (fboundp 'frame-native-width)
  (defun frame-native-width (&optional frame)
    "Return FRAME's native width in pixels (or characters on a terminal)."
    (let ((f (emacs-cc-frame-1--require-framep
              (or frame (emacs-cc-frame-1--selected)))))
      (frame-width f))))

(unless (fboundp 'frame-parent)
  (defun frame-parent (&optional frame)
    "Return the parent frame of FRAME, or nil if it has none."
    (let ((f (emacs-cc-frame-1--require-live
              (or frame (emacs-cc-frame-1--selected)))))
      (frame-parameter f 'parent-frame))))

(unless (fboundp 'frame-pointer-visible-p)
  (defun frame-pointer-visible-p (&optional frame)
    "Return t if the mouse pointer displayed on FRAME is visible."
    (emacs-cc-frame-1--require-framep (or frame (emacs-cc-frame-1--selected)))
    t))

(unless (fboundp 'frame-position)
  (defun frame-position (&optional frame)
    "Return the top left corner of FRAME in pixels."
    (let ((f (emacs-cc-frame-1--require-live
              (or frame (emacs-cc-frame-1--selected)))))
      (cons (or (frame-parameter f 'left) 0)
            (or (frame-parameter f 'top) 0)))))

(provide 'emacs-cc-frame-1)
