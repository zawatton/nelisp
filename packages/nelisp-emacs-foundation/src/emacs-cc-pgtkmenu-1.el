;;; emacs-cc-pgtkmenu-1.el --- PGTK menu primitives -*- lexical-binding: t; -*-

(unless (fboundp 'menu-or-popup-active-p)
  (defun menu-or-popup-active-p ()
    "Return t if a menu or popup dialog is active.
    (On MS Windows, this refers to the selected frame.)

(fn)"
    (and (fboundp 'frame-parameter)
         (frame-parameter (selected-frame) 'menu-or-popup-active))))

(unless (fboundp 'x-menu-bar-open-internal)
  (defun x-menu-bar-open-internal (&optional frame)
    "Start key navigation of the menu bar in FRAME.
This initially opens the first menu bar item and you can then navigate with the
arrow keys, select a menu entry with the return key or cancel with the
escape key.  If FRAME has no menu bar this function does nothing.

(fn &optional FRAME)"
    (let ((target (or frame (selected-frame))))
      (unless (frame-live-p target)
        (signal 'wrong-type-argument (list 'frame-live-p target)))
      (if (and (fboundp 'framep) (eq (framep target) 'x))
          nil
        (error "Window system frame should be used")))))

(provide 'emacs-cc-pgtkmenu-1)
