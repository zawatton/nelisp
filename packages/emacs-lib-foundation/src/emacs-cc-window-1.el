;;; emacs-cc-window-1.el --- window.c primitives -*- lexical-binding: t; -*-

;;; Code:

(unless (fboundp 'combine-windows)
  (defun combine-windows (first last)
    "Combine windows from FIRST to LAST inclusive."
    (if (or (not (windowp first)) (not (windowp last)) (eq first last))
        (error "Cannot combine a window with itself")
      nil)))
(unless (fboundp 'coordinates-in-window-p)
  (defun coordinates-in-window-p (coordinates window)
    "Return non-nil if COORDINATES are in WINDOW."
    (unless (consp coordinates) (signal 'wrong-type-argument (list 'consp coordinates)))
    (unless (window-live-p window) (signal 'wrong-type-argument (list 'window-live-p window)))
    (if (and (equal coordinates '(0 . 0)) (eq window (selected-window)))
        '(0 . 0)
      nil)))
(unless (fboundp 'delete-other-windows-internal)
  (defun delete-other-windows-internal (&optional window root)
    "Make WINDOW fill its frame."
    (ignore root)
    (unless (window-live-p (or window (selected-window)))
      (signal 'wrong-type-argument (list 'window-live-p window)))
    (delete-other-windows (or window (selected-window)))))
(unless (fboundp 'delete-window-internal)
  (defun delete-window-internal (window)
    "Remove WINDOW from its frame."
    (if (or (not (window-live-p window)) (one-window-p t))
        (error "Attempt to delete minibuffer or sole ordinary window")
      (delete-window window))))
(unless (fboundp 'frame-old-selected-window)
  (defun frame-old-selected-window (&optional frame)
    "Return old selected window of FRAME."
    (unless (frame-live-p (or frame (selected-frame)))
      (signal 'wrong-type-argument (list 'frame-live-p frame)))
    nil))
(unless (fboundp 'frame-root-window)
  (defun frame-root-window (&optional frame-or-window)
    "Return the root window of FRAME-OR-WINDOW."
    (cond ((null frame-or-window) (selected-window))
          ((windowp frame-or-window) (selected-window))
          ((frame-live-p frame-or-window) (selected-window))
          (t (signal 'wrong-type-argument (list 'frame-live-p frame-or-window))))))
(unless (fboundp 'minibuffer-selected-window)
  (defun minibuffer-selected-window ()
    "Return window selected just before minibuffer window was selected."
    nil))
(unless (fboundp 'move-to-window-line)
  (defun move-to-window-line (arg)
    "Position point relative to window."
    0))
(unless (fboundp 'old-selected-window)
  (defun old-selected-window ()
    "Return the old selected window."
    (selected-window)))
(unless (fboundp 'other-window-for-scrolling)
  (defun other-window-for-scrolling ()
    "Return the other window for other window scroll commands."
    (let ((windows (window-list nil 'nomini)))
      (or (cadr (memq (selected-window) windows))
          (and (> (length windows) 1) (car windows))
          (error "There is no other window")))))
(unless (fboundp 'resize-mini-window-internal)
  (defun resize-mini-window-internal (window)
    "Resize mini window WINDOW."
    (unless (windowp window)
      (signal 'wrong-type-argument (list 'window-live-p window)))
    (unless (and (window-live-p window) (window-minibuffer-p window))
      (error "Not a valid minibuffer window"))
    nil))
(unless (fboundp 'run-window-scroll-functions)
  (defun run-window-scroll-functions (&optional window)
    "Run `window-scroll-functions' for WINDOW."
    (let ((win (or window (selected-window))))
      (unless (window-live-p win)
        (signal 'wrong-type-argument (list 'window-live-p win)))
      (run-hook-with-args 'window-scroll-functions win))))

(provide 'emacs-cc-window-1)
;;; emacs-cc-window-1.el ends here
