;;; emacs-cc-window-5.el --- window.c primitives -*- lexical-binding: t; -*-

;;; Code:

(require 'emacs-window)
(require 'emacs-frame)

(unless (fboundp 'window-pixel-height)
  (defun window-pixel-height (&optional window)
    "Return the height of window WINDOW in pixels."
    (let ((win (or window (selected-window))))
      (unless (window-valid-p win)
        (signal 'wrong-type-argument (list 'window-valid-p win)))
      (if (emacs-window-parent win)
          (- (1- (frame-height (window-frame win))) (window-height win))
        (window-height win)))))
(unless (fboundp 'window-pixel-left)
  (defun window-pixel-left (&optional window)
    "Return left pixel edge of window WINDOW."
    (let ((win (or window (selected-window))))
      (unless (window-valid-p win)
        (signal 'wrong-type-argument (list 'window-valid-p win)))
      (car (emacs-window-window-edges win)))))
(unless (fboundp 'window-pixel-top)
  (defun window-pixel-top (&optional window)
    "Return top pixel edge of window WINDOW."
    (let ((win (or window (selected-window))))
      (unless (window-valid-p win)
        (signal 'wrong-type-argument (list 'window-valid-p win)))
      (let ((top (cadr (emacs-window-window-edges win))))
        (if (and (/= top 0) (emacs-window-parent win))
            (- (1- (frame-height (window-frame win))) top)
          top)))))
(unless (fboundp 'window-pixel-width)
  (defun window-pixel-width (&optional window)
    "Return the width of window WINDOW in pixels."
    (let ((win (or window (selected-window))))
      (unless (window-valid-p win)
        (signal 'wrong-type-argument (list 'window-valid-p win)))
      (window-width win))))
(unless (fboundp 'window-prev-sibling)
  (defun window-prev-sibling (&optional window)
    "Return the previous sibling window of window WINDOW."
    (let* ((win (or window (selected-window)))
           (siblings (and (window-valid-p win) (window-list))))
      (unless (window-valid-p win)
        (signal 'wrong-type-argument (list 'window-valid-p win)))
      (let ((before (memq win siblings)))
        (and before (not (eq before siblings)) (nth (1- (- (length siblings) (length before))) siblings))))))
(unless (fboundp 'window-resize-apply)
  (defun window-resize-apply (&optional frame horizontal)
    "Apply requested size values for window-tree of FRAME."
    (let ((f (or frame (selected-frame))))
      (unless (frame-live-p f) (signal 'wrong-type-argument (list 'frame-live-p f)))
      (and horizontal t))))
(unless (fboundp 'window-resize-apply-total)
  (defun window-resize-apply-total (&optional frame horizontal)
    "Apply requested total size values for window-tree of FRAME."
    (ignore horizontal)
    (let ((f (or frame (selected-frame))))
      (unless (frame-live-p f) (signal 'wrong-type-argument (list 'frame-live-p f)))
      t)))
(unless (fboundp 'window-right-divider-width)
  (defun window-right-divider-width (&optional window)
    "Return the width in pixels of WINDOW's right divider."
    (let ((win (or window (selected-window))))
      (unless (window-live-p win) (signal 'wrong-type-argument (list 'window-live-p win)))
      0)))
(unless (fboundp 'window-scroll-bar-height)
  (defun window-scroll-bar-height (&optional window)
    "Return the height in pixels of WINDOW's horizontal scrollbar."
    (let ((win (or window (selected-window))))
      (unless (window-live-p win) (signal 'wrong-type-argument (list 'window-live-p win)))
      0)))
(unless (fboundp 'window-scroll-bars)
  (defun window-scroll-bars (&optional window)
    "Get width and type of scroll bars of window WINDOW."
    (let ((win (or window (selected-window))))
      (unless (window-live-p win) (signal 'wrong-type-argument (list 'window-live-p win)))
      (list nil 0 t nil 0 t nil))))
(unless (fboundp 'window-scroll-bar-width)
  (defun window-scroll-bar-width (&optional window)
    "Return the width in pixels of WINDOW's vertical scrollbar."
    (let ((win (or window (selected-window))))
      (unless (window-live-p win) (signal 'wrong-type-argument (list 'window-live-p win)))
      0)))
(unless (fboundp 'window-tab-line-height)
  (defun window-tab-line-height (&optional window)
    "Return the height in pixels of WINDOW's tab-line."
    (let ((win (or window (selected-window))))
      (unless (window-live-p win) (signal 'wrong-type-argument (list 'window-live-p win)))
      0)))

(provide 'emacs-cc-window-5)
;;; emacs-cc-window-5.el ends here
