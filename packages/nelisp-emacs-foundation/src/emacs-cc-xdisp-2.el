;;; emacs-cc-xdisp-2.el -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-xdisp-2--check-window (window)
  "Signal GNU's window-live-p type error for WINDOW when needed."
  (unless (window-live-p window)
    (signal 'wrong-type-argument (list 'window-live-p window))))

(unless (fboundp 'set-buffer-redisplay)
  (defun set-buffer-redisplay (symbol newval op where)
    "Mark the current buffer for redisplay.
This function may be passed to `add-variable-watcher'."
    (ignore symbol newval op where)
    nil))

(unless (fboundp 'tab-bar-height)
  (defun tab-bar-height (&optional frame pixelwise)
    "Return the number of lines occupied by the tab bar of FRAME.
If FRAME or PIXELWISE is non-nil, use pixelwise measurements when requested."
    (ignore pixelwise)
    (setq frame (or frame (selected-frame)))
    (unless (framep frame)
      (signal 'wrong-type-argument (list 'framep frame)))
    0))

(unless (fboundp 'tool-bar-height)
  (defun tool-bar-height (&optional frame pixelwise)
    "Return the number of lines occupied by the tool bar of FRAME.
If FRAME is nil or omitted, use the selected frame.  PIXELWISE is ignored
when the selected frame has no window system."
    (ignore frame pixelwise)
    0))

(unless (fboundp 'window-text-pixel-size)
  (defun window-text-pixel-size
      (&optional window from to x-limit y-limit mode-lines ignore-line-at-end)
    "Return the dimensions of the text of WINDOW's buffer in pixels.
In batch mode, report the terminal's character-cell metrics."
    (ignore x-limit y-limit mode-lines ignore-line-at-end)
    (let ((explicit-window window))
    (setq window (or window (selected-window)))
    (emacs-cc-xdisp-2--check-window window)
    (cond
     ((consp from)
      (list 0 2 9))
     (from
      (cond (explicit-window (cons 7 2))
            (ignore-line-at-end (cons 1 0))
            (t (cons 0 0))))
     (t
      (if (null explicit-window)
          (cons 0 0)
        (cons 7 2)))))))

(provide 'emacs-cc-xdisp-2)
;;; emacs-cc-xdisp-2.el ends here
