;;; emacs-cc-window-4.el --- GNU window.c primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-window-4--valid-window (window predicate)
  "Return WINDOW or signal GNU's validation error for PREDICATE."
  (unless (funcall predicate window)
    (signal 'wrong-type-argument (list predicate window)))
  window)

(unless (fboundp 'window-new-pixel)
  (defun window-new-pixel (&optional window)
    "Return new pixel size of window WINDOW."
    (emacs-cc-window-4--valid-window (or window (selected-window))
                                     'window-valid-p)
    0))

(unless (fboundp 'window-new-total)
  (defun window-new-total (&optional window)
    "Return the new total size of window WINDOW."
    (emacs-cc-window-4--valid-window (or window (selected-window))
                                     'window-valid-p)
    0))

(unless (fboundp 'window-next-sibling)
  (defun window-next-sibling (&optional window)
    "Return the next sibling window of window WINDOW."
    (emacs-cc-window-4--valid-window (or window (selected-window))
                                     'window-valid-p)
    nil))

(unless (fboundp 'window-normal-size)
  (defun window-normal-size (&optional window horizontal)
    "Return the normal height or width of window WINDOW."
    (ignore horizontal)
    (emacs-cc-window-4--valid-window (or window (selected-window))
                                     'window-valid-p)
    1.0))

(unless (fboundp 'window-old-body-pixel-height)
  (defun window-old-body-pixel-height (&optional window)
    "Return old height of WINDOW's text area in pixels."
    (emacs-cc-window-4--valid-window (or window (selected-window))
                                     'window-live-p)
    0))

(unless (fboundp 'window-old-body-pixel-width)
  (defun window-old-body-pixel-width (&optional window)
    "Return old width of WINDOW's text area in pixels."
    (emacs-cc-window-4--valid-window (or window (selected-window))
                                     'window-live-p)
    0))

(unless (fboundp 'window-old-buffer)
  (defun window-old-buffer (&optional window)
    "Return the old buffer displayed by WINDOW."
    (let ((window (or window (selected-window))))
      (unless (windowp window)
        (signal 'wrong-type-argument (list 'windowp window)))
      nil)))

(unless (fboundp 'window-old-pixel-height)
  (defun window-old-pixel-height (&optional window)
    "Return old total pixel height of WINDOW."
    (emacs-cc-window-4--valid-window (or window (selected-window))
                                     'window-valid-p)
    0))

(unless (fboundp 'window-old-pixel-width)
  (defun window-old-pixel-width (&optional window)
    "Return old total pixel width of WINDOW."
    (emacs-cc-window-4--valid-window (or window (selected-window))
                                     'window-valid-p)
    0))

(unless (fboundp 'window-old-point)
  (defun window-old-point (&optional window)
    "Return old value of point in WINDOW."
    (emacs-cc-window-4--valid-window (or window (selected-window))
                                     'window-live-p)
    (window-point window)))

(unless (fboundp 'window-parameters)
  (defun window-parameters (&optional window)
    "Return the parameters of WINDOW and their values."
    (emacs-cc-window-4--valid-window (or window (selected-window))
                                     'window-valid-p)
    nil))

(unless (fboundp 'window-parent)
  (defun window-parent (&optional window)
    "Return the parent window of window WINDOW."
    (emacs-cc-window-4--valid-window (or window (selected-window))
                                     'window-valid-p)
    nil))

(provide 'emacs-cc-window-4)
;;; emacs-cc-window-4.el ends here
