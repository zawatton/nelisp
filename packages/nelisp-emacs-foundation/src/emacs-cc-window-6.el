;;; emacs-cc-window-6.el --- Window geometry C-core primitives -*- lexical-binding: t; -*-

(defun emacs-cc-window-6--check (window predicate)
  (unless (funcall predicate window)
    (signal 'wrong-type-argument (list predicate window)))
  window)

(defun emacs-cc-window-6--window (window predicate)
  (emacs-cc-window-6--check (or window (selected-window)) predicate))

(unless (fboundp 'window-text-height)
  (defun window-text-height (&optional window pixelwise)
    "Return the height in lines of the text display area of WINDOW."
    (window-body-height (emacs-cc-window-6--window window 'window-live-p) pixelwise)))

(unless (fboundp 'window-text-width)
  (defun window-text-width (&optional window pixelwise)
    "Return the width in columns of the text display area of WINDOW."
    (window-body-width (emacs-cc-window-6--window window 'window-live-p) pixelwise)))

(unless (fboundp 'window-top-child)
  (defun window-top-child (&optional window)
    "Return the topmost child window of window WINDOW."
    (emacs-cc-window-6--window window 'window-valid-p)
    nil))

(unless (fboundp 'window-top-line)
  (defun window-top-line (&optional window)
    "Return top line of window WINDOW."
    (cadr (emacs-window-window-edges
           (emacs-cc-window-6--window window 'window-valid-p)))))

(unless (fboundp 'window-total-height)
  (defun window-total-height (&optional window round)
    "Return the height of window WINDOW in canonical lines."
    (ignore round)
    (window-height (emacs-cc-window-6--window window 'window-valid-p))))

(unless (fboundp 'window-total-width)
  (defun window-total-width (&optional window round)
    "Return the total width of window WINDOW in canonical columns."
    (ignore round)
    (window-width (emacs-cc-window-6--window window 'window-valid-p))))

(unless (fboundp 'window-use-time)
  (defun window-use-time (&optional window)
    "Return the use time of window WINDOW."
    (emacs-window-use-time
     (emacs-cc-window-6--window window 'window-live-p))))

(unless (fboundp 'window-vscroll)
  (defun window-vscroll (&optional window pixels-p)
    "Return the amount by which WINDOW is scrolled vertically."
    (emacs-cc-window-6--window window 'window-valid-p)
    (ignore pixels-p)
    0))

(provide 'emacs-cc-window-6)
