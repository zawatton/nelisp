;;; emacs-cc-window-2.el --- Window C-core primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-window-2--check (window predicate)
  (unless (funcall predicate window)
    (signal 'wrong-type-argument (list predicate window)))
  window)

(defun emacs-cc-window-2--selected (window)
  (or window (selected-window)))

(unless (fboundp 'set-window-combination-limit)
  (defun set-window-combination-limit (window limit)
    "Set combination limit of window WINDOW to LIMIT; return LIMIT."
    (emacs-cc-window-2--check window 'window-valid-p)
    (when (window-live-p window)
      (error "Combination limit is meaningful for internal windows only"))
    (let ((entry (assq 'ccore-combination-limit
                       (emacs-window-parameters window))))
      (if entry
          (setcdr entry limit)
        (setf (emacs-window-parameters window)
              (cons (cons 'ccore-combination-limit limit)
                    (emacs-window-parameters window))))
      limit)))

(unless (fboundp 'set-window-cursor-type)
  (defun set-window-cursor-type (window type)
    "Set the `cursor-type' of WINDOW to TYPE."
    (let ((w (emacs-cc-window-2--selected window)))
      (emacs-cc-window-2--check w 'window-live-p)
      (set-window-parameter w 'cursor-type type)
      type)))

(unless (fboundp 'set-window-new-normal)
  (defun set-window-new-normal (window &optional size)
    "Set new normal size of WINDOW to SIZE."
    (emacs-cc-window-2--check (emacs-cc-window-2--selected window) 'window-valid-p)
    (set-window-parameter (emacs-cc-window-2--selected window) 'new-normal size)
    size))

(unless (fboundp 'set-window-new-pixel)
  (defun set-window-new-pixel (window size &optional add)
    "Set new pixel size of WINDOW to SIZE."
    (let* ((w (emacs-cc-window-2--selected window))
           (_ (emacs-cc-window-2--check w 'window-valid-p))
           (value (if add (+ size (or (window-parameter w 'new-pixel) 0)) size)))
      (unless (and (integerp value) (<= 0 value 2147483647))
        (signal 'args-out-of-range (list value 0 2147483647)))
      (set-window-parameter w 'new-pixel value)
      value)))

(unless (fboundp 'set-window-new-total)
  (defun set-window-new-total (window size &optional add)
    "Set new total size of WINDOW to SIZE."
    (let* ((w (emacs-cc-window-2--selected window))
           (_ (emacs-cc-window-2--check w 'window-valid-p))
           (value (if add (+ size (or (window-parameter w 'new-total) 0)) size)))
      (set-window-parameter w 'new-total value)
      value)))

(unless (fboundp 'set-window-scroll-bars)
  (defun set-window-scroll-bars (window &optional width vertical-type height horizontal-type persistent)
    "Set width and type of scroll bars of specified WINDOW."
    (emacs-cc-window-2--check (emacs-cc-window-2--selected window) 'window-live-p)
    (ignore width vertical-type height horizontal-type persistent)
    nil))

(unless (fboundp 'set-window-vscroll)
  (defun set-window-vscroll (window vscroll &optional pixels-p preserve-vscroll-p)
    "Set amount by which WINDOW should be scrolled vertically to VSCROLL."
    (let ((w (emacs-cc-window-2--selected window)))
      (emacs-cc-window-2--check w 'window-valid-p)
      (unless (numberp vscroll) (signal 'wrong-type-argument (list 'numberp vscroll)))
      (ignore preserve-vscroll-p)
      (ignore pixels-p)
      0)))

(unless (fboundp 'split-window-internal)
  (defun split-window-internal (old pixel-size side normal-size &optional refer)
    "Split window OLD."
    (emacs-cc-window-2--check old 'window-valid-p)
    (unless (and (integerp pixel-size) (> pixel-size 0))
      (signal 'wrong-type-argument (list 'integerp pixel-size)))
    (ignore side normal-size refer)
    (error "Sum of sizes of old and new window don’t fit")))

(unless (fboundp 'uncombine-window)
  (defun uncombine-window (window)
    "Uncombine specified WINDOW."
    (emacs-cc-window-2--check window 'window-valid-p)
    nil))

(unless (fboundp 'window-at)
  (defun window-at (x y &optional frame)
    "Return window containing coordinates X and Y on FRAME."
    (unless (integerp x) (signal 'wrong-type-argument (list 'integerp x)))
    (unless (integerp y) (signal 'wrong-type-argument (list 'integerp y)))
    (when frame (emacs-cc-window-2--check frame 'frame-live-p))
    (catch 'found
      (dolist (window (window-list frame t))
        (when (coordinates-in-window-p (cons x y) window)
          (throw 'found window)))
      nil)))

(unless (fboundp 'window-bottom-divider-width)
  (defun window-bottom-divider-width (&optional window)
    "Return the width in pixels of WINDOW's bottom divider."
    (emacs-cc-window-2--check (emacs-cc-window-2--selected window) 'window-live-p)
    0))

(unless (fboundp 'window-bump-use-time)
  (defun window-bump-use-time (&optional window)
    "Mark WINDOW as second most recently used."
    (emacs-cc-window-2--check (emacs-cc-window-2--selected window) 'window-live-p)
    nil))

(provide 'emacs-cc-window-2)

;;; emacs-cc-window-2.el ends here
