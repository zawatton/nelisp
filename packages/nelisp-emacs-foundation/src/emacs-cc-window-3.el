;;; emacs-cc-window-3.el --- window.c batch-compatible primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-window-3--check-window (window live)
  "Check WINDOW using the requested LIVE predicate and return it."
  (let ((predicate (if live 'window-live-p 'window-valid-p)))
    (unless (and (fboundp 'windowp) (windowp window)
                 (fboundp predicate) (funcall predicate window))
      (signal 'wrong-type-argument (list predicate window)))
    window))

(unless (fboundp 'window-configuration-frame)
  (defun window-configuration-frame (config)
    "Return the frame that CONFIG, a window-configuration object, is about."
    (unless (and (fboundp 'window-configuration-p)
                 (window-configuration-p config))
      (signal 'wrong-type-argument (list 'window-configuration-p config)))
    (selected-frame)))

(unless (fboundp 'window-cursor-info)
  (defun window-cursor-info (&optional window)
    "Return information about the cursor of WINDOW."
    (emacs-cc-window-3--check-window (or window (selected-window)) t)
    nil))

(unless (fboundp 'window-cursor-type)
  (defun window-cursor-type (&optional window)
    "Return the `cursor-type' of WINDOW."
    (let* ((win (or window (selected-window)))
           buffer)
      (emacs-cc-window-3--check-window win t)
      (setq buffer (window-buffer win))
      (if (and buffer (bufferp buffer))
          (with-current-buffer buffer
            (if (local-variable-p 'cursor-type) cursor-type t))
        t))))

(unless (fboundp 'window-discard-buffer-from-window)
  (defun window-discard-buffer-from-window (buffer window &optional all)
    "Discard BUFFER from the history lists of WINDOW."
    (unless (bufferp buffer)
      (signal 'wrong-type-argument (list 'bufferp buffer)))
    (unless (and (fboundp 'window-live-p) (window-live-p window))
      (signal 'error (list "Not a live window")))
    (when (fboundp 'window-prev-buffers)
      (set-window-prev-buffers window
                               (cl-remove-if (lambda (entry) (eq (car entry) buffer))
                                             (window-prev-buffers window))))
    (when (fboundp 'window-next-buffers)
      (set-window-next-buffers window
                               (cl-remove-if (lambda (entry) (eq (car entry) buffer))
                                             (window-next-buffers window))))
    (when all
      (dolist (parameter '(quit-restore quit-restore-prev))
        (when (eq (car-safe (window-parameter window parameter)) buffer)
          (set-window-parameter window parameter nil))))
    nil))

(unless (fboundp 'window-fringes)
  (defun window-fringes (&optional window)
    "Return fringe settings for specified WINDOW."
    (emacs-cc-window-3--check-window (or window (selected-window)) t)
    '(0 0 nil nil)))

(unless (fboundp 'window-header-line-height)
  (defun window-header-line-height (&optional window)
    "Return the height in pixels of WINDOW's header-line."
    (emacs-cc-window-3--check-window (or window (selected-window)) t)
    0))

(unless (fboundp 'window-left-child)
  (defun window-left-child (&optional window)
    "Return the leftmost child window of WINDOW."
    (emacs-cc-window-3--check-window (or window (selected-window)) nil)
    nil))

(unless (fboundp 'window-left-column)
  (defun window-left-column (&optional window)
    "Return left column of window WINDOW."
    (let* ((win (or window (selected-window)))
           edges)
      (emacs-cc-window-3--check-window win nil)
      (setq edges (if (fboundp 'window-edges)
                      (window-edges win)
                    (emacs-window-window-edges win)))
      (car edges))))

(unless (fboundp 'window-line-height)
  (defun window-line-height (&optional line window)
    "Return height in pixels of text line LINE in window WINDOW."
    (let ((win (or window (selected-window))))
      (emacs-cc-window-3--check-window win t)
      (when (and line (not (memq line '(header-line mode-line)))
                 (not (integerp line)))
        (signal 'wrong-type-argument (list 'integerp line)))
      nil)))

(unless (fboundp 'window-lines-pixel-dimensions)
  (defun window-lines-pixel-dimensions (&optional window first last body inverse left)
    "Return pixel dimensions of WINDOW's lines."
    (ignore first last body inverse left)
    (emacs-cc-window-3--check-window (or window (selected-window)) t)
    nil))

(unless (fboundp 'window-mode-line-height)
  (defun window-mode-line-height (&optional window)
    "Return the height in pixels of WINDOW's mode-line."
    (emacs-cc-window-3--check-window (or window (selected-window)) t)
    1))

(unless (fboundp 'window-new-normal)
  (defun window-new-normal (&optional window)
    "Return new normal size of window WINDOW."
    (let ((win (emacs-cc-window-3--check-window
                (or window (selected-window)) nil)))
      (or (window-parameter win 'new-normal) 0))))

(provide 'emacs-cc-window-3)

;;; emacs-cc-window-3.el ends here
