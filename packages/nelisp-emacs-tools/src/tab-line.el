;;; tab-line.el --- Minimal tab-line subset for nelisp-emacs -*- lexical-binding: t; -*-

;;; Code:

(defvar tab-line-mode nil
  "Non-nil when the current buffer uses the minimal tab line.")

(defvar global-tab-line-mode nil
  "Non-nil when the minimal tab line is globally enabled.")

(defvar tab-line-format nil
  "Minimal tab-line format placeholder.")

(defun tab-line-mode (&optional arg)
  "Toggle minimal buffer-local tab-line mode."
  (interactive "P")
  (setq tab-line-mode
        (if (null arg)
            (not tab-line-mode)
          (> (prefix-numeric-value arg) 0)))
  (setq tab-line-format (and tab-line-mode '(:eval (buffer-name))))
  tab-line-mode)

(defun global-tab-line-mode (&optional arg)
  "Toggle minimal global tab-line mode."
  (interactive "P")
  (setq global-tab-line-mode
        (if (null arg)
            (not global-tab-line-mode)
          (> (prefix-numeric-value arg) 0)))
  global-tab-line-mode)

(unless (fboundp 'window-tab-line-height)
  (defun window-tab-line-height (&optional _window)
    "Return the minimal tab-line height in text lines."
    (if (or tab-line-mode global-tab-line-mode) 1 0)))

(defun tab-line-tabs-buffer-list ()
  "Return buffers shown by the minimal tab line."
  (buffer-list))

(defun tab-line-tabs-window-buffers ()
  "Return buffers shown by the selected window's minimal tab line."
  (tab-line-tabs-buffer-list))

(defun tab-line-tabs-fixed-window-buffers ()
  "Return buffers shown by the fixed minimal tab line."
  (tab-line-tabs-buffer-list))

(defun tab-line-tab-name-buffer (buffer &optional _buffers)
  "Return BUFFER's display name for the minimal tab line."
  (buffer-name buffer))

(provide 'tab-line)

;;; tab-line.el ends here
