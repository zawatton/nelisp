;;; simple.el --- Lightweight simple.el shim for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; GNU Emacs's lisp/simple.el is large and currently too expensive to
;; cold-load under standalone NeLisp.  This shim provides the small
;; `simple' feature surface needed by the daily-driver smoke lane while
;; the full vendored file remains a compatibility target.

;;; Code:

(unless (boundp 'max-mini-window-lines)
  (defvar max-mini-window-lines 1
    "Maximum minibuffer window height as a line count or frame fraction."))

(unless (boundp 'indent-line-function)
  (defvar indent-line-function nil
    "Function called by `indent-for-tab-command' to indent the current line."))

(unless (boundp 'filter-buffer-substring-functions)
  (defvar filter-buffer-substring-functions nil
    "Obsolete wrapper hook around `buffer-substring--filter'."))

(unless (boundp 'filter-buffer-substring-function)
  (defvar filter-buffer-substring-function #'buffer-substring--filter
    "Function used by `filter-buffer-substring' to filter copied text."))

(unless (boundp 'text-scale-mode)
  (defvar text-scale-mode nil
    "Non-nil when the lightweight text scaling minor mode is active."))

(unless (boundp 'text-scale-mode-amount)
  (defvar text-scale-mode-amount 0
    "Current text scale delta in lightweight simple/face-remap shims."))

(unless (boundp 'text-scale-mode-lighter)
  (defvar text-scale-mode-lighter "+0"
    "Mode line lighter used by lightweight `text-scale-mode' shims."))

(unless (boundp 'face-remapping-alist)
  (defvar face-remapping-alist nil
    "Buffer-local face remapping alist for lightweight simple shims."))

(unless (fboundp 'delete-and-extract-region)
  (defun delete-and-extract-region (beg end)
    "Delete text between BEG and END and return it."
    (let ((text (buffer-substring beg end)))
      (delete-region beg end)
      text)))

(unless (fboundp 'buffer-substring--filter)
  (defun buffer-substring--filter (beg end &optional delete)
    "Default function for `filter-buffer-substring-function'."
    (let ((text (if delete
                    (delete-and-extract-region beg end)
                  (buffer-substring beg end))))
      (if filter-buffer-substring-functions
          (let ((value text))
            (dolist (fn filter-buffer-substring-functions)
              (setq value (funcall fn value beg end delete)))
            value)
        text))))

(unless (fboundp 'filter-buffer-substring)
  (defun filter-buffer-substring (beg end &optional delete)
    "Return filtered buffer text between BEG and END.
When DELETE is non-nil, delete the source text after copying."
    (funcall filter-buffer-substring-function beg end delete)))

(unless (fboundp 'open-line)
  (defun open-line (&optional n)
    "Insert N newlines after point, leaving point before them."
    (interactive "p")
    (let ((count (or n 1))
          (pos (point)))
      (while (> count 0)
        (newline)
        (setq count (1- count)))
      (goto-char pos)
      nil)))

(unless (fboundp 'quoted-insert)
  (defun quoted-insert (&optional arg)
    "Read the next character and insert it ARG times."
    (interactive "p")
    (let ((count (or arg 1))
          (char (read-char)))
      (while (> count 0)
        (self-insert-command 1 char)
        (setq count (1- count)))
      nil)))

(unless (fboundp 'indent-for-tab-command)
  (defun indent-for-tab-command (&optional arg)
    "Indent the current line, or insert a tab when no indenter is set."
    (interactive "P")
    (ignore arg)
    (cond
     ((and (boundp 'indent-line-function)
           (functionp indent-line-function))
      (funcall indent-line-function))
     (t
      (self-insert-command 1 9)))
    nil))

;; GNU simple.el's visual-line family is part of the standard mode surface,
;; but loading the full 9k-line file is outside the headless bootstrap budget.
;; Keep the mode state and wrapping variables usable for packages such as
;; visual-fill-column; the redisplay backend remains responsible for rendering
;; the resulting soft-wrap request.
(unless (boundp 'visual-line-mode-map)
  (defvar visual-line-mode-map (make-sparse-keymap)
    "Keymap used while `visual-line-mode' is active."))

(unless (boundp 'visual-line-mode)
  (defvar visual-line-mode nil
    "Non-nil when visual line wrapping is active in the current buffer."))

(unless (boundp 'visual-line-mode--saved-values)
  (defvar visual-line-mode--saved-values nil
    "Values of `truncate-lines' and `word-wrap' before visual line mode."))

(make-variable-buffer-local 'visual-line-mode--saved-values)

(unless (fboundp 'visual-line-mode--apply-wrap)
  (defun visual-line-mode--apply-wrap (enabled)
    "Set visual line wrapping state."
    (if enabled
        (progn
          (unless visual-line-mode--saved-values
            (setq visual-line-mode--saved-values
                  (list truncate-lines word-wrap)))
          (setq truncate-lines nil word-wrap t))
      (if visual-line-mode--saved-values
          (setq truncate-lines (car visual-line-mode--saved-values)
                word-wrap (car (cdr visual-line-mode--saved-values)))
        (setq truncate-lines nil word-wrap nil))
      (setq visual-line-mode--saved-values nil))))

(unless (fboundp 'visual-line-mode)
  (defun visual-line-mode (&optional arg)
    "Toggle visual line wrapping in the current buffer.
With positive ARG enable it; with zero or negative ARG disable it."
    (interactive "P")
    (make-variable-buffer-local 'visual-line-mode)
    (setq visual-line-mode
          (if (null arg)
              (not visual-line-mode)
            (> (if (numberp arg) arg 1) 0)))
    (visual-line-mode--apply-wrap visual-line-mode)
    visual-line-mode))

(unless (fboundp 'turn-on-visual-line-mode)
  (defun turn-on-visual-line-mode ()
    "Enable `visual-line-mode' in the current buffer."
    (visual-line-mode 1)))

(unless (boundp 'global-visual-line-mode)
  (defvar global-visual-line-mode nil
    "Non-nil when visual line wrapping is enabled globally."))

(unless (boundp 'global-minor-modes)
  (defvar global-minor-modes nil
    "Minor modes enabled globally by lightweight mode shims."))

(unless (fboundp 'global-visual-line-mode--enable-in-buffer)
  (defun global-visual-line-mode--enable-in-buffer ()
    "Enable `visual-line-mode' in the current buffer for global mode."
    (when global-visual-line-mode
      (visual-line-mode 1))))

(unless (fboundp 'global-visual-line-mode--set-hook)
  (defun global-visual-line-mode--set-hook (enabled)
    "Install or remove the global visual line mode major-mode hook."
    (if enabled
        (when (fboundp 'add-hook)
          (add-hook 'after-change-major-mode-hook
                    #'global-visual-line-mode--enable-in-buffer))
      (when (fboundp 'remove-hook)
        (remove-hook 'after-change-major-mode-hook
                     #'global-visual-line-mode--enable-in-buffer)))))

(unless (fboundp 'global-visual-line-mode--set-state)
  (defun global-visual-line-mode--set-state (enabled)
    "Update the global minor mode registry for visual line mode."
    (if enabled
        (add-to-list 'global-minor-modes 'global-visual-line-mode)
      (setq global-minor-modes
            (delq 'global-visual-line-mode global-minor-modes)))))

(unless (fboundp 'global-visual-line-mode)
  (defun global-visual-line-mode (&optional arg)
    "Toggle visual line wrapping in buffers visited after enabling.
With positive ARG enable it; with zero or negative ARG disable it."
    (interactive "P")
    (setq global-visual-line-mode
          (if (null arg)
              (not global-visual-line-mode)
            (> (if (numberp arg) arg 1) 0)))
    (global-visual-line-mode--set-state global-visual-line-mode)
    (global-visual-line-mode--set-hook global-visual-line-mode)
    (when global-visual-line-mode
      (turn-on-visual-line-mode))
    global-visual-line-mode))

(provide 'simple)

;;; simple.el ends here
