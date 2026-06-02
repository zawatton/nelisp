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

(provide 'simple)

;;; simple.el ends here
