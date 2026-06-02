;;; help-fns.el --- Lightweight help-fns shim for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Re-export the daily-driver describe-* implementation as the standard
;; `help-fns' feature without loading GNU Emacs's much larger help-fns.el.

;;; Code:

(defvar help-fns--standalone-p (not (boundp 'emacs-version)))

(defun help-fns--host-load-standard ()
  "Load host Emacs's standard help-fns library."
  (let ((shim-dir (file-truename
                   (file-name-as-directory
                    (file-name-directory (or load-file-name
                                             buffer-file-name)))))
        filtered)
    (dolist (dir load-path)
      (unless (equal (file-truename (file-name-as-directory dir))
                     shim-dir)
        (push dir filtered)))
    (let ((load-path (nreverse filtered)))
      (load "help-fns" nil t))))

(if help-fns--standalone-p
    (require 'emacs-help)
  (help-fns--host-load-standard))

(provide 'help-fns)

;;; help-fns.el ends here
