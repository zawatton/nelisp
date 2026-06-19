;;; help-mode.el --- Lightweight help-mode shim for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Re-export the daily-driver help buffer implementation as the standard
;; `help-mode' feature while the full vendored help-mode.el remains a
;; later compatibility target.

;;; Code:

(defvar help-mode--standalone-p
  (or (fboundp 'nl-write-file)
      (not (boundp 'emacs-version))))

(defun help-mode--host-load-standard ()
  "Load host Emacs's standard help-mode library."
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
      (load "help-mode" nil t))))

(defun help-mode--install-standalone-command (public)
  "Install PUBLIC as a lazy wrapper around `emacs-help'."
  (unless (fboundp public)
    (fset public
          (list 'lambda '(&rest args)
                '(require 'emacs-help)
                (list 'apply (list 'quote public) 'args)))))

(if help-mode--standalone-p
    (dolist (symbol '(help-mode help-go-back help-go-forward))
      (help-mode--install-standalone-command symbol))
  (help-mode--host-load-standard))

(provide 'help-mode)

;;; help-mode.el ends here
