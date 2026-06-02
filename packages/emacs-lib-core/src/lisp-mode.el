;;; lisp-mode.el --- Lightweight lisp-mode shim for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Standalone NeLisp uses the local lightweight Elisp editing surface.
;; Host Emacs delegates to its standard lisp-mode library so test tools
;; that require `lisp-mode' continue to see the full host API.

;;; Code:

(defvar lisp-mode--standalone-p (not (boundp 'emacs-version)))

(defun lisp-mode--host-load-standard ()
  "Load host Emacs's standard lisp-mode library."
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
      (load "lisp-mode" nil t))))

(if lisp-mode--standalone-p
    (progn
      (require 'emacs-mode-builtins)
      (require 'emacs-elisp-mode)
      (require 'emacs-elisp-eval)
      (unless (fboundp 'lisp-mode)
        (defalias 'lisp-mode #'emacs-lisp-mode))
      (unless (fboundp 'indent-sexp)
        (defun indent-sexp (&optional _endpos)
          "Placeholder indentation command for the lightweight Elisp mode."
          (interactive)
          nil)))
  (lisp-mode--host-load-standard))

(provide 'lisp-mode)

;;; lisp-mode.el ends here
