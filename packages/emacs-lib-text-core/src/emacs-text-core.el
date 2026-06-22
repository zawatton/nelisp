;;; emacs-text-core.el --- Reusable text/coding substrate loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Library-first entry point for TXT-owned reusable substrates that other
;; package groups depend on without taking application, frontend, or editor
;; policy.  Keep large optional data tables lazy-loaded by their owning
;; implementation files.

;;; Code:

(defconst emacs-text-core-features
  '(nelisp-regex
    nelisp-coding)
  "Reusable TXT package features loaded by `emacs-text-core'.")

(dolist (feature emacs-text-core-features)
  (require feature))

(provide 'emacs-text-core)

;;; emacs-text-core.el ends here
