;;; emacs-buffer-core.el --- Reusable buffer/search/line loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Library-first entry point for the low-level BUF package group.  It exposes
;; buffer primitives, regex/search bridges, and line/point helpers without
;; forcing file I/O, command-loop, GUI, or application bootstrap code.

;;; Code:

(defconst emacs-buffer-core-features
  '(nelisp-text-buffer
    nelisp-emacs-compat
    emacs-buffer-builtins
    emacs-buffer
    emacs-search-builtins
    emacs-line-builtins)
  "Reusable BUF package features loaded by `emacs-buffer-core'.")

(dolist (feature emacs-buffer-core-features)
  (require feature))

(provide 'emacs-buffer-core)

;;; emacs-buffer-core.el ends here
