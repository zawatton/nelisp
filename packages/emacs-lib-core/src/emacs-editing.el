;;; emacs-editing.el --- Reusable editing-command loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Library-first entry point for edit commands and undo.  Callers should load
;; `emacs-buffer-core' first when they need a standalone buffer substrate;
;; this loader preserves the required undo-before-edit command order.

;;; Code:

(defconst emacs-editing-features
  '(emacs-undo
    emacs-undo-builtins
    emacs-edit-builtins)
  "Reusable editing/undo package features loaded by `emacs-editing'.")

(dolist (feature emacs-editing-features)
  (require feature))

(provide 'emacs-editing)

;;; emacs-editing.el ends here
