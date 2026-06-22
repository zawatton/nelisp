;;; emacs-core.el --- Reusable editor core loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Library-first entry point for editor-core command, minibuffer, keymap,
;; frame/window, face, mode, Info, and Help surfaces.  It depends on the
;; reusable buffer and editing package loaders so callers can require this
;; one feature without going through the full `emacs-init' application
;; bootstrap.

;;; Code:

(require 'emacs-buffer-core)
(require 'emacs-editing)

;; Minibuffer precedes command-loop because `read-command' delegates to
;; completion through the minibuffer surface.
(defconst emacs-core-features
  '(emacs-keymap
    emacs-window
    emacs-minibuffer
    emacs-minibuffer-builtins
    emacs-command-loop
    emacs-command-loop-builtins
    emacs-keymap-builtins
    emacs-frame
    emacs-frame-builtins
    emacs-window-builtins
    emacs-faces
    emacs-faces-builtins
    emacs-mode
    emacs-mode-builtins
    emacs-info
    emacs-help)
  "Reusable CORE package features loaded by `emacs-core'.")

(dolist (feature emacs-core-features)
  (require feature))

(provide 'emacs-core)

;;; emacs-core.el ends here
