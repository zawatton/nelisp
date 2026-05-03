;;; emacs-font-lock-builtins.el --- Unprefixed font-lock bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track K (2026-05-03) — Layer 2 γ-deeper.
;;
;; Bridges the Emacs core *unprefixed* font-lock surface to the
;; prefixed substrate in `emacs-font-lock.el'.  Each definition is
;; gated on `unless (fboundp ...)' so loading inside a host Emacs is
;; a cheap no-op (= host's `font-lock.el' wins).
;;
;; Bridged today:
;;   - font-lock-mode                           (function)
;;   - font-lock-fontify-region                 (function)
;;   - font-lock-fontify-buffer                 (function)
;;   - font-lock-unfontify-region               (function)
;;   - font-lock-unfontify-buffer               (function)
;;   - font-lock-default-fontify-region         (function)
;;   - font-lock-add-keywords                   (function)
;;   - font-lock-remove-keywords                (function)
;;   - font-lock-set-defaults                   (function)
;;
;; State variables (`unless (boundp ...)'):
;;   - font-lock-defaults
;;   - font-lock-keywords
;;   - font-lock-keywords-only
;;   - font-lock-keywords-case-fold-search
;;   - font-lock-syntax-table
;;   - font-lock-multiline
;;   - font-lock-set-defaults
;;
;; Standard face names (`font-lock-keyword-face' etc) are registered
;; by the substrate at load-time — no further wiring needed here.

;;; Code:

(require 'emacs-font-lock)

;;;; --- function bridges -----------------------------------------------

(unless (fboundp 'font-lock-mode)
  (defalias 'font-lock-mode #'emacs-font-lock-mode))

(unless (fboundp 'font-lock-fontify-region)
  (defalias 'font-lock-fontify-region #'emacs-font-lock-fontify-region))

(unless (fboundp 'font-lock-fontify-buffer)
  (defalias 'font-lock-fontify-buffer #'emacs-font-lock-fontify-buffer))

(unless (fboundp 'font-lock-unfontify-region)
  (defalias 'font-lock-unfontify-region #'emacs-font-lock-unfontify-region))

(unless (fboundp 'font-lock-unfontify-buffer)
  (defalias 'font-lock-unfontify-buffer #'emacs-font-lock-unfontify-buffer))

(unless (fboundp 'font-lock-default-fontify-region)
  (defalias 'font-lock-default-fontify-region
    #'emacs-font-lock-default-fontify-region))

(unless (fboundp 'font-lock-add-keywords)
  (defalias 'font-lock-add-keywords #'emacs-font-lock-add-keywords))

(unless (fboundp 'font-lock-remove-keywords)
  (defalias 'font-lock-remove-keywords #'emacs-font-lock-remove-keywords))

(unless (fboundp 'font-lock-set-defaults)
  (defalias 'font-lock-set-defaults #'emacs-font-lock-set-defaults))

;;;; --- variable bridges ----------------------------------------------

(unless (boundp 'font-lock-defaults)
  (defvar font-lock-defaults nil
    "List of font-lock defaults (KEYWORDS [KEYWORDS-ONLY ...])."))

(unless (boundp 'font-lock-keywords)
  (defvar font-lock-keywords nil
    "Active font-lock keywords list for the current buffer."))

(unless (boundp 'font-lock-keywords-only)
  (defvar font-lock-keywords-only nil
    "If non-nil, syntactic fontification is skipped (keyword-only mode)."))

(unless (boundp 'font-lock-keywords-case-fold-search)
  (defvar font-lock-keywords-case-fold-search nil
    "If non-nil, font-lock keyword regexps use case-insensitive matching."))

(unless (boundp 'font-lock-syntax-table)
  (defvar font-lock-syntax-table nil
    "Syntax table to use for font-lock fontification, or nil."))

(unless (boundp 'font-lock-multiline)
  (defvar font-lock-multiline nil
    "If non-nil, multiline font-lock matches are supported."))

(unless (boundp 'font-lock-set-defaults)
  (defvar font-lock-set-defaults nil
    "Non-nil when `font-lock-set-defaults' has run for the current buffer."))

(unless (boundp 'font-lock-comment-face)
  (defvar font-lock-comment-face 'font-lock-comment-face))
(unless (boundp 'font-lock-string-face)
  (defvar font-lock-string-face 'font-lock-string-face))
(unless (boundp 'font-lock-keyword-face)
  (defvar font-lock-keyword-face 'font-lock-keyword-face))
(unless (boundp 'font-lock-function-name-face)
  (defvar font-lock-function-name-face 'font-lock-function-name-face))
(unless (boundp 'font-lock-variable-name-face)
  (defvar font-lock-variable-name-face 'font-lock-variable-name-face))
(unless (boundp 'font-lock-type-face)
  (defvar font-lock-type-face 'font-lock-type-face))
(unless (boundp 'font-lock-constant-face)
  (defvar font-lock-constant-face 'font-lock-constant-face))
(unless (boundp 'font-lock-builtin-face)
  (defvar font-lock-builtin-face 'font-lock-builtin-face))
(unless (boundp 'font-lock-warning-face)
  (defvar font-lock-warning-face 'font-lock-warning-face))
(unless (boundp 'font-lock-doc-face)
  (defvar font-lock-doc-face 'font-lock-doc-face))

(provide 'emacs-font-lock-builtins)

;;; emacs-font-lock-builtins.el ends here
