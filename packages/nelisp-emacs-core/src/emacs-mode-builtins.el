;;; emacs-mode-builtins.el --- Major-mode bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track H (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs unprefixed major-mode framework to the
;; substrate in `emacs-mode.el'.  Function definitions use a
;; host-aware install gate: host Emacs keeps its builtin/simple.el
;; mode framework, while standalone NeLisp overwrites bootstrap stubs
;; with the real mode substrate.  Variables are still gated on
;; `unless (boundp ...)' so host-owned special variables win.
;;
;; Bridged today:
;;   - Variables: major-mode / mode-name / auto-mode-alist /
;;     fundamental-mode-hook / text-mode-hook /
;;     emacs-lisp-mode-hook / change-major-mode-after-body-hook /
;;     after-change-major-mode-hook
;;   - Functions: fundamental-mode / text-mode / emacs-lisp-mode /
;;     run-mode-hooks / kill-all-local-variables / set-auto-mode
;;   - Macro: define-derived-mode
;;
;; Deferred to later γ phases:
;;   - real font-lock-mode integration (= currently no-op)
;;   - syntax-table per-mode binding
;;   - mode-line-format integration

;;; Code:

(require 'emacs-mode)

;;;; --- variable bridges ----------------------------------------------

(unless (boundp 'major-mode)
  (defvar major-mode 'fundamental-mode
    "Track H bridge: the active major-mode symbol."))

(unless (boundp 'mode-name)
  (defvar mode-name "Fundamental"
    "Track H bridge: human-readable mode name."))

(unless (boundp 'auto-mode-alist)
  (defvar auto-mode-alist nil
    "Track H bridge: file-extension → major-mode association list."))

(unless (boundp 'fundamental-mode-hook)
  (defvar fundamental-mode-hook nil
    "Track H bridge: hook run when entering `fundamental-mode'."))

(unless (boundp 'text-mode-hook)
  (defvar text-mode-hook nil
    "Track H bridge: hook run when entering `text-mode'."))

(unless (boundp 'emacs-lisp-mode-hook)
  (defvar emacs-lisp-mode-hook nil
    "Track H bridge: hook run when entering `emacs-lisp-mode'."))

(unless (boundp 'change-major-mode-after-body-hook)
  (defvar change-major-mode-after-body-hook nil
    "Track H bridge: ran by every derived mode's body, after the
parent mode is initialised but before the user hooks fire."))

(unless (boundp 'after-change-major-mode-hook)
  (defvar after-change-major-mode-hook nil
    "Track H bridge: ran after every major-mode switch completes."))

;;;; --- function bridges ----------------------------------------------

(defun emacs-mode-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (not (boundp 'emacs-version))
      (get symbol 'emacs-stub-bulk)
      (not (fboundp symbol))))

(when (emacs-mode-builtins--install-function-p 'fundamental-mode)
  (defalias 'fundamental-mode #'emacs-mode-fundamental-mode))

(when (emacs-mode-builtins--install-function-p 'text-mode)
  (defalias 'text-mode #'emacs-mode-text-mode))

(when (emacs-mode-builtins--install-function-p 'emacs-lisp-mode)
  (defalias 'emacs-lisp-mode #'emacs-mode-emacs-lisp-mode))

(when (emacs-mode-builtins--install-function-p 'run-mode-hooks)
  (defalias 'run-mode-hooks #'emacs-mode-run-mode-hooks))

(when (emacs-mode-builtins--install-function-p 'kill-all-local-variables)
  (defalias 'kill-all-local-variables
    #'emacs-mode-kill-all-local-variables))

(when (emacs-mode-builtins--install-function-p 'set-auto-mode)
  (defalias 'set-auto-mode #'emacs-mode-set-auto-mode))

;;;; --- macro bridge --------------------------------------------------

(when (emacs-mode-builtins--install-function-p 'define-derived-mode)
  (defmacro define-derived-mode (child parent name &optional doc &rest body)
    "Track H bridge: delegates to `emacs-mode-define-derived-mode'."
    `(emacs-mode-define-derived-mode
      ,child ,parent ,name ,doc ,@body)))

(provide 'emacs-mode-builtins)

;;; emacs-mode-builtins.el ends here
