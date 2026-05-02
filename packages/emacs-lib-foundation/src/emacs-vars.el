;;; emacs-vars.el --- NeLisp port of Emacs C core globals.c defvars  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 2.
;;
;; Establishes the small set of Emacs C-core global variables that
;; library code routinely references at load time
;; (`user-emacs-directory', `temporary-file-directory', `system-type',
;; `locale-coding-system').  Phase 1.6 hard-codes sensible defaults
;; rooted at "~/" + "/tmp/" + 'gnu/linux + 'utf-8.  Phase 2 will
;; resolve dynamically once `getenv' is wired through NeLisp's
;; syscall extension and OS introspection lands.

;;; Code:

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Polyfill: NeLisp standalone fallback for Emacs' user-emacs-directory."))

(unless (boundp 'temporary-file-directory)
  (defvar temporary-file-directory "/tmp/"
    "Polyfill: NeLisp standalone fallback for Emacs' temporary-file-directory."))

(unless (boundp 'locale-coding-system)
  (defvar locale-coding-system 'utf-8
    "Polyfill: NeLisp standalone forces utf-8."))

(unless (boundp 'system-type)
  (defvar system-type 'gnu/linux
    "Polyfill: NeLisp standalone defaults to gnu/linux.
Override per-host once `system-type' detection lands."))


(provide 'emacs-vars)

;;; emacs-vars.el ends here
