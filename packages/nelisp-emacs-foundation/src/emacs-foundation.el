;;; emacs-foundation.el --- Reusable foundation layer loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Library-first entry point for the FND package group.  It preserves the
;; load order previously encoded directly in `emacs-init.el', but can be
;; required by libraries that need only the reusable primitive substrate
;; rather than the full nemacs application bootstrap.

;;; Code:

;; Order matters: emacs-eval (defalias) before emacs-list (uses defalias);
;; emacs-fns (plist-get) before emacs-symbol (uses plist-get + plist-put);
;; emacs-list (nreverse, copy-sequence) before emacs-hash (uses both).
(defconst emacs-foundation-features
  '(emacs-fns
    emacs-eval
    emacs-list
    emacs-hash
    emacs-symbol
    emacs-callproc
    emacs-vars
    emacs-char-table
    emacs-backquote
    emacs-error
    emacs-string
    emacs-pcase
    cl-lib
    subr-x
    emacs-cl-macros
    emacs-stub-bulk
    emacs-stub
    emacs-os-detect
    emacs-easy-mmode
    emacs-time
    emacs-numeric
    emacs-subr-extras
    emacs-edebug-stubs)
  "Reusable FND package features loaded by `emacs-foundation'.")

(dolist (feature emacs-foundation-features)
  (require feature))

(provide 'emacs-foundation)

;;; emacs-foundation.el ends here
