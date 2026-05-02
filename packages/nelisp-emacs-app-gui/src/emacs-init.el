;;; emacs-init.el --- nelisp-emacs Layer 2 loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — single-line loader for the Layer 2 Emacs C
;; core ports.  Consumers that want the full set of polyfilled
;; Emacs builtins available before they `require' anything else can
;; just include this one file in their load chain (= e.g. via
;; `ANVIL_MODULE_FILES' in the NeLisp standalone path).
;;
;; Loading is order-sensitive: `emacs-fns' first because the others
;; do not depend on it; later modules might.  Each ported file is
;; idempotent under regular Emacs (= each definition is gated on
;; `unless (fboundp ...)') so this loader is safe to evaluate even
;; under a host Emacs that already provides the C-core originals.

;;; Code:

;; Order matters: emacs-eval (defalias) before emacs-list (uses defalias);
;; emacs-fns (plist-get) before emacs-symbol (uses plist-get + plist-put);
;; emacs-list (nreverse, copy-sequence) before emacs-hash (uses both).
(require 'emacs-fns)
(require 'emacs-eval)
(require 'emacs-list)
(require 'emacs-hash)
(require 'emacs-symbol)
(require 'emacs-callproc)
(require 'emacs-vars)
(require 'emacs-sqlite)
(require 'emacs-backquote)
(require 'emacs-error)
(require 'emacs-string)

(provide 'emacs-init)

;;; emacs-init.el ends here
