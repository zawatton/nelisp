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

;; Doc 51 Phase 3-A' — wire the vendored Emacs lisp/ tree into
;; load-path so `(require 'cl-lib)' / `subr.el' / `seq' / etc. resolve
;; against upstream Emacs sources rather than per-feature L2 polyfills.
;; Caller must set `nelisp-emacs-vendor-root' before loading this file
;; (= the directory containing `vendor/emacs-lisp/'); we prepend the
;; standard subdirectories that the Emacs build adds to load-path.
(when (and (boundp 'nelisp-emacs-vendor-root) nelisp-emacs-vendor-root)
  (let ((root (concat nelisp-emacs-vendor-root "/emacs-lisp")))
    (dolist (sub '("" "/emacs-lisp" "/international" "/textmodes"
                   "/progmodes" "/net" "/url" "/vc" "/calc"
                   "/calendar" "/eshell" "/mail" "/cedet"
                   "/leim" "/term" "/erc" "/org" "/gnus"))
      (let ((path (concat root sub)))
        (when (file-directory-p path)
          (unless (and (boundp 'load-path) (member path load-path))
            (setq load-path (cons path (and (boundp 'load-path) load-path)))))))))

;; Order matters: emacs-eval (defalias) before emacs-list (uses defalias);
;; emacs-fns (plist-get) before emacs-symbol (uses plist-get + plist-put);
;; emacs-list (nreverse, copy-sequence) before emacs-hash (uses both).
;; Order matters: emacs-eval (defalias) before emacs-list (uses defalias);
;; emacs-fns (plist-get) before emacs-symbol (uses plist-get + plist-put);
;; emacs-list (nreverse, copy-sequence) before emacs-hash (uses both).
;; emacs-sqlite-ffi requires nelisp-ffi to be on load-path; it is OPT-IN
;; (= caller adds it to ANVIL_MODULE_FILES) so this loader does not
;; force-require it.  emacs-sqlite stays as the default forwarder layer
;; for the host-Emacs path.
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

;; Phase B2 — subr.el primitives that vendor `cl-lib.el' / `subr-x.el'
;; need at load time but standalone NeLisp does not ship.  Idempotent
;; under host Emacs; trivial cost on standalone (~50 LOC of `defun's).
(require 'emacs-subr-extras)

(provide 'emacs-init)

;;; emacs-init.el ends here
