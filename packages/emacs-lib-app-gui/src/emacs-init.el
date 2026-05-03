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
;; Phase 10 — emacs-stub.el split: pcase / cl-macs subset / time
;; polyfills are now in dedicated modules.  emacs-pcase first (= cl-macs
;; expansions reference pcase patterns).  emacs-time has no L2 deps.
(require 'emacs-pcase)
(require 'emacs-cl-macros)
(require 'emacs-time)
;; Phase E (2026-05-03) — numeric + bitwise primitives split from
;; `emacs-stub.el'.  No L2 deps; loads safely anywhere in the chain.
(require 'emacs-numeric)
;; Phase 9 — real-buffer wrappers around `nelisp-emacs-compat' (T39).
;; Replaces the Phase 8 string-accumulator stubs that were inside
;; `emacs-stub.el' for `with-temp-buffer' / `insert' / `buffer-string'.
(require 'emacs-buffer-builtins)
;; Phase 11.B' — regex / search bridges (re-search-forward / looking-at /
;; match-data / match-string family).  Loads after `emacs-buffer-builtins'
;; because `match-string' uses `buffer-substring' from there.
(require 'emacs-search-builtins)
;; Phase J (Track A, 2026-05-03) — line / column primitives derived
;; on top of `nelisp-ec-buffer-substring' (= bobp / eobp / bolp / eolp
;; / line-beginning-position / line-end-position / beginning-of-line /
;; end-of-line / forward-line / line-number-at-pos).  Loads after
;; `emacs-buffer-builtins' because it uses `save-excursion' and
;; `nelisp-ec-buffer-substring' from there.
(require 'emacs-line-builtins)
;; Track D Phase D (2026-05-03) — file I/O bridges + find-file /
;; save-buffer / write-file / revert-buffer derivations.  Loads after
;; `emacs-buffer-builtins' (= depends on `current-buffer' /
;; `set-buffer' / `generate-new-buffer').
(require 'emacs-fileio-builtins)
;; Track E (2026-05-03) — editing commands + kill-ring (= self-insert
;; / newline / delete-backward-char / kill-region / kill-line / yank
;; / forward-word / backward-word).  Loads after `emacs-line-builtins'
;; (= depends on `emacs-line--eol-pos' for `kill-line').
(require 'emacs-edit-builtins)
;; Phase 11.C'' — keymap.c / frame.c / window.c bridges to the
;; existing `emacs-keymap.el' / `emacs-frame.el' / `emacs-window.el'
;; prefixed implementations.  Each transitively pulls its prefixed
;; module via `(require ...)' so callers using the unprefixed names
;; (= make-keymap / make-frame / selected-window / ...) see real
;; behaviour instead of the previous nil-stub sentinels.
(require 'emacs-keymap-builtins)
(require 'emacs-frame-builtins)
(require 'emacs-window-builtins)

(provide 'emacs-init)

;;; emacs-init.el ends here
