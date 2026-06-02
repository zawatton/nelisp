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
;; load-path so upstream Emacs Lisp sources are available after the
;; local Layer-2 shims/bridges.  v2 standalone NeLisp must let `src/'
;; win for compatibility shims such as `cl-lib' and `keymap'; vendored
;; upstream fills the long tail only after those local overrides.
;; Caller must set `nelisp-emacs-vendor-root' before loading this file
;; (= the directory containing `vendor/emacs-lisp/'); we append the
;; standard subdirectories that the Emacs build adds to load-path.
(when (and (boundp 'nelisp-emacs-vendor-root) nelisp-emacs-vendor-root)
  (let ((root (concat nelisp-emacs-vendor-root "/emacs-lisp")))
    (dolist (sub '("" "/emacs-lisp" "/international" "/textmodes"
                   "/progmodes" "/net" "/url" "/vc" "/calc"
                   "/calendar" "/eshell" "/mail" "/cedet"
                   "/leim" "/term" "/erc" "/org" "/gnus"))
      (let ((path (concat root sub)))
        ;; nelisp does not yet provide `file-directory-p'; if missing we
        ;; just accept the path and let later `require' calls error if
        ;; anything is genuinely absent.
        (when (or (not (fboundp 'file-directory-p))
                  (file-directory-p path))
          (unless (and (boundp 'load-path) (member path load-path))
            (setq load-path
                  (append (and (boundp 'load-path) load-path)
                          (list path)))))))))

;; Phase B5 (= 2026-05-09): also surface this file's own directory on
;; load-path so that the `(require 'emacs-...)' lines below resolve
;; against the bundled `src/' modules under standalone NeLisp where
;; the caller did NOT pre-load emacs-init's directory.  Without this,
;; NeLisp's permissive `require' silently provides the feature without
;; running the file body — the helper definitions never land and
;; downstream stubs (e.g. `define-error') trip with `void-function'.
(when (and (boundp 'load-file-name) load-file-name)
  (let ((dir (file-name-directory load-file-name)))
    (unless (and (boundp 'load-path) (member dir load-path))
      (setq load-path (cons dir (and (boundp 'load-path) load-path))))))

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
;; (2026-05-04) — `emacs-stub' provides ~97 `unless-fboundp'-guarded
;; polyfills (= kbd / add-hook / set-syntax-table / standard-syntax-table
;; / etc.) for the long tail of host-Emacs C primitives that downstream
;; modules call at load-time.  Under host Emacs every guard is a no-op
;; (= the C function is already defined); under the nelisp driver this
;; is the source of truth.  Required early so later modules can use
;; them at their own load-time.
(require 'emacs-stub)
;; Standalone NeLisp cannot yet macroexpand vendored easy-mmode.el's
;; lexical-closure-heavy implementation.  Provide a small Layer 2
;; fallback before vendor files ask for `(require 'easy-mmode)'.
(require 'emacs-easy-mmode)
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
;; Track E.2 (2026-05-03) — undo subsystem.  Loads BEFORE
;; `emacs-edit-builtins' so the editing commands' record-on-mutate
;; guards (`(when (fboundp 'emacs-undo-record-insert) ...)') see
;; the prefixed helpers as bound when they fire.
(require 'emacs-undo-builtins)
;; Track E (2026-05-03) — editing commands + kill-ring (= self-insert
;; / newline / delete-backward-char / kill-region / kill-line / yank
;; / forward-word / backward-word).  Loads after `emacs-line-builtins'
;; (= depends on `emacs-line--eol-pos' for `kill-line').
(require 'emacs-edit-builtins)
;; Track C (2026-05-03) — minibuffer + completion bridges to the
;; existing `emacs-minibuffer.el' prefixed module.  Loads after
;; `emacs-edit-builtins' (= the minibuffer's internal buffer also
;; uses the editing-command surface for line edit).
(require 'emacs-minibuffer-builtins)
;; Track B Phase B.1 (2026-05-03) — command-loop foundation.
;; Provides `read-event' / `read-char' / `read-command' /
;; `this-command-keys' family, plus `this-command' / `last-command' /
;; `unread-command-events' / `quit-flag' / `inhibit-quit' state.
;; Higher-level pieces (`read-key-sequence', `call-interactively',
;; `command-loop-1', `execute-extended-command', keyboard-quit) are
;; layered on top in B.2-B.6.  Loads after `emacs-minibuffer-builtins'
;; because `read-command' delegates to `completing-read'.
(require 'emacs-command-loop-builtins)
;; Phase 11.C'' — keymap.c / frame.c / window.c bridges to the
;; existing `emacs-keymap.el' / `emacs-frame.el' / `emacs-window.el'
;; prefixed implementations.  Each transitively pulls its prefixed
;; module via `(require ...)' so callers using the unprefixed names
;; (= make-keymap / make-frame / selected-window / ...) see real
;; behaviour instead of the previous nil-stub sentinels.
(require 'emacs-keymap-builtins)
(require 'emacs-frame-builtins)
(require 'emacs-window-builtins)
;; Track F (2026-05-03) — face attribute API.  Sits on top of
;; `emacs-redisplay's existing face registry; bridges defface
;; (macro) / make-face / face-attribute / set-face-attribute /
;; face-{foreground,background,list,p}.
(require 'emacs-faces-builtins)
;; Track H (2026-05-03) — major-mode framework MVP.  Bridges
;; `define-derived-mode' (macro) / `fundamental-mode' / `text-mode'
;; / `emacs-lisp-mode' / `run-mode-hooks' / `auto-mode-alist' /
;; `set-auto-mode' / `kill-all-local-variables'.
(require 'emacs-mode-builtins)
;; Track M (2026-05-03) — standalone NeLisp dispatch scaffold.
;; Loads BEFORE Track I so `emacs-process--delegate' can `require'
;; the registry from a populated load path.  Real NeLisp primitive
;; wiring happens on the runtime side (= no-op here under host).
(require 'emacs-standalone)
;; Track I (2026-05-03) — process / subprocess MVP.  Two-mode
;; substrate: under host Emacs delegates to the host C primitives;
;; under standalone NeLisp signals `emacs-process-not-implemented'
;; until NeLisp's process primitives are wired.  Bridges
;; call-process / start-process / make-process / processp /
;; process-list / process-status / process-exit-status /
;; process-buffer / process-name / process-send-string /
;; process-send-eof / delete-process / shell-command /
;; shell-command-to-string + shell-file-name / shell-command-switch.
(require 'emacs-process-builtins)
;; Phase 4 'C' (2026-05-06) — minimal `fill-region' / `count-matches'
;; polyfills so MELPA packages routing word-wrap / regex counting
;; through `with-temp-buffer' + buffer ops (canonical: s.el's
;; `s-word-wrap' / `s-count-matches') run end-to-end without
;; vendor-loading the full `lisp/textmodes/fill.el' (~1800 LOC) or
;; `lisp/replace.el' (~3000 LOC).  Loaded after `emacs-search-builtins'
;; (= `re-search-forward' bridge) and `emacs-buffer-builtins' (=
;; `delete-region' / `insert' / `buffer-substring').
(require 'emacs-textmodes-stub)

(defun emacs-init-load-tui-core-features ()
  "Load the minimal TUI runtime features needed to realise a frame.

The NeLisp driver pays source read/eval cost for every required module.
Batch startup and simple `--eval' forms do not need redisplay or the
TUI event parser, so they stay lazy until `nemacs-main' actually
realises an interactive frame.  Keep this core loader smaller than
`emacs-init-load-editor-features' so interactive startup does not pay
font-lock / mode setup before the first frame exists."
  ;; Use the fast first-frame core unless the full redisplay engine has
  ;; already been loaded by tests or an editor feature.
  (unless (featurep 'emacs-redisplay)
    (require 'emacs-redisplay-core))
  ;; `emacs-tui-event' provides the byte-stream -> key event parser
  ;; that nemacs-main's event loop drains under the nelisp driver.
  (when (locate-library "emacs-tui-event")
    (require 'emacs-tui-event))
  t)

(defun emacs-init-load-editor-features ()
  "Load interactive editor features not needed for first TUI realisation.

This extends `emacs-init-load-tui-core-features' with mode/font-lock
support and unprefixed redisplay trigger bridges.  Callers that only
need a frame should use the core loader instead."
  (emacs-init-load-tui-core-features)
  ;; Track K (2026-05-03) — font-lock MVP.  Bridges
  ;; `font-lock-mode' / `font-lock-fontify-region' /
  ;; `font-lock-fontify-buffer' / `font-lock-add-keywords' /
  ;; `font-lock-remove-keywords' / `font-lock-set-defaults' on top
  ;; of the text-property store in `emacs-buffer.el'.  Also
  ;; registers the standard face symbols (font-lock-keyword-face
  ;; etc) via the Track F face registry.
  (require 'emacs-font-lock-builtins)
  ;; Track R (2026-05-04) — minimal syntax-table for font-lock's
  ;; string / line-comment pre-pass.  Loaded *after*
  ;; emacs-font-lock-builtins so the standard faces are defined.
  (require 'emacs-syntax-table)
  ;; Track T (2026-05-04) — emacs-lisp-mode font-lock keyword set.
  ;; Loaded *after* the syntax-table so the syntactic post-pass is
  ;; available, and *after* emacs-mode (= the hook variable exists).
  (when (locate-library "emacs-mode")
    (require 'emacs-mode))
  (require 'emacs-elisp-mode)
  ;; Track G (2026-05-03) — Doc 43 redisplay close-gate trigger
  ;; bridges.  Wires `force-mode-line-update' / `redraw-display' /
  ;; `redraw-frame' / `redisplay' to the existing
  ;; `emacs-redisplay.el' substrate via a current-handle slot.
  ;; Optional require: only loads when emacs-redisplay's deps
  ;; (= emacs-buffer / emacs-window / emacs-tui-backend) are
  ;; available; missing-feature returns nil instead of erroring out.
  (when (and (locate-library "emacs-buffer")
             (locate-library "emacs-window")
             (locate-library "emacs-tui-backend"))
    (require 'emacs-redisplay-builtins))
  t)

;; Phase B2 — subr.el primitives that vendor `cl-lib.el' / `subr-x.el'
;; need at load time but standalone NeLisp does not ship.  Idempotent
;; under host Emacs; trivial cost on standalone (~50 LOC of `defun's).
(require 'emacs-subr-extras)

;; Phase B3 — Edebug-related primitives that vendor `cl-macs.el'
;; references at load time (= `def-edebug-elem-spec' / `def-edebug-spec'
;; etc.).  Without these, cl-macs.el silently truncates partway and
;; `cl-callf' never gets registered.
(require 'emacs-edebug-stubs)

(provide 'emacs-init)

;;; emacs-init.el ends here
