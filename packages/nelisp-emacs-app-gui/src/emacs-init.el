;;; emacs-init.el --- nelisp-emacs Layer 2 loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Application/bootstrap loader for the Layer 2 Emacs C-core ports.
;; Library consumers should prefer `(require 'nelisp-emacs)'; this file
;; owns vendor load-path setup and app-facing lazy feature loaders.
;;
;; Each ported file is idempotent under regular Emacs (= each definition
;; is gated on `unless (fboundp ...)') so this loader is safe to evaluate
;; even under a host Emacs that already provides the C-core originals.

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
        ;; Avoid filesystem probes here: standalone diagnostics provide an
        ;; explicit load-path and missing entries are harmless until required.
        (unless (and (boundp 'load-path) (member path load-path))
          (setq load-path
                (append (and (boundp 'load-path) load-path)
                        (list path))))))))

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

(require 'nelisp-emacs)

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
  (require 'emacs-tui-backend)
  ;; `emacs-tui-event' provides the byte-stream -> key event parser
  ;; that nemacs-main's event loop drains under the nelisp driver.
  ;; Avoid `locate-library' here: the standalone NeLisp reader currently
  ;; treats some file-system probes as process-stopping errors.  The
  ;; bootstrap load-path is already explicit, so a direct require is the
  ;; stable path in both host and standalone drivers.
  (require 'emacs-tui-event)
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

(provide 'emacs-init)

;;; emacs-init.el ends here
