;;; emacs-keymap-builtins.el --- Unprefixed keymap.c builtin bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 11.C'' — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* keymap builtins (= `make-keymap',
;; `define-key', `lookup-key', `key-binding', ...) to the existing
;; `emacs-keymap-*' prefixed implementations in `emacs-keymap.el',
;; mirroring the Phase 11.B' `emacs-search-builtins.el' pattern.
;;
;; Why this exists: until Phase 11.C'' the unprefixed names lived as
;; nil-stubs inside `emacs-stub.el', which meant standalone NeLisp
;; (= ANVIL_MODULE_FILES path) silently lost real keybinding behaviour
;; even though `emacs-keymap.el' had a working implementation.  The
;; bridge wires the two so callers using either spelling get the same
;; result.
;;
;; Each definition is gated on `unless (fboundp ...)' so loading inside
;; a host Emacs is a cheap no-op (= host's C builtins win).
;;
;; Bridgeable today (= covered by `emacs-keymap.el'):
;;
;;   - `make-keymap' / `make-sparse-keymap' / `keymapp'
;;   - `define-key' (3-arg + ignored REMOVE)
;;   - `lookup-key' / `key-binding'
;;   - `set-keymap-parent' / `keymap-parent'
;;   - `current-global-map' / `current-local-map'
;;   - `use-global-map' / `use-local-map'
;;   - `where-is-internal'
;;
;; Deferred (= keep `emacs-stub.el' nil-stubs):
;;
;;   - `define-key-after': no `emacs-keymap-define-key-after' yet.
;;     The 3-arg `define-key' is a strict subset of the API.
;;
;; Phase 11.C'' also deletes the duplicate stubs that this file
;; supersedes from `emacs-stub.el' (= same load-order shadowing risk
;; that Phase 11.A' / 11.B' fixed for buffer / search).

;;; Code:

(require 'emacs-keymap)

;;;; --- constructors ----------------------------------------------------

(unless (fboundp 'make-keymap)
  (defalias 'make-keymap #'emacs-keymap-make-keymap))

(unless (fboundp 'make-sparse-keymap)
  (defalias 'make-sparse-keymap #'emacs-keymap-make-sparse-keymap))

(unless (fboundp 'keymapp)
  (defalias 'keymapp #'emacs-keymap-keymapp))

;;;; --- mutation --------------------------------------------------------

(unless (fboundp 'define-key)
  (defun define-key (keymap key def &optional remove)
    "Phase 11.C'' polyfill: forward to `emacs-keymap-define-key'.
REMOVE (= unbind KEY when non-nil) is accepted for API parity but the
prefixed substrate has no unbind primitive yet, so we simply pass DEF
through."
    (ignore remove)
    (emacs-keymap-define-key keymap key def)))

(unless (fboundp 'set-keymap-parent)
  (defalias 'set-keymap-parent #'emacs-keymap-set-keymap-parent))

(unless (fboundp 'keymap-parent)
  (defalias 'keymap-parent #'emacs-keymap-keymap-parent))

;;;; --- lookup ----------------------------------------------------------

(unless (fboundp 'lookup-key)
  (defalias 'lookup-key #'emacs-keymap-lookup-key))

(unless (fboundp 'key-binding)
  (defalias 'key-binding #'emacs-keymap-key-binding))

;;;; --- global / local map ----------------------------------------------

(unless (fboundp 'current-global-map)
  (defalias 'current-global-map #'emacs-keymap-current-global-map))

(unless (fboundp 'current-local-map)
  (defalias 'current-local-map #'emacs-keymap-current-local-map))

(unless (fboundp 'use-global-map)
  (defalias 'use-global-map #'emacs-keymap-use-global-map))

(unless (fboundp 'use-local-map)
  (defalias 'use-local-map #'emacs-keymap-use-local-map))

;;;; --- reverse lookup --------------------------------------------------

(unless (fboundp 'where-is-internal)
  (defalias 'where-is-internal #'emacs-keymap-where-is-internal))

(provide 'emacs-keymap-builtins)

;;; emacs-keymap-builtins.el ends here
