;;; nelisp-stdlib-env-shim.el --- Doc 86 §86.3.a env shim skeleton  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 86 §86.3.a / Doc 89 Option C — Tier 0 env shim *skeleton* (=
;; precursor stage).  This file establishes the elisp-side surface of
;; the future global-environment migration WITHOUT yet activating any
;; shadow / switch-over path.  Concretely, this file provides:
;;
;;   1. A `nelisp--global-env' hash-table variable that will host the
;;      elisp-side mirror of `Env::globals' once §86.3.b shadow path
;;      goes live.  In the precursor stage the hash-table is allocated
;;      but never read or written by interpreter callsites — it is a
;;      placeholder so `lisp/nelisp-stdlib-env-shim.el' has a stable
;;      load-time identity.
;;
;;   2. Eleven thin elisp wrappers (`nelisp--global-env-*') that
;;      forward to the 11 Rust slim primitives shipped in
;;      `build-tool/src/eval/env_shim.rs' (= `nelisp--env-globals-*').
;;      Each wrapper is one-line `funcall' of the underlying primitive
;;      so elisp code in §86.3.b can re-bind / `cl-letf' the wrappers
;;      without disturbing the Tier 0 fast path.
;;
;; Out of scope for precursor (= deliberately NOT defined here):
;;
;;   - `boundp' / `fboundp' / `defvar' / `setq' user-visible API
;;     overrides.  Those land in §86.3.b shadow path together with the
;;     dual-update mirror discipline.
;;   - Any rewiring of existing callsites (`eval/mod.rs' /
;;     `eval/special_forms.rs' / Tier 3 dispatch arms in
;;     `eval/builtins.rs') — Doc 89 §4.3 §86.3.c handles that.
;;
;; Bootstrap order constraint (Doc 89 §6.2.1): this file MUST load
;; AFTER `nelisp-jit-substrate.el' (= so `defun' / `defmacro' /
;; `cond' / `let' macros are live) and BEFORE
;; `nelisp-stdlib-eval-special.el' (= so the next stage of the
;; bootstrap can rely on the shim symbols already being interned).
;; The slim Rust primitives are installed by
;; `Env::new_global' BEFORE the STDLIB_IMAGES load loop, so any of the
;; 11 wrappers below can `funcall' the underlying primitive name at
;; load time.

;;; Code:

;;;; --- 1. Mirror placeholder (precursor unused) ---------------------
;;
;; In §86.3.b shadow path, `nelisp--global-env' becomes the elisp-side
;; mirror that interpreter callsites consult ahead of `Env::globals'.
;; The variable is initialised to nil here — actual hash-table
;; allocation is deferred until §86.3.b activates the shadow path,
;; because at this load order (= AFTER jit-strategy / BEFORE
;; eval-special) `make-hash-table' has not yet been defined (=
;; ships in `nelisp-stdlib-hash.el' which loads much later).  The
;; precursor only needs the symbol binding to exist so future
;; consumers can `setq' the table in once it is constructable.

(setq nelisp--global-env nil)

;;;; --- 2. Slim primitive wrappers (1:1 with Rust env_shim.rs) -------
;;
;; Every wrapper below is a direct `funcall' of the corresponding Rust
;; slim primitive.  The naming convention separates the layers:
;;
;;   - Rust primitive : `nelisp--env-globals-*' (= touches Env::globals
;;                       directly, no lexical-frame walk)
;;   - elisp wrapper  : `nelisp--global-env-*' (= one-line forwarder,
;;                       overrideable for §86.3.b shadow path)
;;
;; Wrappers use `defun' (= not `defalias') so future hooks in §86.3.b
;; can adjust the body (= add mirror-write + verify call) without
;; touching the call sites that already use the wrapper name.

(defun nelisp--global-env-lookup-value (sym)
  "Return the global value cell of SYM (Doc 86 §86.3.a precursor wrapper)."
  (nelisp--env-globals-get-value sym))

(defun nelisp--global-env-set-value (sym val)
  "Overwrite the global value cell of SYM with VAL (precursor wrapper)."
  (nelisp--env-globals-set-value sym val))

(defun nelisp--global-env-lookup-function (sym)
  "Return the global function cell of SYM (precursor wrapper)."
  (nelisp--env-globals-get-function sym))

(defun nelisp--global-env-set-function (sym def)
  "Overwrite the global function cell of SYM with DEF (precursor wrapper)."
  (nelisp--env-globals-set-function sym def))

(defun nelisp--global-env-clear-value (sym)
  "Drop the global value cell of SYM (precursor wrapper)."
  (nelisp--env-globals-clear-value sym))

(defun nelisp--global-env-clear-function (sym)
  "Drop the global function cell of SYM (precursor wrapper)."
  (nelisp--env-globals-clear-function sym))

(defun nelisp--global-env-is-bound (sym)
  "Return t iff SYM has a global value cell (precursor wrapper)."
  (nelisp--env-globals-is-bound sym))

(defun nelisp--global-env-is-fbound (sym)
  "Return t iff SYM has a global function cell (precursor wrapper)."
  (nelisp--env-globals-is-fbound sym))

(defun nelisp--global-env-is-constant (sym)
  "Return t iff SYM is flagged as a constant (precursor wrapper)."
  (nelisp--env-globals-is-constant sym))

(defun nelisp--global-env-set-constant (sym flag)
  "Set the constant flag of SYM to FLAG (precursor wrapper)."
  (nelisp--env-globals-set-constant sym flag))

(defun nelisp--global-env-capture-lexical ()
  "Capture the current lexical frames as alist (precursor wrapper)."
  (nelisp--env-globals-capture-lexical))

;;;; --- 3. Provide ---------------------------------------------------
;;
;; `provide' is installed by `nelisp-stdlib.el' which loads AFTER us.
;; Skipping here matches the convention in `nelisp-jit-substrate.el'
;; / `nelisp-stdlib-error.el' (= top of the bootstrap chain).

;;; nelisp-stdlib-env-shim.el ends here
