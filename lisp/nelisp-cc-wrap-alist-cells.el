;;; nelisp-cc-wrap-alist-cells.el --- Doc 111 §111.E #26 wrap_alist_cells  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #26 — `wrap_alist_cells'.  Phase 47
;; reimplementation of the closure-env alist preprocessor currently in
;; Rust at `build-tool/src/eval/env_lexframe.rs::Env::wrap_alist_cells'.
;;
;; The op walks ALIST = `((NAME . VALUE-OR-CELL) ...)' and produces a
;; new alist where every cdr is a `Sexp::Cell'.  Bare values get wrapped
;; in fresh `NlCellRef' (refcount 1) so the elisp lexframe-bind path
;; stores write-through cells.  Each iteration allocates two
;; `NlConsBox' (= outer + inner pair) and at most one `NlCell'; the
;; refcount discipline (= +1 for the new cell, +1 for cloning name +
;; value into the box) is best maintained in a single Rust pass.
;;
;; First ship: dispatch the whole walk to the `nl_wrap_alist_cells'
;; Rust shim, writing into the caller-owned result slot.  Future
;; rewrite: §101.B `cons-walk' primitives + §111.D `cell-make' +
;; §101.D `cons-make' would let the elisp body do the walk + build
;; directly (= the cleaner end state, but ~80 LOC of recursive Phase
;; 47 elisp with careful slot allocation).
;;
;; ABI deps satisfied:
;;   §100.A  `extern-call' — `nl_wrap_alist_cells' (composes §101.B/D + §111.D).

;;; Code:

(defconst nelisp-cc-wrap-alist-cells--source
  '(defun nelisp_wrap_alist_cells (alist-ptr result-slot)
     ;; alist-ptr:   *const Sexp pointing at the closure-env alist.
     ;; result-slot: *mut Sexp 32-byte slot for the wrapped alist (=
     ;;              caller initialises to Sexp::Nil; the Rust shim
     ;;              writes Sexp::Cons(_) on success, Sexp::Nil on
     ;;              malformed input).
     ;;
     ;; Returns: i64 — 1 on success, 0 on malformed input (= the
     ;; Rust impl's `EvalError::Internal' arm).
     (extern-call nl_wrap_alist_cells alist-ptr result-slot))
  "Phase 47 source for Doc 111 §111.E #26 `wrap_alist_cells'.

Wraps the `nl_wrap_alist_cells' Rust shim which performs the whole
cons-walk + cell-wrap + alist rebuild in a single refcount-disciplined
pass.  The result slot is treated as uninitialised on entry (the
shim's `core::ptr::write' avoids dropping prior contents); callers
should set it to `Sexp::Nil' before the call.")

(provide 'nelisp-cc-wrap-alist-cells)

;;; nelisp-cc-wrap-alist-cells.el ends here
