;;; nelisp-cc-bi-quit-flag.el --- Doc 117 §117.B quit-flag atomic ops  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.B — swaps the bodies of the three quit-flag builtins
;; (`(set-quit-flag)' / `(clear-quit-flag)' / `(quit-flag-pending-p)')
;; to Phase 47 elisp objects.  Each kernel is a single composed value
;; form using the Doc 122 §122.E atomic / raw-memory grammar ops.
;;
;; The process-global quit flag lives in the `QUIT_FLAG' static of
;; `build-tool/src/eval/quit.rs' (= an `AtomicI64', value 0 means
;; clear, value 1 means pending — promoted from the previous
;; `AtomicBool' so the §122.E ops, which operate on `*mut i64', can
;; reach the same 8-byte aligned slot).  The Rust extern
;; `nl_quit_flag_ptr() -> *mut i64' surfaces the static's address; the
;; Rust shim in `eval/builtins.rs::bi_{set,clear,quit_flag_pending_p}'
;; calls that getter and passes the pointer (cast through i64) into
;; the elisp body.
;;
;; Function contracts:
;;   nelisp_bi_set_quit_flag (flag-ptr)
;;     — Marks the slot as pending.  Uses `atomic-compare-exchange'
;;       from 0 to 1; if the slot was already 1 the CAS fails but the
;;       semantic post-condition (= slot == 1) still holds, so the
;;       failure is benign + idempotent.  Returns the CAS result
;;       (1 on transition 0→1, 0 if already 1) but the Rust shim
;;       discards it — `set-quit-flag' returns `t' unconditionally.
;;
;;   nelisp_bi_clear_quit_flag (flag-ptr)
;;     — Symmetric clear.  CAS from 1 to 0; if already 0 the CAS fails
;;       benignly.  Returns the CAS result, discarded by the Rust shim.
;;
;;   nelisp_bi_quit_flag_pending_p (flag-ptr)
;;     — Reads the slot via `ptr-read-u64' (= a single aligned `mov'
;;       on x86_64, atomic at the hardware level).  Returns 0 if clear,
;;       1 if pending.  Rust shim converts to `Sexp::T' / `Sexp::Nil'.
;;
;; Phase 47 ops consumed:
;;   §122.E `atomic-compare-exchange'  — SeqCst CAS for set / clear.
;;   §122.E `ptr-read-u64'             — Hardware-atomic aligned load
;;                                       for the pending-p reader.
;;
;; Ordering: the Rust `AtomicI64::{store, load}' calls used `SeqCst';
;; §122.E's `atomic-compare-exchange' uses SeqCst on both success +
;; failure paths, matching the strongest variant.  The `ptr-read-u64'
;; op is a plain load (= relaxed) — but x86_64 TSO gives Acquire-
;; semantics on aligned u64 loads for free, and the read is observe-
;; only (= no other memory operation in the same body to reorder
;; against), so the relaxed-vs-SeqCst distinction is invisible to all
;; callers.  See `nelisp-cc-rc-strong-count.el' for the same argument
;; applied to the refcount-reader twin.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest with 3 output entries (one .o per defun);
;; `build-tool/build.rs' compiles each into `nelisp_bi_*.o' and
;; archives them into the static lib the crate links against.  The
;; Rust side declares the `extern "C"' signatures in
;; `build-tool/src/lib.rs::elisp_cc_spike' and exposes safe wrappers
;; for the integration test in
;; `build-tool/tests/elisp_cc_bi_quit_flag_probe.rs'.
;;
;; Per Doc 117 §4.3: pure migration — no behaviour change.  The Rust
;; shims preserve the arity contract (= 0 args, signal `WrongArity'
;; otherwise) and the t/nil return type; only the underlying atomic
;; transition moves into elisp.

;;; Code:

(defconst nelisp-cc-bi-quit-flag--set-source
  '(defun nelisp_bi_set_quit_flag (flag-ptr)
     ;; flag-ptr: *mut i64 — 8-byte aligned slot, currently 0 or 1.
     ;;
     ;; CAS(flag-ptr, expected=0, new=1).  Returns 1 if the slot was
     ;; 0 (= transitioned to 1), 0 if already 1 (= benign no-op, slot
     ;; remains 1 which is the desired post-condition).
     (atomic-compare-exchange flag-ptr 0 1))
  "Phase 47 source for the Doc 117 §117.B `(set-quit-flag)' swap.

Single-arg function — Phase 47's SysV AMD64 prologue spills
`flag-ptr' (= `*mut i64' to the `QUIT_FLAG' static in
`build-tool/src/eval/quit.rs', surfaced by `nl_quit_flag_ptr')
into the rbp-relative slot 0.  The body is one composed value
form: pass the slot ptr + expected = 0 + new = 1 to the §122.E
`atomic-compare-exchange' extern.

`atomic-compare-exchange' lowers to `call nl_atomic_compare_exchange@PLT'
(= the Rust extern in `build-tool/src/eval/raw_mem.rs' wrapping
`AtomicI64::compare_exchange(0, 1, SeqCst, SeqCst)').  rax holds
1 on success, 0 on failure — both states satisfy the
`(set-quit-flag)' post-condition (slot == 1), so the Rust shim
discards the return and unconditionally yields `Sexp::T'.

No allocation, no relocation in `.text' beyond the single PLT
fixup for `nl_atomic_compare_exchange'.")

(defconst nelisp-cc-bi-quit-flag--clear-source
  '(defun nelisp_bi_clear_quit_flag (flag-ptr)
     ;; flag-ptr: *mut i64 — 8-byte aligned slot, currently 0 or 1.
     ;;
     ;; CAS(flag-ptr, expected=1, new=0).  Symmetric to the set kernel:
     ;; returns 1 if the slot was 1 (= transitioned to 0), 0 if already
     ;; 0 (= benign no-op, slot remains 0).
     (atomic-compare-exchange flag-ptr 1 0))
  "Phase 47 source for the Doc 117 §117.B `(clear-quit-flag)' swap.
Mirrors the set kernel modulo the swap of `expected' / `new'.")

(defconst nelisp-cc-bi-quit-flag--pending-p-source
  '(defun nelisp_bi_quit_flag_pending_p (flag-ptr)
     ;; flag-ptr: *const i64 — 8-byte aligned slot, currently 0 or 1.
     ;;
     ;; Plain aligned u64 load via `ptr-read-u64' at offset 0.  On
     ;; x86_64, an aligned 8-byte load is atomic at the hardware level
     ;; (= the `MOV' instruction is indivisible) so the value observed
     ;; is always one of the prior `compare-exchange' results — never
     ;; a torn read.  Returns 0 if clear, 1 if pending.
     (ptr-read-u64 flag-ptr 0))
  "Phase 47 source for the Doc 117 §117.B `(quit-flag-pending-p)' swap.

Single-arg function reading `*flag-ptr' as a `u64' and returning
it in rax.  Lowers to `call nl_ptr_read_u64@PLT'.  The Rust shim
maps 0 → `Sexp::Nil', non-zero → `Sexp::T'.")

(provide 'nelisp-cc-bi-quit-flag)

;;; nelisp-cc-bi-quit-flag.el ends here
