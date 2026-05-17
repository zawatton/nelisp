;;; nelisp-cc-rc-inc.el --- Doc 123 §123.A refcount-inc elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 123 §123.A — the simplest macro elisp化 from
;; `build-tool/src/eval/rc_primitives.rs' (= 464 Rust LOC) using the
;; Doc 122 §122.E `atomic-fetch-add' grammar op.  This is the first
;; substrate elisp化 stage (= Doc 123-128 chain start) and proof of
;; concept that the §122.E substrate gate is functional for the
;; refcount mutation kernel.
;;
;; Function contract:
;;   box-ptr: raw `*const NlConsBox' coerced to i64 (= the same value
;;            `NlConsBoxRef::as_ptr' returns, reinterpreted as i64 via
;;            the `nl_consbox_ptr -> i64' Rust shim).
;;   returns: the *pre-add* refcount value (= the i64 in rax that
;;            `nl_atomic_fetch_add' produces from the underlying
;;            `AtomicI64::fetch_add(1, SeqCst)' call).
;;
;; The body is one composed value form:
;;
;;   (atomic-fetch-add (+ box-ptr 64) 1)
;;
;; The constant `64' is REFCOUNT_OFFSET = `2 * size_of::<Sexp>()` =
;; `2 * 32`, the byte offset of the `AtomicUsize refcount' field
;; inside an `NlConsBox' (= layout-pinned by `#[repr(C)]' per
;; `build-tool/src/eval/nlconsbox.rs:56' + the compile-time assert at
;; `build-tool/src/eval/nlrc.rs:292').  The Phase 47 emitter compiles
;; the literal as an `add rax, 64' instruction with no relocation.
;;
;; Ordering: §122.E `atomic-fetch-add' uses `Ordering::SeqCst' while
;; Rust's `rc_inc_raw' uses `Ordering::Relaxed'.  SeqCst ⊃ Relaxed
;; (= stronger ordering is always valid for an inc-only operation);
;; the cost is one extra `mfence' on x86_64 which is acceptable for
;; the refcount-bump fast path.  Doc 122.E.2 may add a Relaxed
;; variant later if profiling shows the difference matters.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nelisp_rc_inc.o' and archives it into the static lib cargo links
;; against the crate.  The Rust side declares the `extern "C"'
;; signature in `build-tool/src/lib.rs::elisp_cc_spike' and exposes a
;; safe wrapper `elisp_cc_spike::rc_inc(box_ptr)' for the integration
;; test in `build-tool/tests/elisp_cc_rc_inc_probe.rs'.
;;
;; Stage scope: §123.A ships *only* the elisp kernel + probe.  The
;; Rust-side `bi_nl_rc_inc_strong' primitive in `rc_primitives.rs:140-
;; 154' continues to call `NlConsBoxRef::rc_inc_raw' directly; §123.B
;; will swap that dispatch to call `nelisp_rc_inc' via extern-C.  The
;; existing 8 Rust tests in `rc_primitives.rs::tests' remain green
;; throughout this stage.

;;; Code:

(defconst nelisp-cc-rc-inc--source
  '(defun nelisp_rc_inc (box-ptr)
     (atomic-fetch-add (+ box-ptr 64) 1))
  "Phase 47 source for the Doc 123 §123.A refcount-inc kernel.

Single-arg function — Phase 47's SysV AMD64 prologue spills the
first arg (`box-ptr' = raw `NlConsBox*' as i64) into the rbp-
relative slot 0.  The body is one composed value form: add 64
(= REFCOUNT_OFFSET, the byte distance from the NlConsBox base to
its AtomicUsize trailer per `#[repr(C)]') to the pointer, then
call the §122.E `atomic-fetch-add' extern with delta = 1.

`atomic-fetch-add' lowers to `call nl_atomic_fetch_add@PLT' (= the
Rust extern in `build-tool/src/eval/raw_mem.rs:75' that wraps
`AtomicI64::fetch_add(delta, Ordering::SeqCst)').  rax on return
holds the pre-add i64, which is also this function's return value.

The Rust caller (= future §123.B dispatch swap) ignores the
return value when the call is from `bi_nl_rc_inc_strong' (= the
primitive contract is `Sexp -> Nil'), but the probe in
`tests/elisp_cc_rc_inc_probe.rs' inspects rax to verify atomic
semantics (= prev count == old, post-call slot == old+1).

No allocation, no relocation in `.text' beyond the single PLT
fixup for `nl_atomic_fetch_add'.  This is the smallest possible
substrate-stage `.o' (= ~30 bytes of machine code).")

(provide 'nelisp-cc-rc-inc)

;;; nelisp-cc-rc-inc.el ends here
