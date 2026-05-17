;;; nelisp-cc-rc-dec.el --- Doc 123 §123.B refcount-dec elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 123 §123.B — second substrate elisp化 stage extending §123.A.
;; Pulls the refcount-dec kernel out of
;; `build-tool/src/eval/rc_primitives.rs' (= `rc_dec_no_drop' + the
;; mutation half of `bi_nl_rc_dec_strong') and re-expresses it as a
;; single `atomic-fetch-add' grammar op with `delta = -1' (= the
;; SeqCst fetch-sub semantics required by Doc 79 v6 §4.2's
;; "fetch_sub + return new count" contract).
;;
;; Function contract:
;;   box-ptr: raw `*const NlConsBox' coerced to i64 (= same as §123.A).
;;   returns: the *pre-sub* i64 refcount value (= what the underlying
;;            `nl_atomic_fetch_add(ptr, -1)' returns from
;;            `AtomicI64::fetch_add(-1, SeqCst)').  Callers compare to
;;            1 to decide whether a drop is needed (= prev == 1 →
;;            new == 0 → cycle collector finalizes the box), and apply
;;            `saturating_sub' on the host side to get the
;;            non-negative new count when reporting back to Sexp::Int.
;;
;; The body is one composed value form:
;;
;;   (atomic-fetch-add (+ box-ptr 64) -1)
;;
;; The constant `64' = REFCOUNT_OFFSET = `2 * size_of::<Sexp>()` (=
;; the byte offset of the `AtomicUsize refcount' field inside
;; `NlConsBox' per `#[repr(C)]' + `build-tool/src/eval/nlrc.rs:292'
;; compile-time assert).  Phase 47 emits this as `add rax, 64' with
;; no relocation, identical to §123.A's pattern with the only
;; difference being the `delta = -1' literal that lowers to the
;; second extern-call arg slot.
;;
;; Ordering: §122.E `atomic-fetch-add' uses `Ordering::SeqCst' while
;; Rust's `rc_dec_no_drop' uses `Ordering::Release'.  SeqCst ⊃
;; Release (= stronger ordering is always valid); the difference is
;; one extra `mfence' on x86_64, acceptable for the refcount-drop
;; fast path.  Doc 122.E.2 may add a Release variant later if
;; profiling shows the difference matters.
;;
;; Saturating-sub: `rc_dec_no_drop' returns `prev.saturating_sub(1)'
;; to guard against the pathological underflow case.  This kernel
;; returns the raw pre-sub value; the host-side safe wrapper in
;; `build-tool/src/lib.rs::elisp_cc_spike::rc_dec' is responsible
;; for applying `saturating_sub(1)' if the caller needs the new
;; count.  Splitting the kernel + saturate keeps the elisp body
;; minimal (= one extern-call) and keeps the saturate cmp + cmov in
;; Rust where the `usize' vs `i64' coercion is naturally expressed.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this
;; feature in its manifest; `build-tool/build.rs' compiles the source
;; into `nelisp_rc_dec.o' and archives it into the static lib cargo
;; links against the crate.  The Rust side declares the `extern "C"'
;; signature in `build-tool/src/lib.rs::elisp_cc_spike' and exposes a
;; safe wrapper `elisp_cc_spike::rc_dec(box_ptr)' for the integration
;; test in `build-tool/tests/elisp_cc_rc_dec_probe.rs'.
;;
;; Stage scope: §123.B ships *only* the elisp kernel + probe + Rust
;; extern + safe wrapper.  The Rust-side `rc_dec_no_drop' and
;; `bi_nl_rc_dec_strong' continue to use the existing `fetch_sub'
;; body directly; the dispatch swap (= replace `(*raw).refcount.
;; fetch_sub(1, Release)' with a call to `nelisp_rc_dec') lands in
;; §123.F's sweep commit after all §123.A-D kernels are present.

;;; Code:

(defconst nelisp-cc-rc-dec--source
  '(defun nelisp_rc_dec (box-ptr)
     (atomic-fetch-add (+ box-ptr 64) -1))
  "Phase 47 source for the Doc 123 §123.B refcount-dec kernel.

Single-arg function — Phase 47's SysV AMD64 prologue spills the
first arg (`box-ptr' = raw `NlConsBox*' as i64) into the rbp-
relative slot 0.  The body is one composed value form: add 64
(= REFCOUNT_OFFSET, the byte distance from the NlConsBox base to
its AtomicI64 trailer per `#[repr(C)]') to the pointer, then
call the §122.E `atomic-fetch-add' extern with delta = -1.

`atomic-fetch-add' lowers to `call nl_atomic_fetch_add@PLT' (= the
Rust extern in `build-tool/src/eval/raw_mem.rs:75' that wraps
`AtomicI64::fetch_add(delta, Ordering::SeqCst)') — with delta = -1
this is semantically equivalent to `fetch_sub(1, SeqCst)'.  rax on
return holds the pre-sub i64, which is also this function's return
value.

The Rust safe wrapper (= future §123.F dispatch swap) applies
`saturating_sub(1)' to the return value before reporting the new
count to `Sexp::Int', mirroring the existing `rc_dec_no_drop'
contract.  The probe in `tests/elisp_cc_rc_dec_probe.rs' inspects
both the raw pre-sub return and the post-call slot value to verify
the fetch-sub semantics independently of any host-side saturate.

No allocation, no relocation in `.text' beyond the single PLT
fixup for `nl_atomic_fetch_add'.  Same code-size profile as §123.A
(= ~30 bytes of machine code, one `mov + add + call' sequence).")

(provide 'nelisp-cc-rc-dec)

;;; nelisp-cc-rc-dec.el ends here
