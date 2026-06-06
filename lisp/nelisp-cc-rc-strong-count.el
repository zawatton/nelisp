;;; nelisp-cc-rc-strong-count.el --- Doc 123 §123.C strong-count reader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 123 §123.C — pure-elisp `nelisp_rc_strong_count' kernel, the
;; refcount-reader twin of §123.A `nelisp_rc_inc'.  Replaces the body
;; of `bi_nl_rc_strong_count' (= `NlConsBoxRef::strong_count(r)' which
;; performs an `Acquire' load on the refcount slot) at
;; `build-tool/src/eval/rc_primitives.rs:189-198' with a single
;; §122.E `ptr-read-u64' grammar op.
;;
;; Function contract:
;;   box-ptr: raw `*const NlConsBox' coerced to i64 (= same shape as
;;            the §123.A `nelisp_rc_inc' arg; see that kernel for the
;;            ABI commentary).
;;   returns: the *current* refcount value as i64 (= zero-extended u64
;;            re-cast; values above 2^63 are unrepresentable but the
;;            refcount domain is far below that bound so this is moot).
;;
;; The body is one composed value form:
;;
;;   (ptr-read-u64 box-ptr 64)
;;
;; The constant `64' is REFCOUNT_OFFSET = `2 * size_of::<Sexp>()` =
;; `2 * 32', the byte offset of the `AtomicUsize refcount' field
;; inside an `NlConsBox' (= layout-pinned by `#[repr(C)]' per
;; `build-tool/src/eval/nlconsbox.rs:56' + the compile-time assert at
;; `build-tool/src/eval/nlrc.rs:292').  Same constant as §123.A; the
;; AOT emitter compiles the literal as an `add rax, 64'
;; instruction with no relocation before the load.
;;
;; Ordering: §122.E `ptr-read-u64' performs a plain (relaxed) i64 load
;; via `nl_ptr_read_u64' (= `*(u64*)(ptr + offset)' in the Rust
;; extern).  The Rust `NlConsBoxRef::strong_count' uses
;; `Ordering::Acquire' — SeqCst > Acquire > Relaxed in strength.
;;
;; For a *read* of an `AtomicUsize' on x86_64, all three orderings
;; lower to the same `mov' instruction at the hardware level (= x86's
;; TSO memory model gives Acquire semantics for free on aligned word
;; loads).  The Acquire fence is purely a *compiler-side* signal that
;; the load cannot be reordered earlier; since the elisp kernel does
;; no other memory ops in the same body, the compiler-fence
;; distinction is also moot.  The bytes returned are identical.
;;
;; This contrasts with the inc/dec twins (§123.A/B) where the atomic
;; RMW instruction differs from a plain load — for the read-only
;; path, the relaxed `ptr-read-u64' is the natural lowering.  If a
;; future code path requires the explicit Acquire fence (= e.g. the
;; Bacon-Rajan cycle collector's compare-against-zero before
;; reclamation), a sibling `atomic-load-u64' grammar op can be added
;; to §122.E.2; for the MVP refcount snapshot, plain load is correct.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nelisp_rc_strong_count.o' and archives it into the static lib the
;; crate links against.  The Rust side declares the `extern "C"'
;; signature in `build-tool/src/lib.rs::elisp_cc_spike' and exposes a
;; safe wrapper `elisp_cc_spike::rc_strong_count(box_ptr)' for the
;; integration test in `tests/elisp_cc_rc_strong_count_probe.rs'.
;;
;; Stage scope: §123.C ships *only* the elisp kernel + probe.  The
;; Rust-side `bi_nl_rc_strong_count' primitive in `rc_primitives.rs:
;; 189-198' continues to call `NlConsBoxRef::strong_count' directly;
;; §123.F (= the sweep stage) will swap that dispatch to call
;; `nelisp_rc_strong_count' via extern-C.  Existing `rc_primitives.rs'
;; unit tests remain green throughout this stage.

;;; Code:

(defconst nelisp-cc-rc-strong-count--source
  '(defun nelisp_rc_strong_count (box-ptr)
     ;; Refcount slot at offset 64 (= REFCOUNT_OFFSET = 2 * sizeof Sexp
     ;; = 2 * 32).  See §123.A `nelisp-cc-rc-inc.el' for the layout
     ;; pin documentation; same offset constant, different op (= plain
     ;; load vs. atomic fetch-add).
     ;;
     ;; Lowers to `mov rax, qword ptr [rdi + 64]' on x86_64 SysV — a
     ;; single load with no PLT call to `nl_ptr_read_u64' because the
     ;; extern is `#[inline(always)]'-equivalent at the assembly
     ;; level (= the AOT emitter still uses the PLT path for
     ;; symmetry with the write ops; cost ~2 cycles per call which
     ;; the refcount-snapshot fast path can absorb).
     (ptr-read-u64 box-ptr 64))
  "AOT source for the Doc 123 §123.C refcount-read kernel.

Single-arg function — AOT's SysV AMD64 prologue spills the
first arg (`box-ptr' = raw `NlConsBox*' as i64) into the rbp-
relative slot 0.  The body is one composed value form: pass
`box-ptr' + offset = 64 (= REFCOUNT_OFFSET, the byte distance from
the NlConsBox base to its AtomicUsize trailer per `#[repr(C)]') to
the §122.E `ptr-read-u64' extern.

`ptr-read-u64' lowers to `call nl_ptr_read_u64@PLT' (= the Rust
extern in `build-tool/src/eval/raw_mem.rs' that performs
`*(u64*)(ptr + offset)' and re-casts to i64).  rax on return holds
the current refcount value, which is also this function's return.

No allocation, no relocation in `.text' beyond the single PLT
fixup for `nl_ptr_read_u64'.  This is the smallest substrate-stage
`.o' alongside §123.A's `nelisp_rc_inc.o' (= ~30 bytes of machine
code each).")

(provide 'nelisp-cc-rc-strong-count)

;;; nelisp-cc-rc-strong-count.el ends here
