;;; nelisp-cc-nlconsbox-clone.el --- Doc 124 §124.A NlConsBox Clone elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.A — first stage of the `nl*.rs::Clone/Drop' substrate
;; elisp化 chain (= Doc 123 sibling, scope expanded from `rc_primitives.rs'
;; macros to `nlconsbox.rs / nlvector.rs / nlcell.rs / nlrecord.rs /
;; nlstr.rs' `impl Clone' + `impl Drop' fan-out).  Proof of concept that
;; the Doc 123 §123.A `rc_inc' kernel can drive an `impl Clone' body
;; verbatim — the trait impl is just `rc_inc' + return-the-pointer
;; (= `Self { ptr, _marker }' is the identity fn at the ABI level
;; because `NlConsBoxRef' is `#[repr(transparent)]' over `NonNull<NlConsBox>').
;;
;; Function contract:
;;   box-ptr: raw `*const NlConsBox' coerced to i64 (= the same value
;;            `NlConsBoxRef::as_ptr' returns, reinterpreted as i64 via
;;            the `nl_consbox_ptr -> i64' Rust shim).
;;   returns: `box-ptr' unchanged (= the cloned-handle's pointer, which
;;            shares the same inner box because Clone is a refcount
;;            bump + alias, not a payload copy).
;;
;; The body is a single composed value form using the `prog2'
;; side-effect sequencer pattern from §116.A's reader lexer (=
;; `nelisp_reader_prog2 E V' evaluates both args left-to-right and
;; returns V).  Phase 47's `seq' grammar is statement-only; defun
;; bodies are value-producing expressions, so to combine "do side
;; effect E, then return V" we wrap them in a helper that returns
;; its second arg:
;;
;;   (nelisp_nlconsbox_clone_prog2
;;     (atomic-fetch-add (+ box-ptr 64) 1)
;;     box-ptr)
;;
;; The helper `nelisp_nlconsbox_clone_prog2 (_e v) -> v' is defined
;; alongside the public `nelisp_nlconsbox_clone' entry in a single
;; `(seq DEFUN ...)' manifest, mirroring §116.A's `nelisp_reader_prog2'.
;;
;; The constant `64' is REFCOUNT_OFFSET = `2 * size_of::<Sexp>()` =
;; `2 * 32`, the byte offset of the `AtomicUsize refcount' field
;; inside an `NlConsBox' (= layout-pinned by `#[repr(C)]' per
;; `build-tool/src/eval/nlconsbox.rs:56' + the compile-time assert at
;; `build-tool/src/eval/nlrc.rs:292').  Identical to Doc 123 §123.A's
;; `nelisp_rc_inc' constant; the only difference is the `prog2'
;; wrap that returns `box-ptr' instead of the atomic op's pre-add
;; return (= §123.A's body is just `(atomic-fetch-add ...)' and
;; produces the pre-add count as rax; §124.A's body wraps that in
;; prog2 to override the return value with the input pointer for
;; the caller's `Self { ptr, _marker }' assembly).
;;
;; Ordering: §122.E `atomic-fetch-add' uses `Ordering::SeqCst' while
;; Rust's `impl Clone for NlConsBoxRef' uses `Ordering::Relaxed'.
;; SeqCst ⊃ Relaxed (= stronger ordering is always valid for an inc-
;; only operation); the cost is one extra `mfence' on x86_64 which is
;; acceptable for the refcount-bump fast path.  Doc 122.E.2 may add a
;; Relaxed variant later if profiling shows the difference matters.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nelisp_nlconsbox_clone.o' and archives it into the static lib
;; cargo links against the crate.  The Rust side declares the
;; `extern "C"' signature in `build-tool/src/lib.rs::elisp_cc_spike'
;; and exposes a safe wrapper `elisp_cc_spike::nlconsbox_clone(box_ptr)'
;; for the integration test in
;; `build-tool/tests/elisp_cc_nlconsbox_clone_probe.rs'.
;;
;; Stage scope: §124.A ships *only* the elisp kernel + probe + Rust
;; extern + safe wrapper.  The Rust-side `impl Clone for NlConsBoxRef'
;; body (= `unsafe { (*self.ptr.as_ptr()).refcount.fetch_add(1, Relaxed); }')
;; in `nlconsbox.rs:343-355' continues to use the inline atomic op
;; directly; §124.F's sweep stage will swap the 5 sibling
;; `impl Clone' bodies in one commit after §124.B-E PoCs all green.
;;
;; Drop stage scope: §124.G ships the matching `impl Drop' kernel
;; once Doc 125 §125.A defines the `(dealloc PTR LAYOUT)' grammar
;; op.  §124.A's Clone kernel does *not* need any alloc grammar
;; (= it's pure `rc_inc' + identity).

;;; Code:

(defconst nelisp-cc-nlconsbox-clone--source
  '(seq
    ;; Side-effect sequencer mirroring §116.A `nelisp_reader_prog2':
    ;; evaluate both args left-to-right (= SysV ABI arg marshal order
    ;; is fixed and well-defined), return the second arg.  Used to
    ;; thread the `atomic-fetch-add' side effect through a value
    ;; form that yields `box-ptr' as the public Clone return.
    (defun nelisp_nlconsbox_clone_prog2 (_eff val) val)

    ;; Public entry — bump refcount + return input pointer.
    (defun nelisp_nlconsbox_clone (box-ptr)
      (nelisp_nlconsbox_clone_prog2
       (atomic-fetch-add (+ box-ptr 64) 1)
       box-ptr)))
  "Phase 47 source for the Doc 124 §124.A NlConsBox Clone kernel.

Two-entry `(seq DEFUN ...)' manifest:
- `nelisp_nlconsbox_clone_prog2 (_eff val) -> val' — side-effect
  sequencer, identical pattern to §116.A `nelisp_reader_prog2'.
- `nelisp_nlconsbox_clone (box-ptr) -> box-ptr' — public entry.

Phase 47's SysV AMD64 prologue spills the first arg (`box-ptr' =
raw `NlConsBox*' as i64) into the rbp-relative slot 0.  The
public body marshals (1) the result of `(atomic-fetch-add (+
box-ptr 64) 1)' (= the refcount bump, identical to Doc 123 §123.A
`nelisp_rc_inc') and (2) `box-ptr' into `nelisp_nlconsbox_clone_prog2',
which by definition returns the second arg.  rax on return holds
`box-ptr', matching the trait impl's `Self { ptr, _marker }'
construction (= ABI no-op because `NlConsBoxRef' is `#[repr(transparent)]'
over `NonNull<NlConsBox>').

`atomic-fetch-add' lowers to `call nl_atomic_fetch_add@PLT' (= the
Rust extern in `build-tool/src/eval/raw_mem.rs:75' that wraps
`AtomicI64::fetch_add(delta, Ordering::SeqCst)').  Pre-add i64 in
rax is captured as the first arg to prog2 (= passed in rdi per
SysV ABI), then discarded when prog2 returns its second arg.

The Rust caller (= future §124.F dispatch swap) wraps the return
into `NlConsBoxRef { ptr: self.ptr, _marker: PhantomData }', but
the probe in `tests/elisp_cc_nlconsbox_clone_probe.rs' inspects
both the returned pointer (= must equal the input `box-ptr')
and the post-call refcount slot (= must equal old + 1) to verify
both halves of the Clone contract.

No allocation, no relocation in `.text' beyond the single PLT
fixup for `nl_atomic_fetch_add'.  Code-size profile: ~50-60 bytes
total across both entries (= one `mov + add + call + mov + call'
sequence for the public entry, ~10 bytes for the prog2 helper).")

(provide 'nelisp-cc-nlconsbox-clone)

;;; nelisp-cc-nlconsbox-clone.el ends here
