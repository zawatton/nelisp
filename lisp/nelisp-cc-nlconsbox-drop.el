;;; nelisp-cc-nlconsbox-drop.el --- Doc 124 §124.G NlConsBox Drop elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.G — first Drop-half stage of the `nl*.rs::Clone/Drop'
;; substrate elisp化 chain.  Sibling of §124.A (NlConsBox Clone, SHIPPED
;; 4f8ece23) and matched dependency target for the Doc 125 §125.A
;; alloc/dealloc grammar (SHIPPED 4f8ece23 same commit).  Pulls the
;; "fetch_sub + if-zero-dealloc" half of `impl Drop for NlConsBoxRef'
;; (`build-tool/src/eval/nlconsbox.rs:358-360' driving
;; `nlrc_drop_box!' in `build-tool/src/eval/nlrc.rs:265-275') and
;; re-expresses it in pure elisp using:
;;
;;   - §123.B `rc_dec' semantics (= `atomic-fetch-add' delta = -1)
;;     for the refcount-fetch-sub step.
;;   - §125.A `dealloc-bytes' for the if-zero-refcount free branch.
;;
;; Function contract:
;;   box-ptr: raw `*mut NlConsBox' coerced to i64 (= same shape as
;;            §124.A's clone kernel; the Rust shim
;;            `nl_consbox_ptr_to_i64' reinterprets `NonNull<NlConsBox>'
;;            as i64).
;;   returns: i64 = 1 sentinel for `and'-chain composition.  Mirrors
;;            §125.A `dealloc-bytes' return convention; the matching
;;            Rust `impl Drop' shim discards the return.
;;
;; The body is a two-arm `if' on the pre-sub refcount value:
;;
;;   (if (= (atomic-fetch-add (+ box-ptr 64) -1) 1)
;;       (dealloc-bytes box-ptr 72 8)  ; last ref — free the box
;;     1)                              ; still alive — no-op
;;
;; Constants:
;;   64 = REFCOUNT_OFFSET = `2 * size_of::<Sexp>()` (= byte offset of
;;        the `AtomicUsize refcount' trailer; same value §123.A /
;;        §123.B / §124.A use; pinned by `nlrc.rs:292' compile-time
;;        assert + `sexp_abi_assert.rs:45' size_of::<Sexp> = 32).
;;   72 = SIZE_OF_NLCONSBOX = `2 * size_of::<Sexp>() + size_of::<AtomicUsize>()`
;;        = 32 + 32 + 8.  Matches `mem::size_of::<NlConsBox>()` per
;;        `#[repr(C)]' layout: car @ 0 (32) + cdr @ 32 (32) + refcount
;;        @ 64 (8).  Total alignment = 8 (max of Sexp's 8 and AtomicUsize's 8).
;;    8 = alignof::<NlConsBox> = max(alignof::<Sexp>, alignof::<AtomicUsize>) = 8.
;;
;; Ordering: §122.E `atomic-fetch-add' uses `Ordering::SeqCst' while
;; Rust's `nlrc_drop_box!' uses `Release' for the fetch_sub +
;; `Acquire' fence before the dealloc.  SeqCst ⊃ Release+Acquire (=
;; sequentially-consistent already implies the same happens-before as
;; the explicit fence), so the elisp body is correct without the
;; explicit `fence' op (= we'd add a `fence-acquire' grammar op in a
;; future Doc 122 phase if profiling shows SeqCst's cost matters).
;;
;; Interior payload Drop limitation (= PoC scope, documented as
;; known limitation):
;;
;;   The Rust `nlrc_drop_box!' macro runs `NLRC_DROP_TABLE[tag](raw)'
;;   before the dealloc, which dispatches to
;;   `nlrc_payload_drop::<NlConsBox>' = `std::ptr::drop_in_place' on
;;   the `*mut NlConsBox' (= recursively drops the car + cdr `Sexp'
;;   payloads, which themselves may be `Sexp::Cons(NlConsBoxRef)' /
;;   `Sexp::Vector(NlVectorRef)' / etc holding NlBoxes — those clones
;;   then re-enter `nlrc_drop_box!' for their own type, walking the
;;   whole sub-tree).  Expressing this recursion in elisp requires
;;   the §124.H-K sibling kernels (= one Drop per NlBox variant) plus
;;   a Sexp-tag-dispatch jump table emitted by the §124.L sweep
;;   stage — the recursion cannot land in §124.G alone.
;;
;;   §124.G's PoC body skips the interior drop step.  If the box's
;;   refcount hits 0 the box's heap allocation is freed but the car
;;   and cdr `Sexp' payloads are leaked.  This is acceptable because:
;;
;;     (1) The Rust `impl Drop' body is UNCHANGED in §124.G (= same
;;         §124.A scope split as Clone).  Production callers continue
;;         to drive Drop through `nlrc_drop_box!' which does the full
;;         recursive walk.
;;     (2) The probe in `tests/elisp_cc_nlconsbox_drop_probe.rs' uses
;;         `[u8; 32]' for car/cdr (= no nested Sexp payloads), so the
;;         leak is invisible to the probe's invariants.
;;     (3) §124.L's sweep stage will replace the placeholder `if'
;;         branch with the full tag-dispatch table once §124.H-K and
;;         the per-variant elisp Drop kernels are SHIPPED.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nelisp_nlconsbox_drop.o' and archives it into the static lib
;; cargo links against the crate.  The Rust side declares the
;; `extern "C"' signature in `build-tool/src/lib.rs::elisp_cc_spike'
;; and exposes a safe wrapper `elisp_cc_spike::nlconsbox_drop(box_ptr)'
;; for the integration test in
;; `build-tool/tests/elisp_cc_nlconsbox_drop_probe.rs'.
;;
;; Stage scope: §124.G ships *only* the elisp kernel + probe + Rust
;; extern + safe wrapper.  The Rust-side `impl Drop for NlConsBoxRef'
;; body (= `crate::nlrc_drop_box!(self.ptr.as_ptr(), NlConsBox,
;; SEXP_TAG_CONS)' in `nlconsbox.rs:358-360') continues to drive
;; production Drop dispatch; §124.L's sweep stage will swap the 5
;; sibling `impl Drop' bodies in one commit after §124.H-K sibling
;; PoCs all green (= mirrors the §124.F clone-sweep gate).

;;; Code:

(defconst nelisp-cc-nlconsbox-drop--source
  '(seq
    ;; Side-effect sequencer mirroring §124.A `nelisp_nlconsbox_clone_prog2':
    ;; evaluate both args left-to-right, return the second arg.  Reserved
    ;; for the §124.L sweep stage when the interior-payload drop step is
    ;; threaded between the fetch-sub and the dealloc-bytes call (= the
    ;; recursive walk's return value is discarded, only its side effects
    ;; matter).  Kept here for ABI/source parity with §124.A's two-entry
    ;; manifest shape and to flag the future expansion point.
    (defun nelisp_nlconsbox_drop_prog2 (_eff val) val)

    ;; Public entry — fetch-sub refcount + branch on pre-sub == 1.
    ;;
    ;; Layout (mirrors `nlconsbox.rs:418-431' + `nlrc.rs:292' + `sexp_abi_assert.rs:45'):
    ;;   car      @ 0   (32 bytes — size_of::<Sexp>)
    ;;   cdr      @ 32  (32 bytes — size_of::<Sexp>)
    ;;   refcount @ 64  (8 bytes  — AtomicUsize)
    ;;   total = 72 bytes, align = 8
    ;; Doc 124 §124.L: thread the per-type `nl_consbox_drop_inner'
     ;; (= `NlConsBox::DROP_FN' = `drop_in_place::<NlConsBox>') between
     ;; the fetch-sub and the dealloc-bytes call so the inner car / cdr
     ;; Sexp payloads (= nested NlBox handles) are recursively dropped
     ;; before the outer 72-byte allocation is freed.  Matches the
     ;; `nlrc_drop_box!' macro's ordering: fetch_sub → fence → DROP_FN
     ;; → dealloc.
    (defun nelisp_nlconsbox_drop (box-ptr)
      (if (= (atomic-fetch-add (+ box-ptr 64) -1) 1)
          ;; Last ref — pre-sub was 1, new count is 0.  Drop the
          ;; interior payload (= recursively walk car + cdr via
          ;; `drop_in_place::<NlConsBox>') then free the 72-byte
          ;; allocation.
          (nelisp_nlconsbox_drop_prog2
           (extern-call nl_consbox_drop_inner box-ptr)
           (dealloc-bytes box-ptr 72 8))
        ;; Still alive — pre-sub was > 1.  Return 1 sentinel to match
        ;; the dealloc-bytes arm's return convention so the caller
        ;; sees a uniform `i64 = 1' on both branches.
        1)))
  "Phase 47 source for the Doc 124 §124.G NlConsBox Drop kernel.

Two-entry `(seq DEFUN ...)' manifest mirroring §124.A:
- `nelisp_nlconsbox_drop_prog2 (_eff val) -> val' — side-effect
  sequencer, reserved for §124.L sweep stage's recursive payload
  drop integration.
- `nelisp_nlconsbox_drop (box-ptr) -> i64' — public entry.

Phase 47's SysV AMD64 prologue spills the first arg (`box-ptr' =
raw `*mut NlConsBox' as i64) into the rbp-relative slot 0.  The
public body:

  1. Computes `(+ box-ptr 64)' = address of the AtomicUsize
     refcount trailer per `nlconsbox.rs:418-431' compile-time
     asserts.
  2. Calls `atomic-fetch-add' (= §122.E grammar op, lowers to
     `nl_atomic_fetch_add@PLT' = `AtomicI64::fetch_add(-1, SeqCst)')
     with delta = -1 (= fetch-sub semantics, same as §123.B
     `nelisp_rc_dec').  Pre-sub i64 lands in rax.
  3. Compares rax == 1 via the `=' grammar op (= `cmp rax, 1; sete'
     into a 0/1 i64).
  4. Branches: if pre-sub was 1, the box reached refcount 0 and
     we call `dealloc-bytes' (= §125.A, lowers to
     `nl_dealloc_bytes(box_ptr, 72, 8)' = `std::alloc::dealloc'
     with the NlConsBox layout).  Otherwise return 1 sentinel
     directly (= no-op, the box has more handles alive).

Both branches yield rax = 1 sentinel (= `dealloc-bytes' returns 1
on success per §125.A contract; the else-arm produces the literal
1).  The `if' grammar op merges control flow at the joined return,
so the function's overall return is a single `mov eax, 1' at the
epilogue prefix.

Code-size profile: ~50-60 bytes (= `mov + add + call' for the
atomic op + `cmp + jne' for the branch + `mov + call' for the
dealloc + `mov eax, 1; ret' at the joined epilogue).  Two PLT
fixups: `nl_atomic_fetch_add' and `nl_dealloc_bytes'.

Known limitation: interior payload (car + cdr Sexp) Drop is NOT
recursively walked in this PoC.  If the box hits refcount 0 the
allocation is freed but nested NlBox handles in car/cdr leak.
Production Rust `impl Drop' (via `nlrc_drop_box!') continues to
do the full recursive walk; §124.L sweep stage will lift the
recursion into elisp once §124.H-K sibling kernels SHIP.")

(provide 'nelisp-cc-nlconsbox-drop)

;;; nelisp-cc-nlconsbox-drop.el ends here
