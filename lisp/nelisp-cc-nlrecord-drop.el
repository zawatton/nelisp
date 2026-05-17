;;; nelisp-cc-nlrecord-drop.el --- Doc 124 §124.J NlRecord Drop elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.J — mechanical port of §124.G/H's NlConsBox/NlVector
;; Drop kernel to NlRecord.  Identical shape modulo the per-type
;; layout literals:
;;
;;   §124.G NlConsBox: REFCOUNT_OFFSET = 64, SIZE = 72, ALIGN = 8
;;   §124.H NlVector:  REFCOUNT_OFFSET = 24, SIZE = 32, ALIGN = 8
;;   §124.J NlRecord:  REFCOUNT_OFFSET = 56, SIZE = 64, ALIGN = 8
;;
;; `NlRecord' layout (= `build-tool/src/eval/nlrecord.rs:50-58',
;; `#[repr(C)]'):
;;
;;   offset 0:  `type_tag: Sexp'        (= 32 bytes — type-tag tagged slot)
;;   offset 32: `slots: Vec<Sexp>'      (= 24 bytes — ptr + len + cap triple)
;;   offset 56: `refcount: AtomicUsize' (= 8 bytes — i64 slot)
;;   total = 64 bytes, align = 8
;;
;; The `offset_of!(NlRecord, refcount) == size_of::<Sexp>() +
;; size_of::<Vec<Sexp>>()' compile-time assert at `nlrecord.rs:251-253'
;; pins the 56-byte trailer offset; `size_of::<AtomicUsize>() == 8'
;; assert at `nlrecord.rs:254' pins the 8-byte refcount slot.  Combined:
;; SIZE_OF_NLRECORD = 56 + 8 = 64.
;;
;; Function contract (mirrors §124.G/H):
;;   box-ptr: raw `*mut NlRecord' coerced to i64 (= same shape as
;;            §124.D's clone kernel; the Rust shim `NlRecordRef' is
;;            `#[repr(transparent)]' over `NonNull<NlRecord>' per
;;            `nlrecord.rs:67-71').
;;   returns: i64 = 1 sentinel for `and'-chain composition.  Both arms
;;            yield 1 (= `dealloc-bytes' 1 sentinel on the last-ref
;;            branch, literal 1 on the still-alive branch).
;;
;; The body is a two-arm `if' on the pre-sub refcount value:
;;
;;   (if (= (atomic-fetch-add (+ box-ptr 56) -1) 1)
;;       (dealloc-bytes box-ptr 64 8)  ; last ref — free the box
;;     1)                              ; still alive — no-op
;;
;; Ordering: §122.E `atomic-fetch-add' uses `Ordering::SeqCst' while
;; Rust's `nlrc_drop_box!' uses `Release' for the fetch_sub + `Acquire'
;; fence before the dealloc.  SeqCst ⊃ Release+Acquire (= same
;; reasoning as §124.G/H).
;;
;; Interior payload Drop limitation (= PoC scope, mirrors §124.G/H):
;;
;;   The Rust `nlrc_drop_box!' macro runs `NLRC_DROP_TABLE[tag](raw)'
;;   before the dealloc, which dispatches to
;;   `nlrc_payload_drop::<NlRecord>' = `std::ptr::drop_in_place' on
;;   the `*mut NlRecord'.  For NlRecord this recursively:
;;     (a) Drops `type_tag: Sexp' — walks the tagged enum and drops any
;;         nested NlBox handle.
;;     (b) Drops `slots: Vec<Sexp>' — frees the heap-allocated element
;;         backing store AND walks each `Sexp' slot to drop nested
;;         NlBox handles (= each slot may be `Sexp::Cons' / `Sexp::Cell'
;;         / `Sexp::Record' / etc carrying its own refcounted box).
;;     (c) The `refcount: AtomicUsize' has trivial Drop (= no-op).
;;
;;   §124.J's PoC body skips all interior-drop steps.  If the box's
;;   refcount hits 0:
;;     - The 64-byte outer `NlRecord' allocation is freed.
;;     - The `type_tag: Sexp' payload (= any nested NlBox handle) is
;;       LEAKED.
;;     - The `slots: Vec<Sexp>'s element backing store (= heap-allocated
;;       ptr in the 24-byte Vec header) is LEAKED, plus each Sexp
;;       element's nested NlBox handles.
;;
;;   This is acceptable because:
;;
;;     (1) The Rust `impl Drop' body is UNCHANGED in §124.J (= same
;;         §124.D scope split as Clone).  Production callers continue
;;         to drive Drop through `nlrc_drop_box!' which does the full
;;         recursive walk via `drop_in_place'.
;;     (2) The probe in `tests/elisp_cc_nlrecord_drop_probe.rs' uses
;;         a fresh `alloc_bytes(64, 8)' block seeded only at the
;;         refcount-trailer offset (= the type_tag and slots bytes are
;;         uninitialized garbage; the kernel never reads them, so the
;;         leak is invisible to the probe's invariants).
;;     (3) §124.L's sweep stage will replace the placeholder `if'
;;         branch with the full tag-dispatch table once §124.H-K and
;;         the per-variant elisp Drop kernels are SHIPPED.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nelisp_nlrecord_drop.o' and archives it into the static lib cargo
;; links against the crate.  The Rust side declares the `extern "C"'
;; signature in `build-tool/src/lib.rs::elisp_cc_spike' and exposes a
;; safe wrapper `elisp_cc_spike::nlrecord_drop(box_ptr)' for the
;; integration test in
;; `build-tool/tests/elisp_cc_nlrecord_drop_probe.rs'.
;;
;; Stage scope: §124.J ships *only* the elisp kernel + probe + Rust
;; extern + safe wrapper.  The Rust-side `impl Drop for NlRecordRef'
;; body (= `crate::nlrc_drop_box!(self.ptr.as_ptr(), NlRecord,
;; crate::eval::sexp::SEXP_TAG_RECORD)' in `nlrecord.rs:214-216')
;; continues to drive production Drop dispatch; §124.L's sweep stage
;; will swap the 5 sibling `impl Drop' bodies in one commit after
;; §124.H-K sibling PoCs all green (= mirrors the §124.F clone-sweep
;; gate pattern).

;;; Code:

(defconst nelisp-cc-nlrecord-drop--source
  '(seq
    ;; Side-effect sequencer mirroring §124.G/H `nelisp_nl*_drop_prog2':
    ;; evaluate both args left-to-right, return the second arg.  Reserved
    ;; for the §124.L sweep stage when the interior-payload drop step is
    ;; threaded between the fetch-sub and the dealloc-bytes call (= the
    ;; recursive type_tag + slots-Vec walk's return value is discarded,
    ;; only its side effects matter).  Kept here for ABI/source parity
    ;; with §124.G/H's two-entry manifest shape and to flag the future
    ;; expansion point.
    (defun nelisp_nlrecord_drop_prog2 (_eff val) val)

    ;; Public entry — fetch-sub refcount + branch on pre-sub == 1.
    ;;
    ;; Layout (mirrors `nlrecord.rs:50-58' + `nlrecord.rs:248-254' asserts):
    ;;   type_tag @ 0   (32 bytes — size_of::<Sexp>)
    ;;   slots    @ 32  (24 bytes — size_of::<Vec<Sexp>>)
    ;;   refcount @ 56  (8 bytes  — AtomicUsize)
    ;;   total = 64 bytes, align = 8
    (defun nelisp_nlrecord_drop (box-ptr)
      (if (= (atomic-fetch-add (+ box-ptr 56) -1) 1)
          ;; Last ref — pre-sub was 1, new count is 0.  Free the box.
          ;; Interior payload drop (= drop_in_place type_tag + slots-Vec
          ;; + nested NlBox walk) deferred to §124.L sweep stage; see
          ;; file Commentary for rationale and leak scope.
          (dealloc-bytes box-ptr 64 8)
        ;; Still alive — pre-sub was > 1.  Return 1 sentinel to match
        ;; the dealloc-bytes arm's return convention so the caller
        ;; sees a uniform `i64 = 1' on both branches.
        1)))
  "Phase 47 source for the Doc 124 §124.J NlRecord Drop kernel.

Two-entry `(seq DEFUN ...)' manifest mirroring §124.G/H:
- `nelisp_nlrecord_drop_prog2 (_eff val) -> val' — side-effect
  sequencer, reserved for §124.L sweep stage's recursive payload
  drop integration (= type_tag Sexp + slots Vec<Sexp> walk).
- `nelisp_nlrecord_drop (box-ptr) -> i64' — public entry.

Phase 47's SysV AMD64 prologue spills the first arg (`box-ptr' =
raw `*mut NlRecord' as i64) into the rbp-relative slot 0.  The
public body:

  1. Computes `(+ box-ptr 56)' = address of the AtomicUsize
     refcount trailer per `nlrecord.rs:248-254' compile-time
     asserts (= `offset_of!(NlRecord, refcount) == size_of::<Sexp>()
     + size_of::<Vec<Sexp>>() = 32 + 24 = 56').
  2. Calls `atomic-fetch-add' (= §122.E grammar op, lowers to
     `nl_atomic_fetch_add@PLT' = `AtomicI64::fetch_add(-1, SeqCst)')
     with delta = -1 (= fetch-sub semantics, same as §123.B
     `nelisp_rc_dec').  Pre-sub i64 lands in rax.
  3. Compares rax == 1 via the `=' grammar op (= `cmp rax, 1; sete'
     into a 0/1 i64).
  4. Branches: if pre-sub was 1, the box reached refcount 0 and
     we call `dealloc-bytes' (= §125.A, lowers to
     `nl_dealloc_bytes(box_ptr, 64, 8)' = `std::alloc::dealloc'
     with the `Layout::new::<NlRecord>()' = size 64, align 8 layout).
     Otherwise return 1 sentinel directly (= no-op, the box has more
     handles alive).

Both branches yield rax = 1 sentinel (= `dealloc-bytes' returns 1
on success per §125.A contract; the else-arm produces the literal
1).  The `if' grammar op merges control flow at the joined return,
so the function's overall return is a single `mov eax, 1' at the
epilogue prefix.

Code-size profile: ~50-60 bytes (= `mov + add + call' for the
atomic op + `cmp + jne' for the branch + `mov + call' for the
dealloc + `mov eax, 1; ret' at the joined epilogue).  Two PLT
fixups: `nl_atomic_fetch_add' and `nl_dealloc_bytes'.

Known limitation: interior payload (type_tag Sexp + slots Vec<Sexp>)
Drop is NOT recursively walked in this PoC.  If the box hits
refcount 0 the outer 64-byte allocation is freed but the Vec's
element backing store and any nested NlBox handles in type_tag /
slots leak.  Production Rust `impl Drop' (via `nlrc_drop_box!')
continues to do the full recursive walk; §124.L sweep stage will
lift the recursion into elisp once §124.H-K sibling kernels SHIP.")

(provide 'nelisp-cc-nlrecord-drop)

;;; nelisp-cc-nlrecord-drop.el ends here
