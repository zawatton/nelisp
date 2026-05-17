;;; nelisp-cc-nlvector-drop.el --- Doc 124 §124.H NlVector Drop elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.H — mechanical port of §124.G's NlConsBox Drop kernel
;; to NlVector.  Identical shape modulo the per-type layout literals:
;;
;;   §124.G NlConsBox: REFCOUNT_OFFSET = 64, SIZE = 72, ALIGN = 8
;;   §124.H NlVector:  REFCOUNT_OFFSET = 24, SIZE = 32, ALIGN = 8
;;
;; `NlVector' layout (= `build-tool/src/eval/nlvector.rs:43-49',
;; `#[repr(C)]'):
;;
;;   offset 0:  `value: Vec<Sexp>'        (= 24 bytes — ptr + len + cap triple)
;;   offset 24: `refcount: AtomicUsize'   (= 8 bytes — i64 slot)
;;   total = 32 bytes, align = 8
;;
;; The `offset_of!(NlVector, refcount) == size_of::<Vec<Sexp>>()'
;; compile-time assert at `nlvector.rs:233' pins the 24-byte trailer;
;; `size_of::<AtomicUsize>() == 8' assert at `nlvector.rs:234' pins
;; the 8-byte refcount slot.  Combined: SIZE_OF_NLVECTOR = 24 + 8 = 32.
;;
;; Note on "size": only the outer `NlVector' struct allocation is
;; freed by `dealloc-bytes' here (= 32 bytes via `Layout::new::<NlVector>()'
;; in `nlvector.rs:69, 143').  The Vec's *heap-allocated tail* (= the
;; `ptr' field of the Vec<Sexp> header, pointing at the element backing
;; store) is owned separately by Rust's `Vec' Drop impl and is leaked
;; by this PoC — same scope-limit as §124.G car/cdr payload leak, see
;; "Interior payload Drop limitation" below.
;;
;; Function contract (mirrors §124.G):
;;   box-ptr: raw `*mut NlVector' coerced to i64 (= same shape as
;;            §124.B's clone kernel; the Rust shim `NlVectorRef' is
;;            `#[repr(transparent)]' over `NonNull<NlVector>' per
;;            `nlvector.rs:59-63').
;;   returns: i64 = 1 sentinel for `and'-chain composition.  Both arms
;;            yield 1 (= `dealloc-bytes' 1 sentinel on the last-ref
;;            branch, literal 1 on the still-alive branch).
;;
;; The body is a two-arm `if' on the pre-sub refcount value:
;;
;;   (if (= (atomic-fetch-add (+ box-ptr 24) -1) 1)
;;       (dealloc-bytes box-ptr 32 8)  ; last ref — free the box
;;     1)                              ; still alive — no-op
;;
;; Ordering: §122.E `atomic-fetch-add' uses `Ordering::SeqCst' while
;; Rust's `nlrc_drop_box!' uses `Release' for the fetch_sub + `Acquire'
;; fence before the dealloc.  SeqCst ⊃ Release+Acquire (= same
;; reasoning as §124.G).
;;
;; Interior payload Drop limitation (= PoC scope, mirrors §124.G):
;;
;;   The Rust `nlrc_drop_box!' macro runs `NLRC_DROP_TABLE[tag](raw)'
;;   before the dealloc, which dispatches to
;;   `nlrc_payload_drop::<NlVector>' = `std::ptr::drop_in_place' on
;;   the `*mut NlVector'.  For NlVector this recursively:
;;     (a) Drops `value: Vec<Sexp>' — frees the heap-allocated element
;;         backing store AND walks each `Sexp' element to drop nested
;;         NlBox handles (= each element may be `Sexp::Cons' / `Sexp::Vector'
;;         / etc carrying its own refcounted box, which re-enters
;;         `nlrc_drop_box!' for that type).
;;     (b) The `refcount: AtomicUsize' has trivial Drop (= no-op).
;;
;;   §124.H's PoC body skips both interior-drop steps.  If the box's
;;   refcount hits 0:
;;     - The 32-byte outer `NlVector' allocation is freed.
;;     - The `Vec<Sexp>'s element backing store (heap-allocated ptr in
;;       the 24-byte header) is LEAKED (= one alloc + N Sexp elements,
;;       potentially each carrying nested NlBoxes that are also leaked).
;;
;;   This is acceptable because:
;;
;;     (1) The Rust `impl Drop' body is UNCHANGED in §124.H (= same
;;         §124.G scope split).  Production callers continue to drive
;;         Drop through `nlrc_drop_box!' which does the full recursive
;;         walk via `drop_in_place'.
;;     (2) The probe in `tests/elisp_cc_nlvector_drop_probe.rs' uses
;;         a `ProbeBox' shape `[u8; 24] + AtomicI64' to seed the
;;         refcount slot at the right offset (= no real `Vec<Sexp>'
;;         to free; the value bytes are zeroed and never followed),
;;         so the leak is invisible to the probe's invariants.
;;     (3) §124.L's sweep stage will replace the placeholder `if'
;;         branch with the full tag-dispatch table once §124.H-K and
;;         the per-variant elisp Drop kernels are SHIPPED.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nelisp_nlvector_drop.o' and archives it into the static lib
;; cargo links against the crate.  The Rust side declares the
;; `extern "C"' signature in `build-tool/src/lib.rs::elisp_cc_spike'
;; and exposes a safe wrapper `elisp_cc_spike::nlvector_drop(box_ptr)'
;; for the integration test in
;; `build-tool/tests/elisp_cc_nlvector_drop_probe.rs'.
;;
;; Stage scope: §124.H ships *only* the elisp kernel + probe + Rust
;; extern + safe wrapper.  The Rust-side `impl Drop for NlVectorRef'
;; body (= `crate::nlrc_drop_box!(self.ptr.as_ptr(), NlVector,
;; crate::eval::sexp::SEXP_TAG_VECTOR)' in `nlvector.rs:200-202')
;; continues to drive production Drop dispatch; §124.L's sweep stage
;; will swap the 5 sibling `impl Drop' bodies in one commit after
;; §124.H-K sibling PoCs all green (= mirrors the §124.F / §124.G
;; sweep gate pattern).

;;; Code:

(defconst nelisp-cc-nlvector-drop--source
  '(seq
    ;; Side-effect sequencer mirroring §124.G `nelisp_nlconsbox_drop_prog2':
    ;; evaluate both args left-to-right, return the second arg.  Reserved
    ;; for the §124.L sweep stage when the interior-payload drop step is
    ;; threaded between the fetch-sub and the dealloc-bytes call (= the
    ;; recursive Vec<Sexp> walk's return value is discarded, only its
    ;; side effects matter).  Kept here for ABI/source parity with §124.G's
    ;; two-entry manifest shape and to flag the future expansion point.
    (defun nelisp_nlvector_drop_prog2 (_eff val) val)

    ;; Public entry — fetch-sub refcount + branch on pre-sub == 1.
    ;;
    ;; Layout (mirrors `nlvector.rs:43-49' + `nlvector.rs:230-235' asserts):
    ;;   value    @ 0   (24 bytes — size_of::<Vec<Sexp>>)
    ;;   refcount @ 24  (8 bytes  — AtomicUsize)
    ;;   total = 32 bytes, align = 8
    (defun nelisp_nlvector_drop (box-ptr)
      (if (= (atomic-fetch-add (+ box-ptr 24) -1) 1)
          ;; Last ref — pre-sub was 1, new count is 0.  Free the box.
          ;; Interior Vec<Sexp> tail + nested Sexp NlBox payloads
          ;; deferred to §124.L sweep stage; see file Commentary for
          ;; rationale and leak scope.
          (dealloc-bytes box-ptr 32 8)
        ;; Still alive — pre-sub was > 1.  Return 1 sentinel to match
        ;; the dealloc-bytes arm's return convention so the caller
        ;; sees a uniform `i64 = 1' on both branches.
        1)))
  "Phase 47 source for the Doc 124 §124.H NlVector Drop kernel.

Two-entry `(seq DEFUN ...)' manifest mirroring §124.G:
- `nelisp_nlvector_drop_prog2 (_eff val) -> val' — side-effect
  sequencer, reserved for §124.L sweep stage's recursive payload
  drop integration (= Vec<Sexp> tail + nested NlBox walk).
- `nelisp_nlvector_drop (box-ptr) -> i64' — public entry.

Phase 47's SysV AMD64 prologue spills the first arg (`box-ptr' =
raw `*mut NlVector' as i64) into the rbp-relative slot 0.  The
public body:

  1. Computes `(+ box-ptr 24)' = address of the AtomicUsize
     refcount trailer per `nlvector.rs:230-235' compile-time
     asserts (= `offset_of!(NlVector, refcount) ==
     size_of::<Vec<Sexp>>() = 24').
  2. Calls `atomic-fetch-add' (= §122.E grammar op, lowers to
     `nl_atomic_fetch_add@PLT' = `AtomicI64::fetch_add(-1, SeqCst)')
     with delta = -1 (= fetch-sub semantics, same as §123.B
     `nelisp_rc_dec').  Pre-sub i64 lands in rax.
  3. Compares rax == 1 via the `=' grammar op (= `cmp rax, 1; sete'
     into a 0/1 i64).
  4. Branches: if pre-sub was 1, the box reached refcount 0 and
     we call `dealloc-bytes' (= §125.A, lowers to
     `nl_dealloc_bytes(box_ptr, 32, 8)' = `std::alloc::dealloc'
     with the `Layout::new::<NlVector>()' = size 32, align 8 layout).
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

Known limitation: interior Vec<Sexp> payload Drop is NOT
recursively walked in this PoC.  If the box hits refcount 0 the
outer 32-byte allocation is freed but the Vec's element backing
store (= heap-allocated tail behind the 24-byte Vec header) leaks
along with any nested NlBox handles held by the Sexp elements.
Production Rust `impl Drop' (via `nlrc_drop_box!') continues to
do the full recursive walk; §124.L sweep stage will lift the
recursion into elisp once §124.H-K sibling kernels SHIP.")

(provide 'nelisp-cc-nlvector-drop)

;;; nelisp-cc-nlvector-drop.el ends here
