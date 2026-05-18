;;; nelisp-cc-nlcell-drop.el --- Doc 124 §124.I NlCell Drop elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.I — mechanical port of §124.G's NlConsBox Drop kernel
;; (+ §124.H's NlVector Drop sibling) to NlCell.  Identical shape
;; modulo the per-type layout literals:
;;
;;   §124.G NlConsBox: REFCOUNT_OFFSET = 64, SIZE = 72, ALIGN = 8
;;   §124.H NlVector:  REFCOUNT_OFFSET = 24, SIZE = 32, ALIGN = 8
;;   §124.I NlCell:    REFCOUNT_OFFSET = 32, SIZE = 40, ALIGN = 8
;;
;; `NlCell' layout (= `build-tool/src/eval/nlcell.rs:56-64',
;; `#[repr(C)]'):
;;
;;   offset 0:  `value: Sexp'            (= 32 bytes — single tagged slot)
;;   offset 32: `refcount: AtomicUsize'  (= 8 bytes — i64 slot)
;;   total = 40 bytes, align = 8
;;
;; The `offset_of!(NlCell, refcount) == size_of::<Sexp>()' compile-
;; time assert at `nlcell.rs:288' pins the 32-byte trailer offset;
;; `size_of::<AtomicUsize>() == 8' assert at `nlcell.rs:291' pins the
;; 8-byte refcount slot.  Combined: SIZE_OF_NLCELL = 32 + 8 = 40.
;;
;; Function contract (mirrors §124.G/H):
;;   box-ptr: raw `*mut NlCell' coerced to i64 (= same shape as
;;            §124.C's clone kernel; the Rust shim `NlCellRef' is
;;            `#[repr(transparent)]' over `NonNull<NlCell>' per
;;            `nlcell.rs:78-85').
;;   returns: i64 = 1 sentinel for `and'-chain composition.  Both arms
;;            yield 1 (= `dealloc-bytes' 1 sentinel on the last-ref
;;            branch, literal 1 on the still-alive branch).
;;
;; The body is a two-arm `if' on the pre-sub refcount value:
;;
;;   (if (= (atomic-fetch-add (+ box-ptr 32) -1) 1)
;;       (dealloc-bytes box-ptr 40 8)  ; last ref — free the box
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
;;   `nlrc_payload_drop::<NlCell>' = `std::ptr::drop_in_place' on
;;   the `*mut NlCell'.  For NlCell this recursively:
;;     (a) Drops `value: Sexp' — walks the tagged enum and drops any
;;         nested NlBox handle (= `Sexp::Cons' / `Sexp::Vector' /
;;         `Sexp::Cell' / `Sexp::Record' / `Sexp::MutStr' each carry
;;         their own refcounted box, which re-enters `nlrc_drop_box!'
;;         for that type).
;;     (b) The `refcount: AtomicUsize' has trivial Drop (= no-op).
;;
;;   §124.I's PoC body skips the interior-drop step.  If the box's
;;   refcount hits 0:
;;     - The 40-byte outer `NlCell' allocation is freed.
;;     - The `value: Sexp' payload (= any nested NlBox handle held in
;;       the tagged enum) is LEAKED.
;;
;;   This is acceptable because:
;;
;;     (1) The Rust `impl Drop' body is UNCHANGED in §124.I (= same
;;         §124.C scope split as Clone).  Production callers continue
;;         to drive Drop through `nlrc_drop_box!' which does the full
;;         recursive walk via `drop_in_place'.
;;     (2) The probe in `tests/elisp_cc_nlcell_drop_probe.rs' uses
;;         a fresh `alloc_bytes(40, 8)' block seeded only at the
;;         refcount-trailer offset (= the value bytes are uninitialized
;;         garbage; the kernel never reads them, so the leak is
;;         invisible to the probe's invariants).
;;     (3) §124.L's sweep stage will replace the placeholder `if'
;;         branch with the full tag-dispatch table once §124.H-K and
;;         the per-variant elisp Drop kernels are SHIPPED.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nelisp_nlcell_drop.o' and archives it into the static lib cargo
;; links against the crate.  The Rust side declares the `extern "C"'
;; signature in `build-tool/src/lib.rs::elisp_cc_spike' and exposes a
;; safe wrapper `elisp_cc_spike::nlcell_drop(box_ptr)' for the
;; integration test in
;; `build-tool/tests/elisp_cc_nlcell_drop_probe.rs'.
;;
;; Stage scope: §124.I ships *only* the elisp kernel + probe + Rust
;; extern + safe wrapper.  The Rust-side `impl Drop for NlCellRef'
;; body (= `crate::nlrc_drop_box!(self.ptr.as_ptr(), NlCell,
;; crate::eval::sexp::SEXP_TAG_CELL)' in `nlcell.rs:239-241')
;; continues to drive production Drop dispatch; §124.L's sweep stage
;; will swap the 5 sibling `impl Drop' bodies in one commit after
;; §124.H-K sibling PoCs all green (= mirrors the §124.F clone-sweep
;; gate pattern).

;;; Code:

(defconst nelisp-cc-nlcell-drop--source
  '(seq
    ;; Side-effect sequencer mirroring §124.G/H `nelisp_nl*_drop_prog2':
    ;; evaluate both args left-to-right, return the second arg.  Reserved
    ;; for the §124.L sweep stage when the interior-payload drop step is
    ;; threaded between the fetch-sub and the dealloc-bytes call (= the
    ;; recursive value-Sexp walk's return value is discarded, only its
    ;; side effects matter).  Kept here for ABI/source parity with §124.G/H's
    ;; two-entry manifest shape and to flag the future expansion point.
    (defun nelisp_nlcell_drop_prog2 (_eff val) val)

    ;; Public entry — fetch-sub refcount + branch on pre-sub == 1.
    ;;
    ;; Layout (mirrors `nlcell.rs:56-64' + `nlcell.rs:283-292' asserts):
    ;;   value    @ 0   (32 bytes — size_of::<Sexp>)
    ;;   refcount @ 32  (8 bytes  — AtomicUsize)
    ;;   total = 40 bytes, align = 8
    ;; Doc 124 §124.L: thread `nl_cell_drop_inner' (= `drop_in_place
     ;; ::<NlCell>') between the fetch-sub and the dealloc-bytes call
     ;; so the inner `value: Sexp' (= nested NlBox handle) is dropped
     ;; before the outer 40-byte allocation is freed.  Matches
     ;; `nlrc_drop_box!' ordering.
    (defun nelisp_nlcell_drop (box-ptr)
      (if (= (atomic-fetch-add (+ box-ptr 32) -1) 1)
          ;; Last ref — pre-sub was 1, new count is 0.  Drop interior
          ;; `value: Sexp' then free the 40-byte outer allocation.
          (nelisp_nlcell_drop_prog2
           (extern-call nl_cell_drop_inner box-ptr)
           (dealloc-bytes box-ptr 40 8))
        ;; Still alive — pre-sub was > 1.  Return 1 sentinel to match
        ;; the dealloc-bytes arm's return convention so the caller
        ;; sees a uniform `i64 = 1' on both branches.
        1)))
  "Phase 47 source for the Doc 124 §124.I NlCell Drop kernel.

Two-entry `(seq DEFUN ...)' manifest mirroring §124.G/H:
- `nelisp_nlcell_drop_prog2 (_eff val) -> val' — side-effect
  sequencer, reserved for §124.L sweep stage's recursive payload
  drop integration (= `value: Sexp' tagged-enum walk).
- `nelisp_nlcell_drop (box-ptr) -> i64' — public entry.

Phase 47's SysV AMD64 prologue spills the first arg (`box-ptr' =
raw `*mut NlCell' as i64) into the rbp-relative slot 0.  The
public body:

  1. Computes `(+ box-ptr 32)' = address of the AtomicUsize
     refcount trailer per `nlcell.rs:283-292' compile-time
     asserts (= `offset_of!(NlCell, refcount) == size_of::<Sexp>()
     = 32').
  2. Calls `atomic-fetch-add' (= §122.E grammar op, lowers to
     `nl_atomic_fetch_add@PLT' = `AtomicI64::fetch_add(-1, SeqCst)')
     with delta = -1 (= fetch-sub semantics, same as §123.B
     `nelisp_rc_dec').  Pre-sub i64 lands in rax.
  3. Compares rax == 1 via the `=' grammar op (= `cmp rax, 1; sete'
     into a 0/1 i64).
  4. Branches: if pre-sub was 1, the box reached refcount 0 and
     we call `dealloc-bytes' (= §125.A, lowers to
     `nl_dealloc_bytes(box_ptr, 40, 8)' = `std::alloc::dealloc'
     with the `Layout::new::<NlCell>()' = size 40, align 8 layout).
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

Known limitation: interior `value: Sexp' payload Drop is NOT
recursively walked in this PoC.  If the box hits refcount 0 the
outer 40-byte allocation is freed but any nested NlBox handle in
the value slot leaks.  Production Rust `impl Drop' (via
`nlrc_drop_box!') continues to do the full recursive walk; §124.L
sweep stage will lift the recursion into elisp once §124.H-K
sibling kernels SHIP.")

(provide 'nelisp-cc-nlcell-drop)

;;; nelisp-cc-nlcell-drop.el ends here
