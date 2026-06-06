;;; nelisp-cc-nlchartable-drop.el --- Doc 124 §124.L+ NlCharTable Drop elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.L+ — Drop-sweep follow-up that extends the §124.G-K
;; pattern to the remaining 2 NlBox types (NlBoolVector + NlCharTable)
;; so the macro `nlrc_drop_box!' has zero callers and can be physically
;; deleted from `build-tool/src/eval/nlrc.rs'.  Identical shape to
;; §124.J's NlRecord kernel modulo the per-type layout literals and the
;; inner-drop extern name.
;;
;; `NlCharTable' layout (= `build-tool/src/eval/nlchartable.rs:14-18',
;; `#[repr(C)]'):
;;
;;   offset 0:   `inner: CharTableInner'   (= 120 bytes)
;;   offset 120: `refcount: AtomicUsize'   (= 8 bytes — i64 slot)
;;   total = 128 bytes, align = 8
;;
;; `CharTableInner' (= `build-tool/src/eval/sexp.rs:339-357',
;; `#[repr(C)]') = 120 bytes:
;;
;;   subtype     : Sexp                       (= 32 bytes)
;;   default_val : Sexp                       (= 32 bytes)
;;   entries     : Vec<(i64, Sexp)>           (= 24 bytes — Vec header)
;;   parent      : Option<NlCharTableRef>     (=  8 bytes — niche of NonNull)
;;   extra       : Vec<Sexp>                  (= 24 bytes — Vec header)
;;   total = 32 + 32 + 24 + 8 + 24 = 120 bytes, align = 8
;;
;; The `offset_of!(NlCharTable, refcount) == size_of::<CharTableInner>()'
;; compile-time assert at `nlchartable.rs:121' pins the 120-byte trailer
;; offset; `size_of::<AtomicUsize>() == 8' assert at `nlchartable.rs:122'
;; pins the 8-byte refcount slot.  Combined: SIZE_OF_NLCHARTABLE = 120 +
;; 8 = 128 bytes.
;;
;; Function contract (mirrors §124.G-K):
;;   box-ptr: raw `*mut NlCharTable' coerced to i64.  `NlCharTableRef'
;;            is `#[repr(transparent)]' over `NonNull<NlCharTable>'.
;;   returns: i64 = 1 sentinel for `and'-chain composition.  Both arms
;;            yield 1 (= `dealloc-bytes' 1 sentinel on the last-ref
;;            branch, literal 1 on the still-alive branch).
;;
;; The body is a two-arm `if' on the pre-sub refcount value with the
;; §124.L inner-drop step threaded between the fetch-sub and the
;; dealloc-bytes call (matches `nlrc_drop_box!' ordering):
;;
;;   (if (= (atomic-fetch-add (+ box-ptr 120) -1) 1)
;;       (prog2 (extern-call nl_chartable_drop_inner box-ptr)
;;              (dealloc-bytes box-ptr 128 8)) ; last ref — free
;;     1)                                      ; still alive — no-op
;;
;; The interior payload (subtype Sexp + default_val Sexp + entries
;; Vec<(i64,Sexp)> + parent Option<NlCharTableRef> + extra Vec<Sexp>)
;; is recursively walked by the inner-drop extern (= `drop_in_place
;; ::<NlCharTable>') so each nested NlBox handle's refcount is
;; properly decremented before the outer 128-byte allocation is freed.
;; Notably the `parent: Option<NlCharTableRef>' self-reference re-enters
;; this kernel through `Drop for NlCharTableRef' when the option is
;; `Some(_)' (= chained parent decrement).
;;
;; Ordering: §122.E `atomic-fetch-add' uses `Ordering::SeqCst' while
;; Rust's legacy `nlrc_drop_box!' uses `Release' for the fetch_sub +
;; `Acquire' fence before the dealloc.  SeqCst ⊃ Release+Acquire.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nelisp_nlchartable_drop.o' and archives it into the static lib
;; cargo links against the crate.  The Rust side declares the
;; `extern "C"' signature in `build-tool/src/lib.rs::elisp_cc_spike'
;; and exposes a safe wrapper `elisp_cc_spike::nlchartable_drop(box_ptr)'
;; that the `impl Drop for NlCharTableRef' body in
;; `build-tool/src/eval/nlchartable.rs' dispatches through.

;;; Code:

(defconst nelisp-cc-nlchartable-drop--source
  '(seq
    ;; Side-effect sequencer mirroring §124.G-K `nelisp_nl*_drop_prog2':
    ;; evaluate both args left-to-right, return the second arg.  Threads
    ;; the §124.L inner-drop step (= recursive CharTableInner walk's
    ;; return value is discarded, only its side effects matter) between
    ;; the fetch-sub branch's atomic op and the dealloc-bytes call.
    (defun nelisp_nlchartable_drop_prog2 (_eff val) val)

    ;; Public entry — fetch-sub refcount + branch on pre-sub == 1.
    ;;
    ;; Layout (mirrors `nlchartable.rs:14-18' + asserts at lines 118-123):
    ;;   inner    @ 0    (120 bytes — size_of::<CharTableInner>)
    ;;   refcount @ 120  (8 bytes   — AtomicUsize)
    ;;   total = 128 bytes, align = 8
    ;; Doc 124 §124.L+: thread `nl_chartable_drop_inner' (= `drop_in_place
    ;; ::<NlCharTable>') between the fetch-sub and the dealloc-bytes
    ;; call so the inner CharTableInner (subtype / default_val / entries /
    ;; parent / extra — including any nested NlBox handles and the
    ;; `parent: Option<NlCharTableRef>' self-reference chain) is
    ;; recursively dropped before the outer 128-byte allocation is freed.
    ;; Matches the legacy `nlrc_drop_box!' ordering.
    (defun nelisp_nlchartable_drop (box-ptr)
      (if (= (atomic-fetch-add (+ box-ptr 120) -1) 1)
          ;; Last ref — pre-sub was 1, new count is 0.  Drop interior
          ;; CharTableInner then free the 128-byte outer allocation.
          (nelisp_nlchartable_drop_prog2
           (extern-call nl_chartable_drop_inner box-ptr)
           (dealloc-bytes box-ptr 128 8))
        ;; Still alive — pre-sub was > 1.  Return 1 sentinel to match
        ;; the dealloc-bytes arm's return convention so the caller
        ;; sees a uniform `i64 = 1' on both branches.
        1)))
  "AOT source for the Doc 124 §124.L+ NlCharTable Drop kernel.

Two-entry `(seq DEFUN ...)' manifest mirroring §124.G-K:
- `nelisp_nlchartable_drop_prog2 (_eff val) -> val' — side-effect
  sequencer threading the §124.L inner-drop step (=
  CharTableInner walk) between the fetch-sub and the
  dealloc-bytes call.
- `nelisp_nlchartable_drop (box-ptr) -> i64' — public entry.

AOT's SysV AMD64 prologue spills the first arg (`box-ptr' =
raw `*mut NlCharTable' as i64) into the rbp-relative slot 0.
The public body:

  1. Computes `(+ box-ptr 120)' = address of the AtomicUsize
     refcount trailer per `nlchartable.rs:118-123' compile-time
     asserts (= `offset_of!(NlCharTable, refcount) ==
     size_of::<CharTableInner>() = 32 + 32 + 24 + 8 + 24 = 120').
  2. Calls `atomic-fetch-add' (= §122.E grammar op, lowers to
     `nl_atomic_fetch_add@PLT' = `AtomicI64::fetch_add(-1, SeqCst)')
     with delta = -1 (= fetch-sub semantics).  Pre-sub i64 lands
     in rax.
  3. Compares rax == 1 via the `=' grammar op.
  4. Branches: if pre-sub was 1, the box reached refcount 0 and
     we (a) call `nl_chartable_drop_inner' (= `drop_in_place
     ::<NlCharTable>' wrapper exposed as a name-stable `extern \"C\"'
     symbol so the elisp emitter can issue a PLT call) to walk
     CharTableInner — recursively decrement nested NlBox handles
     plus the `parent: Option<NlCharTableRef>' self-reference
     chain — then (b) call `dealloc-bytes' (= §125.A, lowers to
     `nl_dealloc_bytes(box_ptr, 128, 8)' = `std::alloc::dealloc'
     with the `Layout::new::<NlCharTable>()' = size 128, align 8
     layout).  Otherwise return 1 sentinel directly (= no-op).

Both branches yield rax = 1 sentinel (= `dealloc-bytes' returns 1
on success per §125.A contract; the else-arm produces the literal
1).  The `if' grammar op merges control flow at the joined return.

Code-size profile: ~70-80 bytes (= `mov + add + call' for the
atomic op + `cmp + jne' for the branch + `mov + call' for the
inner-drop + `mov + call' for the dealloc + `mov eax, 1; ret' at
the joined epilogue).  Three PLT fixups: `nl_atomic_fetch_add',
`nl_chartable_drop_inner', and `nl_dealloc_bytes'.")

(provide 'nelisp-cc-nlchartable-drop)

;;; nelisp-cc-nlchartable-drop.el ends here
