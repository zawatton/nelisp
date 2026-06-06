;;; nelisp-cc-nlboolvector-drop.el --- Doc 124 §124.L+ NlBoolVector Drop elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.L+ — Drop-sweep follow-up that extends the §124.G-K
;; pattern to the remaining 2 NlBox types (NlBoolVector + NlCharTable)
;; so the macro `nlrc_drop_box!' has zero callers and can be physically
;; deleted from `build-tool/src/eval/nlrc.rs'.  Identical shape to
;; §124.H's NlVector kernel modulo the inner-drop extern name; the
;; per-type layout literals are identical (REFCOUNT_OFFSET = 24, SIZE =
;; 32, ALIGN = 8) because `Vec<bool>' and `Vec<Sexp>' share the same
;; 24-byte Vec header (= ptr + len + cap triple) — Rust's `Vec<T>'
;; header size is independent of `T'.
;;
;; `NlBoolVector' layout (= `build-tool/src/eval/nlboolvector.rs:11-15',
;; `#[repr(C)]'):
;;
;;   offset 0:  `value: Vec<bool>'       (= 24 bytes — ptr + len + cap triple)
;;   offset 24: `refcount: AtomicUsize'  (= 8 bytes — i64 slot)
;;   total = 32 bytes, align = 8
;;
;; The `offset_of!(NlBoolVector, refcount) == size_of::<Vec<bool>>()'
;; compile-time assert at `nlboolvector.rs:131' pins the 24-byte
;; trailer; `size_of::<AtomicUsize>() == 8' assert at
;; `nlboolvector.rs:132' pins the 8-byte refcount slot.  Combined:
;; SIZE_OF_NLBOOLVECTOR = 24 + 8 = 32.
;;
;; Function contract (mirrors §124.H):
;;   box-ptr: raw `*mut NlBoolVector' coerced to i64.  `NlBoolVectorRef'
;;            is `#[repr(transparent)]' over `NonNull<NlBoolVector>'
;;            per `nlboolvector.rs:17-21'.
;;   returns: i64 = 1 sentinel for `and'-chain composition.  Both arms
;;            yield 1 (= `dealloc-bytes' 1 sentinel on the last-ref
;;            branch, literal 1 on the still-alive branch).
;;
;; The body is a two-arm `if' on the pre-sub refcount value with the
;; §124.L inner-drop step threaded between the fetch-sub and the
;; dealloc-bytes call (matches `nlrc_drop_box!' ordering):
;;
;;   (if (= (atomic-fetch-add (+ box-ptr 24) -1) 1)
;;       (prog2 (extern-call nl_boolvector_drop_inner box-ptr)
;;              (dealloc-bytes box-ptr 32 8)) ; last ref — free
;;     1)                                     ; still alive — no-op
;;
;; Ordering: §122.E `atomic-fetch-add' uses `Ordering::SeqCst' while
;; Rust's legacy `nlrc_drop_box!' uses `Release' for the fetch_sub +
;; `Acquire' fence before the dealloc.  SeqCst ⊃ Release+Acquire.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nelisp_nlboolvector_drop.o' and archives it into the static lib
;; cargo links against the crate.  The Rust side declares the
;; `extern "C"' signature in `build-tool/src/lib.rs::elisp_cc_spike'
;; and exposes a safe wrapper `elisp_cc_spike::nlboolvector_drop(box_ptr)'
;; that the `impl Drop for NlBoolVectorRef' body in
;; `build-tool/src/eval/nlboolvector.rs' dispatches through.

;;; Code:

(defconst nelisp-cc-nlboolvector-drop--source
  '(seq
    ;; Side-effect sequencer mirroring §124.G-K `nelisp_nl*_drop_prog2':
    ;; evaluate both args left-to-right, return the second arg.  Threads
    ;; the §124.L inner-drop step (= recursive `Vec<bool>' tail walk's
    ;; return value is discarded, only its side effects matter) between
    ;; the fetch-sub branch's atomic op and the dealloc-bytes call.
    (defun nelisp_nlboolvector_drop_prog2 (_eff val) val)

    ;; Public entry — fetch-sub refcount + branch on pre-sub == 1.
    ;;
    ;; Layout (mirrors `nlboolvector.rs:11-15' + asserts at lines 129-132):
    ;;   value    @ 0   (24 bytes — size_of::<Vec<bool>>)
    ;;   refcount @ 24  (8 bytes  — AtomicUsize)
    ;;   total = 32 bytes, align = 8
    ;; Doc 124 §124.L+: thread `nl_boolvector_drop_inner' (= `drop_in_place
    ;; ::<NlBoolVector>') between the fetch-sub and the dealloc-bytes
    ;; call so the inner `Vec<bool>' tail is recursively dropped before
    ;; the outer 32-byte allocation is freed.  Matches the legacy
    ;; `nlrc_drop_box!' ordering.
    (defun nelisp_nlboolvector_drop (box-ptr)
      (if (= (atomic-fetch-add (+ box-ptr 24) -1) 1)
          ;; Last ref — pre-sub was 1, new count is 0.  Drop interior
          ;; Vec<bool> tail then free the 32-byte outer allocation.
          (nelisp_nlboolvector_drop_prog2
           (extern-call nl_boolvector_drop_inner box-ptr)
           (dealloc-bytes box-ptr 32 8))
        ;; Still alive — pre-sub was > 1.  Return 1 sentinel to match
        ;; the dealloc-bytes arm's return convention so the caller
        ;; sees a uniform `i64 = 1' on both branches.
        1)))
  "AOT source for the Doc 124 §124.L+ NlBoolVector Drop kernel.

Two-entry `(seq DEFUN ...)' manifest mirroring §124.G-K:
- `nelisp_nlboolvector_drop_prog2 (_eff val) -> val' — side-effect
  sequencer threading the §124.L inner-drop step (= Vec<bool>
  tail walk) between the fetch-sub and the dealloc-bytes call.
- `nelisp_nlboolvector_drop (box-ptr) -> i64' — public entry.

AOT's SysV AMD64 prologue spills the first arg (`box-ptr' =
raw `*mut NlBoolVector' as i64) into the rbp-relative slot 0.
The public body:

  1. Computes `(+ box-ptr 24)' = address of the AtomicUsize
     refcount trailer per `nlboolvector.rs:129-132' compile-time
     asserts (= `offset_of!(NlBoolVector, refcount) ==
     size_of::<Vec<bool>>() = 24'; `Vec<T>' header size is
     independent of `T').
  2. Calls `atomic-fetch-add' (= §122.E grammar op, lowers to
     `nl_atomic_fetch_add@PLT' = `AtomicI64::fetch_add(-1, SeqCst)')
     with delta = -1 (= fetch-sub semantics).  Pre-sub i64 lands
     in rax.
  3. Compares rax == 1 via the `=' grammar op.
  4. Branches: if pre-sub was 1, the box reached refcount 0 and
     we (a) call `nl_boolvector_drop_inner' (= `drop_in_place
     ::<NlBoolVector>' wrapper exposed as a name-stable `extern \"C\"'
     symbol so the elisp emitter can issue a PLT call) to drop
     the interior `Vec<bool>' tail, then (b) call `dealloc-bytes'
     (= §125.A, lowers to `nl_dealloc_bytes(box_ptr, 32, 8)' =
     `std::alloc::dealloc' with the `Layout::new::<NlBoolVector>()'
     = size 32, align 8 layout).  Otherwise return 1 sentinel
     directly (= no-op, the box has more handles alive).

Both branches yield rax = 1 sentinel (= `dealloc-bytes' returns 1
on success per §125.A contract; the else-arm produces the literal
1).  The `if' grammar op merges control flow at the joined return.

Code-size profile: ~70-80 bytes (= `mov + add + call' for the
atomic op + `cmp + jne' for the branch + `mov + call' for the
inner-drop + `mov + call' for the dealloc + `mov eax, 1; ret' at
the joined epilogue).  Three PLT fixups: `nl_atomic_fetch_add',
`nl_boolvector_drop_inner', and `nl_dealloc_bytes'.")

(provide 'nelisp-cc-nlboolvector-drop)

;;; nelisp-cc-nlboolvector-drop.el ends here
