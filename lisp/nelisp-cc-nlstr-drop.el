;;; nelisp-cc-nlstr-drop.el --- Doc 124 §124.K NlStr Drop elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.K — mechanical port of §124.G/H's NlConsBox/NlVector
;; Drop kernel to NlStr.  Identical shape modulo the per-type layout
;; literals:
;;
;;   §124.G NlConsBox: REFCOUNT_OFFSET = 64, SIZE = 72, ALIGN = 8
;;   §124.H NlVector:  REFCOUNT_OFFSET = 24, SIZE = 32, ALIGN = 8
;;   §124.K NlStr:     REFCOUNT_OFFSET = 24, SIZE = 32, ALIGN = 8
;;
;; `NlStr' layout (= `build-tool/src/eval/nlstr.rs:58-68',
;; `#[repr(C)]'):
;;
;;   offset 0:  `value: String'           (= 24 bytes — ptr + len + cap triple)
;;   offset 24: `refcount: AtomicUsize'   (= 8 bytes — i64 slot)
;;   total = 32 bytes, align = 8
;;
;; Note: NlStr shares the same SIZE/REFCOUNT_OFFSET pair as NlVector
;; (= 32 / 24) because `String' and `Vec<Sexp>' share the same 24-byte
;; ptr/len/cap header on 64-bit hosts.  The kernels are interchangeable
;; at the byte level; only the per-type symbol name differs.
;;
;; The `offset_of!(NlStr, refcount) == size_of::<String>()' compile-
;; time assert at `nlstr.rs:783' pins the 24-byte trailer offset;
;; `size_of::<AtomicUsize>() == 8' assert at `nlstr.rs:785' pins the
;; 8-byte refcount slot.  Combined: SIZE_OF_NLSTR = 24 + 8 = 32.
;;
;; Function contract (mirrors §124.G/H):
;;   box-ptr: raw `*mut NlStr' coerced to i64 (= same shape as
;;            §124.E's clone kernel; the Rust shim `NlStrRef' is
;;            `#[repr(transparent)]' over `NonNull<NlStr>' per
;;            `nlstr.rs:78-85').
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
;; reasoning as §124.G/H).
;;
;; Interior payload Drop limitation (= PoC scope, mirrors §124.G/H):
;;
;;   The Rust `nlrc_drop_box!' macro runs `NLRC_DROP_TABLE[tag](raw)'
;;   before the dealloc, which dispatches to
;;   `nlrc_payload_drop::<NlStr>' = `std::ptr::drop_in_place' on
;;   the `*mut NlStr'.  For NlStr this recursively:
;;     (a) Drops `value: String' — frees the heap-allocated UTF-8
;;         backing buffer (= the `ptr' field of the String header,
;;         pointing at the char data).  No nested NlBox handles to
;;         walk (= String is purely a byte buffer, no recursive Sexp).
;;     (b) The `refcount: AtomicUsize' has trivial Drop (= no-op).
;;
;;   §124.K's PoC body skips the interior-drop step.  If the box's
;;   refcount hits 0:
;;     - The 32-byte outer `NlStr' allocation is freed.
;;     - The `String'-owned UTF-8 buffer (= heap-allocated ptr in the
;;       24-byte String header) is LEAKED (= one bytes allocation,
;;       no nested NlBox handles to follow).
;;
;;   This is acceptable because:
;;
;;     (1) The Rust `impl Drop' body is UNCHANGED in §124.K (= same
;;         §124.E scope split as Clone).  Production callers continue
;;         to drive Drop through `nlrc_drop_box!' which does the full
;;         recursive walk via `drop_in_place'.
;;     (2) The probe in `tests/elisp_cc_nlstr_drop_probe.rs' uses
;;         a fresh `alloc_bytes(32, 8)' block seeded only at the
;;         refcount-trailer offset (= the value bytes are uninitialized
;;         garbage; the kernel never reads them, so the leak is
;;         invisible to the probe's invariants).
;;     (3) §124.L's sweep stage will replace the placeholder `if'
;;         branch with the full tag-dispatch table once §124.H-K and
;;         the per-variant elisp Drop kernels are SHIPPED.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nelisp_nlstr_drop.o' and archives it into the static lib cargo
;; links against the crate.  The Rust side declares the `extern "C"'
;; signature in `build-tool/src/lib.rs::elisp_cc_spike' and exposes a
;; safe wrapper `elisp_cc_spike::nlstr_drop(box_ptr)' for the
;; integration test in
;; `build-tool/tests/elisp_cc_nlstr_drop_probe.rs'.
;;
;; Stage scope: §124.K ships *only* the elisp kernel + probe + Rust
;; extern + safe wrapper.  The Rust-side `impl Drop for NlStrRef'
;; body (= `crate::nlrc_drop_box!(self.ptr.as_ptr(), NlStr,
;; crate::eval::sexp::SEXP_TAG_MUT_STR)' in `nlstr.rs:189-191')
;; continues to drive production Drop dispatch; §124.L's sweep stage
;; will swap the 5 sibling `impl Drop' bodies in one commit after
;; §124.H-K sibling PoCs all green (= mirrors the §124.F clone-sweep
;; gate pattern).

;;; Code:

(defconst nelisp-cc-nlstr-drop--source
  '(seq
    ;; Side-effect sequencer mirroring §124.G/H `nelisp_nl*_drop_prog2':
    ;; evaluate both args left-to-right, return the second arg.  Reserved
    ;; for the §124.L sweep stage when the interior-payload drop step is
    ;; threaded between the fetch-sub and the dealloc-bytes call (= the
    ;; recursive String walk's return value is discarded, only its side
    ;; effects matter — frees the UTF-8 backing buffer).  Kept here for
    ;; ABI/source parity with §124.G/H's two-entry manifest shape and to
    ;; flag the future expansion point.
    (defun nelisp_nlstr_drop_prog2 (_eff val) val)

    ;; Public entry — fetch-sub refcount + branch on pre-sub == 1.
    ;;
    ;; Layout (mirrors `nlstr.rs:58-68' + `nlstr.rs:779-786' asserts):
    ;;   value    @ 0   (24 bytes — size_of::<String>)
    ;;   refcount @ 24  (8 bytes  — AtomicUsize)
    ;;   total = 32 bytes, align = 8
    ;; Doc 124 §124.L: thread `nl_str_drop_inner' (= `drop_in_place
     ;; ::<NlStr>') between the fetch-sub and the dealloc-bytes call
     ;; so the inner `value: String' UTF-8 heap buffer is freed before
     ;; the outer 32-byte allocation is freed.  Matches `nlrc_drop_box!'
     ;; ordering.
    (defun nelisp_nlstr_drop (box-ptr)
      (if (= (atomic-fetch-add (+ box-ptr 24) -1) 1)
          ;; Last ref — pre-sub was 1, new count is 0.  Drop interior
          ;; String UTF-8 buffer then free the 32-byte allocation.
          (nelisp_nlstr_drop_prog2
           (extern-call nl_str_drop_inner box-ptr)
           (dealloc-bytes box-ptr 32 8))
        ;; Still alive — pre-sub was > 1.  Return 1 sentinel to match
        ;; the dealloc-bytes arm's return convention so the caller
        ;; sees a uniform `i64 = 1' on both branches.
        1)))
  "AOT source for the Doc 124 §124.K NlStr Drop kernel.

Two-entry `(seq DEFUN ...)' manifest mirroring §124.G/H:
- `nelisp_nlstr_drop_prog2 (_eff val) -> val' — side-effect
  sequencer, reserved for §124.L sweep stage's recursive payload
  drop integration (= String UTF-8 buffer drop_in_place).
- `nelisp_nlstr_drop (box-ptr) -> i64' — public entry.

AOT's SysV AMD64 prologue spills the first arg (`box-ptr' =
raw `*mut NlStr' as i64) into the rbp-relative slot 0.  The
public body:

  1. Computes `(+ box-ptr 24)' = address of the AtomicUsize
     refcount trailer per `nlstr.rs:779-786' compile-time asserts
     (= `offset_of!(NlStr, refcount) == size_of::<String>() = 24').
  2. Calls `atomic-fetch-add' (= §122.E grammar op, lowers to
     `nl_atomic_fetch_add@PLT' = `AtomicI64::fetch_add(-1, SeqCst)')
     with delta = -1 (= fetch-sub semantics, same as §123.B
     `nelisp_rc_dec').  Pre-sub i64 lands in rax.
  3. Compares rax == 1 via the `=' grammar op (= `cmp rax, 1; sete'
     into a 0/1 i64).
  4. Branches: if pre-sub was 1, the box reached refcount 0 and
     we call `dealloc-bytes' (= §125.A, lowers to
     `nl_dealloc_bytes(box_ptr, 32, 8)' = `std::alloc::dealloc'
     with the `Layout::new::<NlStr>()' = size 32, align 8 layout).
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

Known limitation: interior String UTF-8 buffer Drop is NOT
recursively walked in this PoC.  If the box hits refcount 0 the
outer 32-byte allocation is freed but the String's heap-allocated
char buffer (= ptr in the 24-byte String header) leaks.  No
nested NlBox handles to follow (= String is a pure byte buffer).
Production Rust `impl Drop' (via `nlrc_drop_box!') continues to do
the full recursive walk; §124.L sweep stage will lift the walk
into elisp once §124.H-K sibling kernels SHIP.")

(provide 'nelisp-cc-nlstr-drop)

;;; nelisp-cc-nlstr-drop.el ends here
