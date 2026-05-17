;;; nelisp-cc-wrap-alist-cells.el --- Doc 111 §111.E #26 wrap_alist_cells  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #26 — `wrap_alist_cells'.  Doc 115
;; §115.4 — full pure-elisp implementation of the closure-env alist
;; preprocessor.  Replaces the prior ~50 LOC Rust shim
;; `nl_wrap_alist_cells' (= deleted from
;; `build-tool/src/eval/env_lexframe_phase47_shims.rs').
;;
;; Algorithm (= literal transcription of the Rust impl):
;;
;;   For each `(NAME . VALUE)' entry in the input alist, build a new
;;   `(NAME . CELL)' entry where CELL is:
;;     - VALUE itself (refcount-aware-cloned) when VALUE.tag = Cell;
;;     - a fresh `NlCell' wrapping VALUE otherwise (`cell-make').
;;   Then chain the new outer cons cells into a proper-list alist
;;   matching the input's structure.
;;
;; List rebuild via `cons-set-cdr' splicing: the head outer cons is
;; built directly into `result-slot' with a Nil cdr; each subsequent
;; outer cons is built into a single `work-slot' and `cons-set-cdr'd
;; into the chain.  The next iteration's `prev-handle' is computed as
;; `(+ (sexp-payload-ptr prev-handle) 32)' — the address of the
;; just-set `cdr' field inside the previous outer cons's box, which
;; now holds the newly-spliced Sexp::Cons handle of the most recent
;; outer cons.  This single-slot scheme gives bounded scratch
;; independent of input list length.
;;
;; Refcount discipline (= MVP cons-make is raw 32-byte copy without
;; refcount bump):
;;   - For the bare-value path, `cell-make' allocates a fresh NlCell
;;     with refcount 1; the result chain references this NlCell
;;     through `box-inner.cdr', matched 1:1.
;;   - For the already-Cell path, `extern-call nl_sexp_clone_into'
;;     refcount-bumps the input Cell into `cell-slot' before the
;;     subsequent `cons-make' raw-copies it into the inner-pair box.
;;     The +1 refcount on the original Cell matches the result
;;     chain's reference.
;;   - The NAME (typically Sexp::Symbol, an unrefcounted owned
;;     `String') is `nl_sexp_clone_into'd into `name-slot' to allocate
;;     a fresh `String' before `cons-make' raw-copies it into the
;;     inner-pair box — avoiding the double-free that would result
;;     from two owners of the same heap allocation.
;;   - The lib.rs safe wrapper `core::ptr::write's `Sexp::Nil' into
;;     each work slot before returning to prevent the
;;     wrapper-locals' Drop from decrementing refcounts on heap
;;     nodes that the result chain already accounts for.
;;
;; Recursion is used for the tail walker because Phase 47 has no
;; mutable locals — every iteration state passes through arg
;; registers (recursive self-call with 6 args).
;;
;; Signature change: the function now takes 4 extra scratch-slot
;; parameters (`work-slot' / `name-slot' / `cell-slot' / `inner-slot')
;; for the per-iteration cell-wrap + name-clone + outer-cons build.
;; The safe Rust wrapper in `build-tool/src/lib.rs' allocates these
;; on the call stack and passes their pointers, so the public 2-arg
;; API (= `wrap_alist_cells(alist_ptr, result_slot)') is preserved.
;;
;; ABI deps satisfied:
;;   §100.A  `extern-call'      — `nl_sexp_clone_into' (refcount-aware copy).
;;   §101.B  `sexp-payload-ptr' — outer/inner NlConsBox* extraction.
;;   §101.D  `cons-make'        — fresh `Sexp::Cons' allocation.
;;   §101.D  `cons-set-cdr'     — chain splice via refcount-aware setter.
;;   §111.D  `cell-make'        — fresh `NlCell' allocation.
;;   §100.B  `sexp-tag'         — Nil / Cons / Cell tag check.
;;   §100.B  `sexp-write-nil'   — Nil-fill on init / failure path.

;;; Code:

(defconst nelisp-cc-wrap-alist-cells--source
  '(seq
    ;; Build one alist entry's new outer cons into WRITE-SLOT.
    ;;
    ;; OUTER-BOX-PTR is the input outer cons's `NlConsBox*' (= the
    ;; result of `sexp-payload-ptr' on the input outer's `Sexp::Cons').
    ;; As a `*const Sexp' it points at the inner-pair Sexp at offset 0
    ;; (= `NlConsBox.car').  `sexp-payload-ptr OUTER-BOX-PTR' reads
    ;; that Sexp's tag and returns the inner `NlConsBox*' when tag is
    ;; Cons, or 0 for any non-Cons tag (= malformed inner-pair).
    ;;
    ;; On success: WRITE-SLOT = `Sexp::Cons((NAME . CELL) . Nil)' and
    ;; we return a non-zero pointer (= the cons-make's slot result).
    ;; On malformed (inner not Cons): we return 0 without touching
    ;; WRITE-SLOT (= caller is responsible for the Nil-fill).
    ;;
    ;; The 6th `_pad' parameter is a Phase 47 alignment placeholder:
    ;; this defun's body invokes `cons-make' (= 4 pushes alive at the
    ;; nl_alloc_consbox call site).  Odd-arity defuns land at
    ;; body-entry rsp ≡ 8 mod 16, which would make the call site
    ;; rsp ≡ 8 mod 16 (= SysV AMD64 ABI violation).  Padding to even
    ;; arity (6) lands body-entry at ≡ 0 mod 16 so cons-make's call
    ;; site reaches ≡ 0 mod 16.  The pad value is ignored by the
    ;; function body.
    (defun nelisp_wrap_alist_cells_build_one
        (outer-box-ptr write-slot name-slot cell-slot inner-slot _pad)
      (if (= (sexp-payload-ptr outer-box-ptr) 0)
          0
        (and
         ;; Step 1: Refcount-aware clone of NAME into name-slot.
         ;; Avoids the double-free that would result from raw-copying
         ;; an unrefcounted `Sexp::Symbol(String)' into the new
         ;; inner-pair box's car (= `Sexp::clone' for Symbol
         ;; allocates a fresh String).
         ;;
         ;; inner-box = sexp-payload-ptr outer-box-ptr.
         ;; name-ptr  = inner-box       (offset 0  = car of inner pair).
         ;; value-ptr = inner-box + 32  (offset 32 = cdr of inner pair).
         (or (extern-call nl_sexp_clone_into
                          (sexp-payload-ptr outer-box-ptr)
                          name-slot)
             1)
         ;; Step 2: Wrap value into cell-slot (= write the Sexp::Cell
         ;; into cell-slot before the `cons-make' below reads from it).
         ;;
         ;; The wrap branches INLINE on the value's tag instead of
         ;; calling a `cell-ptr' helper, because Phase 47's
         ;; `cons-make' emit pushes its first arg before evaluating
         ;; the second.  A nested function CALL inside cons-make's
         ;; cdr-ptr argument would land at an rsp 8 bytes off the SysV
         ;; AMD64 16-byte boundary (= ABI violation, SIGSEGV in
         ;; release builds).  Materialising cell-slot's value here
         ;; first, then passing the spilled `cell-slot' param-ref to
         ;; cons-make, keeps cons-make's arg-eval CALL-free.
         (if (= (sexp-tag (+ (sexp-payload-ptr outer-box-ptr) 32)) 11)
             ;; Already Sexp::Cell — refcount-bump-clone into cell-slot.
             (or (extern-call nl_sexp_clone_into
                              (+ (sexp-payload-ptr outer-box-ptr) 32)
                              cell-slot)
                 1)
           ;; Bare value — `cell-make' allocates a fresh NlCell wrapping
           ;; the value (= refcount-aware: `nl_alloc_cell' clones).
           (cell-make (+ (sexp-payload-ptr outer-box-ptr) 32)
                      cell-slot))
         ;; Step 3: cons-make (name-slot, cell-slot) into inner-slot.
         ;; Both arg-ptrs are spilled-param refs — no nested CALL
         ;; inside cons-make's evaluation.
         (cons-make name-slot cell-slot inner-slot)
         ;; Step 4: cons-make (inner-slot, Nil) into write-slot.
         (sexp-write-nil write-slot)
         (cons-make inner-slot write-slot write-slot))))

    ;; Tail walker (= K >= 1, head already in result-slot).
    ;;
    ;; CUR-PTR: `*const Sexp' at position K of the input alist (= the
    ;;          cdr of the K-1th outer cons's box).
    ;; PREV-HANDLE: `*const Sexp' pointing at the most-recently-built
    ;;          outer cons's `Sexp::Cons' (= the cons-set-cdr target).
    ;;          For iter 2, this is `result-slot'; for iter K >= 3,
    ;;          it is `(+ (sexp-payload-ptr prev-prev-handle) 32)' =
    ;;          the address of the previous box's `cdr' field, which
    ;;          now holds the spliced Sexp::Cons handle to the most
    ;;          recently built outer cons (refcount-bumped by
    ;;          `nl_consbox_set_cdr's `Sexp::clone' call).
    ;; WORK-SLOT: single ping-pong scratch for the new outer cons.
    ;; NAME-SLOT / CELL-SLOT / INNER-SLOT: per-iteration scratch.
    ;;
    ;; Returns 1 on success (= proper-list Nil terminator hit), 0 on
    ;; malformed input.  On the 0 path the partially-built chain is
    ;; preserved in the caller's slots; the entry function overwrites
    ;; `result-slot' to Nil on the way out.
    ;;
    ;; Implementation note (= Phase 47 outer-cmp-push limitation):
    ;; the pattern `(if (= (FN ...) N) ...)' is dangerous because
    ;; `--emit-cmp' pushes the cmp's B operand BEFORE evaluating A,
    ;; misaligning rsp by 8 bytes at any inner `extern-call' / `call'
    ;; that relies on the body-entry alignment invariant.  We
    ;; therefore branch directly on the function-call's i64 result
    ;; via `(if (FN ...) THEN ELSE)' — rax non-zero -> THEN, rax
    ;; zero -> ELSE — which keeps the call site at the unmodified
    ;; body-entry rsp (= the call's own `--current-defun-arity'-based
    ;; needs-align logic then aligns correctly).
    (defun nelisp_wrap_alist_cells_tail
        (cur-ptr prev-handle work-slot name-slot cell-slot inner-slot)
      (if (= (sexp-tag cur-ptr) 0)
          1
        (if (= (sexp-tag cur-ptr) 7)
            (if (nelisp_wrap_alist_cells_build_one
                 (sexp-payload-ptr cur-ptr)
                 work-slot name-slot cell-slot inner-slot 0)
                (and
                 (cons-set-cdr prev-handle work-slot)
                 (nelisp_wrap_alist_cells_tail
                  (+ (sexp-payload-ptr cur-ptr) 32)
                  (+ (sexp-payload-ptr prev-handle) 32)
                  work-slot
                  name-slot
                  cell-slot
                  inner-slot))
              0)
          0)))

    ;; Entry point.
    ;;
    ;; ALIST-PTR:   `*const Sexp' on the input alist.
    ;; RESULT-SLOT: `*mut Sexp' for the rebuilt alist.
    ;; WORK-SLOT / NAME-SLOT / CELL-SLOT / INNER-SLOT: scratch slots.
    ;;
    ;; Writes `Sexp::Nil' into RESULT-SLOT on any failure path (=
    ;; matches the Rust impl's `result_slot=Nil + return 0' on
    ;; malformed input).  Returns 1 on success, 0 on malformed.
    ;;
    ;; Uses `(if (FN ...) THEN ELSE)' instead of `(if (= (FN ...) 0)
    ;; ...)' — see tail walker's implementation note for the rsp-
    ;; alignment rationale.
    (defun nelisp_wrap_alist_cells
        (alist-ptr result-slot work-slot name-slot cell-slot inner-slot)
      (if (= (sexp-tag alist-ptr) 0)
          (and (sexp-write-nil result-slot) 1)
        (if (= (sexp-tag alist-ptr) 7)
            (if (nelisp_wrap_alist_cells_build_one
                 (sexp-payload-ptr alist-ptr)
                 result-slot name-slot cell-slot inner-slot 0)
                (if (nelisp_wrap_alist_cells_tail
                     (+ (sexp-payload-ptr alist-ptr) 32)
                     result-slot
                     work-slot
                     name-slot
                     cell-slot
                     inner-slot)
                    1
                  (and (sexp-write-nil result-slot) 0))
              (and (sexp-write-nil result-slot) 0))
          (and (sexp-write-nil result-slot) 0))))
    )
  "Phase 47 source for Doc 111 §111.E #26 / Doc 115 §115.4
`wrap_alist_cells'.

Pure-elisp closure-env alist preprocessor: walks `((NAME . VALUE) ...)'
and produces `((NAME . CELL) ...)' where each CELL is either the
input VALUE (when already `Sexp::Cell', refcount-bumped via
`nl_sexp_clone_into') or a fresh `NlCell' wrapping VALUE (via
`cell-make').

Composes `cons-make' (§101.D), `cons-set-cdr' (§101.D), `cell-make'
(§111.D), `sexp-payload-ptr' (§101.B), `sexp-tag' / `sexp-write-nil'
(§100.B), and `extern-call nl_sexp_clone_into' (§100.A).  Single-slot
ping-pong via `cons-set-cdr' + tail recursion give bounded scratch
independent of input list length.

Replaces the ~50 LOC Rust shim `nl_wrap_alist_cells' which has been
removed from `env_lexframe_phase47_shims.rs'.")

(provide 'nelisp-cc-wrap-alist-cells)

;;; nelisp-cc-wrap-alist-cells.el ends here
