;;; nelisp-cc-gc-walk-children.el --- Doc 123 §123.D walk-children kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 123 §123.D — pure-elisp `nelisp_gc_walk_children' kernel, the
;; cycle-collector edge enumerator of `bi_nl_gc_walk_children' at
;; `build-tool/src/eval/rc_primitives.rs:247-262'.  Replaces the
;; `Sexp::Cons(r) => Ok(Sexp::list_from(&[r.car.clone(), r.cdr.clone()]))'
;; arm with two §101.D `cons-make' allocations driven by §122.E
;; `ptr-read-u64' for the box-pointer extraction.
;;
;; Function contract:
;;   sexp-ptr:    raw `*const Sexp' coerced to i64 (= the outer Sexp
;;                value's address).  Caller has already verified the
;;                tag is `SEXP_TAG_CONS = 7' via §123.C `rc_kind'
;;                before invoking this kernel; non-Cons inputs land
;;                on the Rust shim's tag-dispatch fallback in §123.F.
;;   result-slot: `*mut Sexp' 32-byte caller-owned slot — will hold
;;                the head of the 2-list `Sexp::Cons(car . tail)'.
;;   tail-slot:   `*mut Sexp' 32-byte caller-owned scratch — used to
;;                materialize the inner `Sexp::Cons(cdr . nil)'.
;;
;; The body composes three ops via `and':
;;
;;   1. (sexp-write-nil tail-slot)
;;        Seed the cdr-of-cdr position with `Sexp::Nil' before the
;;        inner `cons-make' reads it.  Returns the slot pointer
;;        (non-zero) so `and' short-circuit propagates.
;;
;;   2. (cons-make (+ (ptr-read-u64 sexp-ptr 8) 32) tail-slot tail-slot)
;;        Allocate a fresh `NlConsBox' whose car copies the input
;;        Sexp's cdr field (= located at offset 32 of the input
;;        `NlConsBox', read via `sexp-ptr -> ptr-read-u64 +8' to get
;;        the box base, then `+ 32' for the cdr slot address) and
;;        whose cdr copies `*tail-slot' (= Sexp::Nil from step 1).
;;        Writes `Sexp::Cons(new_box)' into `tail-slot' in place.
;;        Returns the slot pointer in rax.
;;
;;   3. (cons-make (ptr-read-u64 sexp-ptr 8) tail-slot result-slot)
;;        Allocate a second fresh `NlConsBox' whose car copies the
;;        input Sexp's car field (= located at offset 0 of the
;;        input `NlConsBox' = the box base itself) and whose cdr
;;        copies `*tail-slot' (= the `Sexp::Cons' from step 2,
;;        which represents `(cdr . nil)').  Writes
;;        `Sexp::Cons(new_box)' into `result-slot'.  Returns
;;        `result-slot' in rax, which becomes the function's return.
;;
;; Final state: `*result-slot' = `Sexp::Cons((car . (cdr . nil)))'
;; — the proper 2-element list `(car cdr)' matching the Rust body's
;; `Sexp::list_from(&[car, cdr])' output bit-for-bit.
;;
;; Cons-make refcount semantics (= MVP byte-copy):
;;   `cons-make' raw-copies 32-byte Sexp payloads without nested
;;   refcount bumps (per `lisp/nelisp-aot-compiler.el:3579-3635'
;;   docstring).  In the cycle collector's usage pattern, the input
;;   Sexp::Cons is held alive by the calling frame for the duration
;;   of this call, so the raw copies of its car/cdr into the new
;;   boxes are observationally identical to the Rust body's
;;   `r.car.clone()' (= for Int/Float/Nil/T atoms there is no
;;   refcount to bump; for boxed kids the refcount is already
;;   stable across this single-frame composition).  Multi-frame
;;   refcount discipline lands in §123.F's sweep stage when the
;;   Rust shim swaps to call this kernel.
;;
;; Variant coverage scope:
;;   - SEXP_TAG_CONS = 7   — supported (= this kernel's job).
;;   - SEXP_TAG_VECTOR = 8 — DEFERRED to a future stage; the Rust
;;                          arm currently returns Nil so its
;;                          dispatch shim does not yet need an
;;                          elisp kernel.  Doc 79 §5.3.5.b is the
;;                          eventual landing zone.
;;   - SEXP_TAG_RECORD = 12, SEXP_TAG_CELL = 11, SEXP_TAG_CHAR_TABLE = 9,
;;     SEXP_TAG_BOOL_VECTOR = 10, SEXP_TAG_MUT_STR = 6 — same
;;                          DEFERRED stance.
;;   - All atoms (Nil / T / Int / Float / Str / Symbol) — no
;;                          children to walk; the Rust shim returns
;;                          Nil directly without invoking this
;;                          kernel.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this
;; feature in its manifest; `build-tool/build.rs' compiles the
;; source into `nelisp_gc_walk_children.o' and archives it into the
;; static lib the crate links against.  The Rust side declares the
;; `extern "C"' signature in `build-tool/src/lib.rs::elisp_cc_spike'
;; and exposes a safe wrapper
;; `elisp_cc_spike::gc_walk_children(sexp_ptr, result_slot, tail_slot)'
;; for the integration test in
;; `tests/elisp_cc_gc_walk_children_probe.rs'.
;;
;; Stage scope: §123.D ships *only* the elisp kernel + probe.  The
;; Rust-side `bi_nl_gc_walk_children' primitive in `rc_primitives.rs:
;; 247-262' continues to drive its own list construction via
;; `Sexp::list_from'; a future sweep stage will swap the body to
;; call this kernel via extern-C, with a Rust-side tag-check
;; preamble that rejects non-Cons inputs.  Existing
;; `rc_primitives.rs' unit tests remain green throughout this stage.

;;; Code:

(defconst nelisp-cc-gc-walk-children--source
  '(defun nelisp_gc_walk_children (sexp-ptr result-slot tail-slot)
     ;; Compose 3 ops via `and' short-circuit:
     ;;   1. Seed `tail-slot' with `Sexp::Nil' (= `sexp-write-nil'
     ;;      returns the slot pointer, non-zero, so `and' continues).
     ;;   2. Build `(cdr . nil)' in `tail-slot' via `cons-make'.
     ;;      Inner `(+ (ptr-read-u64 sexp-ptr 8) 32)' computes the
     ;;      address of the input `NlConsBox.cdr' field (= offset 32
     ;;      of the box base, which is itself at offset 8 of the
     ;;      outer `Sexp').  `cons-make' raw-copies the 32-byte
     ;;      Sexp value at that address into the new box's car slot.
     ;;   3. Build `(car . tail-slot's Cons)' in `result-slot' via
     ;;      a second `cons-make'.  `(ptr-read-u64 sexp-ptr 8)' is
     ;;      the input `NlConsBox*' = address of the box's car field
     ;;      at offset 0.  `cons-make' raw-copies the car Sexp into
     ;;      the new box and writes `Sexp::Cons(new_box)' to
     ;;      `result-slot'.  Returns `result-slot' in rax.
     ;;
     ;; Two ptr-read-u64 evaluations of the same `(ptr-read-u64
     ;; sexp-ptr 8)' form — AOT does not common-subexpression
     ;; eliminate, so each call lowers to a fresh
     ;; `call nl_ptr_read_u64@PLT'.  The redundant read is cheap
     ;; (~2 cycles + cache hit) and avoids the let-binding gap in
     ;; the AOT grammar (= `let' only supports int-foldable
     ;; constants per `nelisp-aot-compiler.el:1290-1310').
     (and (sexp-write-nil tail-slot)
          (cons-make (+ (ptr-read-u64 sexp-ptr 8) 32)
                     tail-slot
                     tail-slot)
          (cons-make (ptr-read-u64 sexp-ptr 8)
                     tail-slot
                     result-slot)))
  "AOT source for the Doc 123 §123.D walk-children kernel.

Three-arg function — AOT's SysV AMD64 prologue spills
`sexp-ptr' (= raw `*const Sexp' as i64), `result-slot' (= `*mut
Sexp'), `tail-slot' (= `*mut Sexp' scratch) into rbp-relative
slots 0/1/2.

The body is a 3-form `and' chain (`sexp-write-nil', `cons-make',
`cons-make') that produces the head of a 2-list `(car cdr)' in
`result-slot' matching the Rust body's `Sexp::list_from(&[car,
cdr])' output.  See the Commentary for the per-step rationale and
the redundant `ptr-read-u64' note.

Cons-only support: non-Cons inputs are filtered by the §123.F
sweep stage's Rust shim before reaching this kernel.  Vector /
Record / Cell / CharTable / BoolVector / MutStr child walks are
DEFERRED per `docs/design/123-rc-primitives-elisp-rewrite.org §2.4'
to a future stage if scope expands.")

(provide 'nelisp-cc-gc-walk-children)

;;; nelisp-cc-gc-walk-children.el ends here
