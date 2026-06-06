;;; nelisp-cc-bi-make-vector.el --- Doc 117 §117.A.1 bi_make_vector swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.A.1 swaps the body of `(make-vector N INIT)' to a
;; AOT-compiled elisp object.  The Rust side keeps only arg
;; validation (= `require_arity' + `as_int' + the negative-length
;; check) plus the caller-owned result-slot setup; the allocate +
;; fill body itself lives only in the source below.
;;
;; Algorithm (= literal transcription of the Rust impl):
;;
;;   1. Allocate fresh `Sexp::Vector' of length N via `vector-make'
;;      (§115.1).  The new vector is pre-filled with `Sexp::Nil' by
;;      `nl_alloc_vector', refcount = 1.
;;   2. Fill each slot [0, N) with a refcount-bumped clone of INIT
;;      via `vector-slot-set' (§111.E) in a tail-recursive helper.
;;      `vector-slot-set' calls `nl_vector_set_slot' which clones
;;      `*val_ptr' before writing, so INIT's caller-owned refcount
;;      is preserved.
;;
;; Refcount discipline: the new Sexp::Vector in RESULT-SLOT ends with
;; refcount = 1 (the slot is the only owner); each cloned INIT element
;; bumps INIT's refcount by 1, so the caller's INIT handle stays at
;; its original refcount across the call.  The N=0 case skips the
;; fill loop entirely.
;;
;; ABI deps satisfied:
;;   §100    `sexp-int-unwrap'  — read N from N-PTR.
;;   §100    `if' / `+' / `<'   — fill-loop control + counter.
;;   §111.E  `vector-slot-set'  — refcount-safe slot[i] := *INIT-PTR.
;;   §115.1  `vector-make'      — fresh `NlVector(N)' allocator.

;;; Code:

(defconst nelisp-cc-bi-make-vector--source
  '(seq
    (defun nelisp_bi_make_vector_fill (vec-slot init-ptr i n)
      ;; Tail-recursive fill: for i in [0, n), set vec[i] := *init-ptr.
      ;; `vector-slot-set' (§111.E) is refcount-aware (= clones init-ptr
      ;; before writing into the slot), so the caller's INIT handle is
      ;; preserved.  When i == n, return 1 (= terminator sentinel for
      ;; `and' chaining at the call site).
      (if (< i n)
          (and (vector-slot-set vec-slot i init-ptr)
               (nelisp_bi_make_vector_fill vec-slot init-ptr (+ i 1) n))
        1))
    (defun nelisp_bi_make_vector (n-ptr init-ptr result-slot)
      ;; n-ptr:        *const Sexp pointing at `Sexp::Int' (length N).
      ;;               Rust dispatcher has already validated N >= 0.
      ;; init-ptr:     *const Sexp pointing at the fill value.
      ;; result-slot:  *mut Sexp 32-byte slot to receive `Sexp::Vector'.
      ;;
      ;; Returns: result-slot pointer (= rax from the final fill call's
      ;; `and' chain, materialised from `vector-make' for the N=0 fast
      ;; path or the recursive fill terminator's `1' for N>0; either
      ;; way truthy so the outer caller sees a non-null return).
      (and (vector-make (sexp-int-unwrap n-ptr) result-slot)
           (nelisp_bi_make_vector_fill
            result-slot init-ptr 0 (sexp-int-unwrap n-ptr)))))
  "AOT source for the Doc 117 §117.A.1 `(make-vector N INIT)' swap.

`n-ptr' is a `*const Sexp' that the Rust dispatcher has already
validated points at a non-negative `Sexp::Int'.  `init-ptr' is a
`*const Sexp' of the fill value (typically `Sexp::Nil', `Sexp::Int',
or any other Sexp shape — `vector-slot-set' is shape-agnostic).
`result-slot' is a caller-owned 32-byte Sexp slot, initialised to
`Sexp::Nil' by the safe wrapper, which receives the fresh
`Sexp::Vector(NlVectorRef)' on return.

The fill helper is a separate defun so the outer entry-point keeps
the AOT grammar simple (= no inline `while', recursion is the
canonical loop idiom — see `nelisp-cc-length-cons.el' for the same
pattern on the read side).  The terminating `1' on the false arm of
the `<' test feeds the outer `and' so the call site sees a truthy
return even for N=0.")

(provide 'nelisp-cc-bi-make-vector)

;;; nelisp-cc-bi-make-vector.el ends here
