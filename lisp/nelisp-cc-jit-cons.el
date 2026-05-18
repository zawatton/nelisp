;;; nelisp-cc-jit-cons.el --- Doc 120 §120.C jit/cons.rs partial swap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.C — Phase-47-compiled replacements for 4 of 5
;; `jit/cons.rs' trampolines:
;;
;;   - `nl_jit_cons_car'     → `nelisp_jit_cons_car'
;;   - `nl_jit_cons_cdr'     → `nelisp_jit_cons_cdr'
;;   - `nl_jit_cons_setcar'  → `nelisp_jit_cons_setcar'
;;   - `nl_jit_cons_setcdr'  → `nelisp_jit_cons_setcdr'
;;
;; ABI shapes (matching `nl-jit-call-out-{1,2,2i}' bridge primitives
;; + Phase 7.1.6 dlsym direct-CALL semantics):
;;
;;   car/cdr:        `(*const Sexp, *mut Sexp) -> i64'
;;   setcar/setcdr:  `(*const Sexp, *const Sexp, *mut Sexp) -> i64'
;;
;; Return: TRAMPOLINE_OK=0 / TRAMPOLINE_ERR=1.
;;
;; Phase 47 grammar pieces used:
;;   `(sexp-tag PTR)'           — read tag byte at offset 0.
;;   `(sexp-write-nil SLOT)'    — tag-only Nil write into the out slot.
;;   `(extern-call NAME ...)'   — Rust extern dispatch.
;;   `(cons-set-car H V)'       — refcount-safe overwrite of car via
;;                                `nl_consbox_set_car'.
;;   `(cons-set-cdr H V)'       — refcount-safe overwrite of cdr via
;;                                `nl_consbox_set_cdr'.
;;
;; Tag-byte constants used inline (= pinned by §62.5 ABI assert tests
;; in `tests/sexp_repr.rs'):
;;
;;   7 = SEXP_TAG_CONS  (= `nelisp-sexp--tag-cons')
;;   0 = SEXP_TAG_NIL   (= `nelisp-sexp--tag-nil')
;;
;; Refcount discipline note: the existing `cons-car' / `cons-cdr' Phase
;; 47 grammar ops do a raw 32-byte SIMD copy WITHOUT refcount
;; adjustment, which would cause a double-free when the inline car/cdr
;; holds a boxed variant (Cons, Vector, Record, ...).  Following the
;; §120.B `record-type' precedent we instead obtain a `*const Sexp'
;; pointer at the inline slot via a narrow Rust helper
;; (`nl_cons_car_ptr' / `nl_cons_cdr_ptr' in `jit/cons.rs') and then
;; copy refcount-aware through `nl_sexp_clone_into'.
;;
;; `nl_jit_cons_make' moved to Phase 47 elisp in
;; `lisp/nelisp-cc-jit-cons-make.el'.  The `cons-make' Phase 47 opcode
;; passes incoming `*const Sexp' args directly into the NlConsBox raw
;; copy (= `bi_nl_jit_call_out_2' already provides stable pointers),
;; so no clone+forget wrapper is needed in the trampoline.

;;; Code:

(defconst nelisp-cc-jit-cons-car--source
  '(defun nelisp_jit_cons_car (arg out)
     ;; arg: *const Sexp.  out: *mut Sexp.
     ;; Returns: i64 = 0 OK (= Nil → Nil, Cons → car.clone()), 1 ERR.
     ;;
     ;; The TAG_NIL fast path mirrors the Rust trampoline's `(car nil)
     ;; = nil' elisp contract.  The Cons arm obtains `*const Sexp'
     ;; pointing at the inline car field via `nl_cons_car_ptr' (=
     ;; same `nl_record_type_tag_ptr' shape used by §120.B
     ;; `record-type') then refcount-clones through
     ;; `nl_sexp_clone_into'.
     (if (= (sexp-tag arg) 0)
         (and (sexp-write-nil out) 0)
       (if (= (sexp-tag arg) 7)
           (and
            (extern-call
             nl_sexp_clone_into
             (extern-call nl_cons_car_ptr arg)
             out)
            0)
         1)))
  "Phase 47 source for the §120.C `nl_jit_cons_car' swap.

NIL fast path writes `Sexp::Nil' to out (= same as Rust's `(car
nil) = nil' shortcut); Cons arm obtains the car slot pointer via
the `nl_cons_car_ptr' Rust extern (= `&(*box_ptr).car as *const
Sexp', same shape as `nl_record_type_tag_ptr') then refcount-
clones into out via `nl_sexp_clone_into'.  Non-Cons / non-Nil →
TRAMPOLINE_ERR.

The `(and SIDE-EFFECT 0)' idiom threads the side effect through
to a stable 0 return per §120.A `nl_jit_ref_eq' convention.")

(defconst nelisp-cc-jit-cons-cdr--source
  '(defun nelisp_jit_cons_cdr (arg out)
     ;; arg: *const Sexp.  out: *mut Sexp.
     ;; Returns: i64 = 0 OK (= Nil → Nil, Cons → cdr.clone()), 1 ERR.
     (if (= (sexp-tag arg) 0)
         (and (sexp-write-nil out) 0)
       (if (= (sexp-tag arg) 7)
           (and
            (extern-call
             nl_sexp_clone_into
             (extern-call nl_cons_cdr_ptr arg)
             out)
            0)
         1)))
  "Phase 47 source for the §120.C `nl_jit_cons_cdr' swap.

Mirror of `nl_jit_cons_car' with `nl_cons_cdr_ptr' (= `&(*box_ptr
).cdr as *const Sexp', cdr lives at `NlConsBox' offset 32) for
the slot-ptr resolution.")

(defconst nelisp-cc-jit-cons-setcar--source
  '(defun nelisp_jit_cons_setcar (arg val out)
     ;; arg: *const Sexp (Cons handle).  val: *const Sexp.
     ;; out: *mut Sexp.
     ;; Returns: i64 = 0 OK (= arg is Cons; box.car overwritten with
     ;; clone(val); *out = clone(val)), 1 ERR (= non-Cons).
     ;;
     ;; `cons-set-car' resolves arg's payload (= NlConsBox*) and
     ;; forwards to `nl_consbox_set_car' which does the refcount-safe
     ;; drop+write (= same helper Doc 101 §101.D used).  `nl_sexp_clone
     ;; _into' then writes a refcount-bumped clone of `*val' into
     ;; `*out' matching the Rust trampoline's `*out = (*val).clone()'
     ;; bit-for-bit.
     (if (= (sexp-tag arg) 7)
         (and
          (cons-set-car arg val)
          (extern-call nl_sexp_clone_into val out)
          0)
       1))
  "Phase 47 source for the §120.C `nl_jit_cons_setcar' swap.

Tag-checks `arg' against `nelisp-sexp--tag-cons' (= 7) inline; on
match calls `cons-set-car' (= `nl_consbox_set_car' extern) to
refcount-safely overwrite the box's car field with `clone(*val)',
then writes `clone(*val)' into `*out' via `nl_sexp_clone_into'.
Returns 0 on Cons input, 1 on any other variant.")

(defconst nelisp-cc-jit-cons-setcdr--source
  '(defun nelisp_jit_cons_setcdr (arg val out)
     ;; arg: *const Sexp (Cons handle).  val: *const Sexp.
     ;; out: *mut Sexp.
     ;; Returns: i64 = 0 OK, 1 ERR.
     (if (= (sexp-tag arg) 7)
         (and
          (cons-set-cdr arg val)
          (extern-call nl_sexp_clone_into val out)
          0)
       1))
  "Phase 47 source for the §120.C `nl_jit_cons_setcdr' swap.

Mirror of `nl_jit_cons_setcar' with `cons-set-cdr' (= `nl_consbox
_set_cdr' extern) for the cdr-field overwrite.")

(provide 'nelisp-cc-jit-cons)

;;; nelisp-cc-jit-cons.el ends here
