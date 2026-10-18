;;; nelisp-cc-cons-prepend-clone.el --- AOT nl_cons_prepend_clone swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `nl_cons_prepend_clone' Rust extern in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   #[no_mangle]
;;   pub unsafe extern "C" fn nl_cons_prepend_clone(
;;       car_ptr: *const Sexp,
;;       cdr_ptr: *const Sexp,
;;       out: *mut Sexp,
;;   ) -> i64 {
;;       let car_owned = (*car_ptr).clone();
;;       let cdr_owned = (*cdr_ptr).clone();
;;       *out = Sexp::cons(car_owned, cdr_owned);
;;       0
;;   }
;;
;; The function deep-clones *car-ptr and *cdr-ptr into a fresh NlConsBox
;; and writes the resulting `Sexp::Cons(_)' into *out.  Returns 0
;; (TRAMPOLINE_OK) on success.
;;
;; AOT equivalent: the G3 `cons-make-with-clone' fused grammar op
;; (Doc 120.E) performs exactly this sequence in one composed value form:
;;   1. `nl_alloc_consbox' — allocate a fresh NlConsBox (car/cdr = Nil,
;;      refcount = 1).
;;   2. `nl_sexp_clone_into(*car-ptr, &box->car)' — refcount-aware deep
;;      clone of car into the box's car field.
;;   3. `nl_sexp_clone_into(*cdr-ptr, &box->cdr)' — same for cdr.
;;   4. Write `Sexp::Cons(box)' (tag byte 7 + payload ptr) into *out.
;;
;; The `(and ... 0)' chain threads the side-effecting constructor through
;; to a stable `0' return value (= TRAMPOLINE_OK, per the §120.A / §120.C
;; convention).  `cons-make-with-clone' returns SLOT (= out) in rax, which
;; is non-nil (a pointer), so `and' proceeds to the final `0'.
;;
;; This is an identical shape to Doc 120.E `nl_jit_cons_make'; the only
;; difference is the public symbol name and the callers.
;;
;; Callers (via `extern-call nl_cons_prepend_clone'):
;;   - `nelisp-cc-sf-lambda.el': prepends symbol / captured-env before the
;;     closure arg list when building `(closure CAPTURED FORMALS BODY...)'.
;;
;; Function contract:
;;   car-ptr  *const Sexp — caller-owned car value, deep-cloned into box.
;;   cdr-ptr  *const Sexp — caller-owned cdr value, deep-cloned into box.
;;   out      *mut Sexp   — destination Sexp slot; receives `Sexp::Cons(box)'
;;                          after the cloned pair is installed.
;;   returns  i64 = 0     — TRAMPOLINE_OK.
;;
;; Net Rust impact: deleting the 10-LOC Rust body in `special_forms.rs'
;; (= the `#[no_mangle]' declaration + 3 local bindings + return).
;; Arity 3 — same as `nl_jit_cons_make' in Doc 120.E; the AOT
;; SysV AMD64 prologue spills the three `*const Sexp' / `*mut Sexp'
;; arguments into rbp-relative slots 0..2.

;;; Code:

(defconst nelisp-cc-cons-prepend-clone--source
  '(seq
    ;; Doc 215 word ABI.  The fresh cons box is the result object, so it is
    ;; returned as the WORD without a copy.  Both sources are cloned before
    ;; the box is written, so aliasing with the caller's OUT stays safe.
    (defun nl_cons_prepend_clone_w (car-ptr cdr-ptr _pad _pad1)
      (let* ((box (alloc-bytes 32 8)))
        (seq (cons-make-with-clone car-ptr cdr-ptr box)
             (nl_val_store_word box))))
    (defun nl_cons_prepend_clone (car-ptr cdr-ptr out)
      (nl_abi_word_to_rc (nl_cons_prepend_clone_w car-ptr cdr-ptr 0 0) out)))
  "AOT source for `nl_cons_prepend_clone' (special_forms.rs → elisp).

Single defun.  The `cons-make-with-clone' grammar op (Doc 120.E) fuses
NlConsBox allocation + two `nl_sexp_clone_into' calls + tag/payload write
into a single emitted sequence.  Aliasing of out with one of the source
pointers is safe: the clones are read before the destination is written.

Arity 3 (SysV AMD64 prologue: rdi=car-ptr, rsi=cdr-ptr, rdx=out spilled
to rbp-relative slots 0..2).  Return 0 = TRAMPOLINE_OK.

Net Rust impact: -10 LOC (`special_forms.rs' `nl_cons_prepend_clone' body).")

(provide 'nelisp-cc-cons-prepend-clone)

;;; nelisp-cc-cons-prepend-clone.el ends here
