;;; nelisp-cc-jit-cons-car-ptr.el --- AOT nl_cons_car_ptr swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `nl_cons_car_ptr' Rust extern in
;; `build-tool/src/jit/cons.rs'.  The Rust body was:
;;
;;   match &*arg {
;;       Sexp::Cons(_) => (*arg).cons_box_ptr() as *const Sexp,
;;       _ => std::ptr::null(),
;;   }
;;
;; `cons_box_ptr' reads the NonNull<NlConsBox> pointer stored at
;; SEXP_PAYLOAD_OFFSET=8 inside the Sexp enum.  Since NlConsBox is
;; `#[repr(C)]' with `car' at offset 0, that pointer IS `&car as
;; *const Sexp' — matching the AOT `sexp-payload-ptr' op which
;; returns the same box pointer (or 0 for any non-box variant).
;;
;; ABI constants used (§100.B frozen):
;;   SEXP_TAG_CONS = 7
;;   SEXP_PAYLOAD_OFFSET = 8  (= sexp-payload-ptr reads at this offset)
;;   NlConsBox::car @ offset 0 (= box ptr == &car)

;;; Code:

(defconst nelisp-cc-jit-cons-car-ptr--source
  '(defun nl_cons_car_ptr (arg)
     ;; arg: *const Sexp.
     ;; Returns: i64 = NlConsBox* (= &car) when tag==7 (Cons), else 0.
     ;;
     ;; `sexp-payload-ptr' returns the box pointer for any boxed
     ;; variant (Cons tag=7 → NlConsBox*, etc.) and 0 for unboxed
     ;; variants (Nil/T/Int/Float).  Since NlConsBox::car is at offset
     ;; 0, the box pointer is the same address as `*const Sexp' for
     ;; the car field — no arithmetic needed.
     (if (= (sexp-tag arg) 7)
         (sexp-payload-ptr arg)
       0))
  "AOT source for `nl_cons_car_ptr' (jit/cons.rs → elisp).

Returns the NlConsBox* (= &car as *const Sexp, NlConsBox offset 0)
when arg is Sexp::Cons (tag 7), else 0 (null).  Used via
`(extern-call nl_cons_car_ptr arg)' in `nelisp-cc-jit-cons.el'
to obtain the slot pointer before `nl_sexp_clone_into' copies the
car refcount-safely into the output slot.")

(provide 'nelisp-cc-jit-cons-car-ptr)

;;; nelisp-cc-jit-cons-car-ptr.el ends here
