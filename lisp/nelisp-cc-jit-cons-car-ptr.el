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
      (if (= (nl_val_tag arg) 7)
          (let ((word (heap-read-u64 (nl_word_payload arg) 0)))
            ;; Doc 216: a legacy view (000) and a direct cons (010) pass through
            ;; unchanged -- no transient box that could not cross an eval;
            ;; immediates and vectorlikes still materialise.
            ;; Doc 226 D9 stage 3.2: so does a symbol object's WORD (class 110,
            ;; kind 4 or 16): a view made for it would be a transient box that the
            ;; native stack alone keeps alive, and the callers read a symbol
            ;; through the semantic accessors (`sexp-tag', `heap-sexp-data' ...).
            (if (= (logand word 5) 0) word
              (if (= (logand word 7) 6)
                  (if (or (= (logand (heap-read-u64 (- word 6) 0) 63) 4)
                          (= (logand (heap-read-u64 (- word 6) 0) 63) 16))
                      word
                    (extern-call nl_sexp_clone_into word (alloc-bytes 32 8)))
                (extern-call nl_sexp_clone_into word (alloc-bytes 32 8))))) 0))
  "AOT source for `nl_cons_car_ptr' (Doc 147 Phase 3 materialiser).
Doc 216 S2.1: a direct cons WORD (payload | 2) is returned as it is,
like a legacy view; callers read it through the semantic accessors.

Returns a 32B-slot VIEW of the cons CAR when arg is Sexp::Cons (tag 7),
else 0 (null).  Derefs the Sexp::Cons payload at arg+8 to the NlConsBox,
loads the 8B tagged car WORD at box+0, and returns the view: a pointer
WORD passes through (already a 32B child box); an immediate WORD is
materialised into a FRESH 32B box via the arity-2 `nl_val_load' keystone
(one box per call -> no shared-scratch clobber when a consumer holds
two+ accessor results live).  Used via `(extern-call nl_cons_car_ptr
arg)' at the ~302 cons read sites.")

(provide 'nelisp-cc-jit-cons-car-ptr)

;;; nelisp-cc-jit-cons-car-ptr.el ends here
