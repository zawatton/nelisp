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
     ;; arg: *const Sexp.  Returns a 32B-slot VIEW of the car, or 0 when
     ;; arg is not a Cons (tag != 7).
     ;;
     ;; Doc 147 Phase 3 — the NlConsBox car is now an 8-byte tagged WORD
     ;; @ box+0 (was a 32B inline Sexp).  The ~302 consumer sites still
     ;; read a 32B `Sexp' (tag@+0 / payload@+8) from the returned pointer,
     ;; so we MATERIALISE a 32B-slot VIEW exactly like the Phase-2
     ;; `nl_vector_slot_ptr':
     ;;   box   = *(u64*)(arg + 8)        // Sexp::Cons payload (NlConsBox*)
     ;;   word  = *(u64*)(box + 0)        // the 8B tagged car WORD
     ;;   - pointer WORD (low bit 0): return it directly (already a 32B
     ;;     child Sexp box the consumer reads tag/payload from).
     ;;   - immediate WORD (low bit 1): alloc a FRESH 32B box and
     ;;     materialise the immediate into it via `nl_val_load(word, box)'
     ;;     (the Phase 0 keystone).  A fresh box PER immediate call kills
     ;;     the SCRATCH-LIFETIME HAZARD for consumers holding two+ accessor
     ;;     results live (list walks / bucket walks / equal2 recursion).
     (if (= (sexp-tag arg) 7)
         (let ((word (ptr-read-u64 (ptr-read-u64 arg 8) 0)))
           (if (= (logand word 1) 0)
               word
             (extern-call nl_val_load word (alloc-bytes 32 8))))
       0))
  "AOT source for `nl_cons_car_ptr' (Doc 147 Phase 3 materialiser).

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
