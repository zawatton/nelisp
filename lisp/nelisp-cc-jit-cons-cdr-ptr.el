;;; nelisp-cc-jit-cons-cdr-ptr.el --- AOT nl_cons_cdr_ptr swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `nl_cons_cdr_ptr' Rust extern in
;; `build-tool/src/jit/cons.rs'.  The Rust body was:
;;
;;   match &*arg {
;;       Sexp::Cons(_) => &(*(*arg).cons_box_ptr()).cdr as *const Sexp,
;;       _ => std::ptr::null(),
;;   }
;;
;; `cons_box_ptr' returns NonNull<NlConsBox> at SEXP_PAYLOAD_OFFSET=8.
;; NlConsBox is `#[repr(C)]' with layout:
;;   car:      Sexp  @ offset  0
;;   cdr:      Sexp  @ offset 32  (= sizeof::<Sexp>() = 32)
;;   refcount: usize @ offset 64
;;
;; So `&cdr as *const Sexp' = box_ptr + 32.  AOT computes this
;; as `(+ (sexp-payload-ptr arg) 32)'.
;;
;; ABI constants used (§100.B frozen):
;;   SEXP_TAG_CONS = 7
;;   SEXP_PAYLOAD_OFFSET = 8    (= sexp-payload-ptr reads at this offset)
;;   NlConsBox::cdr @ offset 32 (= box ptr + sizeof::<Sexp>())
;;   sizeof::<Sexp>() = 32

;;; Code:

(defconst nelisp-cc-jit-cons-cdr-ptr--source
  '(defun nl_cons_cdr_ptr (arg)
     ;; arg: *const Sexp.  Returns a 32B-slot VIEW of the cdr, or 0 when
     ;; arg is not a Cons (tag != 7).
     ;;
     ;; Doc 147 Phase 3 — the NlConsBox cdr is now an 8-byte tagged WORD
     ;; @ box+8 (was a 32B inline Sexp @ box+32).  MATERIALISE a 32B-slot
     ;; VIEW exactly like `nl_cons_car_ptr' / `nl_vector_slot_ptr':
     ;;   box   = *(u64*)(arg + 8)        // Sexp::Cons payload (NlConsBox*)
     ;;   word  = *(u64*)(box + 8)        // the 8B tagged cdr WORD
     ;;   - pointer WORD (low bit 0): return it directly (32B child box).
     ;;   - immediate WORD (low bit 1): alloc a FRESH 32B box and
     ;;     materialise via `nl_val_load(word, box)' (one box per call ->
     ;;     no shared-scratch clobber under list-walk recursion where the
     ;;     cdr-ptr result is held across the recursive descent).
     (if (= (sexp-tag arg) 7)
         (let ((word (ptr-read-u64 (ptr-read-u64 arg 8) 8)))
           (if (= (logand word 1) 0)
               word
             (extern-call nl_val_load word (alloc-bytes 32 8))))
       0))
  "AOT source for `nl_cons_cdr_ptr' (Doc 147 Phase 3 materialiser).

Returns a 32B-slot VIEW of the cons CDR when arg is Sexp::Cons (tag 7),
else 0 (null).  Derefs the Sexp::Cons payload at arg+8 to the NlConsBox,
loads the 8B tagged cdr WORD at box+8 (offset 32 -> 8), and returns the
view: a pointer WORD passes through; an immediate WORD is materialised
into a FRESH 32B box via the `nl_val_load' keystone (one box per call
-> no clobber when the cdr-ptr is held across list-walk recursion).
Used via `(extern-call nl_cons_cdr_ptr arg)' at the ~302 read sites.")

(provide 'nelisp-cc-jit-cons-cdr-ptr)

;;; nelisp-cc-jit-cons-cdr-ptr.el ends here
