;;; nelisp-cc-jit-cons-cdr-ptr.el --- Phase 47 nl_cons_cdr_ptr swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the `nl_cons_cdr_ptr' Rust extern in
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
;; So `&cdr as *const Sexp' = box_ptr + 32.  Phase 47 computes this
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
     ;; arg: *const Sexp.
     ;; Returns: i64 = &cdr (= box_ptr + 32) when tag==7 (Cons), else 0.
     ;;
     ;; `sexp-payload-ptr' gives NlConsBox* (= &car @ offset 0).
     ;; Adding 32 (= sizeof::<Sexp>()) reaches the cdr field.
     (if (= (sexp-tag arg) 7)
         (+ (sexp-payload-ptr arg) 32)
       0))
  "Phase 47 source for `nl_cons_cdr_ptr' (jit/cons.rs → elisp).

Returns (box_ptr + 32) as *const Sexp — the address of the cdr
field inside the NlConsBox — when arg is Sexp::Cons (tag 7), else
0 (null).  NlConsBox::cdr is at offset 32 = sizeof::<Sexp>().
Used via `(extern-call nl_cons_cdr_ptr arg)' in
`nelisp-cc-jit-cons.el' before `nl_sexp_clone_into' copies cdr.")

(provide 'nelisp-cc-jit-cons-cdr-ptr)

;;; nelisp-cc-jit-cons-cdr-ptr.el ends here
