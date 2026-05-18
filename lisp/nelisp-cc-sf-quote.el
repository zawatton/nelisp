;;; nelisp-cc-sf-quote.el --- Phase 47 nl_sf_quote swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the `sf_quote' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_quote(args: &Sexp) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       expect_len(&parts, "quote", 1)?;
;;       Ok(parts[0].clone())
;;   }
;;
;; `args' is the raw unevaluated argument list for `(quote FORM)' —
;; a Sexp::Cons with car=FORM, cdr=Nil.  The Phase 47 body checks
;; the tag (must be Cons = 7) and that the cdr is Nil (arity == 1),
;; then refcount-clones the car into the output slot via
;; `nl_sexp_clone_into'.
;;
;; ABI constants used (§100.B frozen):
;;   SEXP_TAG_NIL  = 0
;;   SEXP_TAG_CONS = 7
;;   nl_cons_car_ptr: *const Sexp → i64 (= &car of NlConsBox, or 0)
;;   nl_sexp_clone_into: (*const Sexp, *mut Sexp) → ()

;;; Code:

(defconst nelisp-cc-sf-quote--source
  '(defun nl_sf_quote (args out)
     ;; args: *const Sexp = the raw arg list `(FORM)' passed to `quote'.
     ;; out:  *mut Sexp   = result slot.
     ;; Returns: i64 = 0 OK (car cloned into out), 1 ERR (wrong arity).
     ;;
     ;; Tag 7 = Sexp::Cons.  The arity-1 check requires cdr == Nil
     ;; (tag 0).  `nl_cons_car_ptr' returns &NlConsBox.car as i64
     ;; (= *const Sexp).  `nl_sexp_clone_into' writes a refcount-safe
     ;; clone of *src into *dst.  `(and SIDE-EFFECT 0)' threads the
     ;; side effect through to a stable i64=0 return.
     (if (= (sexp-tag args) 7)
         (if (= (sexp-tag (extern-call nl_cons_cdr_ptr args)) 0)
             (and
              (extern-call
               nl_sexp_clone_into
               (extern-call nl_cons_car_ptr args)
               out)
              0)
           1)
       1))
  "Phase 47 source for `nl_sf_quote' (eval/special_forms.rs sf_quote → elisp).

Receives `args' = the raw Cons list passed to the `quote' special
form dispatcher; extracts and refcount-clones the sole argument into
`out'.  Returns 0 on success, 1 on wrong arity (non-Cons args or
cdr ≠ Nil).  Called from the thin Rust shell in `sf_quote' via
`crate::elisp_cc_spike::sf_quote_call'.")

(provide 'nelisp-cc-sf-quote)

;;; nelisp-cc-sf-quote.el ends here
