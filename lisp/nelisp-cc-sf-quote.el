;;; nelisp-cc-sf-quote.el --- AOT nl_sf_quote swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_quote' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_quote(args: &Sexp) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       expect_len(&parts, "quote", 1)?;
;;       Ok(parts[0].clone())
;;   }
;;
;; `args' is the raw unevaluated argument list for `(quote FORM)' —
;; a Sexp::Cons with car=FORM, cdr=Nil.  The AOT body checks
;; the tag (must be Cons = 7) and that the cdr is Nil (arity == 1),
;; then refcount-clones the car into the output slot via
;; `nl_sexp_clone_into'.
;;
;; ABI constants used (§100.B frozen):
;;   SEXP_TAG_NIL  = 0
;;   SEXP_TAG_CONS = 7
;;   nl_cons_car_ptr: *const Sexp → i64 (= &car of NlConsBox, or 0)
;;   nl_cons_cdr_ptr: *const Sexp → i64
;;   nl_sexp_clone_into: (*const Sexp, *mut Sexp) → ()
;;
;; Alignment fix (Level 1 + Level 2, 2026-05-19):
;;   Level 1: `(= (sexp-tag (extern-call nl_cons_cdr_ptr args)) 0)' in
;;   even-arity (2) nl_sf_quote — `--emit-cmp' pushes B=0 before
;;   extern-call, causing rsp ≡ 8 mod 16 at the CALL.  This happens
;;   to not crash in practice (nl_cons_cdr_ptr uses no SSE), but is
;;   undefined behavior.  Fix: extract nl_sf_quote_check_cdr (arity 4,
;;   even) that receives cdr-ptr as register arg 0.
;;   Level 2: nl_cons_car_ptr at call-site arg position 2 (even) → safe.
;;
;; Structure (2 defuns, seq form):
;;   nl_sf_quote_check_cdr (cdr-ptr out car-ptr _pad) — arity 4 (even)
;;   nl_sf_quote            (args out)                 — arity 2 (even)

;;; Code:

(defconst nelisp-cc-sf-quote--source
  '(seq
    ;; cdr-ptr: cdr of args, pre-fetched as register arg 0.
    ;; car-ptr: at call-site arg position 2 (even) — safe.
    ;; sexp-tag is inline (movzx) — no alignment concern.
    ;; Arity 4 (even): body-entry rsp ≡ 0 mod 16.
    (defun nl_sf_quote_check_cdr (cdr-ptr out car-ptr _pad)
      (if (= (sexp-tag cdr-ptr) 0)
          (and (extern-call nl_sexp_clone_into car-ptr out) 0)
        1))

    ;; Public entry: nl_sf_quote(args, out) → i64
    ;; args: *const Sexp = the raw arg list `(FORM)' passed to `quote'.
    ;; out:  *mut Sexp   = result slot.
    ;; Returns: i64 = 0 OK, 1 ERR (wrong arity).
    ;; Arity 2 (even): body-entry rsp ≡ 0.
    ;; Extern-calls: cdr at pos 0 (safe), car at pos 2 (safe).
    (defun nl_sf_quote (args out)
      (if (= (sexp-tag args) 7)
          (nl_sf_quote_check_cdr
           (extern-call nl_cons_cdr_ptr args)   ; pos 0: rsp ≡ 0 ✓
           out                                   ; pos 1: register
           (extern-call nl_cons_car_ptr args)   ; pos 2: rsp ≡ -16 ≡ 0 ✓
           0)
        1)))
  "AOT source for `nl_sf_quote' (eval/special_forms.rs sf_quote → elisp).

Two defuns (seq form).  Alignment-safe CPS structure.

Receives `args' = the raw Cons list passed to the `quote' special
form dispatcher; extracts and refcount-clones the sole argument into
`out'.  Returns 0 on success, 1 on wrong arity (non-Cons args or
cdr ≠ Nil).  Called from the thin Rust shell in `sf_quote' via
`crate::elisp_cc_spike::sf_quote_call'.")

(provide 'nelisp-cc-sf-quote)

;;; nelisp-cc-sf-quote.el ends here
