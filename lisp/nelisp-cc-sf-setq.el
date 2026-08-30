;;; nelisp-cc-sf-setq.el --- AOT nl_sf_setq swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_setq' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_setq(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       if parts.len() % 2 != 0 { return Err(...); }
;;       let mut last = Sexp::Nil;
;;       while let Some(name_form) = iter.next() {
;;           let value_form = iter.next().unwrap();
;;           let name = match name_form { Symbol(s) => s, ... };
;;           let val = eval(&value_form, env)?;
;;           env.set_value(&name, val.clone())?;
;;           last = val;
;;       }
;;       Ok(last)
;;   }
;;
;; `args' is the raw arg list: (SYM1 VAL1 SYM2 VAL2 ...).
;; Pairs: car=symbol-ptr (not eval'd), car(cdr)=value-form (eval'd).
;; Result slot `out' receives each value; last pair's value remains.
;;
;; ABI:
;;   nl_cons_car_ptr: *const Sexp → i64
;;   nl_cons_cdr_ptr: *const Sexp → i64
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;   nl_env_set_value: (*mut c_void, *const Sexp, *const Sexp) → i64
;;
;; Alignment: every extern-call is argument 0 at its call site.
;; CPS chain: each function receives exactly one extern-call result
;; as its first register argument, then does tag-checks or delegates.
;;
;; Structure (8 defuns):
;;   nl_sf_setq_set_rc (set-rc rest env out) — after set_value
;;   nl_sf_setq_rest   (rest sym env out) — after cdr(val-cdr)
;;   nl_sf_setq_evaled (eval-rc sym val-cdr env out _pad6) — after nelisp_eval_call, arity 6
;;   nl_sf_setq_val    (val-form sym val-cdr env out _pad6) — after car(val-cdr), arity 6
;;   nl_sf_setq_sym    (sym val-cdr env out) — after car(args-pair)
;;   nl_sf_setq_pair   (val-cdr args-pair env out) — after cdr(args-pair)
;;   nl_sf_setq_step   (cdr env out _pad) — recursive entry
;;   nl_sf_setq        (args env out _pad) — public entry, arity 4 (even)

;;; Code:

(defconst nelisp-cc-sf-setq--source
  '(seq

    ;; After nl_env_set_value: check rc, then recurse on rest.
    ;; Arity 4 (even).
    (defun nl_sf_setq_set_rc (set-rc rest env out)
      (if (= set-rc 0)
          (nl_sf_setq_step rest env out 0)
        1))

    ;; rest = nl_cons_cdr_ptr(val-cdr) already fetched as first arg.
    ;; Set the evaluated value via nl_env_set_value (extern-call FIRST ✓).
    ;; Arity 4 (even).
    (defun nl_sf_setq_rest (rest sym env out)
      (nl_sf_setq_set_rc
       (extern-call nl_env_set_value env sym out)
       rest env out))

    ;; After nelisp_eval_call: check rc; if Ok, fetch rest (extern-call FIRST ✓).
    ;; Fetch the cdr only after eval: an immediate cdr makes nl_cons_cdr_ptr
    ;; materialise an unrooted slot view, which must not live across eval's GC.
    ;; val-cdr is the real CONS box and is already kept alive by the form.
    ;; Arity 6 (even): _pad6 makes arity even → no prologue sub rsp.
    (defun nl_sf_setq_evaled (eval-rc sym val-cdr env out _pad6)
      (if (= eval-rc 0)
          (nl_sf_setq_rest
           (extern-call nl_cons_cdr_ptr val-cdr)
           sym env out)
        1))

    ;; val-form = nl_cons_car_ptr(val-cdr) already fetched as first arg.
    ;; Now eval val-form via nelisp_eval_call (extern-call FIRST ✓), carrying
    ;; the rooted val-cdr rather than a possibly materialised rest view.
    ;; Arity 6 (even): _pad6 makes arity even.
    (defun nl_sf_setq_val (val-form sym val-cdr env out _pad6)
      (nl_sf_setq_evaled
       (extern-call nelisp_eval_call val-form env out)
       sym val-cdr env out 0))

    ;; sym = nl_cons_car_ptr(args-pair) already fetched as first arg.
    ;; val-cdr = cdr(args-pair) already available.
    ;; Fetch val-form = nl_cons_car_ptr(val-cdr) (extern-call FIRST ✓).
    ;; nl_sf_setq_val is now arity 6; pass 0 as _pad6.
    ;; Arity 4 (even).
    (defun nl_sf_setq_sym (sym val-cdr env out)
      (nl_sf_setq_val
       (extern-call nl_cons_car_ptr val-cdr)
       sym val-cdr env out 0))

    ;; val-cdr = nl_cons_cdr_ptr(args-pair) already fetched as first arg.
    ;; Fetch sym = nl_cons_car_ptr(args-pair) (extern-call FIRST ✓).
    ;; Arity 4 (even).
    (defun nl_sf_setq_pair (val-cdr args-pair env out)
      (nl_sf_setq_sym
       (extern-call nl_cons_car_ptr args-pair)
       val-cdr env out))

    ;; Recursive step: cdr is the remaining (SYM VAL ...) cons list.
    ;; If Nil → done.  Else process next pair: cdr(cdr) FIRST ✓.
    ;; Arity 4 (even).
    (defun nl_sf_setq_step (cdr env out _pad)
      (if (= (sexp-tag cdr) 0)
          0
        (nl_sf_setq_pair
         (extern-call nl_cons_cdr_ptr cdr)
         cdr env out)))

    ;; Public entry: nl_sf_setq(args, env, out, _pad) → i64
    ;; Empty args → 0; else start first pair: cdr(args) FIRST ✓.
    ;; Arity 4 (even): no prologue sub rsp → no double-sub misalignment.
    (defun nl_sf_setq (args env out _pad)
      (if (= (sexp-tag args) 0)
          0
        (nl_sf_setq_pair
         (extern-call nl_cons_cdr_ptr args)
         args env out))))

  "AOT source for `nl_sf_setq' (eval/special_forms.rs sf_setq → elisp).

Eight defuns (seq form).  CPS chain with one extern-call per step.

Entry chain:
  nl_sf_setq → (cdr FIRST) → nl_sf_setq_pair
  → (car FIRST) → nl_sf_setq_sym
  → (car(val-cdr) FIRST) → nl_sf_setq_val
  → (nelisp_eval_call FIRST) → nl_sf_setq_evaled
  → (cdr(val-cdr) FIRST) → nl_sf_setq_rest
  → (nl_env_set_value FIRST) → nl_sf_setq_set_rc
  → nl_sf_setq_step (recurse)

Each extern-call is argument 0 at its call site → rsp 0 mod 16 ✓.")

(provide 'nelisp-cc-sf-setq)

;;; nelisp-cc-sf-setq.el ends here
