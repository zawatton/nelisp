;;; nelisp-cc-sf-setq.el --- Phase 47 nl_sf_setq swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the `sf_setq' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_setq(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       if parts.len() % 2 != 0 { return Err(...); }
;;       let mut last = Sexp::Nil;
;;       let mut iter = parts.into_iter();
;;       while let Some(name_form) = iter.next() {
;;           let value_form = iter.next().unwrap();
;;           let val = eval(&value_form, env)?;
;;           env.set_value(&name, val.clone())?;
;;           last = val;
;;       }
;;       Ok(last)
;;   }
;;
;; ABI:
;;   nl_sf_setq(args: *const Sexp, env: *mut c_void, out: *mut Sexp) -> i64
;;   args = (SYM VAL SYM VAL ... NIL)
;;   Returns 0 on success (out = last assigned value), 1 on error.
;;
;; New thin Rust helper:
;;   nl_env_set_value(env, sym, val) -> i64  (0=ok, 1=err)
;;
;; Phase 47 constraint: `let' can only bind integer constants.
;; All intermediate values are threaded as function parameters.
;;
;; Structure of each pair: sym-ptr = car(args), val-form = car(cdr(args)),
;; next-args = cdr(cdr(args)).  After evaling val into out, call
;; nl_env_set_value to bind it, then recurse on next-args.

;;; Code:

(defconst nelisp-cc-sf-setq--source
  '(seq
    ;; After evaluating val-form into out, bind it.
    ;; sym-ptr: *const Sexp of the symbol.  next: remaining (SYM VAL ...).
    ;; set-rc: result of nl_env_set_value.
    (defun nl_sf_setq_after_set (set-rc next env out)
      (if (= set-rc 0)
          (nl_sf_setq next env out)
        1))
    ;; After evaluating val-form into out, attempt to bind.
    ;; eval-rc: result of nelisp_eval_call for val.
    (defun nl_sf_setq_after_eval (eval-rc sym-ptr next env out)
      (if (= eval-rc 0)
          (nl_sf_setq_after_set
           (extern-call nl_env_set_value env sym-ptr out)
           next env out)
        1))
    ;; Process one pair: sym-ptr is known, rest = (VAL . NEXT-PAIRS).
    ;; rest: cdr of args after the sym.
    (defun nl_sf_setq_pair (sym-ptr rest env out)
      (nl_sf_setq_after_eval
       (extern-call nelisp_eval_call (extern-call nl_cons_car_ptr rest) env out)
       sym-ptr
       (extern-call nl_cons_cdr_ptr rest)
       env out))
    ;; Public entry: nl_sf_setq(args, env, out) -> i64
    ;; args = (SYM . (VAL . NEXT-PAIRS)) or nil.
    (defun nl_sf_setq (args env out)
      (if (= (sexp-tag args) 0)
          0  ;; done (out holds last value, or nil if no pairs)
        (nl_sf_setq_pair
         (extern-call nl_cons_car_ptr args)
         (extern-call nl_cons_cdr_ptr args)
         env out))))
  "Phase 47 source for `nl_sf_setq' (eval/special_forms.rs sf_setq → elisp).

Receives `args' = (SYM VAL ...) as *const Sexp,
`env' = opaque *mut Env as *mut c_void, `out' = result *mut Sexp.
Processes pairs: evaluates each VAL into `out' via `nelisp_eval_call',
then binds via `nl_env_set_value(env, sym, out)'.  `out' holds the last
assigned value on completion.  Returns 0 on success, 1 on error.")

(provide 'nelisp-cc-sf-setq)

;;; nelisp-cc-sf-setq.el ends here
