;;; nelisp-cc-sf-if.el --- Phase 47 nl_sf_if swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the `sf_if' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_if(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       expect_min_len(&parts, "if", 2)?;
;;       let cond = eval(&parts[0], env)?;
;;       if is_truthy(&cond) {
;;           eval(&parts[1], env)
;;       } else {
;;           let mut last = Sexp::Nil;
;;           for f in parts.iter().skip(2) { last = eval(f, env)?; }
;;           Ok(last)
;;       }
;;   }
;;
;; ABI:
;;   nl_sf_if(args: *const Sexp, env: *mut c_void, out: *mut Sexp) -> i64
;;   args = (TEST . (THEN . ELSE-FORMS))
;;   Returns 0 on success, 1 on error.
;;
;; Phase 47 constraint: `let' can only bind integer constants.
;; Intermediate runtime values are threaded as function parameters.
;;
;; Helpers:
;;   nl_eval_is_truthy(form, env) -> i64  (1=truthy, 0=nil, -1=error)
;;   nl_cons_car_ptr(cons) -> i64         (= address of car Sexp)
;;   nl_cons_cdr_ptr(cons) -> i64         (= address of cdr Sexp)
;;   nelisp_eval_call(form, env, out) -> i64

;;; Code:

(defconst nelisp-cc-sf-if--source
  '(seq
    ;; Helper: eval ELSE-FORMS with progn semantics.
    ;; forms: *const Sexp, may be nil.  Returns 0 or 1 (error).
    (defun nl_sf_if_else (forms env out)
      (if (= (sexp-tag forms) 0)
          0  ;; empty else -> nil, ok
        (nl_sf_if_else_step
         (extern-call nl_cons_cdr_ptr forms)
         (extern-call nl_cons_car_ptr forms)
         env out)))
    ;; rest: remaining forms after car-form.  car-form: the current form ptr.
    (defun nl_sf_if_else_step (rest car-form env out)
      (if (= (sexp-tag rest) 0)
          ;; car-form is the last else form: eval into out
          (extern-call nelisp_eval_call car-form env out)
        ;; More forms follow: eval car-form for side effects, continue
        (nl_sf_if_else_discard
         (extern-call nl_eval_is_truthy car-form env)
         rest env out)))
    ;; rc: result of side-effect eval (-1=err, else ok).
    (defun nl_sf_if_else_discard (rc rest env out)
      (if (= rc -1) 1 (nl_sf_if_else rest env out)))
    ;; Helper: dispatch on truthy test result.
    ;; truthy: -1=error, 0=nil, 1=truthy.
    ;; rest: (THEN . ELSE-FORMS) = cdr of original args.
    (defun nl_sf_if_branch (truthy rest env out)
      (if (= truthy -1)
          1  ;; test eval error
        (if (= truthy 1)
            ;; truthy: eval THEN (car rest) into out
            (extern-call nelisp_eval_call
                         (extern-call nl_cons_car_ptr rest)
                         env out)
          ;; nil: eval ELSE forms (cdr rest) with progn semantics
          (nl_sf_if_else
           (extern-call nl_cons_cdr_ptr rest)
           env out))))
    ;; Public entry: nl_sf_if(args, env, out) -> i64
    (defun nl_sf_if (args env out)
      ;; args = (TEST . (THEN . ELSE-FORMS))
      (nl_sf_if_branch
       (extern-call nl_eval_is_truthy (extern-call nl_cons_car_ptr args) env)
       (extern-call nl_cons_cdr_ptr args)
       env out)))
  "Phase 47 source for `nl_sf_if' (eval/special_forms.rs sf_if → elisp).

Receives `args' = (TEST . (THEN . ELSE-FORMS)) as *const Sexp,
`env' = opaque *mut Env as *mut c_void, `out' = result *mut Sexp.
All intermediate values threaded as function args (Phase 47 constraint:
no let with runtime values).  Uses `nl_eval_is_truthy' for condition
test and `nelisp_eval_call' for branch/else evaluation.
Returns 0 on success, 1 on error.")

(provide 'nelisp-cc-sf-if)

;;; nelisp-cc-sf-if.el ends here
