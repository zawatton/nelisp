;;; nelisp-cc-sf-progn.el --- Phase 47 nl_sf_progn swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the `sf_progn' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_progn(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       eval_body(&args_vec(args)?, env)
;;   }
;;   fn eval_body(body: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
;;       let mut last = Sexp::Nil;
;;       for f in body { last = eval(f, env)?; }
;;       Ok(last)
;;   }
;;
;; ABI:
;;   nl_sf_progn(args: *const Sexp, env: *mut c_void, out: *mut Sexp) -> i64
;;   args = (FORM1 . (FORM2 . ... (FORMn . NIL)))
;;   Returns 0 on success (out = last form value), 1 on error.
;;
;; Phase 47 constraint: `let' can only bind integer constants.
;; All intermediate values are threaded as function parameters.
;;
;; Strategy: walk args list.  For all but the last form use
;; `nl_eval_is_truthy' (eval for side effect; no Sexp scratch needed).
;; For the last form use `nelisp_eval_call' into out.

;;; Code:

(defconst nelisp-cc-sf-progn--source
  '(seq
    ;; Helper called when we have both car-form and cdr already computed.
    ;; car-form: *const Sexp of the current form.
    ;; rest: *const Sexp of the remaining list (may be nil).
    (defun nl_sf_progn_step (rest car-form env out)
      (if (= (sexp-tag rest) 0)
          ;; car-form is the last: eval into out
          (extern-call nelisp_eval_call car-form env out)
        ;; More forms: eval car-form for side effect, continue
        (nl_sf_progn_discard
         (extern-call nl_eval_is_truthy car-form env)
         rest env out)))
    ;; rc: result of side-effect eval.  rest: remaining list.
    (defun nl_sf_progn_discard (rc rest env out)
      (if (= rc -1) 1 (nl_sf_progn rest env out)))
    ;; Public entry: nl_sf_progn(args, env, out) -> i64
    (defun nl_sf_progn (args env out)
      (if (= (sexp-tag args) 0)
          0  ;; empty body -> nil in out, ok
        (nl_sf_progn_step
         (extern-call nl_cons_cdr_ptr args)
         (extern-call nl_cons_car_ptr args)
         env out))))
  "Phase 47 source for `nl_sf_progn' (eval/special_forms.rs sf_progn → elisp).

Receives `args' = body forms list as *const Sexp,
`env' = opaque *mut Env as *mut c_void, `out' = result *mut Sexp.
Tail-recursively walks body: intermediate forms via `nl_eval_is_truthy'
(side effects only), last form via `nelisp_eval_call' into `out'.
Returns 0 on success, 1 on error.")

(provide 'nelisp-cc-sf-progn)

;;; nelisp-cc-sf-progn.el ends here
