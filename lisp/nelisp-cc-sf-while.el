;;; nelisp-cc-sf-while.el --- Phase 47 nl_sf_while swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the `sf_while' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_while(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       expect_min_len(&parts, "while", 1)?;
;;       loop {
;;           let cond = eval(&parts[0], env)?;
;;           if !is_truthy(&cond) { return Ok(Sexp::Nil); }
;;           for f in parts.iter().skip(1) { eval(f, env)?; }
;;       }
;;   }
;;
;; ABI:
;;   nl_sf_while(args: *const Sexp, env: *mut c_void, out: *mut Sexp) -> i64
;;   args = (TEST . BODY-FORMS)
;;   Returns 0 on success (out stays nil), 1 on error.
;;
;; Phase 47 constraint: `let' can only bind integer constants.
;; All intermediate values are threaded as function parameters.
;;
;; Strategy: test-ptr = car(args), body-ptr = cdr(args).
;; Loop: eval test via nl_eval_is_truthy; if nil -> done; if truthy ->
;; eval body forms for side effects recursively, then repeat.

;;; Code:

(defconst nelisp-cc-sf-while--source
  '(seq
    ;; Body evaluator: eval all forms in body-ptr for side effects.
    ;; Returns 0 on success, 1 on error.
    (defun nl_sf_while_body (body-ptr env)
      (if (= (sexp-tag body-ptr) 0)
          0  ;; empty body, done
        (nl_sf_while_body_step
         (extern-call nl_cons_cdr_ptr body-ptr)
         (extern-call nl_eval_is_truthy (extern-call nl_cons_car_ptr body-ptr) env)
         env)))
    ;; rest: remaining body forms.  rc: result of current form eval.
    (defun nl_sf_while_body_step (rest rc env)
      (if (= rc -1) 1 (nl_sf_while_body rest env)))
    ;; Main loop: truthy = result of test eval.
    ;; test-ptr: *const Sexp.  body-ptr: *const Sexp.
    (defun nl_sf_while_loop (truthy test-ptr body-ptr env out)
      (if (= truthy -1)
          1  ;; error evaluating test
        (if (= truthy 0)
            0  ;; condition nil: loop done (out stays nil)
          ;; truthy: eval body, then recurse
          (nl_sf_while_after_body
           (nl_sf_while_body body-ptr env)
           test-ptr body-ptr env out))))
    ;; body-rc: result of body evaluation.
    (defun nl_sf_while_after_body (body-rc test-ptr body-ptr env out)
      (if (= body-rc 0)
          (nl_sf_while_loop
           (extern-call nl_eval_is_truthy test-ptr env)
           test-ptr body-ptr env out)
        1))  ;; body error
    ;; Public entry: nl_sf_while(args, env, out) -> i64
    ;; args = (TEST . BODY-FORMS)
    (defun nl_sf_while (args env out)
      (nl_sf_while_loop
       (extern-call nl_eval_is_truthy (extern-call nl_cons_car_ptr args) env)
       (extern-call nl_cons_car_ptr args)
       (extern-call nl_cons_cdr_ptr args)
       env out)))
  "Phase 47 source for `nl_sf_while' (eval/special_forms.rs sf_while → elisp).

Receives `args' = (TEST . BODY-FORMS) as *const Sexp,
`env' = opaque *mut Env as *mut c_void, `out' = result *mut Sexp.
Mutually recursive loop: nl_sf_while_loop tests condition, on truthy
delegates to nl_sf_while_body for side-effect eval, then via
nl_sf_while_after_body recurses to nl_sf_while_loop.
Always returns nil in out.  Returns 0 on success, 1 on error.")

(provide 'nelisp-cc-sf-while)

;;; nelisp-cc-sf-while.el ends here
