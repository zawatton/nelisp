;;; nelisp-cc-sf-function.el --- AOT nl_sf_function swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_function' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_function(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let form = first_arg(args, "function")?;
;;       match &form {
;;           Sexp::Cons(b) if matches!(&b.car, Sexp::Symbol(s) if s == "lambda") => {
;;               let (params, body) = lambda_rest(&form)?;
;;               sf_lambda(&Sexp::cons(params, body), env)
;;           }
;;           _ => Ok(form),
;;       }
;;   }
;;
;; `args' is the raw unevaluated argument list for `(function FORM)' —
;; a Sexp::Cons with car=FORM, cdr=Nil.
;;
;; The AOT body:
;;   1. Checks args is Cons (tag 7).
;;   2. Gets form-ptr = nl_cons_car_ptr(args).
;;   3. If form is not Cons: clone form into out (return it as-is).
;;   4. If form is Cons: get car-of-form-ptr = nl_cons_car_ptr(form-ptr).
;;   5. nl_symbol_is_lambda(car-of-form-ptr): 1 = lambda, 0 = not lambda.
;;   6. If not lambda: clone form into out (return it as-is).
;;   7. If lambda: get cdr(form) = (formals body...) and call nl_sf_lambda
;;      directly (in the same linked archive).
;;
;; ABI constants used (§100.B frozen):
;;   SEXP_TAG_CONS = 7
;;   nl_cons_car_ptr:    *const Sexp → i64 (ptr to car of NlConsBox)
;;   nl_cons_cdr_ptr:    *const Sexp → i64 (ptr to cdr of NlConsBox)
;;   nl_symbol_is_lambda: *const Sexp → i64 (1 if Symbol("lambda"), else 0)
;;   nl_sexp_clone_into: (*const Sexp, *mut Sexp) → ()
;;
;; Alignment (all defuns even-arity; every extern-call is arg position 0):
;;   nl_sf_function(args env out s1)                       — arity 4 (even)
;;   nl_sf_function_got_form(form args env out s1 _pad)    — arity 6 (even)
;;   nl_sf_function_got_car(car form env out s1 _pad)      — arity 6 (even)
;;   nl_sf_function_check_lambda(is-lam form env out s1 _) — arity 6 (even)
;;   nl_sf_function_do_lambda(lambda-args env out s1)      — arity 4 (even)
;;
;; Cross-object call: nl_sf_function_do_lambda calls nl_sf_lambda via
;; extern-call (symbol resolved by dlsym from nelisp-cc-sf-lambda.o
;; which is linked into the same binary).

;;; Code:

(defconst nelisp-cc-sf-function--source
  '(seq
    ;; Doc 215 word ABI.  `nl_sf_function_w' answers the closure WORD for
    ;; #'(lambda ...), the form's own WORD otherwise, or 0 with the signal
    ;; stashed.  FORM comes from `nl_cons_car_ptr' and may be a view of an
    ;; immediate, so it goes through `nl_val_store_word' (Doc 215 R4).
    ;; S1 is the caller's scratch slot for the lambda builder.
    (defun nl_sf_function_do_lambda_w (lambda-args env _pad s1)
      (extern-call nl_sf_lambda_w lambda-args env 0 s1))

    (defun nl_sf_function_check_lambda_w (is-lambda form env _pad s1 _pad1)
      (if (= is-lambda 1)
          (nl_sf_function_do_lambda_w (extern-call nl_cons_cdr_ptr form) env 0 s1)
        (nl_val_store_word form)))

    (defun nl_sf_function_got_car_w (car form env _pad s1 _pad1)
      (nl_sf_function_check_lambda_w (extern-call nl_symbol_is_lambda car) form env 0 s1 0))

    (defun nl_sf_function_got_form_w (form args env _pad s1 _pad1)
      (if (= (sexp-tag form) 7)
          (nl_sf_function_got_car_w (extern-call nl_cons_car_ptr form) form env 0 s1 0)
        (nl_val_store_word form)))

    ;; `(function)' with no argument fails the form.
    (defun nl_sf_function_w (args env _pad s1)
      (if (= (sexp-tag args) 7)
          (nl_sf_function_got_form_w (extern-call nl_cons_car_ptr args) args env 0 s1 0)
        (nl_abi_fail 1 0)))

    ;; (rc, OUT) adapter.
    (defun nl_sf_function (args env out s1)
      (nl_abi_word_to_rc (nl_sf_function_w args env 0 s1) out)))

  "AOT source for `nl_sf_function' (eval/special_forms.rs sf_function → elisp).

Five defuns (seq form).  CPS chain with extern-call at arg-0 each step.

Algorithm:
  1. Check args is Cons (tag 7).
  2. form = nl_cons_car_ptr(args)  [extern-call FIRST ✓].
  3. If form not Cons: nl_sexp_clone_into(form, out) → return 0.
  4. car = nl_cons_car_ptr(form)   [extern-call FIRST ✓].
  5. is-lambda = nl_symbol_is_lambda(car)  [extern-call FIRST ✓].
  6. If not lambda: nl_sexp_clone_into(form, out) → return 0.
  7. lambda-args = nl_cons_cdr_ptr(form)  [extern-call FIRST ✓].
  8. extern-call nl_sf_lambda(lambda-args, env, out, s1) [dlsym from same binary].

ABI: nl_sf_function(args *const Sexp, env *mut c_void, out *mut Sexp, s1 *mut Sexp) → i64.
Called from the thin Rust shell `sf_function' via `crate::elisp_cc_spike::sf_function_call'.")

(provide 'nelisp-cc-sf-function)

;;; nelisp-cc-sf-function.el ends here
