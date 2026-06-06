;;; nelisp-cc-sf-if.el --- AOT nl_sf_if swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_if' Rust body in
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
;; `args' is the raw arg list: (TEST THEN ELSE...).
;;
;; ABI:
;;   nl_cons_car_ptr: *const Sexp → i64
;;   nl_cons_cdr_ptr: *const Sexp → i64
;;   nl_eval_is_truthy: (*const Sexp, *mut c_void) → i64
;;     1=truthy, 0=nil, -1=eval error
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;
;; Alignment: every extern-call is argument 0 at its call site.
;; Pattern: (f (extern-call g ...) other-args...) — g called with 0
;; items on stack, result passed as first register arg to f. ✓
;; Never: (= (extern-call g ...) LITERAL) — that pushes LITERAL first.
;;
;; Structure (10 defuns):
;;   Else-branch (progn-like walk):
;;   nl_sf_if_else_rc  (rc cdr env out)  — check eval rc, recurse on cdr
;;   nl_sf_if_else_eval(car cdr env out) — call nelisp_eval_call (FIRST)
;;   nl_sf_if_else_cdr (cdr2 cdr env out)— get car(cdr) (FIRST), then eval
;;   nl_sf_if_else     (els env out _pad)— else-list walk entry
;;   Then-branch:
;;   nl_sf_if_then_eval(form env out _pad)— call nelisp_eval_call (FIRST)
;;   Branch dispatch:
;;   nl_sf_if_branch   (truthy cdr env out)— dispatch on truthy
;;   Preamble:
;;   nl_sf_if_truthy   (truthy cdr env out)— after nl_eval_is_truthy
;;   nl_sf_if_test_ptr (test-ptr cdr env out)— get truthy (FIRST)
;;   nl_sf_if_cdr      (cdr args env out) — get test-ptr=car(args) (FIRST)
;;   nl_sf_if          (args env out _pad)— public entry, arity 4 (even)

;;; Code:

(defconst nelisp-cc-sf-if--source
  '(seq

    ;;--- Else-branch helpers (progn-like sequential eval) ---

    ;; After eval of one else-form: check rc and recurse.
    ;; Arity 4 (even).
    (defun nl_sf_if_else_rc (rc cdr env out)
      (if (= rc 0)
          (nl_sf_if_else cdr env out 0)
        1))

    ;; Eval car via nelisp_eval_call (extern-call FIRST ✓), then check.
    ;; Arity 4 (even).
    (defun nl_sf_if_else_eval (car cdr env out)
      (nl_sf_if_else_rc
       (extern-call nelisp_eval_call car env out)
       cdr env out))

    ;; cdr2 = nl_cons_cdr_ptr(cdr) already fetched (passed as first arg).
    ;; Get car = nl_cons_car_ptr(cdr) (extern-call FIRST ✓), then eval.
    ;; Arity 4 (even).
    (defun nl_sf_if_else_cdr (cdr2 cdr env out)
      (nl_sf_if_else_eval
       (extern-call nl_cons_car_ptr cdr)
       cdr2 env out))

    ;; Walk else-list as progn.
    ;; If Nil → return 0 (out stays Nil); else fetch cdr first (FIRST ✓).
    ;; Arity 4 (even).
    (defun nl_sf_if_else (els env out _pad)
      (if (= (sexp-tag els) 0)
          0
        (nl_sf_if_else_cdr
         (extern-call nl_cons_cdr_ptr els)
         els env out)))

    ;;--- Then-branch helper ---

    ;; Eval the THEN form directly.  nelisp_eval_call returns 0=Ok/1=Err
    ;; which matches our ABI, so we return it directly.
    ;; Body: single extern-call with 0 items on stack ✓.
    ;; Arity 4 (even).
    (defun nl_sf_if_then_eval (form env out _pad)
      (extern-call nelisp_eval_call form env out))

    ;;--- Branch dispatch ---

    ;; Dispatch on truthy (result of nl_eval_is_truthy).
    ;; cdr = cdr of args = (THEN ELSE...) cons.
    ;; Arity 4 (even).
    (defun nl_sf_if_branch (truthy cdr env out)
      (if (= truthy -1)
          ;; Test eval errored.
          1
        (if (= truthy 0)
            ;; Test is nil/false: eval else-list = cdr of cdr.
            (nl_sf_if_else
             (extern-call nl_cons_cdr_ptr cdr)
             env out 0)
          ;; Test is truthy: eval then = car of cdr.
          (nl_sf_if_then_eval
           (extern-call nl_cons_car_ptr cdr)
           env out 0))))

    ;;--- Preamble (thread args CPS-style) ---

    ;; truthy is already computed; pass it with cdr to branch.
    ;; Arity 4 (even).
    (defun nl_sf_if_truthy (truthy cdr env out)
      (nl_sf_if_branch truthy cdr env out))

    ;; test-ptr = car(args) already fetched as first arg.
    ;; Compute truthy = nl_eval_is_truthy(test-ptr, env) (FIRST ✓).
    ;; Arity 4 (even).
    (defun nl_sf_if_test_ptr (test-ptr cdr env out)
      (nl_sf_if_truthy
       (extern-call nl_eval_is_truthy test-ptr env)
       cdr env out))

    ;; cdr = nl_cons_cdr_ptr(args) already fetched as first arg.
    ;; Get test-ptr = nl_cons_car_ptr(args) (extern-call FIRST ✓).
    ;; Arity 4 (even).
    (defun nl_sf_if_cdr (cdr args env out)
      (nl_sf_if_test_ptr
       (extern-call nl_cons_car_ptr args)
       cdr env out))

    ;; Public entry: nl_sf_if(args, env, out, _pad) → i64
    ;; Get cdr-of-args first (extern-call FIRST ✓).
    ;; Arity 4 (even): no prologue sub rsp → no double-sub misalignment.
    (defun nl_sf_if (args env out _pad)
      (nl_sf_if_cdr
       (extern-call nl_cons_cdr_ptr args)
       args env out)))

  "AOT source for `nl_sf_if' (eval/special_forms.rs sf_if → elisp).

Ten defuns (seq form).  Each extern-call is argument 0 at its call site.

Entry chain:
  nl_sf_if → nl_sf_if_cdr → nl_sf_if_test_ptr → nl_sf_if_truthy →
  nl_sf_if_branch → then: nl_sf_if_then_eval
                  → else: nl_sf_if_else → nl_sf_if_else_cdr →
                          nl_sf_if_else_eval → nl_sf_if_else_rc → (recurse)

Alignment fix: every extern-call is the first evaluated argument so rsp
is 0 mod 16 when each extern-call instruction executes.")

(provide 'nelisp-cc-sf-if)

;;; nelisp-cc-sf-if.el ends here
