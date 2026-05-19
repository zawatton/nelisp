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
;; `args' is the raw arg list: (TEST BODY...).
;; test-ptr = car(args), body-ptr = cdr(args).
;; Loop: eval test → if truthy, eval body forms, recurse.
;; Result is always nil (out slot unchanged from initial Nil).
;;
;; ABI:
;;   nl_cons_car_ptr: *const Sexp → i64
;;   nl_cons_cdr_ptr: *const Sexp → i64
;;   nl_eval_is_truthy: (*const Sexp, *mut c_void) → i64
;;     1=truthy, 0=nil, -1=eval error
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;
;; Alignment: every extern-call is argument 0 at its call site.
;; CPS chain: each extern-call result arrives as first parameter.
;;
;; Structure (11 defuns):
;;   Body eval (progn-like, discards results):
;;   nl_sf_while_body_done (eval-rc body-cdr test body env out) — arity 6 (even)
;;   nl_sf_while_body_eval (car body-cdr test body env out)     — arity 6 (even)
;;   nl_sf_while_body_cdr  (cdr2 body test body-top env out)    — arity 6 (even)
;;   nl_sf_while_body      (body test body-top env out _pad6)   — arity 6 (even)
;;   Loop kernel:
;;   nl_sf_while_loop      (truthy test body env out _pad6)     — arity 6 (even)
;;   nl_sf_while_iter      (test body env out)                  — arity 4 (even)
;;   Preamble:
;;   nl_sf_while_with_body (body test-ptr env out)              — arity 4 (even)
;;   nl_sf_while_start     (test-ptr args env out)              — arity 4 (even)
;;   nl_sf_while           (args env out _pad)                  — arity 4 (even)

;;; Code:

(defconst nelisp-cc-sf-while--source
  '(seq

    ;;--- Body eval helpers ---

    ;; After eval of one body form: check rc, then advance body list.
    ;; Arity 6 (even): _pad6 / body-top make arity even.
    ;; test: TEST form ptr (for next iteration).
    ;; body-top: top of body list (cdr(args)) for body restart after recurse.
    (defun nl_sf_while_body_done (eval-rc body-cdr test body-top env out)
      (if (= eval-rc 0)
          (nl_sf_while_body body-cdr test body-top env out 0)
        1))

    ;; car = nl_cons_car_ptr(body) already fetched as first arg.
    ;; Eval this body form via nelisp_eval_call (extern-call FIRST ✓).
    ;; Arity 6 (even).
    (defun nl_sf_while_body_eval (car body-cdr test body-top env out)
      (nl_sf_while_body_done
       (extern-call nelisp_eval_call car env out)
       body-cdr test body-top env out))

    ;; body-cdr = nl_cons_cdr_ptr(body) already fetched as first arg.
    ;; Now get car(body) (extern-call FIRST ✓) to eval.
    ;; Arity 6 (even).
    (defun nl_sf_while_body_cdr (body-cdr body test body-top env out)
      (nl_sf_while_body_eval
       (extern-call nl_cons_car_ptr body)
       body-cdr test body-top env out))

    ;; Body loop entry: walk (BODY-FORM...) cons list, eval each, discard result.
    ;; When Nil: all forms done → start next loop iteration via nl_sf_while_iter.
    ;; body-top: the original body list (cdr(args)) for the next iteration.
    ;; Arity 6 (even): _pad6 makes arity even.
    (defun nl_sf_while_body (body test body-top env out _pad6)
      (if (= (sexp-tag body) 0)
          ;; Body exhausted: start next test iteration.
          (nl_sf_while_iter test body-top env out)
        ;; body is Cons: get cdr first (FIRST ✓), then eval car.
        (nl_sf_while_body_cdr
         (extern-call nl_cons_cdr_ptr body)
         body test body-top env out)))

    ;;--- Loop kernel ---

    ;; Dispatch on truthy (result of nl_eval_is_truthy(test, env)).
    ;; test: ptr to TEST form (for recursion).
    ;; body: ptr to body-list cons = cdr(args) (for body eval).
    ;; Arity 6 (even).
    (defun nl_sf_while_loop (truthy test body env out _pad6)
      (if (= truthy -1)
          ;; Test eval errored.
          1
        (if (= truthy 0)
            ;; Test is nil/false: exit loop, return 0 (out stays Nil).
            0
          ;; Test is truthy: eval body forms, then recurse.
          (nl_sf_while_body body test body env out 0))))

    ;; Eval test via nl_eval_is_truthy (extern-call FIRST ✓).
    ;; test: ptr to TEST form sexp.
    ;; body: ptr to body cons list = cdr(args).
    ;; nl_sf_while_loop is arity 6; pass 0 as _pad6.
    ;; Arity 4 (even).
    (defun nl_sf_while_iter (test body env out)
      (nl_sf_while_loop
       (extern-call nl_eval_is_truthy test env)
       test body env out 0))

    ;; body = cdr(args) already fetched as first arg.
    ;; test-ptr = car(args) already available as second arg.
    ;; Delegate to nl_sf_while_iter.
    ;; Arity 4 (even).
    (defun nl_sf_while_with_body (body test-ptr env out)
      (nl_sf_while_iter test-ptr body env out))

    ;; test-ptr = car(args) already fetched as first arg.
    ;; Now fetch body = cdr(args) (extern-call FIRST ✓) and delegate.
    ;; Arity 4 (even).
    (defun nl_sf_while_start (test-ptr args env out)
      (nl_sf_while_with_body
       (extern-call nl_cons_cdr_ptr args)
       test-ptr env out))

    ;; Public entry: nl_sf_while(args, env, out, _pad) → i64
    ;; args: *const Sexp = (TEST BODY...).
    ;; env:  *mut c_void.
    ;; out:  *mut Sexp   = result (always stays Nil; while returns nil).
    ;; _pad: unused alignment pad (makes arity 4 = even).
    ;; Returns 0=Ok, 1=Err.
    ;; Empty args (Nil) → 0 immediately.
    ;; Else: get test = car(args) (extern-call FIRST ✓) and delegate.
    (defun nl_sf_while (args env out _pad)
      (if (= (sexp-tag args) 0)
          0
        (nl_sf_while_start
         (extern-call nl_cons_car_ptr args)
         args env out))))

  "Phase 47 source for `nl_sf_while' (eval/special_forms.rs sf_while → elisp).

Nine defuns (seq form).  CPS chain with one extern-call per step.

Entry chain:
  nl_sf_while → (car(args) FIRST) → nl_sf_while_start
  → (cdr(args) FIRST) → nl_sf_while_with_body
  → nl_sf_while_iter
  → (nl_eval_is_truthy FIRST) → nl_sf_while_loop
    truthy=0: return 0
    truthy=-1: return 1
    truthy=1: → nl_sf_while_body → nl_sf_while_body_cdr
      → (car FIRST) → nl_sf_while_body_eval
      → (nelisp_eval_call FIRST) → nl_sf_while_body_done
      → nl_sf_while_body (recurse on body tail)
      → when body Nil: nl_sf_while_iter (next test eval)

Key fix vs v1: nl_sf_while_iter now takes explicit (test body env out) so
`body = cdr(args)' is correctly threaded through, not confused with cdr(test).
Every extern-call is at argument position 0 → rsp ≡ 0 mod 16 ✓.")

(provide 'nelisp-cc-sf-while)

;;; nelisp-cc-sf-while.el ends here
