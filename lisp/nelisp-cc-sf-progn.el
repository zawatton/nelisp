;;; nelisp-cc-sf-progn.el --- Phase 47 nl_sf_progn swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the `sf_progn' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_progn(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       eval_body(&parts, env)
;;   }
;;
;; `args' is the raw unevaluated argument list for `(progn FORM...)'
;; — a cons list (or Nil for empty progn).  Each form is eval'd in
;; sequence via `nelisp_eval_call'; result is the last form's value.
;;
;; ABI constants used:
;;   SEXP_TAG_NIL  = 0
;;   nl_cons_car_ptr: *const Sexp → i64 (= &car, or 0 on non-Cons)
;;   nl_cons_cdr_ptr: *const Sexp → i64 (= &cdr, or 0 on non-Cons)
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;
;; Alignment notes — "extern-call first" rule:
;;   Every `(extern-call ...)' is passed as the FIRST argument at its
;;   call site so rsp is 0 mod 16 (0 items on stack) when the call
;;   happens.  Continuation functions receive the extern-call result
;;   as their first parameter and do all tag-checks / tail-calls after.
;;
;; Structure (5 defuns):
;;   nl_sf_progn_eval_step (eval-rc cdr env out) — arity 4, even
;;     called after nelisp_eval_call; checks rc and recurses on cdr.
;;   nl_sf_progn_body_step (car-ptr cdr-ptr env out) — arity 4, even
;;     fetches car-ptr (already computed), calls nelisp_eval_call.
;;   nl_sf_progn_get_car (cdr-ptr cdr-of-cdr env out) — arity 4, even
;;     called after nl_cons_cdr_ptr(cdr); gets car of cdr for eval.
;;   nl_sf_progn_step (cdr env out _pad) — arity 4, even
;;     recursive step: if cdr Nil → return 0; else fetch cdr's car.
;;   nl_sf_progn (args env out _pad) — arity 4, even
;;     public entry: empty-list check, then step into args.

;;; Code:

(defconst nelisp-cc-sf-progn--source
  '(seq
    ;; Called after `nelisp_eval_call(car, env, out)' returns.
    ;; eval-rc: return code from nelisp_eval_call (0=Ok, 1=Err).
    ;; cdr-ptr: *const Sexp for the tail of the remaining list.
    ;; Arity 4 (even): eval-rc, cdr-ptr, env, out.
    (defun nl_sf_progn_eval_step (eval-rc cdr-ptr env out)
      (if (= eval-rc 0)
          ;; Recurse on tail.
          (nl_sf_progn_step cdr-ptr env out 0)
        ;; Propagate error.
        1))

    ;; Called with car-ptr already fetched from the cons.
    ;; Calls nelisp_eval_call(car-ptr, env, out) — extern-call FIRST arg ✓.
    ;; Arity 4 (even): car-ptr, cdr-ptr, env, out.
    (defun nl_sf_progn_body_step (car-ptr cdr-ptr env out)
      (nl_sf_progn_eval_step
       (extern-call nelisp_eval_call car-ptr env out)
       cdr-ptr env out))

    ;; Called after nl_cons_cdr_ptr(cdr) — cdr-of-cdr is 1st arg.
    ;; Gets car of cdr for eval.
    ;; Arity 4 (even): cdr-of-cdr, cdr, env, out.
    (defun nl_sf_progn_get_cdr (cdr-of-cdr cdr env out)
      (nl_sf_progn_body_step
       (extern-call nl_cons_car_ptr cdr)
       cdr-of-cdr env out))

    ;; Recursive step.  cdr is the remaining tail cons list.
    ;; If cdr is Nil (tag 0) the last value is already in *out → return 0.
    ;; Otherwise, get car and eval it.
    ;; Arity 4 (even): cdr, env, out, _pad.
    (defun nl_sf_progn_step (cdr env out _pad)
      (if (= (sexp-tag cdr) 0)
          0
        ;; cdr is Cons: fetch cdr-of-cdr first (extern-call FIRST arg) ✓
        (nl_sf_progn_get_cdr
         (extern-call nl_cons_cdr_ptr cdr)
         cdr env out)))

    ;; Public entry: nl_sf_progn(args, env, out, _pad) → i64
    ;; args: *const Sexp = the raw `(FORM ...)' arg list.
    ;; env:  *mut c_void = &mut Env cast.
    ;; out:  *mut Sexp   = result slot.
    ;; _pad: unused alignment pad (makes arity 4 = even).
    ;; Returns: 0=Ok (last form value in *out), 1=Err.
    ;; Empty progn (args is Nil, tag 0) → out stays Nil, returns 0.
    ;; Arity 4 (even): no prologue sub rsp → no needs-align double-sub.
    (defun nl_sf_progn (args env out _pad)
      (if (= (sexp-tag args) 0)
          ;; Empty progn → return nil.
          0
        ;; Non-empty: get cdr first (extern-call FIRST) then eval car.
        (nl_sf_progn_get_cdr
         (extern-call nl_cons_cdr_ptr args)
         args env out))))
  "Phase 47 source for `nl_sf_progn' (eval/special_forms.rs sf_progn → elisp).

Five defuns (seq form):
  `nl_sf_progn_eval_step (eval-rc cdr-ptr env out)' — arity-4 continuation
    after nelisp_eval_call; propagates error or recurses on tail.
  `nl_sf_progn_body_step (car-ptr cdr-ptr env out)' — arity-4 helper
    calls nelisp_eval_call (extern-call FIRST), delegates to eval_step.
  `nl_sf_progn_get_cdr (cdr-of-cdr cdr env out)' — arity-4 helper
    called after nl_cons_cdr_ptr; then calls nl_cons_car_ptr (FIRST) to
    get the car for eval.
  `nl_sf_progn_step (cdr env out _pad)' — arity-4 recursive entry
    checks if cdr is Nil (done) or fetches cdr-of-cdr (FIRST) to recurse.
  `nl_sf_progn (args env out _pad)' — arity-4 public entry
    empty-progn shortcut or delegates to nl_sf_progn_get_cdr.

Alignment: every extern-call is argument 0 at its call site → rsp is
0 mod 16 when the extern-call happens.")

(provide 'nelisp-cc-sf-progn)

;;; nelisp-cc-sf-progn.el ends here
