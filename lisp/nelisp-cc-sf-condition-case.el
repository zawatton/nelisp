;;; nelisp-cc-sf-condition-case.el --- AOT nl_sf_condition_case swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_condition_case' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_condition_case(args, env):
;;       parts = list_elements(args)         // (VAR PROTECTED CLAUSE...)
;;       var = parts[0] (Symbol or Nil)
;;       protected = parts[1]
;;       handlers = parts[2..]
;;       match eval(protected, env):
;;           Ok(v) => Ok(v)
;;           Err(e):
;;               for handler in handlers:
;;                   if clause_matches(handler[0], e.tag):
;;                       push_frame; bind var → e.signal_data;
;;                       result = eval_body(handler[1..], env);
;;                       pop_frame; return result
;;               Err(e)  // no match — reraise
;;
;; `args' is the raw unevaluated arg list `(VAR PROTECTED CLAUSE...)':
;;   car(args)          = VAR (Symbol naming err binding, or Nil)
;;   car(cdr(args))     = PROTECTED form
;;   cdr(cdr(args))     = CLAUSE list — each `(TAG-OR-LIST . BODY)'.
;;
;; ABI externs used:
;;   nl_cons_car_ptr / nl_cons_cdr_ptr — standard cons walkers.
;;   nelisp_eval_call_with_err: (form, env, out, err_out) → i64
;;     Sibling of `nelisp_eval_call' that writes `signal_data()' to
;;     `*err_out' on rc=1 (so the .o can intercept errors via a side
;;     channel rather than reading the env stash variable).
;;   nl_cc_match_and_bind: (clauses, err_inout, var, env) → i64
;;     Walks `*clauses' looking for a clause whose tag matches `*err_inout';
;;     on match pushes lexical frame + binds `*var' to err + writes BODY
;;     (cdr(clause)) into `*err_inout'.  On no-match re-stashes err into
;;     `nelisp--last-signal-data' for the caller's `consume_stashed_error'.
;;   nelisp_eval_call: standard eval entry — used for handler body eval.
;;   nl_env_pop_frame: matches the push done by `nl_cc_match_and_bind'.
;;
;; Slot usage of the public ABI `(args, env, out, s1)':
;;   args: input list, immutable.
;;   env:  `&mut Env'.
;;   out:  result slot — holds protected form result (Ok path) or the
;;         final handler body form result (Err path).
;;   s1:   scratch.  During protected-form eval: err slot for
;;         `nelisp_eval_call_with_err'.  After `nl_cc_match_and_bind'
;;         success: holds the BODY cons list (overwritten from err) which
;;         we walk for handler body progn.
;;
;; Alignment: every defun has even arity (4 or 6) and every extern-call
;; appears as argument 0 at its call site so rsp ≡ 0 mod 16 at the call.
;;
;; Structure (13 defuns, seq form):
;;   --- Body walk after match ---
;;   nl_sf_cc_ret           (pop-rc body-rc _p2 _p3)              — arity 4
;;   nl_sf_cc_finish        (body-rc env out _pad)                — arity 4
;;   nl_sf_cc_body_step     (eval-rc cdr-body env out)            — arity 4
;;   nl_sf_cc_body_eval     (car-ptr cdr-body env out)            — arity 4
;;   nl_sf_cc_body_cdr      (cdr-body body env out)               — arity 4
;;   nl_sf_cc_body          (body env out _pad)                   — arity 4
;;   --- Match dispatch ---
;;   nl_sf_cc_after_match   (match-rc env out s1 _p5 _p6)         — arity 6
;;   nl_sf_cc_after_eval    (rc clauses-ptr var-ptr env out s1)   — arity 6
;;   nl_sf_cc_dispatch      (clauses-ptr var-ptr protected-ptr env out s1) — arity 6
;;   --- Args destructuring ---
;;   nl_sf_cc_split_rest    (protected-ptr cdr1 var-ptr env out s1) — arity 6
;;   nl_sf_cc_with_var      (var-ptr args cdr1 env out s1)        — arity 6
;;   nl_sf_cc_after_cdr1    (cdr1 args env out s1 _p6)            — arity 6
;;   nl_sf_condition_case   (args env out s1)                     — arity 4

;;; Code:

(defconst nelisp-cc-sf-condition-case--source
  '(seq

    ;;--- Body walk after match (handler progn) ---

    ;; After nl_env_pop_frame: discard pop-rc, return body-rc.
    ;; Arity 4 (even).
    (defun nl_sf_cc_ret (pop-rc body-rc _p2 _p3)
      body-rc)

    ;; body-rc = result of handler body eval (0=Ok or 1=Err).
    ;; Pop frame unconditionally (extern-call FIRST), then return body-rc.
    ;; Arity 4 (even).
    (defun nl_sf_cc_finish (body-rc env out _pad)
      (nl_sf_cc_ret
       (extern-call nl_env_pop_frame env)
       body-rc 0 0))

    ;; After nelisp_eval_call on one body form: rc=0 → recurse on tail.
    ;; rc!=0 → pop frame, return 1 (stash already done by eval).
    ;; Arity 4 (even).
    (defun nl_sf_cc_body_step (eval-rc cdr-body env out)
      (if (= eval-rc 0)
          (nl_sf_cc_body cdr-body env out 0)
        (nl_sf_cc_finish 1 env out 0)))

    ;; car-ptr already fetched. Eval one body form into out (extern-call FIRST).
    ;; Arity 4 (even).
    (defun nl_sf_cc_body_eval (car-ptr cdr-body env out)
      (nl_sf_cc_body_step
       (extern-call nelisp_eval_call car-ptr env out)
       cdr-body env out))

    ;; cdr-body already fetched. Get car-ptr for eval (extern-call FIRST).
    ;; Arity 4 (even).
    (defun nl_sf_cc_body_cdr (cdr-body body env out)
      (nl_sf_cc_body_eval
       (extern-call nl_cons_car_ptr body)
       cdr-body env out))

    ;; Recursive body-eval entry. body: ptr to remaining body cons list.
    ;; If Nil (tag 0) → pop frame + return 0 (last value already in *out).
    ;; Else: fetch cdr first, then car (extern-call FIRST chain).
    ;; Arity 4 (even).
    (defun nl_sf_cc_body (body env out _pad)
      (if (= (sexp-tag body) 0)
          (nl_sf_cc_finish 0 env out 0)
        (nl_sf_cc_body_cdr
         (extern-call nl_cons_cdr_ptr body)
         body env out)))

    ;;--- Match dispatch ---

    ;; After nl_cc_match_and_bind: match-rc=0 = frame pushed, *s1 now holds
    ;; the BODY cons list (overwritten from err).  match-rc=1 = no clause
    ;; matched (helper re-stashed err to nelisp--last-signal-data).
    ;; On match: walk body forms (s1 is the body cons start).
    ;; On miss: return 1 (no frame pushed).
    ;; Arity 6 (even).
    (defun nl_sf_cc_after_match (match-rc env out s1 _p5 _p6)
      (if (= match-rc 0)
          (nl_sf_cc_body s1 env out 0)
        1))

    ;; After nelisp_eval_call_with_err: rc=0 → return 0 (result in *out).
    ;; rc=1 → *s1 contains err sexp; dispatch to match helper (extern-call FIRST).
    ;; Arity 6 (even).
    (defun nl_sf_cc_after_eval (rc clauses-ptr var-ptr env out s1)
      (if (= rc 0)
          0
        (nl_sf_cc_after_match
         (extern-call nl_cc_match_and_bind clauses-ptr s1 var-ptr env)
         env out s1 0 0)))

    ;; Eval the PROTECTED form via nelisp_eval_call_with_err (extern-call FIRST).
    ;; The 4th extern arg `s1' acts as the err slot — *s1 becomes signal_data()
    ;; sexp on rc=1, untouched on rc=0.
    ;; Arity 6 (even).
    (defun nl_sf_cc_dispatch (clauses-ptr var-ptr protected-ptr env out s1)
      (nl_sf_cc_after_eval
       (extern-call nelisp_eval_call_with_err protected-ptr env out s1)
       clauses-ptr var-ptr env out s1))

    ;;--- Args destructuring ---

    ;; cdr1 = cdr(args) already fetched.  We've also got var-ptr =
    ;; car(args).  protected-ptr = car(cdr1) is passed in as arg 0.
    ;; Now we need clauses-ptr = cdr(cdr1) — extern-call FIRST.
    ;; Arity 6 (even).
    (defun nl_sf_cc_split_rest (protected-ptr cdr1 var-ptr env out s1)
      (nl_sf_cc_dispatch
       (extern-call nl_cons_cdr_ptr cdr1)
       var-ptr protected-ptr env out s1))

    ;; var-ptr fetched, cdr1 in hand.  Now fetch protected-ptr = car(cdr1)
    ;; (extern-call FIRST), then delegate to split_rest.
    ;; Arity 6 (even).
    (defun nl_sf_cc_with_var (var-ptr args cdr1 env out s1)
      (nl_sf_cc_split_rest
       (extern-call nl_cons_car_ptr cdr1)
       cdr1 var-ptr env out s1))

    ;; cdr1 fetched.  Check it is also a Cons (= condition-case has at least
    ;; (VAR PROTECTED)); else return 1 (malformed args, no stash needed —
    ;; caller's consume_stashed_error will fall through to Internal).
    ;; On well-formed args: get var-ptr = car(args) (extern-call FIRST).
    ;; Arity 6 (even).
    (defun nl_sf_cc_after_cdr1 (cdr1 args env out s1 _p6)
      (if (= (sexp-tag cdr1) 7)
          (nl_sf_cc_with_var
           (extern-call nl_cons_car_ptr args)
           args cdr1 env out s1)
        1))

    ;; Public entry: nl_sf_condition_case(args, env, out, s1) → i64
    ;; args: *const Sexp = (VAR PROTECTED CLAUSE...) — must be Cons (tag 7).
    ;; env:  *mut c_void = &mut Env.
    ;; out:  *mut Sexp   = result slot (Nil initially from Rust thin shell).
    ;; s1:   *mut Sexp   = err/body scratch slot (Nil initially).
    ;; Returns: 0=Ok (result in *out), 1=Err (err stashed in env var).
    ;; Empty args (Nil tag 0) → return 1 (malformed).
    ;; Arity 4 (even).
    (defun nl_sf_condition_case (args env out s1)
      (if (= (sexp-tag args) 7)
          (nl_sf_cc_after_cdr1
           (extern-call nl_cons_cdr_ptr args)
           args env out s1 0)
        1)))

  "AOT source for `nl_sf_condition_case'.

13 defuns (seq form) — CPS chain implementing args destructuring,
protected-form eval with err interception, clause matching with var
binding (both delegated to `nl_cc_match_and_bind' Rust helper), handler
body progn-eval, and final frame pop.

Entry chain (success path):
  nl_sf_condition_case
  → (cdr(args) FIRST) → nl_sf_cc_after_cdr1
  → (car(args) FIRST) → nl_sf_cc_with_var
  → (car(cdr1) FIRST) → nl_sf_cc_split_rest
  → (cdr(cdr1) FIRST) → nl_sf_cc_dispatch
  → (nelisp_eval_call_with_err FIRST) → nl_sf_cc_after_eval
  rc=0: return 0
  rc=1: → (nl_cc_match_and_bind FIRST) → nl_sf_cc_after_match
    match-rc=0: → nl_sf_cc_body (handler body progn walk)
      → (cdr(body) FIRST) → nl_sf_cc_body_cdr
      → (car(body) FIRST) → nl_sf_cc_body_eval
      → (nelisp_eval_call FIRST) → nl_sf_cc_body_step → nl_sf_cc_body
      When body Nil: → nl_sf_cc_finish
      → (nl_env_pop_frame FIRST) → nl_sf_cc_ret → body_rc
    match-rc=1: return 1 (no frame, helper already stashed err)

All defuns have even arity; every extern-call is argument 0 at its
call site → body-entry rsp ≡ 0 mod 16 ✓.

Replaces Rust `sf_condition_case' + `clause_matches' + `eval_handler'
(~51 LOC).  New Rust helper `nl_cc_match_and_bind' (~30 LOC) does the
clause-tag matching (`is_error_subtype' shared with the now-unused arms)
plus frame push + var bind + err re-stash on miss.  Net: ~-25 Rust LOC.")

(provide 'nelisp-cc-sf-condition-case)

;;; nelisp-cc-sf-condition-case.el ends here
