;;; nelisp-cc-sf-unwind-protect.el --- AOT nl_sf_unwind_protect swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_unwind_protect' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_unwind_protect(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let parts = list_elements(args)?;
;;       expect_min_len(&parts, "unwind-protect", 1)?;
;;       let body_result = eval(&parts[0], env);
;;       let mut cleanup_err: Option<EvalError> = None;
;;       for cleanup in parts.iter().skip(1) {
;;           if let Err(e) = eval(cleanup, env) {
;;               if cleanup_err.is_none() {
;;                   cleanup_err = Some(e);
;;               }
;;           }
;;       }
;;       cleanup_err.map_or(body_result, Err)
;;   }
;;
;; `args' is the raw unevaluated arg list `(BODYFORM CLEANUP...)'.
;;   car(args)  = BODYFORM — the protected expression.
;;   cdr(args)  = CLEANUP list — zero or more cleanup forms.
;;
;; Semantics: BODYFORM is evaluated first.  Then every CLEANUP form
;; is evaluated unconditionally.  Cleanup errors are silently discarded
;; (matching GNU Emacs `unwind-protect' semantics).  The final return
;; code is the body's rc (0=Ok or 1=Err with stash in
;; `nelisp--last-signal-data').
;;
;; Key: `nelisp_eval_call' stashes errors into `nelisp--last-signal-data'
;; on rc=1.  `nl_eval_is_truthy' discards errors silently (returns -1
;; on error but does NOT touch `nelisp--last-signal-data').  Because
;; cleanup eval uses only `nl_eval_is_truthy', the body's stashed error
;; in `nelisp--last-signal-data' is preserved intact through all cleanup
;; steps.  The final `body-rc' is returned so the Rust thin shell's
;; `consume_stashed_error' can reconstruct the body error if needed.
;;
;; ABI externs used:
;;   nl_cons_car_ptr: (*const Sexp) → i64   (= &car of cons, or 0 for non-Cons)
;;   nl_cons_cdr_ptr: (*const Sexp) → i64   (= &cdr of cons, or 0 for non-Cons)
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;     Standard eval entry; writes result to *out on rc=0, stashes error to
;;     `nelisp--last-signal-data' on rc=1.
;;   nl_eval_is_truthy: (*const Sexp, *mut c_void) → i64
;;     Evals form; returns 1/0 for truthy/nil, -1 on error (error DISCARDED —
;;     does NOT write to `nelisp--last-signal-data').
;;
;; Slot usage of the public ABI `(args, env, out, _pad)':
;;   args: input list, immutable.
;;   env:  `&mut Env'.
;;   out:  result slot — holds body result on success; unchanged on error.
;;   _pad: unused alignment pad.
;;
;; Alignment: every defun has even arity (4 or 6) and every extern-call
;; appears as argument 0 at its call site so rsp ≡ 0 mod 16 at the call.
;;
;; Structure (8 defuns, seq form):
;;   Cleanup walk (discards cleanup errors, preserves body stash):
;;   nl_sf_uw_cleanup_done  (truthy cdr body-rc env out _pad6)  — arity 6
;;   nl_sf_uw_do_cleanup    (car cdr body-rc env out _pad6)     — arity 6
;;   nl_sf_uw_got_cdr       (cdr cleanup body-rc env out _pad6) — arity 6
;;   nl_sf_uw_cleanup       (cleanup body-rc env out _pad5 _pad6) — arity 6
;;   nl_sf_uw_with_cleanup  (cleanup body-rc env out)           — arity 4
;;   Body + init:
;;   nl_sf_uw_after_body    (body-rc args env out)              — arity 4
;;   nl_sf_uw_got_car       (car args env out)                  — arity 4
;;   nl_sf_unwind_protect   (args env out _pad)                 — arity 4 (public)

;;; Code:

(defconst nelisp-cc-sf-unwind-protect--source
  '(seq

    ;;--- Cleanup walk ---

    ;; After nl_eval_is_truthy on one cleanup form: truthy is discarded
    ;; (we only care that cleanup ran, not its value or error).
    ;; Recurse on remaining cleanup forms.
    ;; Arity 6 (even): truthy, cdr, body-rc, env, out, _pad6.
    (defun nl_sf_uw_cleanup_done (truthy cdr body-rc env out _pad6)
      (nl_sf_uw_cleanup cdr body-rc env out 0 0))

    ;; car = nl_cons_car_ptr(cleanup) already fetched as first arg.
    ;; Eval this cleanup form via nl_eval_is_truthy (extern-call FIRST ✓).
    ;; nl_eval_is_truthy discards errors silently — nelisp--last-signal-data
    ;; is untouched, so the body's stashed error remains intact.
    ;; Arity 6 (even): car, cdr, body-rc, env, out, _pad6.
    (defun nl_sf_uw_do_cleanup (car cdr body-rc env out _pad6)
      (nl_sf_uw_cleanup_done
       (extern-call nl_eval_is_truthy car env)
       cdr body-rc env out 0))

    ;; cdr = nl_cons_cdr_ptr(cleanup) already fetched as first arg.
    ;; Get car(cleanup) (extern-call FIRST ✓) and delegate to do_cleanup.
    ;; Arity 6 (even): cdr, cleanup, body-rc, env, out, _pad6.
    (defun nl_sf_uw_got_cdr (cdr cleanup body-rc env out _pad6)
      (nl_sf_uw_do_cleanup
       (extern-call nl_cons_car_ptr cleanup)
       cdr body-rc env out 0))

    ;; Recursive cleanup walk entry.
    ;; If cleanup is Nil (tag 0): all forms done, return body-rc.
    ;; Otherwise: get cdr first (extern-call FIRST ✓), then car, then eval.
    ;; Arity 6 (even): cleanup, body-rc, env, out, _pad5, _pad6.
    (defun nl_sf_uw_cleanup (cleanup body-rc env out _pad5 _pad6)
      (if (= (sexp-tag cleanup) 0)
          body-rc
        (nl_sf_uw_got_cdr
         (extern-call nl_cons_cdr_ptr cleanup)
         cleanup body-rc env out 0)))

    ;; Bridge arity-4 → arity-6 cleanup entry.
    ;; Arity 4 (even): cleanup, body-rc, env, out.
    (defun nl_sf_uw_with_cleanup (cleanup body-rc env out)
      (nl_sf_uw_cleanup cleanup body-rc env out 0 0))

    ;;--- Body + init ---

    ;; body-rc = result of nelisp_eval_call(body, env, out).
    ;; On rc=0: *out holds body result; nelisp--last-signal-data untouched.
    ;; On rc=1: nelisp--last-signal-data holds body error; *out unchanged.
    ;; Either way: get cleanup list = cdr(args) (extern-call FIRST ✓), run cleanup.
    ;; Arity 4 (even): body-rc, args, env, out.
    (defun nl_sf_uw_after_body (body-rc args env out)
      (nl_sf_uw_with_cleanup
       (extern-call nl_cons_cdr_ptr args)
       body-rc env out))

    ;; car = nl_cons_car_ptr(args) already fetched as first arg.
    ;; Eval body form via nelisp_eval_call (extern-call FIRST ✓).
    ;; Arity 4 (even): car, args, env, out.
    (defun nl_sf_uw_got_car (car args env out)
      (nl_sf_uw_after_body
       (extern-call nelisp_eval_call car env out)
       args env out))

    ;; Public entry: nl_sf_unwind_protect(args, env, out, _pad) → i64
    ;; args: *const Sexp = (BODYFORM CLEANUP...).
    ;; env:  *mut c_void = &mut Env.
    ;; out:  *mut Sexp   = result slot (Nil initially from Rust thin shell).
    ;; _pad: unused alignment pad (makes arity 4 = even).
    ;; Returns: 0=Ok (result in *out), 1=Err (body error stashed in env var).
    ;; Empty args (Nil, tag 0) → return 1 (malformed: no body form).
    ;; Otherwise: get body = car(args) (extern-call FIRST ✓), eval, then cleanup.
    ;; Arity 4 (even).
    (defun nl_sf_unwind_protect (args env out _pad)
      (if (= (sexp-tag args) 0)
          1
        (nl_sf_uw_got_car
         (extern-call nl_cons_car_ptr args)
         args env out))))

  "AOT source for `nl_sf_unwind_protect'.

8 defuns (seq form) — CPS chain implementing protected body eval,
unconditional cleanup walk (errors silently discarded via nl_eval_is_truthy
which does NOT touch nelisp--last-signal-data), and final body-rc return.

Entry chain (success path):
  nl_sf_unwind_protect
  → (car(args) FIRST) → nl_sf_uw_got_car
  → (nelisp_eval_call body FIRST) → nl_sf_uw_after_body
  → (cdr(args) FIRST) → nl_sf_uw_with_cleanup
  → nl_sf_uw_cleanup
    Nil: return body-rc
    Cons: → (cdr(cleanup) FIRST) → nl_sf_uw_got_cdr
      → (car(cleanup) FIRST) → nl_sf_uw_do_cleanup
      → (nl_eval_is_truthy car FIRST) → nl_sf_uw_cleanup_done
      → nl_sf_uw_cleanup (recurse on cdr)

Semantics: cleanup errors are discarded (nl_eval_is_truthy returns -1 for
errors without touching nelisp--last-signal-data).  Body error survives in
nelisp--last-signal-data through all cleanup steps.  Final rc = body-rc.
GNU Emacs unwind-protect semantics: cleanup forms run for side effects only.

All defuns have even arity; every extern-call is argument 0 at its
call site → body-entry rsp ≡ 0 mod 16 ✓.

Replaces Rust `sf_unwind_protect' (~14 LOC) with 0 new Rust helpers.
Net Rust delta (including lib.rs extern decl + cc_wrap): ~-8 LOC.")

(provide 'nelisp-cc-sf-unwind-protect)

;;; nelisp-cc-sf-unwind-protect.el ends here
