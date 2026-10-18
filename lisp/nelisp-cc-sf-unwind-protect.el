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
;; is evaluated unconditionally.  If BODYFORM completed normally, the
;; first cleanup error is returned with the usual stashed signal data.
;; If BODYFORM already stashed a non-local exit, cleanup forms still run
;; through the stashing evaluator.  A cleanup non-local exit supersedes the
;; body exit, matching Emacs `unwind-protect'; otherwise the saved body exit
;; kind/tag/value is restored.  The final return code is either the first
;; cleanup non-local exit, or the body's rc (0=Ok or 1=Err/throw with the M6
;; arena stash intact).
;;
;; Key: `nelisp_eval_call' stashes errors into `nelisp--last-signal-data'
;; on rc=1.  Cleanup eval after a normal body uses `nelisp_eval_call' into
;; a scratch slot so cleanup errors are stashed and visible to condition-case
;; / the diagnostic printer.  Cleanup eval after a body non-local exit
;; snapshots the M6 stash FLAG/TAG/VAL slots, uses `nelisp_eval_call', and
;; restores the body stash only when the cleanup completed normally.
;;
;; ABI externs used:
;;   nl_cons_car_ptr: (*const Sexp) → i64   (= &car of cons, or 0 for non-Cons)
;;   nl_cons_cdr_ptr: (*const Sexp) → i64   (= &cdr of cons, or 0 for non-Cons)
;;   nl_sexp_clone_into: (*const Sexp, *mut Sexp) → ()
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;     Standard eval entry; writes result to *out on rc=0, stashes error to
;;     `nelisp--last-signal-data' on rc=1.
;;   nl_eval_is_truthy: (*const Sexp, *mut c_void) → i64
;;     Evals form; returns 1/0 for truthy/nil, -1 on error.  Used by related
;;     special forms, not by this cleanup path because cleanup exits must stay
;;     visible.
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
;; Structure (12 defuns, seq form):
;;   Cleanup walk (normal body: propagates cleanup error;
;;                 body exit: cleanup can override; otherwise preserve stash):
;;   nl_sf_uw_cleanup_done  (truthy cdr body-rc env out _pad6)  — arity 6
;;   nl_sf_uw_cleanup_evaled (cleanup-rc cdr body-rc env out _pad6) — arity 6
;;   nl_sf_uw_do_cleanup_preserve (scratch car cdr body-rc env out) — arity 6
;;   nl_sf_uw_cleanup_after_body_exit (cleanup-rc flag-save tag-save val-save cdr body-rc env out) — arity 8
;;   nl_sf_uw_do_cleanup_body_exit (flag-save tag-save val-save car cdr body-rc env out) — arity 8
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

    ;; After a normally completed cleanup following a body non-local exit:
    ;; cleanup's value is discarded; recurse on remaining cleanup forms.
    ;; Arity 6 (even): truthy, cdr, body-rc, env, out, _pad6.
    (defun nl_sf_uw_cleanup_done (truthy cdr body-rc env out _pad6)
      (nl_sf_uw_cleanup cdr body-rc env out 0 0))

    ;; After nelisp_eval_call on a cleanup form for a normal body:
    ;; rc=0 continues to the next cleanup, rc=1 propagates the stashed
    ;; cleanup error and leaves *out holding the protected body's value.
    ;; Arity 6 (even): cleanup-rc, cdr, body-rc, env, out, _pad6.
    (defun nl_sf_uw_cleanup_evaled (cleanup-rc cdr body-rc env out _pad6)
      (if (= cleanup-rc 0)
          (nl_sf_uw_cleanup cdr body-rc env out 0 0)
        cleanup-rc))

    ;; scratch = temporary result slot.  Eval this cleanup form via
    ;; nelisp_eval_call so cleanup errors are stashed normally.
    ;; Arity 6 (even): scratch, car, cdr, body-rc, env, out.
    (defun nl_sf_uw_do_cleanup_preserve (scratch car cdr body-rc env out)
      (nl_sf_uw_cleanup_evaled
       (extern-call nelisp_eval_call car env scratch)
       cdr body-rc env out 0))

    ;; After evaluating a cleanup while the body already has a non-local exit:
    ;; cleanup-rc=0 restores the body FLAG/TAG/VAL and continues the cleanup
    ;; walk; cleanup-rc!=0 keeps the cleanup's freshly stashed exit, so cleanup
    ;; throws/errors override the protected body's exit.
    ;; Arity 8 (even): cleanup-rc, flag-save, tag-save, val-save, cdr, body-rc,
    ;; env, out.
    (defun nl_sf_uw_cleanup_after_body_exit
        (cleanup-rc flag-save tag-save val-save cdr body-rc env out)
      (if (= cleanup-rc 0)
          (seq
           (nl_sexp_clone_into tag-save 268435480)
           (nl_sexp_clone_into val-save 268435512)
           (ptr-write-u64 268435472 0 flag-save)
           (dealloc-bytes tag-save 32 8)
           (dealloc-bytes val-save 32 8)
           (nl_sf_uw_cleanup cdr body-rc env out 0 0))
        (seq
         (dealloc-bytes tag-save 32 8)
         (dealloc-bytes val-save 32 8)
         cleanup-rc)))

    ;; Evaluate cleanup for an already non-locally exiting body.  The caller
    ;; saved the body stash before this call; cleanup_after_body_exit restores
    ;; it only when cleanup returns normally.
    ;; Arity 8 (even): flag-save, tag-save, val-save, car, cdr, body-rc, env,
    ;; out.
    (defun nl_sf_uw_do_cleanup_body_exit
        (flag-save tag-save val-save car cdr body-rc env out)
      (let* ((scratch (alloc-bytes 32 8)))
        (nl_sf_uw_cleanup_after_body_exit
         (extern-call nelisp_eval_call car env scratch)
         flag-save tag-save val-save cdr body-rc env out)))

    ;; car = nl_cons_car_ptr(cleanup) already fetched as first arg.
    ;; If body was normal, evaluate cleanup through nelisp_eval_call so
    ;; cleanup errors propagate.  If body already non-locally exited, save its
    ;; stash before cleanup and restore it only when cleanup completes normally.
    ;; Arity 6 (even): car, cdr, body-rc, env, out, _pad6.
    (defun nl_sf_uw_do_cleanup (car cdr body-rc env out _pad6)
      (if (= body-rc 0)
          (let* ((scratch (alloc-bytes 32 8)))
            (nl_sf_uw_do_cleanup_preserve scratch car cdr body-rc env out))
        (let* ((flag-save (ptr-read-u64 268435472 0))
               (tag-save (alloc-bytes 32 8))
               (val-save (alloc-bytes 32 8)))
          (seq
           (nl_sexp_clone_into 268435480 tag-save)
           (nl_sexp_clone_into 268435512 val-save)
           (nl_sf_uw_do_cleanup_body_exit
            flag-save tag-save val-save car cdr body-rc env out)))))

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
         (extern-call nl_cons_cdr_word cleanup)
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
       (extern-call nl_cons_cdr_word args)
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

12 defuns (seq form) — CPS chain implementing protected body eval,
unconditional cleanup walk, and final rc selection.  Cleanup exits are
propagated via nelisp_eval_call.  When the protected body already has a
non-local exit in flight, cleanup may override it; otherwise the saved M6
FLAG/TAG/VAL stash is restored so the body exit survives.

Entry chain (success path):
  nl_sf_unwind_protect
  → (car(args) FIRST) → nl_sf_uw_got_car
  → (nelisp_eval_call body FIRST) → nl_sf_uw_after_body
  → (cdr(args) FIRST) → nl_sf_uw_with_cleanup
  → nl_sf_uw_cleanup
    Nil: return body-rc
    Cons: → (cdr(cleanup) FIRST) → nl_sf_uw_got_cdr
      → (car(cleanup) FIRST) → nl_sf_uw_do_cleanup
        body-rc=0:
          → (nelisp_eval_call cleanup FIRST) → nl_sf_uw_cleanup_evaled
          → rc=0 recurse, rc=1 return cleanup rc
        body-rc=1:
          → save body stash FLAG/TAG/VAL
          → (nelisp_eval_call cleanup FIRST)
          → cleanup rc=0 restores body stash and recurses
          → cleanup rc=1 keeps cleanup stash and returns rc=1

Semantics: cleanup errors after a normal body propagate normally.  Body error
or throw survives in the M6 arena stash through normally returning cleanup
steps.  A cleanup error or throw overrides the body exit.  Final rc = cleanup
rc on cleanup non-local exit, otherwise body-rc.

All defuns have even arity; every extern-call is argument 0 at its
call site → body-entry rsp ≡ 0 mod 16 ✓.

Replaces Rust `sf_unwind_protect' (~14 LOC) with 0 new Rust helpers.
Net Rust delta (including lib.rs extern decl + cc_wrap): ~-8 LOC.")

(provide 'nelisp-cc-sf-unwind-protect)

;;; nelisp-cc-sf-unwind-protect.el ends here
