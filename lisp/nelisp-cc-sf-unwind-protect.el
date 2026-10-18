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
    ;; Arity 6 (even): truthy, cdr, body-rc, env, vslot, _pad6.
    (defun nl_sf_uw_cleanup_done (truthy cdr body-rc env vslot _pad6)
      (nl_sf_uw_cleanup cdr body-rc env vslot 0 0))

    ;; After nelisp_eval_call on a cleanup form for a normal body:
    ;; rc=0 continues to the next cleanup, rc=1 propagates the stashed
    ;; cleanup error and leaves *vslot holding the protected body's value.
    ;; Arity 6 (even): cleanup-rc, cdr, body-rc, env, vslot, _pad6.
    (defun nl_sf_uw_cleanup_evaled (cleanup-rc cdr body-rc env vslot _pad6)
      (if (= cleanup-rc 0)
          (nl_sf_uw_cleanup cdr body-rc env vslot 0 0)
        cleanup-rc))

    ;; scratch = temporary result slot.  Eval this cleanup form via
    ;; nelisp_eval_call so cleanup errors are stashed normally.
    ;; Arity 6 (even): scratch, car, cdr, body-rc, env, vslot.
    (defun nl_sf_uw_do_cleanup_preserve (scratch car cdr body-rc env vslot)
      (nl_sf_uw_cleanup_evaled
       (extern-call nelisp_eval_call car env scratch)
       cdr body-rc env vslot 0))

    ;; After evaluating a cleanup while the body already has a non-local exit:
    ;; cleanup-rc=0 restores the body FLAG/TAG/VAL and continues the cleanup
    ;; walk; cleanup-rc!=0 keeps the cleanup's freshly stashed exit, so cleanup
    ;; throws/errors override the protected body's exit.
    ;; Arity 8 (even): cleanup-rc, flag-save, tag-save, val-save, cdr, body-rc,
    ;; env, vslot.
    (defun nl_sf_uw_cleanup_after_body_exit
        (cleanup-rc flag-save tag-save val-save cdr body-rc env vslot)
      (if (= cleanup-rc 0)
          (seq
           (nl_sexp_clone_into tag-save 268435480)
           (nl_sexp_clone_into val-save 268435512)
           (ptr-write-u64 268435472 0 flag-save)
           (dealloc-bytes tag-save 32 8)
           (dealloc-bytes val-save 32 8)
           (nl_sf_uw_cleanup cdr body-rc env vslot 0 0))
        (seq
         (dealloc-bytes tag-save 32 8)
         (dealloc-bytes val-save 32 8)
         cleanup-rc)))

    ;; Evaluate cleanup for an already non-locally exiting body.  The caller
    ;; saved the body stash before this call; cleanup_after_body_exit restores
    ;; it only when cleanup returns normally.
    ;; Arity 8 (even): flag-save, tag-save, val-save, car, cdr, body-rc, env,
    ;; vslot.
    (defun nl_sf_uw_do_cleanup_body_exit
        (flag-save tag-save val-save car cdr body-rc env vslot)
      (let* ((scratch (alloc-bytes 32 8)))
        (nl_sf_uw_cleanup_after_body_exit
         (extern-call nelisp_eval_call car env scratch)
         flag-save tag-save val-save cdr body-rc env vslot)))

    ;; car = nl_cons_car_ptr(cleanup) already fetched as first arg.
    ;; If body was normal, evaluate cleanup through nelisp_eval_call so
    ;; cleanup errors propagate.  If body already non-locally exited, save its
    ;; stash before cleanup and restore it only when cleanup completes normally.
    ;; Arity 6 (even): car, cdr, body-rc, env, vslot, _pad6.
    (defun nl_sf_uw_do_cleanup (car cdr body-rc env vslot _pad6)
      (if (= body-rc 0)
          (let* ((scratch (alloc-bytes 32 8)))
            (nl_sf_uw_do_cleanup_preserve scratch car cdr body-rc env vslot))
        (let* ((flag-save (ptr-read-u64 268435472 0))
               (tag-save (alloc-bytes 32 8))
               (val-save (alloc-bytes 32 8)))
          (seq
           (nl_sexp_clone_into 268435480 tag-save)
           (nl_sexp_clone_into 268435512 val-save)
           (nl_sf_uw_do_cleanup_body_exit
            flag-save tag-save val-save car cdr body-rc env vslot)))))

    ;; cdr = nl_cons_cdr_ptr(cleanup) already fetched as first arg.
    ;; Get car(cleanup) (extern-call FIRST ✓) and delegate to do_cleanup.
    ;; Arity 6 (even): cdr, cleanup, body-rc, env, vslot, _pad6.
    (defun nl_sf_uw_got_cdr (cdr cleanup body-rc env vslot _pad6)
      (nl_sf_uw_do_cleanup
       (extern-call nl_cons_car_ptr cleanup)
       cdr body-rc env vslot 0))

    ;; Recursive cleanup walk entry.
    ;; If cleanup is Nil (tag 0): all forms done, return body-rc.
    ;; Otherwise: get cdr first (extern-call FIRST ✓), then car, then eval.
    ;; Arity 6 (even): cleanup, body-rc, env, vslot, _pad5, _pad6.
    (defun nl_sf_uw_cleanup (cleanup body-rc env vslot _pad5 _pad6)
      (if (= (sexp-tag cleanup) 0)
          body-rc
        (nl_sf_uw_got_cdr
         (extern-call nl_cons_cdr_word cleanup)
         cleanup body-rc env vslot 0)))

    ;; Bridge arity-4 → arity-6 cleanup entry.
    ;; Arity 4 (even): cleanup, body-rc, env, vslot.
    (defun nl_sf_uw_with_cleanup (cleanup body-rc env vslot)
      (nl_sf_uw_cleanup cleanup body-rc env vslot 0 0))

    ;;--- Body + init ---

    ;; body-rc = result of nelisp_eval_call(body, env, vslot).
    ;; On rc=0: *vslot holds body result; nelisp--last-signal-data untouched.
    ;; On rc=1: nelisp--last-signal-data holds body error; *vslot unchanged.
    ;; Either way: get cleanup list = cdr(args) (extern-call FIRST ✓), run cleanup.
    ;; Arity 4 (even): body-rc, args, env, vslot.
    (defun nl_sf_uw_after_body (body-rc args env vslot)
      (nl_sf_uw_with_cleanup
       (extern-call nl_cons_cdr_word args)
       body-rc env vslot))

    ;; car = nl_cons_car_ptr(args) already fetched as first arg.
    ;; Eval body form via nelisp_eval_call (extern-call FIRST ✓).
    ;; Arity 4 (even): car, args, env, vslot.
    ;; Doc 215 word ABI.  `nl_sf_unwind_protect_w' answers the body's value
    ;; WORD, or 0 with the body's (or a cleanup's) signal stashed.  The body
    ;; value must survive the cleanup forms (Doc 215 R2), so it is stored
    ;; in a root slot VSLOT reserved under a root mark for the whole form;
    ;; the cleanup chain above only carries VSLOT.  The result is a fresh
    ;; copy of VSLOT made before the mark is released (no safepoint after).
    (defun nl_sf_uw_stored (w args env vslot)
      (if (= w 0)
          (nl_sf_uw_after_body 1 args env vslot)
        (nl_sf_uw_after_body (nl_abi_word_to_rc w vslot) args env vslot)))

    (defun nl_sf_uw_got_car_w (car args env vslot)
      (nl_sf_uw_stored (extern-call nelisp_eval_call_w car env 0) args env vslot))

    (defun nl_sf_uw_value_w (box vslot)
      (seq (ptr-write-u64 box 0 (ptr-read-u64 vslot 0))
           (ptr-write-u64 box 8 (ptr-read-u64 vslot 8))
           (ptr-write-u64 box 16 (ptr-read-u64 vslot 16))
           (ptr-write-u64 box 24 (ptr-read-u64 vslot 24))
           (nl_val_store_word box)))

    (defun nl_sf_uw_release_w (w env mark _pad)
      (seq (nl_root_release env mark) w))

    (defun nl_sf_uw_finish_w (rc env mark vslot)
      (nl_sf_uw_release_w
       (if (= rc 0) (nl_sf_uw_value_w (alloc-bytes 32 8) vslot) (nl_abi_fail rc 0))
       env mark 0))

    (defun nl_sf_uw_slot_w (args env mark vslot)
      (nl_sf_uw_finish_w
       (nl_sf_uw_got_car_w (extern-call nl_cons_car_ptr args) args env vslot)
       env mark vslot))

    (defun nl_sf_uw_marked_w (args env _pad mark)
      (nl_sf_uw_slot_w args env mark (nl_root_reserve env)))

    ;; `(unwind-protect)' with no body form fails the form.
    (defun nl_sf_unwind_protect_w (args env _pad _pad1)
      (if (= (sexp-tag args) 0)
          (nl_abi_fail 1 0)
        (nl_sf_uw_marked_w args env 0 (nl_root_mark env))))

    ;; (rc, OUT) adapter.
    (defun nl_sf_unwind_protect (args env out _pad)
      (nl_abi_word_to_rc (nl_sf_unwind_protect_w args env 0 0) out)))

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
