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
;; Handler-body materialised-view hazard: carry the body CONS itself across
;; `nelisp_eval_call', then take its cdr only after eval returns.
;; `nl_cons_cdr_ptr' materialises a fresh unrooted view for an immediate cdr
;; (including Nil), so a cdr view must not survive eval's possible collection.
;; The first BODY arrives through arena scratch `s1', rather than as the real
;; child box used by the other fixed walkers.  Clone that CONS word once into
;; the dynamic root stack, so its stable 32-byte view also survives the eval;
;; the enclosing condition-case form already keeps the CONS payload alive.
;;
;; Structure (19 defuns, seq form):
;;   --- Body walk after match ---
;;   nl_sf_cc_ret           (pop-rc body-rc _p2 _p3)              — arity 4
;;   nl_sf_cc_released      (release-rc body-rc env out)          — arity 4
;;   nl_sf_cc_finish        (body-rc root-mark env out)           — arity 4
;;   nl_sf_cc_body_step     (eval-rc body root-mark env out _p6)  — arity 6
;;   nl_sf_cc_body_eval     (car-ptr body root-mark env out _p6)  — arity 6
;;   nl_sf_cc_body_walk     (body root-mark env out _p5 _p6)      — arity 6
;;   nl_sf_cc_body_cloned   (clone-rc root-slot root-mark env out _p6) — arity 6
;;   nl_sf_cc_body_slot     (root-slot body root-mark env out _p6) — arity 6
;;   nl_sf_cc_body_mark     (root-mark body env out)              — arity 4
;;   nl_sf_cc_body          (body env out _pad)                   — arity 4
;;   --- Match dispatch ---
;;   nl_sf_cc_after_match   (match-rc env out s1 _p5 _p6)         — arity 6
;;   nl_sf_cc_after_success (succ-rc env out s1 _p5 _p6)          — arity 6
;;   nl_sf_cc_try_success   (clone-rc clauses-ptr var-ptr env out s1) — arity 6
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
    ;; Doc 215 word ABI.  `nl_sf_condition_case_w' answers the protected
    ;; form's value WORD, a handler body's value WORD, or 0 with the signal
    ;; stashed (no clause matched, or the handler failed).  S1 is the
    ;; caller's scratch slot: the error / success datum, then the handler
    ;; body.  Arity even; each extern-call is the first argument.

    ;; After nl_env_pop_frame (the handler's binding frame): discard pop-rc.
    (defun nl_sf_cc_ret (pop-rc w _p2 _p3)
      w)

    (defun nl_sf_cc_released_w (release-rc w env _pad)
      (nl_sf_cc_ret (extern-call nl_env_pop_frame env) w 0 0))

    (defun nl_sf_cc_finish_w (w root-mark env _pad)
      (nl_sf_cc_released_w (extern-call nl_root_release env root-mark) w env 0))

    ;; The handler body is cloned into a reserved root slot (S1 is scratch)
    ;; and run as a progn.  Its WORD crosses only the root release and the
    ;; frame pop, neither a safepoint (Doc 215 R2).
    (defun nl_sf_cc_body_cloned_w (clone-rc root-slot root-mark env)
      (nl_sf_cc_finish_w (nl_sf_progn_w root-slot env 0 0) root-mark env 0))

    (defun nl_sf_cc_body_slot_w (root-slot body root-mark env)
      (nl_sf_cc_body_cloned_w (extern-call nl_sexp_clone_into body root-slot)
                              root-slot root-mark env))

    (defun nl_sf_cc_body_mark_w (root-mark body env _pad)
      (nl_sf_cc_body_slot_w (extern-call nl_root_reserve env) body root-mark env))

    (defun nl_sf_cc_body_w (body env _pad _pad1)
      (nl_sf_cc_body_mark_w (extern-call nl_root_mark env) body env 0))

    ;; A clause matched: clear the signal flag (and the backtrace snapshot)
    ;; before the handler runs.  No match: the signal stays stashed.
    (defun nl_sf_cc_after_match_w (match-rc env _pad s1)
      (if (= match-rc 0)
          (seq (ptr-write-u64 268435472 0 0)
               (ptr-write-u64 (data-addr nl_bt_snapshot) 0 0)
               (nl_sf_cc_body_w s1 env 0 0))
        0))

    ;; No :success clause: read the protected value's WORD from its rooted
    ;; holder. Matching may allocate views; the original header stays traced.
    (defun nl_sf_cc_after_success_w (succ-rc env pv s1)
      (if (= succ-rc 0)
          (nl_sf_cc_body_w s1 env 0 0)
        (nl_cons_car_word pv)))

    (defun nl_sf_cc_try_success_w (clone-rc clauses-ptr var-ptr env pv s1)
      (nl_sf_cc_after_success_w
       (extern-call nl_cc_success_and_bind clauses-ptr s1 var-ptr env)
       env pv s1))

    (defun nl_sf_cc_after_eval_w (w clauses-ptr var-ptr env pv s1)
      (if (/= w 0)
          (seq
           (nl_abi_word_to_rc (nelisp_cons_construct_w w 3 0) pv)
           (nl_sf_cc_try_success_w (nl_abi_word_to_rc (nl_cons_car_word pv) s1)
                                   clauses-ptr var-ptr env pv s1))
        (nl_sf_cc_after_match_w
         (extern-call nl_cc_match_and_bind clauses-ptr s1 var-ptr env)
         env 0 s1)))

    ;; Keep the saved WORD in a checked root until matching/handler evaluation
    ;; finishes. Only the error datum remains in the legacy S1 view.
    (defun nl_sf_cc_dispatch_w (clauses-ptr var-ptr protected-ptr env _pad s1)
      (let* ((mark (nl_root_mark env)) (pv (nl_root_reserve env)))
        (if (= pv 0)
            (seq (nl_root_release env mark) (nl_abi_fail 1 0))
          (let* ((w (nl_sf_cc_after_eval_w
                     (extern-call nelisp_eval_call_with_err_w protected-ptr env 0 s1)
                     clauses-ptr var-ptr env pv s1)))
            (seq (nl_root_release env mark) w)))))

    (defun nl_sf_cc_split_rest_w (protected-ptr cdr1 var-ptr env _pad s1)
      (nl_sf_cc_dispatch_w (extern-call nl_cons_cdr_word cdr1)
                           var-ptr protected-ptr env 0 s1))

    (defun nl_sf_cc_with_var_w (var-ptr args cdr1 env _pad s1)
      (nl_sf_cc_split_rest_w (extern-call nl_cons_car_word cdr1) cdr1 var-ptr env 0 s1))

    (defun nl_sf_cc_after_cdr1_w (cdr1 args env _pad s1 _p6)
      (if (= (sexp-tag cdr1) 7)
          (nl_sf_cc_with_var_w (extern-call nl_cons_car_word args) args cdr1 env 0 s1)
        (nl_abi_fail 1 0)))

    ;; Own the error/body scratch across protected evaluation, clause matching
    ;; and the handler. Caller scratch is arena storage and may be reused by GC.
    (defun nl_sf_condition_case_w (args env _pad _caller-scratch)
      (let* ((mark (nl_root_mark env)) (s1 (nl_root_reserve env)))
        (if (= s1 0)
            (seq (nl_root_release env mark) (nl_abi_fail 1 0))
          (let* ((w (if (= (sexp-tag args) 7)
                        (nl_sf_cc_after_cdr1_w (nl_cons_cdr_word args) args env 0 s1 0)
                      (nl_abi_fail 1 0))))
            ;; Root release has no safepoint. W remains in the return register
            ;; until the caller publishes it in its own precise root (R2).
            (seq (nl_root_release env mark) w)))))

    ;; (rc, OUT) adapter.
    (defun nl_sf_condition_case (args env out s1)
      (nl_abi_word_to_rc (nl_sf_condition_case_w args env 0 s1) out)))

  "AOT source for `nl_sf_condition_case'.

22 defuns (seq form) — CPS chain implementing args destructuring,
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
  rc=0: → (nl_sexp_clone_into FIRST) → nl_sf_cc_try_success
    → (nl_cc_success_and_bind FIRST) → nl_sf_cc_after_success
    succ-rc=0: → nl_sf_cc_body (common handler body walk below)
    succ-rc=1: return 0
  rc=1: → (nl_cc_match_and_bind FIRST) → nl_sf_cc_after_match
    match-rc=0: → nl_sf_cc_body (common handler body walk below)
    match-rc=1: return 1 (no frame, helper already stashed err)

Common handler body walk (after either match kind pushes a frame):
  nl_sf_cc_body → (nl_root_mark FIRST) → nl_sf_cc_body_mark
  → (nl_root_reserve FIRST) → nl_sf_cc_body_slot
  → (nl_sexp_clone_into FIRST) → nl_sf_cc_body_cloned
  → nl_sf_cc_body_walk
  → (car(body) FIRST) → nl_sf_cc_body_eval
  → (nelisp_eval_call FIRST) → nl_sf_cc_body_step
  → (cdr(body) FIRST, after eval) → nl_sf_cc_body_walk
  When body Nil or eval fails: → nl_sf_cc_finish
  → (nl_root_release FIRST) → nl_sf_cc_released
  → (nl_env_pop_frame FIRST) → nl_sf_cc_ret → body_rc

All defuns have even arity; every extern-call is argument 0 at its
call site → body-entry rsp ≡ 0 mod 16 ✓.

Replaces Rust `sf_condition_case' + `clause_matches' + `eval_handler'
(~51 LOC).  New Rust helper `nl_cc_match_and_bind' (~30 LOC) does the
clause-tag matching (`is_error_subtype' shared with the now-unused arms)
plus frame push + var bind + err re-stash on miss.  Net: ~-25 Rust LOC.")

(provide 'nelisp-cc-sf-condition-case)

;;; nelisp-cc-sf-condition-case.el ends here
