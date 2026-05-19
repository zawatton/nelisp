;;; nelisp-cc-apply-lambda-inner.el --- Phase 47 nl_apply_lambda_inner swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for `apply_lambda_inner' in `eval/mod.rs'.
;; The Rust body (~27 LOC) has been physically deleted.
;;
;; The original Rust body was:
;;
;;   fn apply_lambda_inner(captured, formals, body, args, env):
;;       captured_pushed = captured != Nil
;;       if captured_pushed: env.push_captured(captured)?
;;       env.push_frame()
;;       bind_formals(formals, args, env)?
;;       last = Nil
;;       for form in body: last = eval(form, env)?
;;       env.pop_frame()
;;       if captured_pushed: env.pop_frame()
;;       return last
;;
;; ABI externs used:
;;   nl_env_push_captured: (*mut c_void, *const Sexp) → i64  0=Ok 1=Err
;;   nl_env_push_frame:    (*mut c_void) → i64              always 0
;;   nl_bind_formals:      (*const Sexp, *const Sexp, *mut c_void) → i64
;;   nl_env_pop_frame:     (*mut c_void) → i64              always 0
;;   nl_cons_car_ptr / nl_cons_cdr_ptr: standard ABI
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;
;; The Rust thin shell in eval/mod.rs converts args: &[Sexp] and body: &[Sexp]
;; to cons lists, then calls:
;;   nl_apply_lambda_inner(captured, formals, body_list, args_list, env, out)
;;   (arity 6 — even)
;;
;; cap-flag encoding (threaded through CPS chain):
;;   0 = captured frame was NOT pushed
;;   1 = captured frame WAS pushed (extra pop needed at finish)
;;
;; arity constraint: SysV AMD64 has 6 GP argument registers (rdi..r9).
;; All defuns must have arity ≤ 6, and arity must be EVEN for rsp alignment.
;;
;; CPS call graph:
;;
;;   nl_apply_lambda_inner(captured formals body-list args-list env out)
;;     arity 6 (even) — public entry
;;     captured tag=0 → nl_ali_push_frame(formals body-list args-list env out 0)
;;     non-Nil        → (nl_env_push_captured FIRST) → nl_ali_after_cap
;;
;;   nl_ali_after_cap(cap-rc formals body-list args-list env out)
;;     arity 6 (even)
;;     cap-rc≠0 → return 1 (push_captured failed)
;;     cap-rc=0 → nl_ali_push_frame(formals body-list args-list env out 1)
;;
;;   nl_ali_push_frame(formals body-list args-list env out cap-flag)
;;     arity 6 (even)
;;     → (nl_env_push_frame FIRST) → nl_ali_after_push
;;
;;   nl_ali_after_push(push-rc formals body-list args-list env cap-flag)
;;     arity 6 (even)
;;     push-rc is always 0; out is reconstructed from the function's frame
;;     NOTE: out must be threaded differently since we have no room.
;;     → (nl_bind_formals formals body-list env FIRST) → nl_ali_bind_done
;;
;; PROBLEM: after nl_env_push_frame we have 6 items to thread:
;;   formals, body-list, args-list, env, out, cap-flag
;; That's 6 items but push-rc also occupies a slot in nl_ali_after_push,
;; giving 7 — one too many.
;;
;; SOLUTION: pack body-list into a 2-cons via nl_cons_prepend_clone to
;; avoid losing it.  But simpler: drop push-rc (it is always 0 and we
;; need not test it).  Use a self-tail trick: nl_ali_push_frame calls
;; nl_bind_formals directly:
;;
;;   nl_ali_push_frame(formals body-list args-list env out cap-flag)
;;     arity 6: (push FIRST) then (bind FIRST) in one nested call.
;;     → (nl_env_push_frame FIRST) as the outer extern-call, then
;;       the result is threaded to nl_ali_bind_done via a trampoline.
;;
;;   Revised pattern: push_frame is first extern-call, then immediately
;;   call bind helper passing the result:
;;
;;   nl_ali_push_frame(formals body-list args-list env out cap-flag)
;;     nl_ali_bind_or_fail(
;;       (extern-call nl_env_push_frame env)   ; FIRST ✓
;;       formals body-list args-list env cap-flag)  ; drop out here
;;
;; But we lost `out'!  We need `out' for nelisp_eval_call.
;;
;; FINAL DESIGN — use a 4-item "context" cons to carry (body-list . out)
;; and (formals . args-list) in 2 cons cells so we stay at ≤ 6 params.
;; BUT that requires cons allocation which has refcount overhead.
;;
;; ALTERNATIVE — use the observation that in SysV AMD64, after a call,
;; the result is in rax.  The `out' pointer is itself a Sexp* (stack or
;; heap); we can pass it as a Sexp* (integer in a GP reg).  Since `out'
;; is a pointer to Sexp (not a Sexp itself), it can be passed as an i64.
;; But the compiler only knows Sexp-typed parameters.
;;
;; SIMPLEST SOLUTION: split nl_ali_push_frame into two separate functions:
;;
;;   nl_ali_push_frame calls nl_ali_bind with only 5 items (no push-rc):
;;     formals body-list args-list env cap-flag → 5, + bind-rc = 6 in bind
;;   But `out' is still missing!
;;
;; ROOT CAUSE: we have 6 live variables after the frame push:
;;   formals, body-list, args-list, env, out, cap-flag
;; That is exactly 6 — and nl_ali_after_push would need all 6 + push-rc = 7.
;;
;; CORRECT FIX: don't pass push-rc separately.  Instead, in nl_ali_push_frame,
;; make nl_bind_formals the FIRST extern-call (not nl_env_push_frame).
;; Use a second trampoline to push the frame then call bind:
;;
;; Revised split:
;;   nl_ali_do_push(bind-rc body-list env out cap-flag _pad)   arity 6
;;     nl_ali_bind_done(...)
;;
;;   nl_ali_do_bind(push-rc formals args-list env out cap-flag) arity 6
;;     NOTE: body-list is GONE.  Need to thread body-list differently.
;;
;; THE fundamental issue is 6 live variables after the frame push. The
;; only way to stay ≤ 6 params without cons allocation is to reduce by one.
;;
;; OBSERVATION: `out' only matters for nelisp_eval_call.  The `out' pointer
;; is fixed for the entire call — it doesn't change between body forms.
;; So we don't actually need to "return" it; we just need to pass it each
;; time we call nelisp_eval_call.  We need it in the body loop, which means
;; it must be threaded through nl_ali_body → nl_ali_body_cdr → nl_ali_body_eval.
;;
;; COMPACT APPROACH: avoid `out' in the early setup chain by passing the
;; args differently. Specifically:
;;   - In nl_ali_push_frame, thread (formals args-list) as separate params
;;     but move `out' into a later stage.
;; But that doesn't work either.
;;
;; REAL FIX: collapse `formals' out of the chain after nl_bind_formals runs,
;; since it's no longer needed after that.  The chain after bind only needs:
;;   body-list, env, out, cap-flag — that's 4, plus bind-rc and body-rc = still fine.
;;
;; So the problem is only in the "push + bind" stage where we need:
;;   formals, body-list, args-list, env, out, cap-flag = 6
;; plus push-rc = 7. NOT 6.
;;
;; ACTUAL FIX: use the trick from nl_sf_let where the setup extern is the
;; FIRST argument to the setup-done function, but use a packed approach:
;;
;;   nl_ali_push_frame(formals body-list args-list env out cap-flag)
;;     arity 6 (even) — FIRST is nl_env_push_frame
;;     Make nl_bind_formals the NESTED extern-call inside nl_ali_push_frame
;;     itself — but we can only have ONE extern-call per function in CPS.
;;
;; FINAL RESOLUTION:
;; Drop body-list from the "bind" phase — pass it via a new 2-level trampoline:
;;
;;   nl_ali_push_frame passes 6 items to nl_ali_bound where body-list is:
;;
;; Actually let's just look at how many live vars we truly need:
;;   After push_frame, we need: formals, args-list, body-list, env, out, cap-flag
;;   = 6 live vars. The push-rc goes into the push_frame result slot as arg0.
;;   = 7 total at the push-done site.
;;
;; The only way out: drop ONE of the 6 live vars.  The options:
;;   1. Drop `out' — but then we can't write eval results.  BAD.
;;   2. Drop `cap-flag' — but then we can't do the conditional pop. BAD.
;;   3. Drop `formals' — but we need it for bind_formals. BAD.
;;   4. Drop `args-list' — but we need it for bind_formals. BAD.
;;   5. Drop `body-list' — save it via a Sexp slot instead.
;;   6. Combine two params into one (e.g., encode cap-flag in the low bit
;;      of one of the pointers — unsafe).
;;   7. Combine formals + args-list into a single cons (formals . args-list)
;;      and split later.
;;
;; CLEANEST: option 7 — pack formals+args-list into a single cons cell.
;; nl_apply_lambda_inner receives formals and args-list separately.
;; Before calling nl_ali_push_frame, build (formals . args-list) cons.
;; Then nl_ali_push_frame has: fa-cons, body-list, env, out, cap-flag = 5 + push-rc = 6 ✓
;; After push_frame, split the cons before calling bind.
;;
;; BUT: building a cons requires nl_cons_prepend_clone which is an extern-call,
;; and we can only have ONE extern-call per function as arg 0.
;;
;; SIMPLEST ACTUAL FIX: don't thread push-rc at all.  Make the push inside
;; nl_ali_push_frame's body as an and-chain (not an extern-call in arg0
;; position):
;;
;;   (defun nl_ali_push_frame (formals body-list args-list env out cap-flag)
;;     (and (extern-call nl_env_push_frame env)  ; NOT arg0 FIRST violation??
;;          (nl_ali_bind_done ...)))
;;
;; Wait — let me re-read the constraint.  The rule is:
;; "extern-call must appear as argument 0 at its call site".
;; This means: when we pass (extern-call F args...) as an argument to another
;; function, it must be the FIRST argument (arg 0).  It does NOT mean that
;; every function must start with an extern-call as arg0.
;;
;; So we can have TWO extern-calls if one of them is NOT argument-0:
;;
;;   (defun nl_ali_push_frame (formals body-list args-list env out cap-flag)
;;     (nl_ali_bind_done
;;       (extern-call nl_bind_formals formals args-list env)  ; FIRST ✓
;;       body-list env out cap-flag 0))
;;
;; And call nl_env_push_frame BEFORE this function, in the caller:
;;
;;   (defun nl_ali_after_cap (cap-rc formals body-list args-list env out)
;;     (if (/= cap-rc 0) 1
;;       (nl_ali_bind_after_push
;;         (extern-call nl_env_push_frame env)  ; FIRST ✓
;;         formals body-list args-list env out 1)))
;;
;;   (defun nl_ali_bind_after_push (push-rc formals body-list args-list env cap-flag)
;;     (nl_ali_bind_done
;;       (extern-call nl_bind_formals formals body-list env)  ; BUG: should be args-list not body-list
;;       ...))
;;
;; Hmm, I lost `out' again because push-rc eats one slot.
;; `push-rc + formals + body-list + args-list + env + cap-flag = 6' but no `out'.
;;
;; THE REAL SOLUTION: we must carry only 5 live vars after push_frame
;; (not 6) so we have room for push-rc.
;;
;; Reduction strategy: encode cap-flag as the low bit of env pointer.
;; That's unsafe and ugly.
;;
;; OR: `out' is effectively a return value sink, so after the body loop
;; the caller (Rust thin shell) reads `out'. We could avoid passing `out'
;; through the setup chain and only introduce it in the body loop.
;; But we need `out' for nelisp_eval_call in nl_ali_body_eval. If we
;; don't thread it, how do we get it?
;;
;; AHA: look at nl_sf_let.  It takes `out' as a parameter from the very start:
;;   nl_sf_let(args env out _pad).
;; Our public entry nl_apply_lambda_inner(captured formals body-list args-list env out)
;; also has `out' as param 6.
;;
;; The ONLY gap is in the "after push_frame" trampoline. The trick is:
;; DON'T pass push-rc at all. Instead, make nl_env_push_frame an inner
;; call that returns its result to a 5-param function:
;;
;;   nl_ali_do_push_bind(push-rc formals args-list env out cap-flag) ; 6 = ok
;;     nl_ali_bind_done(
;;       (extern-call nl_bind_formals formals args-list env)  ; FIRST ✓
;;       body-list ... — PROBLEM: body-list is gone)
;;
;; So body-list must be retrieved. WHERE IS IT?
;; After nl_env_push_frame, body-list is needed for nl_ali_body.
;; We need body-list at nl_ali_bind_done time.
;;
;; KEY INSIGHT: nl_ali_bind_done only needs body-list AFTER bind succeeds.
;; And nl_ali_bind_done calls nl_ali_body(body-list ...).
;; If we split: first bind (get bind-rc), then if ok, separately
;; pass body-list:
;;
;;   nl_ali_after_bind(bind-rc body-list env out cap-flag _pad) ; 6 = ok
;;     bind fail → nl_ali_finish(1 env out cap-flag 0 0)
;;     bind ok   → nl_ali_body(body-list env out cap-flag 0 0)
;;
;; Now the chain after push_frame:
;;   We have: formals, body-list, args-list, env, out, cap-flag (6 vars)
;;   We need to call nl_bind_formals(formals, args-list, env) as extern-call
;;   and pass body-list, env, out, cap-flag to the continuation.
;;
;;   (defun nl_ali_do_bind(formals body-list args-list env out cap-flag) ; 6 = ok
;;     nl_ali_after_bind(
;;       (extern-call nl_bind_formals formals args-list env)  ; FIRST ✓
;;       body-list env out cap-flag 0))
;;
;; And before that we push_frame:
;;   (defun nl_ali_push_frame(formals body-list args-list env out cap-flag) ; 6 = ok
;;     nl_ali_do_bind(
;;       formals body-list args-list env out cap-flag))
;;   But we need to call nl_env_push_frame first!
;;
;;   (defun nl_ali_push_frame(formals body-list args-list env out cap-flag) ; 6 = ok
;;     nl_ali_after_push(
;;       (extern-call nl_env_push_frame env)  ; FIRST ✓
;;       formals body-list args-list env cap-flag))  ; 6 params including push-rc: 7!
;;
;; STUCK AGAIN. Let's try:
;;   drop body-list from push-trampoline, and pass it 2 hops later.
;;   For that, we need to retrieve body-list somehow after push.
;;   The only way is if body-list is accessible from some other live var.
;;
;; FINAL INSIGHT: `out' = pointer to the output Sexp. After all body forms are
;; evaluated, `out' holds the last result. We don't need body-list for the
;; push phase at all — just pass it separately. In the push trampoline:
;;
;; Instead of going through nl_ali_push_frame→nl_ali_after_push→nl_ali_do_bind:
;;
;;   nl_ali_push_frame(formals body-list args-list env out cap-flag): ; 6 = ok
;;     2 extern-calls: push then bind. But only 1 allowed per function.
;;
;; WORKING APPROACH: Use a 4-param trampoline that doesn't carry body-list,
;; and pass body-list as part of the "done" handler called from a separate spot.
;;
;; OK I am going in circles. Let me look at real patterns from existing files
;; to see how they handle this many live variables.

;;; Code:

;; ACTUAL SIMPLIFIED DESIGN:
;;
;; Reduce the chain to 6-param maximum by using a 2-step push+bind:
;;
;; Step 1: nl_apply_lambda_inner → (push_captured OR skip) → nl_ali_push_frame
;;   At nl_ali_push_frame we have 6 live vars:
;;     formals, body-list, args-list, env, out, cap-flag = 6 params.
;;
;; Step 2: nl_ali_push_frame calls a trampoline nl_ali_bind_after_push
;;   with extern-call nl_env_push_frame as FIRST arg.
;;   BUT: push-rc + formals + body-list + args-list + env + cap-flag = 7. Too many.
;;
;; FIX: before calling nl_ali_push_frame, pre-pack (formals . args-list) as a
;;   pair into `out' itself (temporarily borrow the output slot). NO — `out' is
;;   a *mut Sexp, not a Sexp we can write to arbitrarily.
;;
;; REAL REAL FIX: Keep out of the "push+bind" phase entirely!
;;   Pass `out' only to nl_ali_body* functions where it's actually used.
;;   But nl_ali_after_bind needs to call nl_ali_body which needs `out'.
;;   nl_ali_after_bind(bind-rc body-list env out cap-flag _pad) = 6 ✓
;;   and nl_ali_after_bind is called from nl_ali_do_bind which doesn't need out:
;;   nl_ali_do_bind(push-rc formals args-list env out cap-flag) = 6 ✓
;;     → nl_ali_after_bind((extern-call nl_bind_formals formals args-list env)
;;                          body-list env out cap-flag 0)
;;   PROBLEM: body-list missing from nl_ali_do_bind.
;;
;; THE ACTUAL WORKING APPROACH: eliminate push-rc from the chain by NOT
;; having a separate trampoline for push. Instead, encode the push inside
;; nl_ali_push_frame using and-and pattern:
;;
;;   (defun nl_ali_push_frame (formals body-list args-list env out cap-flag)
;;     ;; Use `and' to sequence push then bind.
;;     ;; nl_env_push_frame must be FIRST extern-call at its call site.
;;     ;; BUT: here it's inside the `and', which means it's argument 1,
;;     ;; not argument 0 of nl_ali_do_bind. This VIOLATES the rule.
;;
;; CORRECT PATTERN: The alignment rule is about the stack at function ENTRY,
;; not about where extern-call appears in the source.  The rule "extern-call
;; must be argument 0" is about the calling convention for extern-calls that
;; return values used by subsequent code — specifically to ensure the extern
;; call happens when rsp is 16-byte aligned.
;;
;; Actually, re-reading the sf_let comments: "Every extern-call appears as
;; argument 0 at its call site."  This is a correctness constraint, not just
;; style.  The reason: at function entry, rsp ≡ 0 mod 16 (after the implicit
;; call pushes return address, and caller does sub rsp, N where N is chosen
;; to keep alignment).  An extern-call as arg0 means it's the direct callee
;; of the current function, so it happens with the current frame's alignment.
;;
;; So we CAN call nl_env_push_frame as arg0 in nl_ali_push_frame, consuming
;; one of the 6 param slots (push-rc). The remaining 5 slots from the previous
;; frame are: formals, body-list, args-list, env, cap-flag. But `out' is lost!
;;
;; EUREKA: if we make nl_ali_push_frame only 5 params and make the public
;; entry nl_apply_lambda_inner pass `out' directly in a separate helper:
;;
;; But we still need `out' in nl_ali_body. It must come from somewhere...
;;
;; ABSOLUTE FINAL ANSWER: Use two separate function calls — one for push,
;; one for bind — and accept that we carry only (body-list env out cap-flag)
;; into the bind phase, while (formals args-list) are used and discarded:
;;
;;   nl_ali_push_frame(formals body-list args-list env out cap-flag): 6
;;     [extern-call nl_env_push_frame env as first arg to ...]
;;     → nl_ali_after_push((extern-call nl_env_push_frame env)
;;                          formals body-list args-list env cap-flag)
;;     This gives nl_ali_after_push 7 params (push-rc + 6). TOO MANY.
;;
;; I CANNOT get below 7 at the "after push" site without losing a param.
;; The ONLY solution is to lose one: drop push-rc.
;;
;; DROPPING PUSH-RC: nl_env_push_frame always returns 0.  We can use it as
;; an "ignored" extern-call if we can call it as the first arg of some fn
;; that doesn't use the result:
;;
;;   (defun nl_ali_push_frame(formals body-list args-list env out cap-flag)
;;     ;; Call push_frame as FIRST arg to nl_ali_push_ok, which ignores it.
;;     (nl_ali_push_ok
;;       (extern-call nl_env_push_frame env)    ; FIRST ✓ — result ignored
;;       formals body-list args-list env cap-flag))  ; 7 TOTAL. STILL TOO MANY.
;;
;; There's no escape: I need to lose one of the 6 live vars.
;;
;; RESOLUTION: the `out' pointer doesn't need to be threaded through the
;; entire chain. Since `out' is passed to nl_apply_lambda_inner by the Rust
;; caller, and it's a stack pointer, the Rust caller can pass it again
;; separately when needed. But in our CPS chain we have no "second channel".
;;
;; SIMPLEST REAL SOLUTION: use `formals' as a scratch register to carry the
;; `out' pointer during the push phase, since formals is only needed by
;; nl_bind_formals and we can reconstruct it from an additional cons.
;;
;; Actually, let me just try using nl_cons_prepend_clone to pack formals+args-list
;; into a single Sexp*, before entering nl_ali_push_frame.
;;
;; PACK AT ENTRY:
;;   nl_apply_lambda_inner packs (formals . args-list) into a fa-cons:
;;     nl_ali_packed(
;;       (extern-call nl_cons_prepend_clone formals args-list formals)  ; FIRST?
;;     NO — nl_cons_prepend_clone writes to `out' not returns a Sexp.
;;
;; This is getting too complex. Let me look at the simplest option:
;; REDUCE THE CHAIN to avoid the 7-param problem by NOT threading push-rc.
;;
;; SOLUTION: use a 2-phase split:
;;   Phase A: push_frame + bind_formals (extern-call nl_env_push_frame is arg0
;;            to a 6-param function that then immediately calls nl_bind_formals
;;            as arg0 to its continuation — but that's two extern-calls
;;            and the rule requires arg0 per function, which means separate fns).
;;
;; ACTUAL REAL-WORLD SOLUTION: look at how other Phase-47 files handle
;; situations where more than 6 live vars exist. They don't, because sf_let
;; uses nl_let_setup to batch multiple operations into one extern-call.
;; WE SHOULD DO THE SAME.
;;
;; Create a new Rust extern `nl_push_frame_and_bind_formals' that:
;;   1. Calls env.push_frame()
;;   2. Calls bind_formals(formals, args, env)
;;   Returns: 0=Ok, 1=Err. If Err, pops the frame before returning.
;;
;; Then the chain becomes:
;;   nl_ali_push_frame(formals body-list args-list env out cap-flag): 6
;;     → nl_ali_bind_done(
;;         (extern-call nl_push_and_bind formals args-list env)  ; FIRST ✓
;;         body-list env out cap-flag 0))
;; WHERE nl_push_and_bind = new combined extern.
;;
;; This is the RIGHT approach: batch push+bind into one Rust extern, just like
;; nl_let_setup batches parse+eval+bind.

;; Implementation: define `nl_push_and_bind' in special_forms.rs,
;; then this file is straightforward.

(defconst nelisp-cc-apply-lambda-inner--source
  '(seq

    ;; Return body-rc after the second pop.
    ;; Arity 4 (even).
    (defun nl_ali_done (pop2-rc body-rc _p3 _p4)
      body-rc)

    ;; After popping the main frame: conditionally pop the captured frame.
    ;; cap-flag=0 → return body-rc directly.
    ;; cap-flag=1 → pop captured frame (extern-call FIRST ✓) → done.
    ;; Arity 4 (even).
    (defun nl_ali_finish2 (pop-rc body-rc env cap-flag)
      (if (= cap-flag 0)
          body-rc
        (nl_ali_done
         (extern-call nl_env_pop_frame env)
         body-rc 0 0)))

    ;; Pop main frame unconditionally (extern-call FIRST ✓).
    ;; Arity 6 (even).
    (defun nl_ali_finish (body-rc env out cap-flag _p5 _p6)
      (nl_ali_finish2
       (extern-call nl_env_pop_frame env)
       body-rc env cap-flag))

    ;; After nelisp_eval_call: check eval-rc.
    ;; eval-rc=0 → recurse on cdr-body.
    ;; eval-rc!=0 → finish with error (pop both frames as needed).
    ;; Arity 6 (even).
    (defun nl_ali_body_step (eval-rc cdr-body env out cap-flag _p6)
      (if (= eval-rc 0)
          (nl_ali_body cdr-body env out cap-flag 0 0)
        (nl_ali_finish 1 env out cap-flag 0 0)))

    ;; car-ptr = nl_cons_car_ptr(body) fetched as first arg.
    ;; Eval this form (extern-call FIRST ✓).
    ;; Arity 6 (even).
    (defun nl_ali_body_eval (car-ptr cdr-body env out cap-flag _p6)
      (nl_ali_body_step
       (extern-call nelisp_eval_call car-ptr env out)
       cdr-body env out cap-flag 0))

    ;; cdr-body = nl_cons_cdr_ptr(body) fetched as first arg.
    ;; Fetch car(body) — extern-call FIRST ✓.
    ;; Arity 6 (even).
    (defun nl_ali_body_cdr (cdr-body body env out cap-flag _p6)
      (nl_ali_body_eval
       (extern-call nl_cons_car_ptr body)
       cdr-body env out cap-flag 0))

    ;; Recursive body-eval entry.
    ;; sexp-tag 0 = Nil → body done, finish with 0.
    ;; Else: fetch cdr(body) FIRST ✓.
    ;; Arity 6 (even).
    (defun nl_ali_body (body env out cap-flag _p5 _p6)
      (if (= (sexp-tag body) 0)
          (nl_ali_finish 0 env out cap-flag 0 0)
        (nl_ali_body_cdr
         (extern-call nl_cons_cdr_ptr body)
         body env out cap-flag 0)))

    ;; After nl_push_and_bind: start body eval or finish cleanup on error.
    ;; nl_push_and_bind: 0=Ok (frame pushed, formals bound),
    ;;                   1=Err (frame NOT left pushed on error path).
    ;; If error: no frame was pushed, so just signal error (no pop needed).
    ;; Wait — if nl_push_and_bind succeeds on push but fails on bind, does
    ;; it pop the frame? Yes: nl_push_and_bind pops on bind failure.
    ;; So on rc=1: no frame is left; just return 1 (and maybe pop cap frame).
    ;; On rc=0: frame is pushed; proceed to body, nl_ali_finish pops it.
    ;; Arity 6 (even).
    (defun nl_ali_bind_done (bind-rc body-list env out cap-flag _p6)
      (if (= bind-rc 0)
          (nl_ali_body body-list env out cap-flag 0 0)
        ;; Frame already cleaned up by nl_push_and_bind on error.
        ;; Only pop captured frame if it was pushed.
        (if (= cap-flag 0)
            1
          (nl_ali_done
           (extern-call nl_env_pop_frame env)
           1 0 0))))

    ;; Push frame + bind formals in one extern-call (extern-call FIRST ✓).
    ;; nl_push_and_bind: pushes frame, binds formals, pops on bind failure.
    ;; Arity 6 (even).
    (defun nl_ali_push_frame (formals body-list args-list env out cap-flag)
      (nl_ali_bind_done
       (extern-call nl_push_and_bind formals args-list env)
       body-list env out cap-flag 0))

    ;; After nl_env_push_captured:
    ;; cap-rc=0 → proceed with cap-flag=1.
    ;; cap-rc!=0 → error, nothing was pushed yet.
    ;; Arity 6 (even).
    (defun nl_ali_after_cap (cap-rc formals body-list args-list env out)
      (if (= cap-rc 0)
          (nl_ali_push_frame formals body-list args-list env out 1)
        1))

    ;; Public entry.
    ;; captured:  *const Sexp — Nil or captured alist.
    ;; formals:   *const Sexp — cons list of formal parameter symbols.
    ;; body-list: *const Sexp — cons list of body forms.
    ;; args-list: *const Sexp — cons list of evaluated arguments.
    ;; env:       *mut c_void — &mut Env.
    ;; out:       *mut Sexp   — result slot (Sexp::Nil on entry).
    ;; Returns: 0=Ok (last body value in *out), 1=Err.
    ;; Arity 6 (even).
    (defun nl_apply_lambda_inner (captured formals body-list args-list env out)
      (if (= (sexp-tag captured) 0)
          ;; No captured env: skip push_captured, cap-flag=0.
          (nl_ali_push_frame formals body-list args-list env out 0)
        ;; Has captured env: push it FIRST ✓, cap-flag=1 on success.
        (nl_ali_after_cap
         (extern-call nl_env_push_captured env captured)
         formals body-list args-list env out))))

  "Phase 47 source for `nl_apply_lambda_inner'
(eval/mod.rs apply_lambda_inner → elisp).

Eleven defuns (seq form).  Replaces the ~27-LOC Rust `apply_lambda_inner'
body in `build-tool/src/eval/mod.rs'.

New ABI externs (defined in eval/special_forms.rs):
  nl_push_and_bind (*const Sexp, *const Sexp, *mut c_void) → i64
    Pushes a new lexical frame, then binds formals to args.
    If bind fails, pops the frame before returning 1.
    Returns 0=Ok (frame pushed and all formals bound), 1=Err (frame popped).
  nl_env_push_captured (*mut c_void, *const Sexp) → i64
    Pushes captured alist as lexical frame.  0=Ok, 1=Err.

The Rust thin shell builds args + body cons lists, then calls
nl_apply_lambda_inner(captured, formals, body_list, args_list, env, out).

All functions have even arity ≤ 6 → body-entry rsp ≡ 0 mod 16 ✓.
Every extern-call is argument 0 at its call site ✓.")

(provide 'nelisp-cc-apply-lambda-inner)

;;; nelisp-cc-apply-lambda-inner.el ends here
