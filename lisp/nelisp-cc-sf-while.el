;;; nelisp-cc-sf-while.el --- AOT nl_sf_while swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_while' Rust body in
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
;;
;; Doc 152 §11.14 fix (STACK OVERFLOW on long loops).  The previous version
;; closed the guest loop by tail-calling back into `nl_sf_while_iter' at the
;; end of each iteration.  nelisp-cc does NOT eliminate tail calls, so every
;; guest iteration pushed ~11 native frames; a pure `(while (< i N) ...)' loop
;; SIGSEGV'd at N~=650k by overflowing the driver's 1 GiB mmap native stack
;; (manifesting as a null deref in nelisp_mirror_walk_bucket / nl_cons_cdr_ptr
;; -- the run-suite blocker).
;;
;; This rewrite drives the guest loop with an AOT host `(while ...)' loop in
;; the public `nl_sf_while', calling a `nl_sf_while_step' helper that does ONE
;; iteration (eval test; if truthy eval body) and RETURNS a status instead of
;; recursing into the next iteration.  So native stack depth is O(body depth),
;; independent of the iteration count.  This is exactly the proven GC-sweep
;; pattern `(while (= status 0) (setq status (helper ...)))' (cf. the chunk
;; sweep loop in scripts/nelisp-standalone-build.el).
;;
;; Invariants kept from the working CPS version:
;;   - every extern-call is at argument position 0 of its call site (rsp
;;     alignment), so each helper does exactly one extern-call;
;;   - every defun arity is 4 (even -> rsp 16-aligned at call sites);
;;   - state (args/test/body/env/out) is threaded through PARAMETERS, never
;;     relied on as locals surviving across a call (nelisp-cc does not preserve
;;     arbitrary locals across calls -- the original reason for the CPS shape).
;;     The only across-call local is `status' in the host loop, which works
;;     just as `hdr' does in the GC sweep `while'.
;;
;; Body-walk GC invariant:
;;   `nl_cons_cdr_ptr' returns the real child box for a pointer cdr, but
;;   materialises a fresh unrooted view for an immediate cdr (including Nil).
;;   Therefore the body CONS itself is carried across `nelisp_eval_call', and
;;   its cdr is taken only after eval returns; no materialised view crosses
;;   eval's possible collection.
;;
;; Structure (10 defuns, all arity 4):
;;   nl_sf_while_body_done       (eval-rc body env out)
;;   nl_sf_while_body_eval       (car body env out)
;;   nl_sf_while_body            (body env out _pad)
;;   nl_sf_while_body_start      (args env out _pad)
;;   nl_sf_while_step3           (truthy args env out)
;;   nl_sf_while_step2           (test args env out)
;;   nl_sf_while_step            (args env out _pad)
;;   nl_sf_while_midform_collect (_a _b _c _d)
;;   nl_sf_while_out_nil         (out _b _c _d)
;;   nl_sf_while                 (args env out _pad)
;;
;; Chain:
;;   nl_sf_while host loop
;;   -> nl_sf_while_step -> (car(args) FIRST) -> nl_sf_while_step2
;;   -> (nl_eval_is_truthy FIRST) -> nl_sf_while_step3
;;   -> nl_sf_while_body_start -> (cdr(args) FIRST) -> nl_sf_while_body
;;   -> (car(body) FIRST) -> nl_sf_while_body_eval
;;   -> (nelisp_eval_call FIRST) -> nl_sf_while_body_done
;;   -> (cdr(body) FIRST, after eval) -> nl_sf_while_body (recurse)
;;   -> status 0: nl_sf_while_midform_collect, then next host iteration
;;   -> status 2: nl_sf_while_out_nil
;;
;; Status codes (nl_sf_while_step / nl_sf_while_body):
;;   step: 0 = continue (test truthy, body ok), 1 = error, 2 = done (test nil).
;;   body: 0 = all body forms ok, 1 = a body form errored.
;;
;; ABI:
;;   nl_cons_car_ptr / nl_cons_cdr_ptr: *const Sexp -> i64
;;   nl_eval_is_truthy: (*const Sexp, *mut c_void) -> i64   (1=truthy/0=nil/-1=err)
;;   nelisp_eval_call:  (*const Sexp, *mut c_void, *mut Sexp) -> i64  (0=ok/1=err)

;;; Code:

(defconst nelisp-cc-sf-while--source
  '(seq

    ;;--- Body-form walk (bounded by body LENGTH, not iteration count) ---
    ;; CPS over the (FORM...) list; recursion depth = number of body forms
    ;; (small, fixed), so this never grows with the iteration count.

    ;; After eval of one body form: check rc, then fetch the body tail
    ;; (extern-call FIRST) and advance.  Taking the cdr only after eval avoids
    ;; carrying an unrooted materialised immediate-cdr view across eval's GC;
    ;; body is the real CONS box and is already kept alive by the form.
    (defun nl_sf_while_body_done (eval-rc body env out)
      (if (= eval-rc 0)
          (nl_sf_while_body
           (extern-call nl_cons_cdr_ptr body)
           env out 0)
        1))

    ;; car = nl_cons_car_ptr(body) (fetched by caller as arg 0).
    ;; Eval it via nelisp_eval_call (extern-call FIRST), result -> out, carrying
    ;; the rooted body CONS rather than a possibly materialised cdr view.
    (defun nl_sf_while_body_eval (car body env out)
      (nl_sf_while_body_done
       (extern-call nelisp_eval_call car env out)
       body env out))

    ;; Walk the body-form list, eval each (discard).  Returns 0 (all ok) or
    ;; 1 (a form errored).  Body Nil -> 0.
    (defun nl_sf_while_body (body env out _pad)
      (if (= (sexp-tag body) 0)
          0
        (nl_sf_while_body_eval
         (extern-call nl_cons_car_ptr body)
         body env out)))

    ;;--- One iteration (eval test; if truthy eval body); returns status ---

    ;; body-start: body = cdr(args) (extern-call FIRST), then walk it.
    (defun nl_sf_while_body_start (args env out _pad)
      (nl_sf_while_body
       (extern-call nl_cons_cdr_ptr args)
       env out 0))

    ;; Dispatch on truthy.  1 -> eval body (0 continue / 1 err); 0 -> 2 (done);
    ;; -1 -> 1 (test eval error).
    (defun nl_sf_while_step3 (truthy args env out)
      (if (= truthy 1)
          (nl_sf_while_body_start args env out 0)
        (if (= truthy -1) 1 2)))

    ;; test fetched by caller (arg 0).  Eval truthiness (extern-call FIRST).
    (defun nl_sf_while_step2 (test args env out)
      (nl_sf_while_step3
       (extern-call nl_eval_is_truthy test env)
       args env out))

    ;; One step: test = car(args) (extern-call FIRST), then dispatch.
    ;; Returns 0 continue / 1 error / 2 done.
    (defun nl_sf_while_step (args env out _pad)
      (nl_sf_while_step2
       (extern-call nl_cons_car_ptr args)
       args env out))

    ;;--- Mid-form GC collect (Doc 152 §11.41 Stage 4b step 2) ---

    ;; Invoke the GC unit's gated mid-form collect at the while backedge (after a
    ;; COMPLETE iteration, status==0) -- the one point where per-iteration scratch
    ;; (arg-list cons / materialising-accessor box / test+body intermediates) is
    ;; provably dead, with only env / out / recorded-roots / rootstack holding
    ;; live values.  `nl_gc_midform_collect' is gated (enable + alloc-debt;
    ;; reader boot enables it after freezing the boot watermark) and reads its
    ;; root-set from nl_gc_loop_ctx, so this takes no arguments.
    ;; arity 4 (even -> rsp 16-aligned at the call site), exactly one extern-call.
    (defun nl_sf_while_midform_collect (_a _b _c _d)
      (extern-call nl_gc_midform_collect))

    ;;--- Result fixup: `while' is a statement -- it always yields Nil ---

    ;; nl_sf_while_body_eval reuses `out' as scratch for each body form's
    ;; nelisp_eval_call, so on normal loop exit `out' is left holding the
    ;; LAST body form's value instead of Nil (real Elisp: `while' always
    ;; returns nil, unconditionally, on every non-error exit -- including
    ;; after 1+ iterations, not just the already-correct zero-iteration
    ;; case where `out' is simply never touched).  This does the same
    ;; raw 4-word zero-write `nl_sci_copy' (nelisp-cc-sexp-clone-into.el)
    ;; uses for a bit-identical Sexp clone: tag 0 (Nil) with an all-zero
    ;; payload is a valid Nil slot, so no source pointer / clone call is
    ;; needed, just the same `ptr-write-u64' primitive.  Arity 4 (even);
    ;; no extern-call inside, so no alignment concern.
    (defun nl_sf_while_out_nil (out _b _c _d)
      (and (ptr-write-u64 out 0  0)
           (ptr-write-u64 out 8  0)
           (ptr-write-u64 out 16 0)
           (ptr-write-u64 out 24 0)
           0))

    ;;--- Public entry: host-iterate the steps (O(1) iteration-count stack) ---

    ;; args: *const Sexp = (TEST BODY...).  env: *mut c_void.
    ;; out:  *mut Sexp.  On a normal (non-error) exit this is reset to Nil
    ;;       just before returning -- see `nl_sf_while_out_nil' above --
    ;;       regardless of what the last body form's value was.
    ;; _pad: alignment pad (arity 4 = even).  Returns 0=Ok, 1=Err.
    ;; Empty args (Nil) -> 0 (out untouched, already Nil per caller convention).
    ;; `status' is the sole across-call local (like `hdr' in the GC sweep
    ;; `while').
    (defun nl_sf_while (args env out _pad)
      (if (= (sexp-tag args) 0)
          0
        (let ((status 0))
          (seq
           (while (= status 0)
             ;; Doc 152 §11.41 Stage 4b step 2: after a completed iteration
             ;; (status==0 = continue), hit the gated mid-form collect.  args /
             ;; env / out are PARAMETERS (preserved across the call); `status' is
             ;; the sole across-call local -- no new local is introduced (§11.14).
             (seq
              (setq status (nl_sf_while_step args env out 0))
              (if (= status 0)
                  (nl_sf_while_midform_collect 0 0 0 0)
                0)))
           (if (= status 1)
               1
             (nl_sf_while_out_nil out 0 0 0))))))
    nil)

  "AOT source for `nl_sf_while' (eval/special_forms.rs sf_while -> elisp).

Ten defuns (seq form), all with even arity for SysV AMD64 stack alignment.

Doc 152 §11.14 host-iterate rewrite: the guest loop is driven by an AOT host
`(while ...)' in `nl_sf_while' calling `nl_sf_while_step' (one iteration ->
status), so native stack depth no longer grows with the iteration count (the
prior CPS tail-recursion overflowed the 1 GiB native stack at ~650k iters
because nelisp-cc has no TCO).  Each helper does one extern-call at arg 0,
arity 4 (even) keeps rsp 16-aligned, and state is threaded through parameters
(only `status' lives across a call, exactly as `hdr' does in the GC sweep
loop).")

(provide 'nelisp-cc-sf-while)

;;; nelisp-cc-sf-while.el ends here
