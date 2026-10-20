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
;;   nl_eval_is_truthy: (WORD, *mut c_void) -> i64   (1=truthy/0=nil/-1=err)
;;   nelisp_eval_call:  (*const Sexp, *mut c_void, *mut Sexp) -> i64  (0=ok/1=err)

;;; Code:

(defconst nelisp-cc-sf-while--source
  '(seq
    ;; Doc 215 word ABI.  `nl_sf_while_w' answers nil (3) on a normal exit
    ;; or 0 with the signal stashed.  The per-iteration chain answers a
    ;; status: 0 = continue, 1 = signalled, 2 = test was nil.  Body values
    ;; are discarded, so nothing is held across an iteration.

    ;; The cdr WORD of a cons view (0 for a non-cons, like
    ;; `nl_cons_cdr_ptr'): the body tail is usually the nil terminator,
    ;; an immediate, so no view is materialised per iteration.
    (defun nl_sf_while_body_word (body)
      (if (= (sexp-tag body) 7)
          (heap-read-u64 (heap-sexp-payload body) 8)
        0))

    ;; Walk the body forms (recursion depth = body length).
    (defun nl_sf_while_form_done (w body env _pad)
      (if (= w 0) 1 (nl_sf_while_forms (nl_sf_while_body_word body) env 0 0)))

    (defun nl_sf_while_form (car body env _pad)
      (nl_sf_while_form_done (extern-call nelisp_eval_call_w car env 0) body env 0))

    (defun nl_sf_while_forms (body env _pad _pad1)
      (if (= (nl_val_tag body) 0)
          0
        (nl_sf_while_form (extern-call nl_cons_car_word body) body env 0)))

    (defun nl_sf_while_cond (truthy args env _pad)
      (if (= truthy 1)
          (nl_sf_while_forms (nl_sf_while_body_word args) env 0 0)
        (if (= truthy -1) 1 2)))

    (defun nl_sf_while_test (test args env _pad)
      (nl_sf_while_cond (extern-call nl_eval_is_truthy test env) args env 0))

    (defun nl_sf_while_iter (args env _pad _pad1)
      (nl_sf_while_test (extern-call nl_cons_car_word args) args env 0))

    ;; Doc 152 §11.41 Stage 4b step 2: the gated mid-form collect at the
    ;; backedge, after a complete iteration (status 0), where per-iteration
    ;; scratch is dead.  Arity 4, exactly one extern-call.
    (defun nl_sf_while_midform_collect (_a _b _c _d)
      (extern-call nl_gc_midform_collect))

    ;; `status' is the sole across-call local (§11.14).
    (defun nl_sf_while_w (args env _pad _pad1)
      (if (= (sexp-tag args) 0)
          3
        (let ((status 0))
          (seq
           (while (= status 0)
             (seq
              (setq status (nl_sf_while_iter args env 0 0))
              (if (= status 0)
                  (nl_sf_while_midform_collect 0 0 0 0)
                0)))
           (if (= status 1) 0 3)))))

    ;; (rc, OUT) adapter.
    (defun nl_sf_while (args env out _pad)
      (nl_abi_word_to_rc (nl_sf_while_w args env 0 0) out))
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
