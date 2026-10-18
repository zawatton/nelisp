;;; nelisp-cc-sf-let.el --- AOT nl_sf_let swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_let' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_let_common(args, env, "let", sequential=false):
;;       parts = args_vec(args)          // (BINDINGS BODY...)
;;       bindings = list_elements(parts[0])
;;       // Pre-eval all values before pushing frame:
;;       values = [(parse_let_binding(b, env)) for b in bindings]
;;       with_frame(env):
;;           for (name, val) in values: env.bind_local(name, val)
;;           eval_body(parts[1..], env)
;;
;; `args' is the raw unevaluated arg list for `(let BINDINGS BODY...)'.
;;   car(args) = BINDINGS list — each element is either:
;;     - Sexp::Symbol(name)         → bind name to nil
;;     - (name value-form)          → bind name to eval(value-form)
;;   cdr(args) = BODY forms list.
;;
;; ABI externs used:
;;   nl_cons_car_ptr: *const Sexp → i64 (= &car, or 0 on non-Cons)
;;   nl_cons_cdr_ptr: *const Sexp → i64 (= &cdr, or 0 on non-Cons)
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;   nl_let_setup: (*const Sexp, *mut c_void, i64) → i64
;;     Parses + pre-evals the bindings list, pushes a lexical frame,
;;     and binds all variables.  Third arg: 0=parallel (let).
;;     Returns: 0=Ok (frame IS pushed), 1=Err (no frame pushed).
;;   nl_env_pop_frame: (*mut c_void) → i64
;;     Pops the topmost lexical frame.  Returns 0.
;;
;; Alignment notes:
;;   Every function has even arity so body-entry rsp ≡ 0 mod 16.
;;   Every extern-call appears as argument 0 at its call site.
;;
;; Body-loop GC invariant:
;;   nl_cons_cdr_ptr returns a real child box for a pointer cdr, but
;;   materialises a fresh unrooted view for an immediate cdr (including Nil).
;;   Therefore the body CONS itself is carried across nelisp_eval_call, and its
;;   cdr is taken only after eval returns; no materialised view crosses eval's
;;   possible collection.
;;
;; Structure (8 defuns):
;;   nl_sf_let_ret        (pop-rc body-rc _p2 _p3) — arity 4
;;     Return body-rc after frame pop.
;;   nl_sf_let_finish     (body-rc env out _pad) — arity 4
;;     Pop frame (extern-call FIRST), delegate to nl_sf_let_ret.
;;   nl_sf_let_body_step  (eval-rc body env out) — arity 4
;;     After eval, get cdr(body) (extern-call FIRST) and advance.
;;   nl_sf_let_body_eval  (car-ptr body env out) — arity 4
;;     Eval one body form (extern-call FIRST), delegate to step.
;;   nl_sf_let_body       (body env out _pad) — arity 4
;;     Recursive body-eval entry.  If Nil → finish (0).
;;     Else: fetch car(body) FIRST → body_eval.
;;   nl_sf_let_setup_done (setup-rc args env out) — arity 4
;;     If setup failed → return 1.  Else get cdr(args) FIRST → body.
;;   nl_sf_let_got_bindings (bindings args env out) — arity 4
;;     Call nl_let_setup FIRST on bindings, delegate to setup_done.
;;   nl_sf_let            (args env out _pad) — arity 4
;;     Public entry: if args is Nil → error.
;;     Else get car(args) FIRST → bindings.

;;; Code:

(defconst nelisp-cc-sf-let--source
  '(seq
    ;; Doc 215 word ABI.  `nl_sf_let_w' answers the body's value WORD (nil
    ;; 3 for an empty body), or 0 with the signal stashed.  The body is a
    ;; progn (`nl_sf_let_body_w'); the frame is popped after it on success
    ;; and on error alike.  The WORD crosses only `nl_env_pop_frame', which
    ;; has no safepoint (Doc 215 R2).  Arity 4; extern-call first.

    ;; After nl_env_pop_frame: a failed pop (-1) fails the form.
    (defun nl_sf_let_ret_w (pop-rc w _p2 _p3)
      (if (= pop-rc -1) (nl_abi_fail 1 0) w))

    (defun nl_sf_let_finish_w (w env _pad _pad1)
      (nl_sf_let_ret_w (extern-call nl_env_pop_frame env) w 0 0))

    (defun nl_sf_let_body_w (body env _pad _pad1)
      (nl_sf_let_finish_w (nl_sf_progn_w body env 0 0) env 0 0))

    (defun nl_sf_let_setup_done_w (setup-rc args env _pad)
      (if (= setup-rc 0)
          (nl_sf_let_body_w (extern-call nl_cons_cdr_word args) env 0 0)
        (nl_abi_fail setup-rc 0)))

    (defun nl_sf_let_got_bindings_w (bindings args env _pad)
      (nl_sf_let_setup_done_w (extern-call nl_let_setup bindings env 0) args env 0))

    ;; `(let)' with no binding list fails the form.
    (defun nl_sf_let_w (args env _pad _pad1)
      (if (= (sexp-tag args) 0)
          (nl_abi_fail 1 0)
        (nl_sf_let_got_bindings_w (extern-call nl_cons_car_ptr args) args env 0)))

    ;; (rc, OUT) adapter.
    (defun nl_sf_let (args env out _pad)
      (nl_abi_word_to_rc (nl_sf_let_w args env 0 0) out)))

  "AOT source for `nl_sf_let' (eval/special_forms.rs sf_let → elisp).

Eight defuns (seq form).  CPS chain decomposed into frame-setup +
body-eval + frame-pop phases.

Externs:
  nl_let_setup (bindings, env, sequential=0) — pre-evals bindings,
    pushes lexical frame, binds all vars.
  nl_env_pop_frame (env) — pops topmost lexical frame.
  nelisp_eval_call, nl_cons_car_ptr, nl_cons_cdr_ptr — standard ABI.

Entry chain:
  nl_sf_let → (car(args) FIRST) → nl_sf_let_got_bindings
  → (nl_let_setup FIRST) → nl_sf_let_setup_done
  → (cdr(args) FIRST) → nl_sf_let_body
  → (car(body) FIRST) → nl_sf_let_body_eval
  → (nelisp_eval_call FIRST) → nl_sf_let_body_step
  → (cdr(body) FIRST, after eval) → nl_sf_let_body
  When body exhausted: → nl_sf_let_finish
  → (nl_env_pop_frame FIRST) → nl_sf_let_ret → body_rc

All functions have even arity → body-entry rsp ≡ 0 mod 16 ✓.
Every extern-call is argument 0 at its call site ✓.")

(provide 'nelisp-cc-sf-let)

;;; nelisp-cc-sf-let.el ends here
