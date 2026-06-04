;;; nelisp-cc-sf-let.el --- Phase 47 nl_sf_let swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the `sf_let' Rust body in
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
;; Structure (7 defuns):
;;   nl_sf_let_ret        (pop-rc body-rc _p2 _p3) — arity 4
;;     Return body-rc after frame pop.
;;   nl_sf_let_finish     (body-rc env out _pad) — arity 4
;;     Pop frame (extern-call FIRST), delegate to nl_sf_let_ret.
;;   nl_sf_let_body_step  (eval-rc cdr-body env out) — arity 4
;;     Advance body cons list after one eval.
;;   nl_sf_let_body_eval  (car-ptr cdr-body env out) — arity 4
;;     Eval one body form (extern-call FIRST), delegate to step.
;;   nl_sf_let_body_cdr   (cdr-body body env out) — arity 4
;;     Get car(body) for eval (extern-call FIRST), delegate to eval.
;;   nl_sf_let_body       (body env out _pad) — arity 4
;;     Recursive body-eval entry.  If Nil → finish (0).
;;     Else: fetch cdr(body) FIRST → body_cdr.
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

    ;; After nl_env_pop_frame: discard pop-rc, return body-rc.
    ;; Arity 4 (even).
    (defun nl_sf_let_ret (pop-rc body-rc _p2 _p3)
      body-rc)

    ;; body-rc = result of body eval (0=Ok or 1=Err).
    ;; Pop frame unconditionally (extern-call FIRST ✓), then return body-rc.
    ;; Arity 4 (even).
    (defun nl_sf_let_finish (body-rc env out _pad)
      (nl_sf_let_ret
       (extern-call nl_env_pop_frame env)
       body-rc 0 0))

    ;; After nelisp_eval_call on one body form: check rc, advance list.
    ;; eval-rc=0 → recurse on remaining body forms.
    ;; eval-rc!=0 → finish with error (pop frame).
    ;; Arity 4 (even).
    (defun nl_sf_let_body_step (eval-rc cdr-body env out)
      (if (= eval-rc 0)
          (nl_sf_let_body cdr-body env out 0)
        (nl_sf_let_finish 1 env out 0)))

    ;; car-ptr = nl_cons_car_ptr(body) already fetched as first arg.
    ;; Eval this form (extern-call FIRST ✓), delegate to step.
    ;; Arity 4 (even).
    (defun nl_sf_let_body_eval (car-ptr cdr-body env out)
      (nl_sf_let_body_step
       (extern-call nelisp_eval_call car-ptr env out)
       cdr-body env out))

    ;; cdr-body = nl_cons_cdr_ptr(body) already fetched as first arg.
    ;; Get car(body) for eval (extern-call FIRST ✓).
    ;; Arity 4 (even).
    (defun nl_sf_let_body_cdr (cdr-body body env out)
      (nl_sf_let_body_eval
       (extern-call nl_cons_car_ptr body)
       cdr-body env out))

    ;; Recursive body-eval entry.
    ;; body: remaining forms cons list.
    ;; If Nil → all forms done, pop frame + return 0.
    ;; Else: fetch cdr(body) FIRST ✓, then eval car.
    ;; Arity 4 (even).
    (defun nl_sf_let_body (body env out _pad)
      (if (= (sexp-tag body) 0)
          (nl_sf_let_finish 0 env out 0)
        (nl_sf_let_body_cdr
         (extern-call nl_cons_cdr_ptr body)
         body env out)))

    ;; setup-rc = result of nl_let_setup (0=Ok, 1=Err).
    ;; If error → return 1 (no frame pushed, no pop needed).
    ;; Else get body = cdr(args) (extern-call FIRST ✓).
    ;; Arity 4 (even).
    (defun nl_sf_let_setup_done (setup-rc args env out)
      (if (= setup-rc 0)
          (nl_sf_let_body
           (extern-call nl_cons_cdr_ptr args)
           env out 0)
        1))

    ;; bindings = car(args) already fetched as first arg.
    ;; Call nl_let_setup(bindings, env, 0/*parallel*/) FIRST ✓.
    ;; Arity 4 (even).
    (defun nl_sf_let_got_bindings (bindings args env out)
      (nl_sf_let_setup_done
       (extern-call nl_let_setup bindings env 0)
       args env out))

    ;; Public entry: nl_sf_let(args, env, out, _pad) → i64
    ;; args: *const Sexp = (BINDINGS BODY...) arg list.
    ;; env:  *mut c_void = &mut Env.
    ;; out:  *mut Sexp   = result slot.
    ;; _pad: unused alignment pad (arity 4 = even).
    ;; Returns: 0=Ok (result in *out), 1=Err.
    ;; Empty args (Nil) → error (let requires bindings list).
    ;; Else: get bindings = car(args) FIRST ✓.
    (defun nl_sf_let (args env out _pad)
      (if (= (sexp-tag args) 0)
          1
        (nl_sf_let_got_bindings
         (extern-call nl_cons_car_ptr args)
         args env out))))

  "Phase 47 source for `nl_sf_let' (eval/special_forms.rs sf_let → elisp).

Nine defuns (seq form).  CPS chain decomposed into frame-setup +
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
  → (cdr(body) FIRST) → nl_sf_let_body_cdr
  → (car(body) FIRST) → nl_sf_let_body_eval
  → (nelisp_eval_call FIRST) → nl_sf_let_body_step → nl_sf_let_body
  When body exhausted: → nl_sf_let_finish
  → (nl_env_pop_frame FIRST) → nl_sf_let_ret → body_rc

All functions have even arity → body-entry rsp ≡ 0 mod 16 ✓.
Every extern-call is argument 0 at its call site ✓.")

(provide 'nelisp-cc-sf-let)

;;; nelisp-cc-sf-let.el ends here
