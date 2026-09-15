;;; nelisp-cc-sf-let-star.el --- AOT nl_sf_let_star swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_let_star' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_let_common(args, env, "let*", sequential=true):
;;       parts = args_vec(args)          // (BINDINGS BODY...)
;;       bindings = list_elements(parts[0])
;;       with_frame(env):
;;           // Sequential: each binding can reference previous ones.
;;           for b in bindings:
;;               (name, val) = parse_let_binding(b, env)  // eval under current env
;;               env.bind_local(name, val)
;;           eval_body(parts[1..], env)
;;
;; `args' is the raw unevaluated arg list for `(let* BINDINGS BODY...)'.
;;   car(args) = BINDINGS list — each element is either:
;;     - Sexp::Symbol(name)         → bind name to nil
;;     - (name value-form)          → bind name to eval(value-form)
;;   cdr(args) = BODY forms list.
;;
;; The key difference from `let': nl_let_setup is called with
;; sequential=1 for one binding at a time.  Each binding gets its own
;; frame, preserving earlier cells already captured by closures and allowing
;; lexical and dynamic bindings of the same name to coexist.
;;
;; Body-loop GC invariant:
;;   nl_cons_cdr_ptr returns a real child box for a pointer cdr, but
;;   materialises a fresh unrooted view for an immediate cdr (including Nil).
;;   Therefore the body CONS itself is carried across nelisp_eval_call, and its
;;   cdr is taken only after eval returns; no materialised view crosses eval's
;;   possible collection.
;;
;; ABI externs used:
;;   nl_cons_car_ptr: *const Sexp → i64
;;   nl_cons_cdr_ptr: *const Sexp → i64
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;   nl_let_setup: (*const Sexp, *mut c_void, i64) → i64
;;     Parses + evaluates the bindings list, pushes a lexical frame,
;;     and binds all variables.  Third arg: 1=sequential (let*).
;;     Returns: 0=Ok (frame IS pushed), 1=Err (no frame pushed).
;;   nl_env_pop_frame: (*mut c_void) → i64
;;     Pops the topmost lexical frame.  Returns 0.
;;
;; Body and cleanup helpers:
;;   nl_sf_let_star_ret        (pop-rc body-rc _p2 _p3) — arity 4
;;   nl_sf_let_star_finish     (body-rc env out _pad) — arity 4
;;   nl_sf_let_star_body_step  (eval-rc body env out) — arity 4
;;   nl_sf_let_star_body_eval  (car-ptr body env out) — arity 4
;;   nl_sf_let_star_body       (body env out _pad) — arity 4
;;   nl_sf_let_star_setup_done (setup-rc args env out) — arity 4
;;   nl_sf_let_star_got_bindings (bindings args env out) — arity 4
;;   nl_sf_let_star            (args env out _pad) — arity 4
;;
;; The setup helpers below root a singleton binding list and call
;; nl_let_setup with sequential=1 for each binding.  Recursive cleanup
;; preserves outer frames while later initializers and the body execute.

;;; Code:

(defconst nelisp-cc-sf-let-star--source
  '(seq

    ;; After nl_env_pop_frame: discard pop-rc, return body-rc.
    ;; Arity 4 (even).
    (defun nl_sf_let_star_ret (pop-rc body-rc _p2 _p3)
      body-rc)

    ;; body-rc = result of body eval (0=Ok or 1=Err).
    ;; Pop frame unconditionally (extern-call FIRST ✓), then return body-rc.
    ;; Arity 4 (even).
    (defun nl_sf_let_star_finish (body-rc env out _pad)
      (nl_sf_let_star_ret
       (extern-call nl_env_pop_frame env)
       body-rc 0 0))

    ;; After nelisp_eval_call on one body form: check rc, then advance list.
    ;; eval-rc=0 → fetch cdr(body) (extern-call FIRST ✓), then recurse.
    ;; Fetching the cdr only after eval avoids carrying an unrooted
    ;; materialised immediate-cdr view across eval's possible collection;
    ;; body is the real CONS box and is already kept alive by the form.
    ;; eval-rc!=0 → finish with error (pop frame).
    ;; Arity 4 (even).
    (defun nl_sf_let_star_body_step (eval-rc body env out)
      (if (= eval-rc 0)
          (nl_sf_let_star_body
           (extern-call nl_cons_cdr_ptr body)
           env out 0)
        (nl_sf_let_star_finish 1 env out 0)))

    ;; car-ptr = nl_cons_car_ptr(body) already fetched as first arg.
    ;; Eval this form (extern-call FIRST ✓), carrying the rooted body CONS
    ;; rather than a possibly materialised cdr view.
    ;; Arity 4 (even).
    (defun nl_sf_let_star_body_eval (car-ptr body env out)
      (nl_sf_let_star_body_step
       (extern-call nelisp_eval_call car-ptr env out)
       body env out))

    ;; Recursive body-eval entry.
    ;; body: remaining forms cons list.
    ;; If Nil → all forms done, pop frame + return 0.
    ;; Else: fetch car(body) FIRST ✓.
    ;; Arity 4 (even).
    (defun nl_sf_let_star_body (body env out _pad)
      (if (= (sexp-tag body) 0)
          (nl_sf_let_star_finish 0 env out 0)
        (nl_sf_let_star_body_eval
         (extern-call nl_cons_car_ptr body)
         body env out)))

    ;; setup-rc = result of nl_let_setup (0=Ok, 1=Err).
    ;; If error → return 1 (no frame pushed, no pop needed).
    ;; Else get body = cdr(args) (extern-call FIRST ✓).
    ;; Arity 4 (even).
    (defun nl_sf_let_star_setup_done (setup-rc args env out)
      (if (= setup-rc 0)
          (nl_sf_let_star_body
           (extern-call nl_cons_cdr_ptr args)
           env out 0)
        1))

    (defun nl_sf_let_star_one_done (rc env mark _pad)
      (seq (nl_root_release env mark) rc))

    (defun nl_sf_let_star_one_rooted (binding env mark one)
      (let* ((nil-slot (alloc-bytes 32 8)))
        (seq
         (nl_cons_write_nil nil-slot)
         (cons-make-with-clone binding nil-slot one)
         (nl_sf_let_star_one_done
          (nl_let_setup one env 1) env mark 0))))

    (defun nl_sf_let_star_one_marked (binding env mark _pad)
      (nl_sf_let_star_one_rooted binding env mark (nl_root_reserve env)))

    (defun nl_sf_let_star_one (binding env)
      (nl_sf_let_star_one_marked binding env (nl_root_mark env) 0))

    ;; The innermost body pops its own frame.  Each recursive caller pops
    ;; exactly its frame, including initializer errors and nonlocal exits.
    ;; Take cdr only after setup: an immediate cdr view must not cross eval.
    (defun nl_sf_let_star_next (rc bindings args env out _pad)
      (if (= rc 0)
          (if (= (sexp-tag (nl_cons_cdr_ptr bindings)) 0)
              (nl_sf_let_star_body (nl_cons_cdr_ptr args) env out 0)
            (nl_sf_let_star_finish
             (nl_sf_let_star_got_bindings (nl_cons_cdr_ptr bindings) args env out)
             env out 0))
        1))

    ;; Preserve the original empty/invalid-list handling.  A rooted singleton
    ;; reuses the existing binding parser without keeping a temporary unrooted
    ;; cons alive across initializer evaluation and possible collection.
    (defun nl_sf_let_star_got_bindings (bindings args env out)
      (if (= (sexp-tag bindings) 7)
          (nl_sf_let_star_next
           (nl_sf_let_star_one (nl_cons_car_ptr bindings) env)
           bindings args env out 0)
        (nl_sf_let_star_setup_done
         (extern-call nl_let_setup bindings env 1)
         args env out)))

    ;; Public entry: nl_sf_let_star(args, env, out, _pad) → i64
    ;; args: *const Sexp = (BINDINGS BODY...) arg list.
    ;; env:  *mut c_void = &mut Env.
    ;; out:  *mut Sexp   = result slot.
    ;; _pad: unused alignment pad (arity 4 = even).
    ;; Returns: 0=Ok (result in *out), 1=Err.
    ;; Empty args (Nil) → error (let* requires bindings list).
    ;; Else: get bindings = car(args) FIRST ✓.
    (defun nl_sf_let_star (args env out _pad)
      (seq (nl_cons_write_nil out)
       (if (= (sexp-tag args) 0)
          1
        (nl_sf_let_star_got_bindings
         (extern-call nl_cons_car_ptr args)
         args env out)))))

  "AOT source for `nl_sf_let_star' (eval/special_forms.rs sf_let_star → elisp).

Each binding uses nl_let_setup with sequential=1 and its own frame.
Later initializers see earlier bindings without replacing their cells.
Singleton lists are rooted across initializer evaluation.  The innermost
body and each recursive caller pop their own frame on success or error.

Entry chain:
  nl_sf_let_star → (car(args) FIRST) → nl_sf_let_star_got_bindings
  → nl_sf_let_star_one → nl_sf_let_star_next (recurse for later bindings)
  → (cdr(args) FIRST) → nl_sf_let_star_body
  → (car(body) FIRST) → nl_sf_let_star_body_eval
  → (nelisp_eval_call FIRST) → nl_sf_let_star_body_step
  → (cdr(body) FIRST, after eval) → nl_sf_let_star_body
  When body exhausted: → nl_sf_let_star_finish
  → (nl_env_pop_frame FIRST) → nl_sf_let_star_ret → body_rc

All functions have even arity → body-entry rsp ≡ 0 mod 16 ✓.
Every extern-call is argument 0 at its call site ✓.")

(provide 'nelisp-cc-sf-let-star)

;;; nelisp-cc-sf-let-star.el ends here
