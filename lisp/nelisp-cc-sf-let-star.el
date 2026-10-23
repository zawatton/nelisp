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
    ;; Doc 215 word ABI.  `nl_sf_let_star_w' answers the body's value WORD
    ;; (nil 3 for an empty body), or 0 with the signal stashed.  Each
    ;; binding pushes a frame; the innermost body pops its own and every
    ;; recursive caller pops exactly its own, on success and on error.

    (defun nl_sf_let_star_ret_w (pop-rc w _p2 _p3)
      (if (= pop-rc -1) (nl_abi_fail 1 0) w))

    (defun nl_sf_let_star_finish_w (w env _pad _pad1)
      (nl_sf_let_star_ret_w (extern-call nl_env_pop_frame env) w 0 0))

    (defun nl_sf_let_star_body_w (body env _pad _pad1)
      (nl_sf_let_star_finish_w (nl_sf_progn_w body env 0 0) env 0 0))

    (defun nl_sf_let_star_setup_done_w (setup-rc args env _pad)
      (if (= setup-rc 0)
          (nl_sf_let_star_body_w (extern-call nl_cons_cdr_word args) env 0 0)
        (nl_abi_fail setup-rc 0)))

    (defun nl_sf_let_star_one_done (rc env mark _pad)
      (seq (nl_root_release env mark) rc))

    ;; Doc 216 S6.2: ONE is a WORD root slot holding the singleton list.
    (defun nl_sf_let_star_one_rooted (binding env mark one)
      (let* ((list (nelisp_cons_construct_w binding 3 0)))
        (seq
         (ptr-write-u64 one 0 list)
         (nl_sf_let_star_one_done
          (nl_let_setup list env 1) env mark 0))))

    (defun nl_sf_let_star_one_marked (binding env mark _pad)
      (nl_sf_let_star_one_rooted binding env mark (nl_root_word env)))

    (defun nl_sf_let_star_one (binding env)
      (nl_sf_let_star_one_marked binding env (nl_root_mark env) 0))

    ;; Take cdr only after setup: an immediate cdr view must not cross eval.
    (defun nl_sf_let_star_next_w (rc bindings args env _pad _pad1)
      (if (= rc 0)
          (if (= (sexp-tag (nl_cons_cdr_word bindings)) 0)
              (nl_sf_let_star_body_w (nl_cons_cdr_word args) env 0 0)
            (nl_sf_let_star_finish_w
             (nl_sf_let_star_got_bindings_w (nl_cons_cdr_ptr bindings) args env 0)
             env 0 0))
        (nl_abi_fail rc 0)))

    ;; A rooted singleton reuses the binding parser without keeping a
    ;; temporary unrooted cons alive across initializer evaluation.
    (defun nl_sf_let_star_got_bindings_w (bindings args env _pad)
      (if (= (sexp-tag bindings) 7)
          (nl_sf_let_star_next_w
           (nl_sf_let_star_one (nl_cons_car_word bindings) env)
           bindings args env 0 0)
        ;; The end of the list: nil, else GNU's CHECK_LIST_END (VARLIST, XCAR (args))
        ;; (eval.c, FletX) signals (wrong-type-argument listp VARLIST) with the
        ;; whole variable list.
        (nl_sf_let_star_setup_done_w
         (if (= (sexp-tag bindings) 0)
             (extern-call nl_let_setup bindings env 1)
           (seq (bf_wrong_type_listp (wf_word_view (nl_cons_car_word args))) 1))
         args env 0)))

    ;; `(let*)' with no binding list fails the form.
    (defun nl_sf_let_star_w (args env _pad _pad1)
      (if (= (sexp-tag args) 0)
          (nl_abi_fail 1 0)
        (nl_sf_let_star_got_bindings_w (extern-call nl_cons_car_ptr args) args env 0)))

    ;; (rc, OUT) adapter.
    (defun nl_sf_let_star (args env out _pad)
      (nl_abi_word_to_rc (nl_sf_let_star_w args env 0 0) out)))

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
