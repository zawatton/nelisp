;;; nelisp-cc-sf-if.el --- AOT nl_sf_if swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_if' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_if(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       expect_min_len(&parts, "if", 2)?;
;;       let cond = eval(&parts[0], env)?;
;;       if is_truthy(&cond) {
;;           eval(&parts[1], env)
;;       } else {
;;           let mut last = Sexp::Nil;
;;           for f in parts.iter().skip(2) { last = eval(f, env)?; }
;;           Ok(last)
;;       }
;;   }
;;
;; `args' is the raw arg list: (TEST THEN ELSE...).
;;
;; ABI:
;;   nl_cons_car_ptr: *const Sexp → i64
;;   nl_cons_cdr_ptr: *const Sexp → i64
;;   nl_eval_is_truthy: (WORD, *mut c_void) → i64
;;     1=truthy, 0=nil, -1=eval error
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;
;; Alignment: every extern-call is argument 0 at its call site.
;; Pattern: (f (extern-call g ...) other-args...) — g called with 0
;; items on stack, result passed as first register arg to f. ✓
;; Never: (= (extern-call g ...) LITERAL) — that pushes LITERAL first.
;;
;; Else-loop GC invariant:
;;   nl_cons_cdr_ptr returns a real child box for a pointer cdr, but
;;   materialises a fresh unrooted view for an immediate cdr (including Nil).
;;   Therefore the else-list CONS itself is carried across nelisp_eval_call,
;;   and its cdr is taken only after eval returns; no materialised view crosses
;;   eval's possible collection.
;;
;; Structure (9 defuns):
;;   Else-branch (progn-like walk):
;;   nl_sf_if_else_rc  (rc els env out)  — check rc, get cdr after eval (FIRST)
;;   nl_sf_if_else_eval(car els env out) — call nelisp_eval_call (FIRST)
;;   nl_sf_if_else     (els env out _pad)— else-list walk entry
;;   Then-branch:
;;   nl_sf_if_then_eval(form env out _pad)— call nelisp_eval_call (FIRST)
;;   Branch dispatch:
;;   nl_sf_if_branch   (truthy cdr env out)— dispatch on truthy
;;   Preamble:
;;   nl_sf_if_truthy   (truthy cdr env out)— after nl_eval_is_truthy
;;   nl_sf_if_test_ptr (test-ptr cdr env out)— get truthy (FIRST)
;;   nl_sf_if_cdr      (cdr args env out) — get test-ptr=car(args) (FIRST)
;;   nl_sf_if          (args env out _pad)— public entry, arity 4 (even)

;;; Code:

(defconst nelisp-cc-sf-if--source
  '(seq
    ;; Doc 215 word ABI.  `nl_sf_if_w' answers the chosen branch's value
    ;; WORD (nil 3 for an empty else), or 0 with the signal stashed.
    ;; Same CPS chain as before: each extern-call is the first argument,
    ;; arity 4 (even).
    (defun nl_sf_if_then_w (form env _pad _pad1)
      (extern-call nelisp_eval_call_w form env 0))

    ;; TRUTHY: 1 true, 0 false, -1 the test signalled (already stashed).
    ;; The else list is a progn; it is read after the test, from the real
    ;; (THEN ELSE...) cons, so no materialised view lives across eval.
    (defun nl_sf_if_branch_w (truthy cdr env _pad)
      (if (= truthy -1)
          0
        (if (= truthy 0)
            (nl_sf_progn_w (extern-call nl_cons_cdr_word cdr) env 0 0)
          (nl_sf_if_then_w (extern-call nl_cons_car_word cdr) env 0 0))))

    (defun nl_sf_if_test_w (test-ptr cdr env _pad)
      (nl_sf_if_branch_w (extern-call nl_eval_is_truthy test-ptr env) cdr env 0))

    (defun nl_sf_if_cdr_w (cdr args env _pad)
      (nl_sf_if_test_w (extern-call nl_cons_car_word args) cdr env 0))

    (defun nl_sf_if_w (args env _pad _pad1)
      (nl_sf_if_cdr_w (extern-call nl_cons_cdr_ptr args) args env 0))

    ;; (rc, OUT) adapter.
    (defun nl_sf_if (args env out _pad)
      (nl_abi_word_to_rc (nl_sf_if_w args env 0 0) out)))

  "AOT source for `nl_sf_if' (eval/special_forms.rs sf_if → elisp).

Nine defuns (seq form).  Each extern-call is argument 0 at its call site.

Entry chain:
  nl_sf_if → nl_sf_if_cdr → nl_sf_if_test_ptr → nl_sf_if_truthy →
  nl_sf_if_branch → then: nl_sf_if_then_eval
                  → else: nl_sf_if_else → nl_sf_if_else_eval →
                          nl_sf_if_else_rc → (cdr after eval) → (recurse)

Alignment fix: every extern-call is the first evaluated argument so rsp
is 0 mod 16 when each extern-call instruction executes.")

(provide 'nelisp-cc-sf-if)

;;; nelisp-cc-sf-if.el ends here
