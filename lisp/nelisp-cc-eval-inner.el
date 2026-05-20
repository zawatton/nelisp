;;; nelisp-cc-eval-inner.el --- Phase 47 nl_eval_inner swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for `eval_inner' + `apply_combiner' cluster in
;; `build-tool/src/eval/mod.rs'.
;;
;; Rust bodies deleted from eval/mod.rs (~124 LOC):
;;   eval_inner, apply_combiner, is_macro, is_builtin_value,
;;   is_elisp_apply_helper, eval_delegated, delegate_to_elisp_apply,
;;   delegate_macro_to_elisp, expand_macro, walk_proper_list.
;;
;; This .o exports `nl_eval_inner(form env out _pad) -> i64'.
;; It handles sexp-tag dispatch and delegates the Cons path to the
;; `nl_eval_inner_cons' Rust extern (in eval/special_forms.rs).
;;
;; ABI externs used:
;;   nl_sexp_clone_into(src_ptr dst_ptr)    — refcount-aware clone (void)
;;   nl_cons_car_ptr(cons_ptr) → i64        — *const Sexp of car
;;   nl_cons_cdr_ptr(cons_ptr) → i64        — *const Sexp of cdr
;;   nl_env_lookup_val(name env out) → i64  — Symbol var lookup (0=ok,1=err)
;;   nl_cell_get_value(cell out) → i64      — Cell value extract (0=ok,1=err)
;;   nl_eval_inner_cons(car cdr env out) → i64 — full apply_combiner (0=ok,1=err)
;;
;; Alignment rule: every `(extern-call ...)' appears as argument 0 at its
;; call site → rsp ≡ 0 mod 16 when CALL executes ✓.
;; All defun arities are even ≤ 6 ✓.
;; No `let*' with runtime values (Phase 47 let = compile-time constants only).
;;
;; Sexp tag constants:
;;   4=Symbol  7=Cons  11=Cell  (others = self-evaluating)

;;; Code:

(defconst nelisp-cc-eval-inner--source
  '(seq

    ;; ─── Self-evaluating: ignore clone return, return 0 ───
    (defun nl_ei_self_eval_done (_ret _dummy) 0)

    ;; ─── Top-level dispatcher ───

    ;; nl_eval_inner(form env out _pad) → i64
    ;; Tag dispatch on form.  Every extern-call is first arg ✓.
    (defun nl_eval_inner (form env out _pad)
      (cond
       ((= (sexp-tag form) 4)
        (extern-call nl_env_lookup_val form env out))
       ((= (sexp-tag form) 7)
        (nl_ei_cons_dispatch (extern-call nl_cons_car_ptr form) form env out))
       ((= (sexp-tag form) 11)
        (extern-call nl_cell_get_value form out))
       (t
        (nl_ei_self_eval_done (extern-call nl_sexp_clone_into form out) 0))))

    ;; Fetch cdr of Cons form (first arg = extern-call result ✓), then apply.
    (defun nl_ei_cons_dispatch (car-ptr form env out)
      (nl_ei_cons_tail
       (extern-call nl_cons_cdr_ptr form)
       car-ptr env out))

    ;; Call nl_eval_inner_cons with (car, cdr, env, out).
    ;; extern-call is first arg ✓.
    (defun nl_ei_cons_tail (cdr-ptr car-ptr env out)
      (extern-call nl_eval_inner_cons car-ptr cdr-ptr env out))

    )

  "Phase 47 source for `nl_eval_inner'.
Five defuns.  sexp-tag dispatch; Cons path delegates to nl_eval_inner_cons Rust extern.
Every extern-call is first arg ✓.  All arities even ≤ 6 ✓.")

(provide 'nelisp-cc-eval-inner)

;;; nelisp-cc-eval-inner.el ends here
