;;; nelisp-cc-eval-inner.el --- AOT nl_eval_inner swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for `eval_inner' + `apply_combiner' cluster in
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
;; No `let*' with runtime values (AOT let = compile-time constants only).
;;
;; Sexp tag constants:
;;   4=Symbol  7=Cons  11=Cell  (others = self-evaluating)
;;
;; --- Silent-truncation fix (fix/silent-truncation-signal) ---
;;
;; `nl_env_lookup_val' returns rc=1 on an unbound variable but, unlike the
;; unbound-HEAD-symbol case in `nl_eval_inner_cons' (which stashes a
;; `(void-function SYM)' signal via `nl_cons_stash_void_function' in
;; evalport-combiner-cons), it left the shared M6 signal-stash region
;; (flag@268435472, TAG@268435480, VAL@268435512 -- see the `bf_signal'/
;; `bf_error' comment block in nelisp-standalone-build.el) completely
;; untouched.  Two consumers key off that same flag:
;;   1. `nl_eval_source_report_error''s caller in `nl_eval_source_all'
;;      only reports+halts when flag != 0 -- so this rc=1 was silently
;;      swallowed instead of reported.
;;   2. `nelisp_eval_call_with_err' (the condition-case-trapping eval
;;      variant) only populates err_out when flag == 1 -- so
;;      `condition-case' could never match this signal, and the whole
;;      protected form (or top-level form) was silently truncated.
;; `nl_stash_void_variable' below builds the same `(void-variable SYM)'
;; signal shape real Emacs uses and writes it into the M6 region, mirroring
;; `nl_cons_stash_void_function' field-for-field (same 13-char tag-name
;; buffer layout, same cons/pair assembly, same flag + mutation-epoch
;; bump) so both consumers now see a genuine stashed signal.  `nl_ei_var_done'
;; wires it into the Symbol(4) dispatch arm without changing the rc value
;; itself (an unbound reference still returns rc=1 and still aborts the
;; rest of the enclosing compound form exactly as before -- only the
;; "invisible" part of the defect is fixed).

;;; Code:

(defconst nelisp-cc-eval-inner--source
  '(seq

    ;; ─── Self-evaluating: ignore clone return, return 0 ───
    (defun nl_ei_self_eval_done (_ret _dummy) 0)

    ;; ─── Unbound-variable signal stash (parity with void-function) ───
    ;;
    ;; Builds `(void-variable NAME_PTR)' into the M6 stash: TAG@268435480 =
    ;; the "void-variable" Symbol, VAL@268435512 = (NAME_PTR).  Field-for-
    ;; field copy of `nl_cons_stash_void_function' (evalport-combiner-cons):
    ;; same tag-buffer layout (16-byte scratch, 13 real bytes), same
    ;; clone/nil/pair assembly (including the otherwise-unread `signal_slot'
    ;; cons, kept only because that extra `nelisp_cons_construct' call is
    ;; what bumps TAG_SLOT/PAIR_SLOT's refcounts before the raw 32-byte
    ;; `bf_sig_copy32' copies -- dropping it would be an unproven refcount-
    ;; safety deviation from the working pattern), same flag + mutation-
    ;; epoch bump.  `env' is unused (matches `nl_cons_stash_void_function',
    ;; which also takes an unused `env' for signature symmetry).
    (defun nl_stash_void_variable (env name_ptr)
      (let* ((tag_buf (alloc-bytes 16 1))
             (tag_slot (alloc-bytes 32 8))
             (clone_slot (alloc-bytes 32 8))
             (nil_slot (alloc-bytes 32 8))
             (pair_slot (alloc-bytes 32 8))
             (signal_slot (alloc-bytes 32 8)))
        (seq
         (seq (ptr-write-u64 tag_buf 0 8241998730394955638)
              (ptr-write-u64 (+ tag_buf 8) 0 435610083689)
              (nl_alloc_symbol tag_buf 13 tag_slot)
              (nl_sexp_clone_into name_ptr clone_slot))
         (nl_cons_write_nil nil_slot)
         (seq (nelisp_cons_construct clone_slot nil_slot pair_slot)
              (nelisp_cons_construct tag_slot pair_slot signal_slot))
         (seq (bf_sig_copy32 268435480 tag_slot)
              (bf_sig_copy32 268435512 pair_slot)
              (ptr-write-u64 268435472 0 1)
              (atomic-fetch-add 268435544 1)
              1))))

    ;; After the extern-call lookup (arg0, per the ABI alignment rule):
    ;; rc=0 -> value already written to `out' by the lookup itself, return
    ;; 0 unchanged.  rc=1 -> unbound; stash the void-variable signal (the
    ;; rc value returned upward is still 1, so the caller's abort-the-rest-
    ;; of-the-form behavior is byte-identical to before this fix).
    (defun nl_ei_var_done (rc form env _pad)
      (if (= rc 0) 0 (nl_stash_void_variable env form)))

    ;; ─── Top-level dispatcher ───

    ;; nl_eval_inner(form env out _pad) → i64
    ;; Tag dispatch on form.  Every extern-call is first arg ✓.
    (defun nl_eval_inner (form env out _pad)
      (cond
       ((or (= (sexp-tag form) 4) (= (sexp-tag form) 16))
        (nl_ei_var_done (extern-call nl_env_lookup_val form env out) form env 0))
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

  "AOT source for `nl_eval_inner'.
Seven defuns (added `nl_stash_void_variable' + `nl_ei_var_done' for the
silent-truncation fix: an unbound Symbol(4) lookup now stashes a
`(void-variable SYM)' M6 signal, same shape/mechanism as the existing
`(void-function SYM)' stash for an unbound head symbol, instead of
returning rc=1 with nothing recorded).  sexp-tag dispatch; Cons path
delegates to nl_eval_inner_cons Rust extern.
Every extern-call is first arg ✓.  All arities even ≤ 6 ✓.")

(provide 'nelisp-cc-eval-inner)

;;; nelisp-cc-eval-inner.el ends here
