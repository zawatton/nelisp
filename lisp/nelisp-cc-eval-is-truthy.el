;;; nelisp-cc-eval-is-truthy.el --- nl_eval_is_truthy Phase 47 swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 134 Stage 134.A re-provision of `nl_eval_is_truthy' as a
;; Phase 47-compiled elisp .o after commit fa8932eb deleted its Rust
;; `#[no_mangle]' body from `build-tool/src/eval/mod.rs'.
;;
;; Recovered signature (from fa8932eb^:build-tool/src/eval/mod.rs):
;;
;;   #[no_mangle]
;;   pub unsafe extern "C" fn nl_eval_is_truthy(
;;       form: *const Sexp,
;;       env:  *mut std::ffi::c_void,
;;   ) -> i64 {
;;       match super::eval(&*form, &mut *(env as *mut Env)) {
;;           Ok(Sexp::Nil) => 0,
;;           Ok(_)         => 1,
;;           Err(_)        => -1,
;;       }
;;   }
;;
;; The function evaluates FORM in ENV and returns:
;;   0   — evaluated result is Sexp::Nil (falsy)
;;   1   — evaluated result is any non-Nil Sexp (truthy)
;;  -1   — evaluation produced an error
;;
;; This is an extern-call-eval implementation: it calls `nelisp_eval_call'
;; (the Rust C-ABI wrapper for `super::eval', retained in mod.rs) to
;; evaluate the form and writes the result into a 32-byte heap-allocated
;; scratch slot.  The scratch slot is freed on all return paths.
;;
;; `nelisp_eval_call' signature:
;;   extern "C" fn nelisp_eval_call(
;;       form: *const Sexp,
;;       env:  *mut c_void,
;;       out:  *mut Sexp,
;;   ) -> i64    // 0 = Ok, non-0 = Err
;;
;; After the call the scratch slot contains the evaluated Sexp.
;; `sexp-tag' reads byte 0 of the slot (= the variant discriminant).
;; `nelisp-sexp--tag-nil' = 0.  Any non-zero tag means truthy.
;;
;; NlConsBox layout (§100.B frozen):
;;   sizeof::<Sexp>() = 32  → scratch allocation = 32 bytes, align 8.
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *const Sexp  (form to evaluate)
;;   rsi = *mut c_void  (Env pointer)
;;   return: i64 in rax — 0 (falsy), 1 (truthy), or -1 (error)
;;
;; Alignment rule: `extern-call' appears as argument 0 at its call
;; site.  In `nl_eit_with_scratch' the `(extern-call nelisp_eval_call
;; form env scratch)' is argument 0 of `nl_eit_rc_check', so rsp is
;; aligned to 16 when CALL executes ✓.  `alloc-bytes' and `sexp-tag'
;; are Phase 47 native ops, not extern-calls, so they have no
;; alignment constraint.
;;
;; Linux-x86_64 only — same `:requires-arch x86_64' gate as sibling
;; files that use `extern-call' / `alloc-bytes' / `dealloc-bytes'.
;;
;; Helper structure (4 defuns + 1 public entry):
;;
;;   nl_eit_prog1 (val _eff) -> val
;;     2-arg sequencer: evaluates both args left-to-right, returns the
;;     first.  Used to pair the result integer with the `dealloc-bytes'
;;     side-effect without losing the value (unlike `and', which
;;     short-circuits on a falsy first arg — i.e., 0 = falsy/Nil
;;     result would incorrectly drop the dealloc).
;;
;;   nl_eit_tag_check (tag scratch) -> i64
;;     Checks tag: 0 = Nil ⇒ return 0 (falsy); non-0 ⇒ return 1
;;     (truthy).  Deallocates `scratch' as a side-effect via prog1.
;;     Arity 2 (even) ✓.
;;
;;   nl_eit_rc_check (rc scratch) -> i64
;;     Checks eval return code: 0 = Ok ⇒ call nl_eit_tag_check;
;;     non-0 = Err ⇒ free scratch and return -1.
;;     Arity 2 (even) ✓.
;;
;;   nl_eit_with_scratch (scratch form env) -> i64
;;     Calls `nelisp_eval_call' (extern-call FIRST ✓), passes result
;;     and scratch to `nl_eit_rc_check'.
;;     Arity 3 (odd) — Phase 47 emits the rsp alignment pad via the
;;     `--needs-align' branch (same as `nelisp_meta_walk' arity 3).
;;
;;   nl_eval_is_truthy (form env) -> i64
;;     Public C-ABI entry.  Allocates 32-byte scratch via `alloc-bytes'
;;     (native op, no alignment constraint), dispatches to
;;     `nl_eit_with_scratch'.  Arity 2 (even) ✓.

;;; Code:

(defconst nelisp-cc-eval-is-truthy--source
  '(seq

    ;; ---------------------------------------------------------------
    ;; nl_eit_prog1: evaluate both args, return the first.
    ;; Used to pair a return value with a dealloc-bytes side-effect.
    ;; Returns VAL regardless of whether it is 0 (falsy in Elisp —
    ;; `and' would short-circuit, losing the dealloc).
    ;; Arity 2 (even) ✓.
    (defun nl_eit_prog1 (val _eff) val)

    ;; ---------------------------------------------------------------
    ;; nl_eit_tag_check: decide truthy/falsy from the evaluated Sexp tag.
    ;; tag:    i64 = sexp-tag(scratch) — byte 0 of the Sexp variant slot.
    ;;         0 = Sexp::Nil (nelisp-sexp--tag-nil); any other = truthy.
    ;; scratch: *mut Sexp — the 32-byte heap slot to free as side-effect.
    ;; Returns 0 (falsy) or 1 (truthy) and frees scratch via nl_eit_prog1.
    ;; Arity 2 (even) ✓.
    (defun nl_eit_tag_check (tag scratch)
      (nl_eit_prog1
       (if (= tag 0) 0 1)
       (dealloc-bytes scratch 32 8)))

    ;; ---------------------------------------------------------------
    ;; nl_eit_rc_check: branch on the return code from nelisp_eval_call.
    ;; rc:     i64 — 0 = eval Ok, non-0 = eval error.
    ;; scratch: *mut Sexp — result slot; contains evaluated Sexp when rc=0.
    ;; On error: frees scratch and returns -1.
    ;; On success: delegates to nl_eit_tag_check to read the tag and free.
    ;; Arity 2 (even) ✓.
    (defun nl_eit_rc_check (rc scratch)
      (if (= rc 0)
          (nl_eit_tag_check (sexp-tag scratch) scratch)
        (nl_eit_prog1 -1 (dealloc-bytes scratch 32 8))))

    ;; ---------------------------------------------------------------
    ;; nl_eit_with_scratch: evaluate FORM via nelisp_eval_call (extern-call
    ;; FIRST ✓ — it is argument 0 of nl_eit_rc_check, so rsp is aligned
    ;; to 16 when the CALL instruction executes), then pass rc + scratch
    ;; to nl_eit_rc_check for tag check and cleanup.
    ;; scratch: *mut Sexp — 32-byte heap slot (owned by this call chain).
    ;; form:    *const Sexp — unevaluated form pointer.
    ;; env:     *mut c_void — Env pointer.
    ;; Arity 3 (odd) — Phase 47 emits rsp alignment pad ✓.
    (defun nl_eit_with_scratch (scratch form env)
      (nl_eit_rc_check
       (extern-call nelisp_eval_call form env scratch)
       scratch))

    ;; ---------------------------------------------------------------
    ;; nl_eval_is_truthy: public C-ABI entry.
    ;; form: *const Sexp — unevaluated form to test.
    ;; env:  *mut c_void — Env pointer.
    ;; Returns 0 (nil), 1 (truthy), or -1 (eval error).
    ;; alloc-bytes is a Phase 47 native op (not extern-call): no
    ;; alignment constraint.  The scratch slot is passed to
    ;; nl_eit_with_scratch which frees it on all paths.
    ;; Arity 2 (even) ✓.
    (defun nl_eval_is_truthy (form env)
      (nl_eit_with_scratch (alloc-bytes 32 8) form env))

    )
  "Doc 134 Stage 134.A Phase 47 source for `nl_eval_is_truthy'.

Five-entry `(seq DEFUN ...)' manifest.

Recovered from fa8932eb^:build-tool/src/eval/mod.rs:
  pub unsafe extern \"C\" fn nl_eval_is_truthy(form, env) -> i64 {
    match super::eval(&*form, &mut *(env as *mut Env)) {
      Ok(Sexp::Nil) => 0, Ok(_) => 1, Err(_) => -1 } }

Extern-call-eval type: calls nelisp_eval_call (retained Rust wrapper)
to evaluate FORM, writes result to a heap scratch slot, checks tag.

Returns: 0 (Nil/falsy), 1 (truthy), -1 (eval error).
Arity alignment: nl_eval_is_truthy arity 2, nl_eit_with_scratch arity 3
(odd → --needs-align), nl_eit_rc_check/nl_eit_tag_check/nl_eit_prog1
arity 2.  extern-call nelisp_eval_call is arg-0 of nl_eit_rc_check ✓.

Phase 47 ops:
  alloc-bytes 32 8 / dealloc-bytes 32 8  — scratch Sexp slot.
  extern-call nelisp_eval_call           — evaluate form.
  sexp-tag                               — read Sexp::Nil discriminant.
  = if literal-0 literal-1 literal-(-1) — dispatch and return.

Net Rust delta: zero.  Resolves the nl_eval_is_truthy undefined symbol.")

(provide 'nelisp-cc-eval-is-truthy)

;;; nelisp-cc-eval-is-truthy.el ends here
