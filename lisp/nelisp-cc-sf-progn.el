;;; nelisp-cc-sf-progn.el --- AOT nl_sf_progn swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_progn' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_progn(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       eval_body(&parts, env)
;;   }
;;
;; `args' is the raw unevaluated argument list for `(progn FORM...)'
;; — a cons list (or Nil for empty progn).  Each form is eval'd in
;; sequence via `nelisp_eval_call'; result is the last form's value.
;;
;; ABI constants used:
;;   SEXP_TAG_NIL  = 0
;;   nl_cons_car_ptr: *const Sexp → i64 (= &car, or 0 on non-Cons)
;;   nl_cons_cdr_ptr: *const Sexp → i64 (= &cdr, or 0 on non-Cons)
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;
;; Alignment notes — "extern-call first" rule:
;;   Every `(extern-call ...)' is passed as the FIRST argument at its
;;   call site so rsp is 0 mod 16 (0 items on stack) when the call
;;   happens.  Continuation functions receive the extern-call result
;;   as their first parameter and do all tag-checks / tail-calls after.
;;
;; GC invariant: nl_cons_cdr_ptr materialises a fresh unrooted view when
;; the cdr is immediate (including Nil).  Carry the real cons box across
;; nelisp_eval_call and take its cdr only after eval, so that materialised-view
;; hazard cannot cross a possible collection.
;;
;; Structure (4 defuns):
;;   nl_sf_progn_eval_step (eval-rc cdr env out) — arity 4, even
;;     called after nelisp_eval_call; checks rc, then fetches cdr and recurses.
;;   nl_sf_progn_body_step (car-ptr cdr env out) — arity 4, even
;;     receives car-ptr already computed, calls nelisp_eval_call.
;;   nl_sf_progn_step (cdr env out _pad) — arity 4, even
;;     recursive step: if cdr Nil → return 0; else fetch its car for eval.
;;   nl_sf_progn (args env out _pad) — arity 4, even
;;     public entry: empty-list check, then evaluates args' car.

;;; Code:

(defconst nelisp-cc-sf-progn--source
  '(seq
    ;; Doc 215 word ABI.  `nl_sf_progn_w' answers the last form's value
    ;; WORD (nil 3 for an empty body), or 0 with the signal stashed.
    ;; Arity 4 (even) throughout; every extern-call is the first argument.

    ;; Evaluate one body form (CUR is the real cons whose car it is).
    (defun nl_sf_progn_eval_w (car-ptr cur env _pad)
      (extern-call nelisp_eval_call_w car-ptr env 0))

    ;; The next loop state after CUR: 0 at the end, else the tail.  Read
    ;; the cdr WORD; only a non-nil immediate (dotted tail) still needs the
    ;; materialised view.
    (defun nl_sf_progn_next (cur _pad)
      (let ((tail (ptr-read-u64 (ptr-read-u64 cur 8) 8)))
        (if (= (logand tail 1) 1)
            (if (= tail 3) 0 (extern-call nl_cons_cdr_ptr cur))
          (if (= (sexp-tag tail) 0) 0 tail))))

    ;; The returned WORD is produced by the last eval with no safepoint
    ;; after it, so it needs no root (Doc 215 R1/R2).
    (defun nl_sf_progn_w (args env _pad _pad1)
      (let ((state (if (= (sexp-tag args) 0) 0 args)) (w 3))
        (seq
         (while (> state 0)
           (seq
            (setq w (nl_sf_progn_eval_w
                     (extern-call nl_cons_car_ptr state) state env 0))
            (setq state (if (= w 0) -1 (nl_sf_progn_next state 0)))))
         (if (= state -1) 0 w))))

    ;; (rc, OUT) adapter.
    (defun nl_sf_progn (args env out _pad)
      (nl_abi_word_to_rc (nl_sf_progn_w args env 0 0) out)))
  "AOT source for `nl_sf_progn' (eval/special_forms.rs sf_progn → elisp).

Four defuns (seq form):
  `nl_sf_progn_eval_step (eval-rc cdr env out)' — arity-4 continuation
    after nelisp_eval_call; propagates error or fetches the tail and recurses.
  `nl_sf_progn_body_step (car-ptr cdr env out)' — arity-4 helper
    calls nelisp_eval_call (extern-call FIRST), carrying the real cons box.
  `nl_sf_progn_step (cdr env out _pad)' — arity-4 recursive entry
    checks if cdr is Nil (done) or fetches its car (FIRST) for eval.
  `nl_sf_progn (args env out _pad)' — arity-4 public entry
    empty-progn shortcut or fetches args' car (FIRST) for eval.

The cdr is deliberately taken after eval: nl_cons_cdr_ptr materialises an
unrooted view for an immediate cdr, and such a view cannot safely cross an
eval that may collect.

Alignment: every extern-call is argument 0 at its call site → rsp is
0 mod 16 when the extern-call happens.")

(provide 'nelisp-cc-sf-progn)

;;; nelisp-cc-sf-progn.el ends here
