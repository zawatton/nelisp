;;; nelisp-cc-sf-setq.el --- AOT nl_sf_setq swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_setq' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_setq(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       if parts.len() % 2 != 0 { return Err(...); }
;;       let mut last = Sexp::Nil;
;;       while let Some(name_form) = iter.next() {
;;           let value_form = iter.next().unwrap();
;;           let name = match name_form { Symbol(s) => s, ... };
;;           let val = eval(&value_form, env)?;
;;           env.set_value(&name, val.clone())?;
;;           last = val;
;;       }
;;       Ok(last)
;;   }
;;
;; `args' is the raw arg list: (SYM1 VAL1 SYM2 VAL2 ...).
;; Pairs: car=symbol-ptr (not eval'd), car(cdr)=value-form (eval'd).
;; Result slot `out' receives each value; last pair's value remains.
;;
;; ABI:
;;   nl_cons_car_ptr: *const Sexp → i64
;;   nl_cons_cdr_ptr: *const Sexp → i64
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;   nl_env_set_value: (*mut c_void, *const Sexp, *const Sexp) → i64
;;
;; Alignment: every extern-call is argument 0 at its call site.
;; CPS chain: each function receives exactly one extern-call result
;; as its first register argument, then does tag-checks or delegates.
;;
;; perf/setq-pair-cdr-raw-word (segment G2): `nl_sf_setq_evaled' used to
;; fetch the next-pair-or-Nil cdr via the materialising `nl_cons_cdr_ptr'
;; (lisp/nelisp-cc-jit-cons-cdr-ptr.el), which allocates a fresh 32-byte
;; box whenever that cdr is an immediate WORD -- which it always is for
;; the common one-pair `setq' (the Nil list terminator).  `nl_sf_setq_
;; cdr_word' reads the raw 8-byte tagged WORD directly instead, and
;; `nl_sf_setq_rest' / `nl_sf_setq_set_rc' / `nl_sf_setq_step' now thread
;; that WORD (classified via `nl_val_tag', not `sexp-tag') instead of a
;; materialised pointer.  See the commentary on `nl_sf_setq_cdr_word'
;; below for why this is behavior-preserving.
;;
;; Structure (9 defuns):
;;   nl_sf_setq_cdr_word (val-cdr) — raw cdr WORD, no allocation
;;   nl_sf_setq_set_rc (set-rc rest env out) — after set_value
;;   nl_sf_setq_rest   (rest sym env out) — after cdr-word(val-cdr)
;;   nl_sf_setq_evaled (eval-rc sym val-cdr env out _pad6) — after nelisp_eval_call, arity 6
;;   nl_sf_setq_val    (val-form sym val-cdr env out _pad6) — after car(val-cdr), arity 6
;;   nl_sf_setq_sym    (sym val-cdr env out) — after car(args-pair)
;;   nl_sf_setq_pair   (val-cdr args-pair env out) — after cdr(args-pair)
;;   nl_sf_setq_step   (cdr env out _pad) — recursive entry, cdr is a raw WORD
;;   nl_sf_setq        (args env out _pad) — public entry, arity 4 (even)

;;; Code:

(defconst nelisp-cc-sf-setq--source
  '(seq
    ;; Doc 215 word ABI.  `nl_sf_setq_w' answers the last assigned value
    ;; WORD (nil 3 for `(setq)'), or 0 with the signal stashed.  Each
    ;; extern-call is the first argument; arity even.

    ;; The cdr WORD of the (VAL . REST) cons (0 for a non-cons, like
    ;; `nl_cons_cdr_ptr'); usually the nil terminator, an immediate.
    (defun nl_sf_setq_cdr_word (val-cdr)
      (if (= (sexp-tag val-cdr) 7)
          (heap-read-u64 (heap-sexp-payload val-cdr) 8)
        0))

    ;; Next pair, or W (the value just assigned) at the end.  W is
    ;; referenced from the variable's cell by now, so it stays reachable.
    (defun nl_sf_setq_step_w (rest env w _pad)
      (if (= (nl_val_tag rest) 0)
          w
        (nl_sf_setq_pair_w (extern-call nl_cons_cdr_ptr rest) rest env 0)))

    (defun nl_sf_setq_set_w (set-rc rest env w)
      (if (= set-rc 0) (nl_sf_setq_step_w rest env w 0) (nl_abi_fail set-rc 0)))

    (defun nl_sf_setq_rest_w (rest w sym env _pad _pad6)
      (nl_sf_setq_set_w (extern-call nl_env_set_value env sym w) rest env w))

    ;; Doc 226 D9 stage 3.2: PAIR is the form's own cons (stable under the
    ;; evaluation); the variable is read from it after the value is
    ;; computed.  A symbol stored as its object WORD has no 32-byte view in
    ;; the form, so `nl_cons_car_ptr' makes a transient one, and a view
    ;; made before the evaluation would be held across a collection by
    ;; nothing but the native stack.
    (defun nl_sf_setq_evaled_w (w pair val-cdr env _pad _pad6)
      (if (= w 0)
          0
        (if (if (= (logand pair 7) 2)
                (if (= (logand val-cdr 7) 2) (= (ptr-read-u64 val-cdr 6) 3) 0)
              0)
            ;; Read NAME after evaluation, as on the general path. No
            ;; transient symbol view or binding pointer crosses that call.
            (let* ((rc (extern-call nl_env_set_value env (ptr-read-u64 (- pair 2) 0) w)))
              (if (= rc 0) w (nl_abi_fail rc 0)))
        ;; The setter and its cell/mirror writers accept WORDs. A view
        ;; here would be cloned into the binding and then retained by
        ;; every cons/record that reads that scalar variable.
        (nl_sf_setq_rest_w (nl_sf_setq_cdr_word val-cdr) w
                           (extern-call nl_cons_car_ptr pair) env 0 0))))

    (defun nl_sf_setq_val_w (val-form pair val-cdr env _pad _pad6)
      (nl_sf_setq_evaled_w (extern-call nelisp_eval_call_w val-form env 0)
                           pair val-cdr env 0 0))

    (defun nl_sf_setq_sym_w (pair val-cdr env _pad)
      (nl_sf_setq_val_w (extern-call nl_cons_car_word val-cdr) pair val-cdr env 0 0))

    (defun nl_sf_setq_pair_w (val-cdr args-pair env _pad)
      (nl_sf_setq_sym_w args-pair val-cdr env 0))

    (defun nl_sf_setq_w (args env _pad _pad1)
      (if (= (logand args 7) 2)
          (let* ((tail (ptr-read-u64 args 6)))
            (if (if (= (logand tail 7) 2) (= (ptr-read-u64 tail 6) 3) 0)
                (nl_sf_setq_evaled_w
                 (extern-call nelisp_eval_call_w (ptr-read-u64 (- tail 2) 0) env 0)
                 args tail env 0 0)
              (nl_sf_setq_pair_w (extern-call nl_cons_cdr_ptr args) args env 0)))
        (if (= (sexp-tag args) 0)
          3
        (nl_sf_setq_pair_w (extern-call nl_cons_cdr_ptr args) args env 0))))

    ;; (rc, OUT) adapter.
    (defun nl_sf_setq (args env out _pad)
      (nl_abi_word_to_rc (nl_sf_setq_w args env 0 0) out)))

  "AOT source for `nl_sf_setq' (eval/special_forms.rs sf_setq → elisp).

Eight defuns (seq form).  CPS chain with one extern-call per step.

Entry chain:
  nl_sf_setq → (cdr FIRST) → nl_sf_setq_pair
  → (car FIRST) → nl_sf_setq_sym
  → (car(val-cdr) FIRST) → nl_sf_setq_val
  → (nelisp_eval_call FIRST) → nl_sf_setq_evaled
  → (cdr(val-cdr) FIRST) → nl_sf_setq_rest
  → (nl_env_set_value FIRST) → nl_sf_setq_set_rc
  → nl_sf_setq_step (recurse)

Each extern-call is argument 0 at its call site → rsp 0 mod 16 ✓.")

(provide 'nelisp-cc-sf-setq)

;;; nelisp-cc-sf-setq.el ends here
