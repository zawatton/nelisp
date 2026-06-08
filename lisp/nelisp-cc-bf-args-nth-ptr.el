;;; nelisp-cc-bf-args-nth-ptr.el --- AOT nl_bf_args_nth_ptr swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `nl_bf_args_nth_ptr' Rust extern in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was (~11 LOC):
;;
;;   #[no_mangle]
;;   pub unsafe extern "C" fn nl_bf_args_nth_ptr(
;;       args_ptr: *const Sexp, idx: i64) -> i64 {
;;       let mut cur = &*args_ptr;
;;       let mut remaining = idx;
;;       while let Sexp::Cons(b) = cur {
;;           if remaining == 0 { return &b.car as *const Sexp as i64; }
;;           remaining -= 1;
;;           cur = &b.cdr;
;;       }
;;       0
;;   }
;;
;; Contract:
;;   args_ptr  *const Sexp — pointer to a Sexp::Cons list (arg list).
;;   idx       i64         — 0-based index of the argument to fetch.
;;   returns   i64 = *const Sexp (pointer to the car field of the
;;                  idx-th cons cell), or 0 when idx >= list length.
;;
;; Layout constants (§100.B + Doc 147 frozen):
;;   SEXP_TAG_CONS = 7
;;   Cons car/cdr slots are 8-byte tagged WORDs; use nl_cons_car_ptr and
;;   nl_cons_cdr_ptr to obtain 32B Sexp views.
;;
;; Implementation:
;;   `nl_bf_args_nth_step (cur remaining)' — recursive list walker.
;;     cur:       i64 = *const Sexp of the current list cell.
;;     remaining: i64 countdown (0 → return car ptr of cur).
;;
;;   `(sexp-tag cur)' reads the tag byte at [cur+0].  When == 7 (Cons):
;;     - remaining == 0: return `(nl_cons_car_ptr cur)'.
;;     - remaining >  0: recurse with cur = `(nl_cons_cdr_ptr cur)' and
;;                       remaining - 1.
;;   Non-Cons (nil or end): return 0.
;;
;;   `nl_bf_args_nth_ptr' is the public entry, a thin wrapper over the
;;   recursive helper (keeps arity even = 2 on both functions).
;;
;; AOT ops consumed:
;;   `(sexp-tag PTR)'          — tag byte check (inline).
;;   `nl_cons_car_ptr' / `nl_cons_cdr_ptr' — materialised cons slot views.
;;   `(- A B)'                — counter decrement.
;;   `(if TEST THEN ELSE)'     — dispatch.
;;   `(= A B)'                 — comparison.
;;   Recursive defun call      — tail-call walk pattern (CPS-style).
;;
;; Arity: both defuns arity 2 (even) → body-entry rsp ≡ 0 mod 16.
;; `nl_bf_args_nth_step' is a private helper (no callers outside this .o
;; at link time) but must be a named defun since AOT `let' is
;; compile-time-constant-only.
;;
;; Net Rust impact: deletes the 11-LOC `nl_bf_args_nth_ptr' body from
;; `build-tool/src/eval/special_forms.rs'.  The `#[no_mangle]' symbol
;; `nl_bf_args_nth_ptr' is now provided by this `.o'.  The private
;; helper `nl_bf_args_nth_step' is also exported (local to the .o from
;; the caller's perspective — no conflict with other .o files).

;;; Code:

(defconst nelisp-cc-bf-args-nth-ptr--source
  '(seq

    ;; nl_bf_args_nth_step — recursive list-walker helper.
    ;;
    ;; cur:       *const Sexp — current position in the cons list.
    ;; remaining: i64 countdown (0 = return car-ptr here).
    ;;
    ;; Tail-call recursion: each step follows the cdr through
    ;; `nl_cons_cdr_ptr', which materialises 8-byte tagged WORD cdrs
    ;; as 32B Sexp views.
    ;;
    ;; Doc 147 Phase 3 shrank cons car/cdr to 8-byte tagged WORD slots.
    ;; Use nl_cons_car_ptr/nl_cons_cdr_ptr so immediate values are
    ;; materialised as 32B Sexp views before bind_formals consumes them.
    ;;
    ;; Arity 2 (even).
    (defun nl_bf_args_nth_step (cur remaining)
      (if (= (sexp-tag cur) 7)
          (if (= remaining 0)
              (nl_cons_car_ptr cur)
            (nl_bf_args_nth_step (nl_cons_cdr_ptr cur) (- remaining 1)))
        0))

    ;; nl_bf_args_nth_ptr — public entry point.
    ;;
    ;; args_ptr: *const Sexp — head of the argument cons list.
    ;; idx:      i64         — 0-based index.
    ;; Returns:  i64 = *const Sexp of args[idx].car, or 0.
    ;;
    ;; Arity 2 (even).
    (defun nl_bf_args_nth_ptr (args_ptr idx)
      (nl_bf_args_nth_step args_ptr idx)))

  "AOT source for `nl_bf_args_nth_ptr' (special_forms.rs → elisp).

Two defuns in a `seq' form:
  `nl_bf_args_nth_step (cur remaining)' — recursive walker (arity 2).
  `nl_bf_args_nth_ptr  (args_ptr idx)' — public entry (arity 2).

The walker follows cons cells via nl_cons_cdr_ptr so Doc 147 8-byte
tagged WORD cdr slots are materialised when needed.

On a hit (remaining == 0 and cur is Cons):
  return = nl_cons_car_ptr(cur), a 32B Sexp view of the car value.

Called via `(extern-call nl_bf_args_nth_ptr args IDX)' from
`nelisp-cc-bind-formals.el' in the Required-mode dispatch branch
of `nl_bf_tag'.")

(provide 'nelisp-cc-bf-args-nth-ptr)

;;; nelisp-cc-bf-args-nth-ptr.el ends here
