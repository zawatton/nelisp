;;; nelisp-cc-sf-lambda.el --- AOT nl_sf_lambda swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_lambda' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_lambda(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       expect_min_len(&parts, "lambda", 1)?;
;;       let formals = parts[0].clone();
;;       let body: Vec<Sexp> = parts.iter().skip(1).cloned().collect();
;;       let captured = env.capture_lexical();
;;       let mut chain = vec![Sexp::Symbol("closure".into()), captured, formals];
;;       chain.extend(body);
;;       Ok(Sexp::list_from(&chain))
;;   }
;;
;; `args' is the raw unevaluated argument list for `(lambda FORMALS BODY...)' —
;; a Sexp::Cons with car=FORMALS, cdr=(BODY...) or Nil.
;;
;; The AOT body builds `(closure captured FORMALS BODY...)':
;;   1. Checks args is Cons (tag 7).
;;   2. Captures lexical env into `out' via `nl_env_capture_lexical'.
;;   3. `nl_cons_prepend_clone(out, args, out)' → out = (captured . args)
;;      = (captured FORMALS BODY...).  Clones both out (captured-env) and
;;      args (borrowd from caller) with proper refcount management.
;;   4. Writes symbol "closure" (7 bytes) to `s1' scratch slot.
;;   5. `nl_cons_prepend_clone(s1, out, out)' → out = (closure captured FORMALS BODY...).
;;      Clones s1 (closure symbol) and old out (the tail list).
;;   6. Returns 0 on success.
;;
;; Refcount discipline:
;;   `nl_cons_prepend_clone(car_ptr, cdr_ptr, out)' clones *car_ptr and *cdr_ptr
;;   before assigning to *out (dropping any prior *out value).  Aliasing
;;   `car_ptr == out' and `cdr_ptr == out' are both safe because reads happen
;;   before the write.
;;
;; ABI constants used (§100.B frozen):
;;   SEXP_TAG_CONS = 7
;;   nl_env_capture_lexical: (*mut c_void, *mut Sexp) → i64
;;     Captures current lexical env; writes result into *out; returns 0.
;;   nl_cons_prepend_clone: (*const Sexp, *const Sexp, *mut Sexp) → i64
;;     Clones both car and cdr; builds Sexp::Cons; assigns to *out; returns 0.
;;
;; Alignment (all defuns even-arity; extern-call is always arg 0):
;;   nl_sf_lambda(args env out s1)                     — arity 4 (even)
;;   nl_sf_lambda_captured(cap-rc args out s1)         — arity 4 (even)
;;   nl_sf_lambda_fill(buf s1)                         — arity 2 (even)
;;   nl_sf_lambda_alloc_sym(s1 _pad)                   — arity 2 (even)
;;   nl_sf_lambda_with_tail(_ args out s1, _pad)       — arity 6 (even)  [unused _pad]
;;   nl_sf_lambda_with_sym(sym-ok out s1 _pad)         — arity 4 (even)
;;
;; "closure" ASCII: 99 108 111 115 117 114 101 (7 bytes)

;;; Code:

(defconst nelisp-cc-sf-lambda--source
  '(seq

    ;; Fill 7 ASCII bytes for "closure" into buf, write Sexp::Symbol to s1,
    ;; free buf.  Returns (dealloc-bytes …) = 1 sentinel (truthy for and-chain).
    ;; Arity 2 (even).
    (defun nl_sf_lambda_fill (buf s1)
      (and (ptr-write-u8 buf 0 99)    ; c
           (ptr-write-u8 buf 1 108)   ; l
           (ptr-write-u8 buf 2 111)   ; o
           (ptr-write-u8 buf 3 115)   ; s
           (ptr-write-u8 buf 4 117)   ; u
           (ptr-write-u8 buf 5 114)   ; r
           (ptr-write-u8 buf 6 101)   ; e
           (sexp-write-symbol s1 buf 7)
           (dealloc-bytes buf 7 1)))

    ;; Allocate 7-byte buffer and fill the "closure" symbol into s1.
    ;; alloc-bytes is an inline op (not an extern-call), so the call
    ;; `(nl_sf_lambda_fill (alloc-bytes 7 1) s1)' is alignment-safe.
    ;; Returns the dealloc-bytes sentinel (truthy = success).
    ;; Arity 2 (even).
    (defun nl_sf_lambda_alloc_sym (s1 _pad)
      (nl_sf_lambda_fill (alloc-bytes 7 1) s1))

    ;; s1 = Symbol("closure") (written by nl_sf_lambda_alloc_sym).
    ;; out = (captured FORMALS BODY...) (written by nl_cons_prepend_clone in caller).
    ;; Prepend s1 before out: nl_cons_prepend_clone(s1, out, out) — FIRST ✓.
    ;; nl_cons_prepend_clone reads *s1 and *out (cdr) BEFORE writing *out (drop+assign).
    ;; Result: out = (closure captured FORMALS BODY...).
    ;; Returns 0 on success, 1 if symbol allocation failed.
    ;; Arity 4 (even).
    (defun nl_sf_lambda_with_sym (sym-ok out s1 _pad)
      (if sym-ok
          (and (extern-call nl_cons_prepend_clone s1 out out) 0)
        1))

    ;; After nl_cons_prepend_clone(out, args, out): out = (captured FORMALS BODY...).
    ;; Write "closure" symbol to s1 (internal call — not extern-call).
    ;; Then prepend symbol via nl_sf_lambda_with_sym.
    ;; _ is unused return from prior nl_cons_prepend_clone (truthy).
    ;; Arity 6 (even).
    (defun nl_sf_lambda_with_tail (_ args out s1 _pad5 _pad6)
      (nl_sf_lambda_with_sym
       (nl_sf_lambda_alloc_sym s1 0)
       out s1 0))

    ;; After nl_env_capture_lexical: out = captured env (cap-rc=0 always).
    ;; Prepend captured env before args: nl_cons_prepend_clone(out, args, out) — FIRST ✓.
    ;; nl_cons_prepend_clone clones *out (captured-env) and *args (borrowed from caller).
    ;; nl_cons_prepend_clone drops old *out via assignment before writing new Cons.
    ;; Result: out = (captured FORMALS BODY...).
    ;; Arity 4 (even).
    (defun nl_sf_lambda_captured (cap-rc args out s1)
      (nl_sf_lambda_with_tail
       (extern-call nl_cons_prepend_clone out args out)   ; FIRST ✓
       args out s1 0 0))

    ;; Public entry: nl_sf_lambda(args, env, out, s1) → i64
    ;; args: *const Sexp = (FORMALS BODY...) — must be Cons (tag 7).
    ;; env:  *mut c_void = current lexical environment.
    ;; out:  *mut Sexp   = result slot (starts as Sexp::Nil in Rust thin shell).
    ;; s1:   *mut Sexp   = scratch slot for "closure" symbol (starts as Sexp::Nil).
    ;; Returns 0 on success, 1 on error (args not Cons).
    ;; Arity 4 (even): body-entry rsp ≡ 0 mod 16 ✓.
    ;; extern-call nl_env_capture_lexical is at arg position 0 ✓.
    (defun nl_sf_lambda (args env out s1)
      (if (= (sexp-tag args) 7)
          (nl_sf_lambda_captured
           (extern-call nl_env_capture_lexical env out)   ; FIRST ✓
           args out s1)
        1)))

  "AOT source for `nl_sf_lambda' (eval/special_forms.rs sf_lambda → elisp).

Six defuns (seq form).  Uses nl_cons_prepend_clone for refcount-safe cons building.

Algorithm:
  1. Check args is Cons (tag 7).
  2. nl_env_capture_lexical(env, out) → out = captured-env  [extern-call FIRST ✓].
  3. nl_cons_prepend_clone(out, args, out) → out = (captured FORMALS BODY...)
     [extern-call FIRST ✓; clones captured-env + args with proper refcounts].
  4. nl_sf_lambda_alloc_sym: alloc-bytes + fill 7 bytes + sexp-write-symbol
     → s1 = Symbol(\"closure\").
  5. nl_cons_prepend_clone(s1, out, out) → out = (closure captured FORMALS BODY...)
     [extern-call FIRST ✓; clones closure symbol + tail list].
  6. Return 0.

ABI: nl_sf_lambda(args *const Sexp, env *mut c_void, out *mut Sexp, s1 *mut Sexp) → i64.
Called from the thin Rust shell `sf_lambda' via `crate::elisp_cc_spike::sf_lambda_call'.

New ABI dep: nl_cons_prepend_clone(*const Sexp, *const Sexp, *mut Sexp) → i64.
Defined in `eval/special_forms.rs'.  Clones both inputs; assigns Cons to *out.")

(provide 'nelisp-cc-sf-lambda)

;;; nelisp-cc-sf-lambda.el ends here
