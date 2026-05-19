;;; nelisp-cc-bi-nl-fact-i64.el --- bi_nl_fact_i64 wrapper swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 swap for the Rust `bi_nl_fact_i64' thin shell in
;; `build-tool/src/eval/builtins.rs'.  Composes:
;;   - `sexp-int-unwrap' (§100) to read N from `arg-ptr'
;;   - range check (`< 0', `> 20') for the i64-safe factorial range
;;   - `(extern-call nelisp_fact_i64 N)' into the §99.C elisp fact .o
;;   - `sexp-int-make' (§100) to write the result into `result-slot'
;;
;; Signature:
;;   (nelisp_bi_nl_fact_i64 ARG-PTR RESULT-SLOT) -> i64
;;     ARG-PTR     : *const Sexp pointing at a `Sexp::Int' (caller has
;;                   already validated tag = Int via Rust dispatch arm).
;;     RESULT-SLOT : *mut Sexp 32-byte slot pre-initialised to Nil.
;;   Returns: 0=Ok (result-slot contains Sexp::Int(N!)),
;;            1=Err (N out of 0..=20 range; result-slot untouched).
;;
;; The Rust dispatch arm becomes a 1-arity-check + Int-tag-check
;; preamble plus a single ABI call; the algorithmic body (range
;; check + recursive multiply via §99.C) lives only here.

;;; Code:

(defconst nelisp-cc-bi-nl-fact-i64--source
  '(defun nelisp_bi_nl_fact_i64 (arg-ptr result-slot)
     (if (< (sexp-int-unwrap arg-ptr) 0)
         1
       (if (> (sexp-int-unwrap arg-ptr) 20)
           1
         (and (sexp-int-make
               result-slot
               (extern-call nelisp_fact_i64 (sexp-int-unwrap arg-ptr)))
              0))))
  "Phase 47 source for the `nl-fact-i64' Rust-wrapper swap.

Two-arg function — `arg-ptr' is a `*const Sexp::Int' pointer (caller
has tag-validated) and `result-slot' is a caller-owned 32-byte slot.
Nested `if' encodes the lower + upper range guard since the grammar
lacks `or' / `cond'.  The `and' chains the `sexp-int-make' (which
writes the slot and returns its truthy pointer) with the success
return value `0' so the function rc is 0=Ok.  Range failures both
arms return literal `1' so the Rust dispatch arm converts to
`EvalError::Internal' with the range message.")

(provide 'nelisp-cc-bi-nl-fact-i64)

;;; nelisp-cc-bi-nl-fact-i64.el ends here
