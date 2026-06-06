;;; nelisp-cc-bi-signal.el --- Doc 127 bi_signal tag-dispatch swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 127 — moves the symbol-name tag-dispatch of the `(signal TAG DATA)'
;; builtin from Rust into a AOT elisp object.  The Rust shim keeps:
;;
;;   * arity validation                      (2 args)
;;   * `WrongType' guard                     (TAG must be `Sexp::Symbol')
;;   * `EvalError' construction              (Quit / ArithError / WrongType /
;;                                            UserError) from the i64
;;                                            discriminant returned here
;;   * data-field extraction                 (arith-error msg, wrong-type
;;                                            expected/got)
;;
;; The elisp body's job: compare the TAG symbol against the three
;; builtin-signal names ("quit", "arith-error", "wrong-type-argument")
;; using the AOT `symbol-eq' grammar op (= tag-byte guard +
;; byte-wise name comparison) and return an i64 discriminant:
;;
;;   0  — "quit"
;;   1  — "arith-error"
;;   2  — "wrong-type-argument"
;;   3  — any other tag  (= `EvalError::UserError')
;;
;; The Rust shim constructs the three known `Sexp::Symbol' values as
;; stack-local variables and passes their addresses alongside `tag-ptr'
;; (= `&args[0]') so `symbol-eq' can compare by name bytes without
;; any heap allocation inside this body.
;;
;; AOT ops consumed:
;;   `symbol-eq'   — require both tags = `Sexp::Symbol', then compare
;;                   name bytes; returns i64 0 or 1.
;;
;; Function contract:
;;   tag-ptr          *const Sexp — caller-validated `Sexp::Symbol' (= TAG).
;;   quit-ptr         *const Sexp — Sexp::Symbol("quit") on Rust stack.
;;   arith-ptr        *const Sexp — Sexp::Symbol("arith-error") on Rust stack.
;;   wrong-type-ptr   *const Sexp — Sexp::Symbol("wrong-type-argument") on Rust stack.
;;   returns          i64 discriminant 0-3 (see table above).
;;
;; Per Doc 127 §4.3: pure migration — no behaviour change.  The Rust
;; shim preserves the full `EvalError' contract; only the symbol-name
;; comparison step moves into elisp, yielding ≥ 20 Rust LOC savings.

;;; Code:

(defconst nelisp-cc-bi-signal--dispatch-source
  '(defun nelisp_bi_signal_dispatch (tag-ptr quit-ptr arith-ptr wrong-type-ptr)
     ;; tag-ptr:        *const Sexp — the TAG symbol (Sexp::Symbol).
     ;; quit-ptr:       *const Sexp — Sexp::Symbol("quit").
     ;; arith-ptr:      *const Sexp — Sexp::Symbol("arith-error").
     ;; wrong-type-ptr: *const Sexp — Sexp::Symbol("wrong-type-argument").
     ;;
     ;; Compare TAG against the three known builtin-signal names via
     ;; `symbol-eq' (= tag-byte check + byte-wise name comparison).
     ;; Return 0/1/2/3 for quit/arith-error/wrong-type-argument/other.
     (if (= (symbol-eq tag-ptr quit-ptr) 1)
         0
       (if (= (symbol-eq tag-ptr arith-ptr) 1)
           1
         (if (= (symbol-eq tag-ptr wrong-type-ptr) 1)
             2
           3))))
  "AOT source for the Doc 127 `(signal TAG DATA)' tag-dispatch swap.

Four-argument function — AOT's SysV AMD64 prologue spills all
four `*const Sexp' arguments into consecutive rbp-relative slots.
The body is a nested if-chain that calls `symbol-eq' twice or thrice
(short-circuit on first match):

  `(symbol-eq tag-ptr quit-ptr)'       — if match → rax = 0, return.
  `(symbol-eq tag-ptr arith-ptr)'      — if match → rax = 1, return.
  `(symbol-eq tag-ptr wrong-type-ptr)' — if match → rax = 2, else rax = 3.

`symbol-eq' emits a tag-byte guard (`movzx rax, byte [rdi]'; `cmp
rax, SEXP_TAG_SYMBOL = 4'; `jnz false') for each input pointer, then
the byte-loop string comparison (= a PLT call to the shared helper
emitted by `nelisp-aot-compiler--emit-string-eq-core').  Both
sides are caller-guaranteed `Sexp::Symbol' values so the tag guard
always passes.

Return value: i64 in rax.  No memory writes, no allocation, no PLT
symbols beyond the string-eq-core helper shared with `symbol-eq'
usages elsewhere.  The Rust shim dispatches on this i64 to construct
the appropriate `EvalError' variant.")

(provide 'nelisp-cc-bi-signal)

;;; nelisp-cc-bi-signal.el ends here
