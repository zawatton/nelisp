;;; nelisp-cc-symbol-is-lambda.el --- AOT nl_symbol_is_lambda elisp .o  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Moves the `#[no_mangle] extern "C" fn nl_symbol_is_lambda' body from
;; `build-tool/src/eval/special_forms.rs' into a AOT elisp `.o'.
;;
;; Contract: `(nl_symbol_is_lambda SYM)' — `*const Sexp' — returns i64:
;;   1 if SYM is `Sexp::Symbol("lambda")', 0 otherwise.
;;
;; Implementation: one `symbol-name-eq' grammar op call (= inline
;; tag-byte guard + byte-wise name comparison against the literal
;; "lambda"), matching the Rust `match &*sym { Sexp::Symbol(s) if s ==
;; "lambda" => 1, _ => 0 }' body exactly.
;;
;; Arity 1 (odd) — body-entry rsp ≡ 8 mod 16.  `symbol-name-eq' is
;; an inline grammar op with no PLT call, so no alignment issue arises.
;;
;; The defun name `nl_symbol_is_lambda' is the exported symbol;
;; existing callers that use `extern-call nl_symbol_is_lambda' resolve
;; to this .o without change.

;;; Code:

(defconst nelisp-cc-symbol-is-lambda--source
  '(defun nl_symbol_is_lambda (sym)
     ;; sym: *const Sexp (rdi).
     ;; Returns i64: 1 if Symbol("lambda"), 0 otherwise.
     (symbol-name-eq sym "lambda"))
  "AOT source for `nl_symbol_is_lambda'.

Replaces the Rust `#[no_mangle] extern \"C\" fn nl_symbol_is_lambda'
in `build-tool/src/eval/special_forms.rs'.  Single inline
`symbol-name-eq' op — tag-byte guard + byte-wise name compare.

The defun name `nl_symbol_is_lambda' is the exported symbol; callers
using `extern-call nl_symbol_is_lambda' resolve unchanged.")

(provide 'nelisp-cc-symbol-is-lambda)

;;; nelisp-cc-symbol-is-lambda.el ends here
