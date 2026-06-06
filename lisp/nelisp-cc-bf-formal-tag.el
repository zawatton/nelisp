;;; nelisp-cc-bf-formal-tag.el --- AOT nl_bf_formal_tag swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `nl_bf_formal_tag' Rust extern in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was (~9 LOC):
;;
;;   #[no_mangle]
;;   pub unsafe extern "C" fn nl_bf_formal_tag(name_ptr: *const Sexp) -> i64 {
;;       match &*name_ptr {
;;           Sexp::Symbol(s) => match s.as_str() {
;;               "&optional" => 1,
;;               "&rest"     => 2,
;;               _           => 0,
;;           },
;;           _ => -1,
;;       }
;;   }
;;
;; Contract:
;;   name_ptr  *const Sexp — pointer to any Sexp value (formal parameter name).
;;   returns   i64:
;;               1  = Sexp::Symbol("&optional")
;;               2  = Sexp::Symbol("&rest")
;;               0  = Sexp::Symbol (any other symbol)
;;              -1  = not a Sexp::Symbol (= WrongType sentinel for caller)
;;
;; AOT ops consumed:
;;   `(sexp-tag PTR)'            — reads tag byte at [PTR+0], returns i64.
;;   `(symbol-name-eq PTR "L")' — inline tag-check + length-check +
;;        byte-loop compare against compile-time UTF-8 literal bytes.
;;        Returns 0/1.  The tag check inside symbol-name-eq is redundant
;;        with our outer sexp-tag guard but harmless; the guard is still
;;        required to distinguish "non-Symbol → -1" from "Symbol-but-
;;        non-matching → 0".
;;   `(if TEST THEN ELSE)'       — short-circuit dispatch.
;;
;; ABI constants (§100.B frozen):
;;   SEXP_TAG_SYMBOL = 4
;;
;; Arity: 2 (even; `_pad' keeps rsp ≡ 0 mod 16 at body entry for
;; consistency, even though the function body contains no PLT calls and
;; alignment would be irrelevant in practice).  Callers (Rust or
;; elisp.o) invoke with 1 argument (rdi); rsi (_pad) is garbage but
;; never read.
;;
;; Net Rust impact: deletes the 9-LOC `nl_bf_formal_tag' body from
;; `build-tool/src/eval/special_forms.rs'.  The `#[no_mangle]' symbol
;; is now provided by this `.o' and resolved by the linker at archive
;; bundle time.  No lib.rs changes required (not in `extern "C"'
;; block — only called from `nl_bind_formals_impl.o' via
;; `extern-call nl_bf_formal_tag name-ptr').

;;; Code:

(defconst nelisp-cc-bf-formal-tag--source
  '(defun nl_bf_formal_tag (name_ptr _pad)
     ;; name_ptr: *const Sexp — the formal parameter name Sexp.
     ;; _pad:     i64 — unused alignment slot (arity 2 = even).
     ;; Returns:  i64: 1=&optional, 2=&rest, 0=other-symbol, -1=non-symbol.
     ;;
     ;; SEXP_TAG_SYMBOL = 4.  The outer sexp-tag guard distinguishes
     ;; the -1 non-Symbol case from the 0 "not a keyword" case.
     ;; `symbol-name-eq' does its own internal tag+length check; the
     ;; outer guard ensures we never misinterpret a non-Symbol as 0.
     (if (= (sexp-tag name_ptr) 4)
         (if (= (symbol-name-eq name_ptr "&optional") 1)
             1
           (if (= (symbol-name-eq name_ptr "&rest") 1)
               2
             0))
       -1))
  "AOT source for `nl_bf_formal_tag' (special_forms.rs → elisp).

Classifies a formal-parameter Sexp:
  1  = Sexp::Symbol(\"&optional\")
  2  = Sexp::Symbol(\"&rest\")
  0  = Sexp::Symbol (any other symbol)
 -1  = not a Sexp::Symbol (WrongType)

Body is a pure if-chain with two inline `symbol-name-eq' guards
(no PLT calls).  Arity 2 (even) with unused `_pad' slot.

Called via `(extern-call nl_bf_formal_tag name-ptr)' from
`nelisp-cc-bind-formals.el' dispatch helper `nl_bf_dispatch'.")

(provide 'nelisp-cc-bf-formal-tag)

;;; nelisp-cc-bf-formal-tag.el ends here
