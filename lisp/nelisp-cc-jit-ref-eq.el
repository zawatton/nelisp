;;; nelisp-cc-jit-ref-eq.el --- Doc 120 §120.A ref-eq swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.A — AOT-compiled replacement for the Rust
;; `nl_jit_ref_eq' trampoline in `build-tool/src/jit/predicate.rs'.
;; ABI shape `(*const Sexp, *const Sexp, *mut Sexp) -> i64' (=
;; `:trampoline-binary-ctor'), reachable via `(nl-jit-call-out-2
;; "nelisp_jit_ref_eq" a b)' from `lisp/nelisp-jit-strategy.el'.
;; Writes `Sexp::T' / `Sexp::Nil' into `*out' directly; always
;; succeeds (= returns 0).
;;
;; The Rust impl was a thin pass-through to `sexp_eq' which is
;; identical to the path `nl_sexp_eq' takes — see the Doc 87 §10.2
;; judgment call in the original Rust trampoline header.  The
;; AOT body composes the same shape via `extern-call' to
;; `nl_sexp_eq' + conditional `sexp-write-t' / `sexp-write-nil'.
;;
;; Return-value convention: the trampoline's i64 return is the err-
;; status (0 = ok, non-0 = error).  This body always succeeds, so
;; the `(and SIDE-EFFECT 0)' idiom threads the rax = slot pointer
;; through the side-effect write, then short-circuits at the `0'
;; sentinel because `and' jumps to end on zero with rax already
;; zero.

;;; Code:

(defconst nelisp-cc-jit-ref-eq--source
  '(defun nelisp_jit_ref_eq (a b out)
     ;; a, b: *const Sexp inputs.
     ;; out:  *mut Sexp 32-byte slot for the boolean result.
     ;; Returns: i64 = 0 (= always succeeds).
     (if (= (extern-call nl_sexp_eq a b) 0)
         (and (sexp-write-nil out) 0)
       (and (sexp-write-t out) 0)))
  "AOT source for the §120.A `nl_jit_ref_eq' swap.

Delegates to the `nl_sexp_eq' Rust extern (= `#[no_mangle]'
wrapper around `sexp_eq' in `eval/special_forms.rs') and then
writes `Sexp::T' / `Sexp::Nil' into `*out' via the dedicated
tag-byte writers.  The trailing `0' inside each `and' chain is
the err-status sentinel returned in rax (= matches the Rust
trampoline's `0' return).")

(provide 'nelisp-cc-jit-ref-eq)

;;; nelisp-cc-jit-ref-eq.el ends here
