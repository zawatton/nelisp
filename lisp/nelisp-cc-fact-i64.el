;;; nelisp-cc-fact-i64.el --- Doc 99 §99.C first elisp-only builtin  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 99 §99.C spike source — recursive i64 factorial implemented
;; entirely in elisp via the Phase 47 grammar.  The Rust builtin
;; `bi_nl_fact_i64' (in `build-tool/src/eval/builtins.rs') is a thin
;; Sexp-unwrap / Sexp-wrap shim around an `extern "C"' call to this
;; function — the actual computation lives nowhere except this file
;; (= no Rust counterpart in production code).
;;
;; The function is callable from elisp as `(nl-fact-i64 N)' where N
;; is an integer in 0..20 (= the fixnum-safe range; `21!' overflows
;; signed i64).  The Rust shim signals `arith-error' for out-of-range
;; inputs.
;;
;; This is the smallest "real swap" pattern that future Stage 99.D
;; migrations will follow: a `bi_*' becomes a Sexp-unwrap / extern-C /
;; Sexp-wrap shim, and the algorithmic body moves to elisp where the
;; Phase 47 chain compiles it to a `.o' linked into the binary.
;;
;; The Phase 47 grammar requires wrapping the defun in `(seq ...)' so
;; the parser's pre-scan adds the function to its defuns alist before
;; parsing the body — without that, the recursive self-call
;; `(nelisp_fact_i64 (- n 1))' would signal `:not-value-expr'.

;;; Code:

(defconst nelisp-cc-fact-i64--source
  '(seq
    (defun nelisp_fact_i64 (n)
      (if (<= n 1)
          1
        (* n (nelisp_fact_i64 (- n 1))))))
  "The §99.C spike source — recursive i64 factorial in Phase 47 grammar.
Kept as a defconst so `scripts/compile-elisp-objects.el' can read the
canonical source without re-parsing a comment.  The leading `seq' is
required by the Phase 47 parser to enable self-recursion (= the seq
pre-scan registers the function name before parsing the body).")

(provide 'nelisp-cc-fact-i64)

;;; nelisp-cc-fact-i64.el ends here
