;;; nelisp-cc-int-neg.el --- Doc 100 §100 Tier A: integer negation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 100 Tier A wear-test #1 — Phase 47 grammar exercise.
;;
;; `(nl-int-neg N)' returns `(- N)' for any i64 N.  This function does
;; not exist on the Rust side and never did; it is a new elisp-only
;; builtin (= the §99.C `nl-fact-i64' pattern) that exercises the new
;; §100.B grammar forms (`sexp-int-unwrap' / `sexp-int-make') plus the
;; existing arith grammar (`-' with one literal operand).
;;
;; The body computes `0 - n' rather than a one-instruction `neg' op
;; because Phase 47's arith grammar canonicalises to a binary form.
;; A future grammar extension could collapse this to a single `neg
;; rax' but the current 3-instruction sequence is already a single
;; cache-line and the swap exists to validate the pattern, not to
;; micro-optimise.

;;; Code:

(defconst nelisp-cc-int-neg--source
  '(defun nelisp_int_neg (arg0 result-slot)
     (sexp-int-make result-slot
                    (- 0 (sexp-int-unwrap arg0))))
  "Phase 47 source for `nl-int-neg' Tier A wear-test.
Two-arg function: reads the i64 payload at `*arg0', subtracts it
from 0, writes the result into `*result-slot' as a fresh
`Sexp::Int'.  Returns `result-slot' for caller ergonomics.")

(provide 'nelisp-cc-int-neg)

;;; nelisp-cc-int-neg.el ends here
