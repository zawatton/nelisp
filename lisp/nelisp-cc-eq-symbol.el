;;; nelisp-cc-eq-symbol.el --- Doc 101 §101.C bi_eq Symbol swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 101 §101.C — swap the `(eq SYMBOL SYMBOL)' fast path from Rust
;; string comparison into a Phase 47-compiled elisp `.o'.  The body
;; assumes caller-owned storage for the result Sexp and writes only
;; the tag byte for `nil' / `t' via the dedicated grammar ops.

;;; Code:

(defconst nelisp-cc-eq-symbol--source
  '(defun nelisp_eq_symbol (arg0 arg1 result-slot)
     (if (= 0 (symbol-eq arg0 arg1))
         (sexp-write-nil result-slot)
       (sexp-write-t result-slot)))
  "Phase 47 source for the §101.C `(eq SYMBOL SYMBOL)' swap.

`arg0' and `arg1' are `*const Sexp' inputs; `result-slot' is a
caller-owned `*mut Sexp'.  `symbol-eq' returns the i64 boolean 0/1,
and the body materialises the corresponding atom by writing only the
tag byte of `result-slot' (`nil' = 0, `t' = 1).")

(provide 'nelisp-cc-eq-symbol)

;;; nelisp-cc-eq-symbol.el ends here
