;;; nelisp-cc-length-cons.el --- Doc 101 §101.B bi_length Cons swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 101 §101.B moves the proper-list walk for `(length X)' on
;; `Sexp::Cons' / `nil' inputs into a Phase 47-compiled elisp object.
;; The Rust side keeps only the dispatch + caller-owned result-slot
;; setup; the loop body itself lives only in the source below.

;;; Code:

(defconst nelisp-cc-length-cons--source
  '(seq
    (defun nelisp_length_cons_walk (cur-ptr n)
      (if (= cur-ptr 0)
          n
        (nelisp_length_cons_walk
         (cons-cdr-raw-from-box cur-ptr)
         (+ n 1))))
    (defun nelisp_length_cons (arg0 result-slot)
      (sexp-int-make result-slot
                     (nelisp_length_cons_walk
                      (sexp-payload-ptr arg0)
                      0))))
  "Phase 47 source for the Doc 101 §101.B `(length CONS)' swap.

`arg0' is a `*const Sexp' expected to point at either `Sexp::Cons(_)'
or `Sexp::Nil'.  `sexp-payload-ptr' seeds the walk with the
`NlConsBox*' payload for Cons and 0 for Nil.  The helper defun keeps
the walk in Phase 47's current expressible subset (= recursion +
integer compare/add + self-call) while `cons-cdr-raw-from-box'
follows the cdr edge only when it is itself a Cons; any non-Cons tail
produces 0 and terminates the walk.

The return value is materialized into the caller-owned 32-byte Sexp
slot by `sexp-int-make', which also returns the same slot pointer in
rax for ergonomic parity with the other Doc 100/101 helpers.")

(provide 'nelisp-cc-length-cons)

;;; nelisp-cc-length-cons.el ends here
