;;; nelisp-cc-aref-vector.el --- Doc 111 §111.C aref Vector swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.C swaps the Vector arm of `(aref ARR IDX)' to a
;; AOT-compiled elisp object.  Rust pre-validates the Vector
;; type and bounds, then this body unwraps the integer index and
;; copies the selected 32-byte Sexp element into the caller-owned
;; result slot.

;;; Code:

(defconst nelisp-cc-aref-vector--source
  '(defun nelisp_aref_vector (arg0 arg1 result-slot)
     ;; arg0: *const Sexp pointing at Sexp::Vector(_).
     ;; arg1: *const Sexp pointing at Sexp::Int(idx) — bounds-checked in Rust shim.
     ;; result-slot: *mut Sexp 32-byte slot.
     (vector-ref arg0 (sexp-int-unwrap arg1) result-slot)))

(provide 'nelisp-cc-aref-vector)

;;; nelisp-cc-aref-vector.el ends here
