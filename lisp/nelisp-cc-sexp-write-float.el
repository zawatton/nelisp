;;; nelisp-cc-sexp-write-float.el --- Doc 122 §122.G grammar op probe  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.G introduces the `sexp-write-float' Phase 47 grammar
;; op:
;;
;;   (sexp-write-float SLOT VALUE)
;;     — Call `nl_sexp_write_float(slot, val: f64)' which writes
;;       `Sexp::Float(val)' into `*slot' inline (= tag byte 3 at
;;       offset 0, f64 payload at offset 8, no heap box) and
;;       returns SLOT in rax.  VALUE is an f64-class flat-leaf
;;       ref (= parameter with `:type f64' annotation).
;;
;; This file packages the op as a single Phase 47-compiled `defun'
;; so the `tests/elisp_cc_sexp_write_float_probe.rs' integration
;; test can probe the round-trip end-to-end.  Pattern mirrors
;; `nelisp-cc-sexp-write-str.el' for §122.A's sibling allocator ops
;; with one twist: the Phase 47 MVP requires uniform param classes
;; per defun, so both SLOT and VAL are declared `(:type f64)' and
;; the test harness bit-casts the slot pointer via
;; `f64::from_bits(ptr as u64)' before invocation.  The emit code
;; in `nelisp-phase47-compiler--emit-sexp-write-float' tolerates
;; either class and moves the f64-class bit pattern back into rdi
;; before the call instruction.

;;; Code:

(defconst nelisp-cc-sexp-write-float--source
  '(defun nelisp_sexp_write_float ((slot :type f64) (val :type f64))
     ;; ABI: extern "C" fn(f64, f64) -> *mut Sexp.  SLOT arrives in
     ;; xmm0 (= bit-cast of the `*mut Sexp' pointer to f64), VAL in
     ;; xmm1 (= the f64 payload to materialise as `Sexp::Float').
     ;; The grammar op lowers SLOT back to GP rdi via MOVQ before
     ;; calling `nl_sexp_write_float'.
     ;;
     ;; The Phase 47 MVP forbids mixed-class params in a single
     ;; defun, so the test harness emulates the would-be `(slot val
     ;; :type f64)' shape by bit-casting the pointer through xmm0;
     ;; the bit pattern survives unchanged through the spill round
     ;; trip and ends up in rdi as a valid `*mut Sexp' on entry to
     ;; the helper.
     (sexp-write-float slot val))
  "Phase 47 source for the Doc 122 §122.G `sexp-write-float' grammar
op probe.  Receives the slot pointer as a bit-cast f64 in xmm0 and
the value as an f64 in xmm1; lowers to `nl_sexp_write_float(rdi:
slot, xmm0: val)' and returns the slot pointer in rax.")

(provide 'nelisp-cc-sexp-write-float)

;;; nelisp-cc-sexp-write-float.el ends here
