;;; nelisp-cc-truncate-float.el --- Doc 119 bi_truncate Float arm swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 119 §119.A — Wave 18v swap of the `(truncate FLOAT)' Float arm
;; using G4+G5 grammar.  The Rust inline `Sexp::Float(x) => Ok(Sexp::Int(*x as i64))'
;; compiles to a C-style truncation-toward-zero (= CVTTSD2SI), which is
;; exactly what G5 `f64-to-i64-trunc' emits.  G4 `sexp-float-unwrap'
;; reads the f64 bit-pattern as i64; `bits-to-f64' bridges it back to
;; an f64-class leaf; `f64-to-i64-trunc' then emits CVTTSD2SI.
;;
;; Function contract (mirrors `nelisp_truncate_int'):
;;   arg0:        *const Sexp pointing at a Sexp::Float(x) variant.
;;   result-slot: *mut Sexp pointing at a 32-byte caller-owned slot.
;;   returns:     result-slot (= same pointer, for ergonomics).
;;
;; The Rust shim (builtins.rs `bi_truncate' Float arm) enforces the
;; Sexp::Float tag *before* calling; this body assumes the precondition
;; (= no sexp-tag check needed before sexp-float-unwrap).
;;
;; ABI note: only two GP-class args (= even arity → rsp ≡ 0 mod 16
;; on entry).  No f64-class params — arg0 + result-slot are both
;; *const/*mut Sexp pointers (GP).  The f64 value is accessed via
;; the `sexp-float-unwrap' + `bits-to-f64' chain entirely within the
;; body.
;;
;; AOT grammar ops consumed:
;;   G4 `sexp-float-unwrap' — read f64 bits as i64 from Sexp::Float
;;   G5 `bits-to-f64'       — i64 bit-pattern → f64-leaf (MOVQ xmm0, rax)
;;   G5 `f64-to-i64-trunc'  — CVTTSD2SI rax, xmm0 (truncate toward zero)
;;      `sexp-int-make'      — write Sexp::Int(rax) into *result-slot
;;
;; Linux-x86_64 only — same arch gate as `nelisp-cc-bi-f64-trunc.el'.

;;; Code:

(defconst nelisp-cc-truncate-float--source
  '(defun nelisp_truncate_float (arg0 result-slot)
     (sexp-int-make result-slot
       (f64-to-i64-trunc
         (bits-to-f64
           (sexp-float-unwrap arg0)))))
  "AOT source for the Doc 119 §119.A `bi_truncate' Float arm swap.

Two-argument function — SysV AMD64 prologue spills arg0 (* const
Sexp pointing at a Sexp::Float) into rbp-relative slot 0 and
result-slot (*mut Sexp) into slot 1.

The body is a 3-level composed form:
  1. `sexp-float-unwrap arg0'    — load the 8-byte f64 payload of
     *arg0 as an i64 bit-pattern (= offset 8 from tag byte at
     offset 0).
  2. `bits-to-f64 (rax)'         — MOVQ xmm0, rax bridging the
     i64 bit-pattern into the f64-leaf class.
  3. `f64-to-i64-trunc (xmm0)'   — CVTTSD2SI rax, xmm0 (truncate
     toward zero, matching C-cast `(i64) x').
  4. `sexp-int-make result-slot rax' — write Sexp::Int(rax) into
     *result-slot; return result-slot in rax.

Rust `Sexp::Float(x) => Ok(Sexp::Int(*x as i64))' inline removed
from builtins.rs (net -1 LOC).  `cc_slot_1' shim wires the call
identical to the Int arm precedent in Doc 100 §100.C.")

(provide 'nelisp-cc-truncate-float)

;;; nelisp-cc-truncate-float.el ends here
