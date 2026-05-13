;;; nelisp-cc-jit-math.el --- Doc 110 §110.F math.rs swap sources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 110 §110.F — Phase 47 source for the 3 `jit/math.rs'
;; trampoline swaps (`float' identity / `exp' / `log').  Each
;; defconst below carries the canonical elisp source for one
;; `nl_jit_float_*' symbol; the build chain compiles each into
;; a single C-callable symbol that replaces the corresponding
;; Rust trampoline in `build-tool/src/jit/math.rs'.
;;
;; The 1-arg f64→f64 shape uses the `(f64-call SYM ARG)' grammar
;; form introduced in this stage:
;;   - On x86_64 it lowers to `CALL rel32' + R_X86_64_PLT32
;;     reloc against SYM (= libm `exp' / `log' linked via the
;;     standard `libm.so' that `bin/nelisp' already depends on
;;     transitively through `std').
;;   - On aarch64 it lowers to `BL imm26' + R_AARCH64_CALL26
;;     reloc.
;;
;; Identity (`nl_jit_float_float') is just a ref to the f64
;; param — the compiler emits a leaf-load into the return reg.

;;; Code:

(defconst nelisp-cc-jit-math-float--source
  '(defun nl_jit_float_float ((x :type f64))
     x)
  "Phase 47 source for `nl_jit_float_float' (= identity).
ABI: `extern \"C\" fn(f64) -> f64'.  Body is a single f64 ref
that compiles to `MOVSD xmm0, [rbp - 8]' on x86_64 / `LDUR d0,
[x29, #-16]' on aarch64 (= pure passthrough; the spill+reload
through the local frame is wasteful but matches the standard
prologue / epilogue pattern).")

(defconst nelisp-cc-jit-math-exp--source
  '(defun nl_jit_float_exp ((x :type f64))
     (f64-call exp x))
  "Phase 47 source for `nl_jit_float_exp' (= libm `exp').
ABI: `extern \"C\" fn(f64) -> f64'.  Calls libm `exp' via the
SysV / AAPCS f64 calling convention (= xmm0 / d0 in, xmm0 / d0
out).  IEEE 754 semantics propagate from libm — NaN in → NaN
out, ±inf as documented.")

(defconst nelisp-cc-jit-math-log--source
  '(defun nl_jit_float_log ((x :type f64))
     (f64-call log x))
  "Phase 47 source for `nl_jit_float_log' (= libm natural log).
ABI: `extern \"C\" fn(f64) -> f64'.  Calls libm `log' (= base-e
natural logarithm).  `(log 0.0)' = -inf, `(log negative)' = NaN,
`(log NaN)' = NaN — all forwarded from libm.")

(provide 'nelisp-cc-jit-math)

;;; nelisp-cc-jit-math.el ends here
