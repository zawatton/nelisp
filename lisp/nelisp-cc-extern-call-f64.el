;;; nelisp-cc-extern-call-f64.el --- Doc 122 §122.C f64 extern-call probes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.C introduces the extended `extern-call' grammar form
;; that classifies args per the SysV AMD64 ABI: i64 → rdi-r9 GP regs,
;; f64 → xmm0-7.  Return type defaults to i64 (= rax); the new
;; `extern-call-f64' head op marks an f64 return read from xmm0.
;;
;; This file packages three thin AOT wrappers around libm
;; unary f64 → f64 functions (= sin / cos / sqrt) so the
;; `tests/elisp_cc_extern_call_f64_probe.rs' integration test can
;; drive each round-trip independently.  Each wrapper exercises:
;;
;;   - f64 arg placement via xmm0 (single-arg pool slot).
;;   - f64 return read from xmm0.
;;   - The new `extern-call-f64' grammar form's PLT32 reloc against
;;     a libm symbol (= `sin', `cos', `sqrt' are linked through the
;;     standard `libm.so' that `bin/nelisp' already depends on
;;     transitively via `std', same as Doc 110 §110.F `nl_jit_float_
;;     exp' / `nl_jit_float_log').
;;
;; The new type-annotated arg shape `(:f64 EXPR)' is what
;; distinguishes f64 args from the legacy `:i64' default; bare arg
;; expressions (= used by all pre-§122.C call sites) still parse as
;; `:i64' and lower through the existing rdi-r9 GP path.  The
;; backward compatibility check is implicit (= every existing test
;; that exercises `(extern-call ...)' still passes).

;;; Code:

(defconst nelisp-cc-extern-call-f64--sqrt-source
  '(defun nelisp_libm_sqrt ((x :type f64))
     ;; ABI: extern "C" fn(f64) -> f64 against libm `sqrt'.
     ;; Lowers to `MOVSD xmm0, [rbp - 8]; CALL rel32 sqrt; <ret>'
     ;; with the result already in xmm0 per SysV AMD64.
     (extern-call-f64 sqrt (:f64 x)))
  "AOT source for the Doc 122 §122.C f64 extern-call probe.
Wraps libm `sqrt'.  Verifies f64-arg placement (= xmm0) + f64
return (= xmm0) + PLT32 reloc against an extern libc / libm
symbol with the new `extern-call-f64' head.")

(defconst nelisp-cc-extern-call-f64--sin-source
  '(defun nelisp_libm_sin ((x :type f64))
     (extern-call-f64 sin (:f64 x)))
  "AOT source — libm `sin' wrapper.  Same ABI as `sqrt' but
covers a different libm symbol (= proves the PLT32 reloc machinery
generalises to any extern f64 → f64 function rather than special-
casing one symbol).")

(defconst nelisp-cc-extern-call-f64--cos-source
  '(defun nelisp_libm_cos ((x :type f64))
     (extern-call-f64 cos (:f64 x)))
  "AOT source — libm `cos' wrapper.  Third probe in the
sin/cos/sqrt triplet.  Together the three exercise:
  - All-positive arg domain (= sqrt 4.0).
  - Trigonometric arg domain crossing zero (= sin / cos 0.0 / pi).
  - Distinct extern symbols (= proves no symbol-name collision in
    the per-`.o' relocation table emitted by AOT).")

(provide 'nelisp-cc-extern-call-f64)

;;; nelisp-cc-extern-call-f64.el ends here
