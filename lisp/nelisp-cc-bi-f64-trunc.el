;;; nelisp-cc-bi-f64-trunc.el --- Doc 118 bi_f64_trunc AOT swap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 118 — moves the computation core of the `nelisp--f64-trunc'
;; builtin (= mode dispatch + sexp→f64 conversion + division +
;; truncation) out of `build-tool/src/eval/builtins.rs::bi_f64_trunc'
;; and into a AOT elisp object.  The Rust shim shrinks to arity
;; check + WrongType guard on the mode symbol + call into this `.o'.
;;
;; Function contract:
;;   mode-ptr  *const Sexp — a Sexp::Symbol ("floor" / "ceiling" /
;;             "round" / "truncate"); tag-checked before this is
;;             called (Rust shim validates).  Passed directly so the
;;             mode dispatch here can use G1 `symbol-name-eq'.
;;   num-ptr   *const Sexp — numerator (Int / Float / Nil).
;;   denom-ptr *const Sexp — denominator (Int / Float / Nil).
;;   out-ptr   *mut Sexp   — caller-owned result slot (pre-init Nil).
;;
;;   returns  0 (= success, out-ptr holds Sexp::Int(result))
;;            1 (= unknown mode, Rust shim signals Internal error)
;;
;; Computation strategy:
;;
;;   Division:  `nl_bi_f64_trunc_div_bits(num, denom)' — a thin
;;   `#[no_mangle] pub extern "C"` Rust helper that converts each
;;   Sexp to f64 (Int→cvtsi2sd, Float→raw bits, Nil→0.0) and returns
;;   `(num/denom).to_bits() as i64'.  The result is the f64 quotient
;;   as an i64 bit-pattern, lifted into an f64-leaf via `bits-to-f64'.
;;
;;   Mode dispatch: 4 arms using G1 `symbol-name-eq' checks.
;;     "truncate": `(f64-to-i64-trunc (bits-to-f64 DIV_BITS))'
;;     "floor":    `(f64-to-i64-trunc (f64-call floor    BF))'
;;     "ceiling":  `(f64-to-i64-trunc (f64-call ceil     BF))'
;;     "round":    `(f64-to-i64-trunc (f64-call round    BF))'
;;   where BF = `(bits-to-f64 (extern-call nl_bi_f64_trunc_div_bits
;;                               num-ptr denom-ptr))'
;;   Each branch independently recomputes the division — this is
;;   correct because exactly one branch executes at runtime.
;;
;;   Result: `sexp-int-make out-ptr I64' writes Sexp::Int into
;;   *out-ptr and returns out-ptr in rax (= 0-sentinel on success).
;;   Unknown mode falls through to `return 1' in a separate defun
;;   body (= see :ret-unknown below) — we use a cond chain where
;;   the final else returns 1 without writing out-ptr.
;;
;; AOT ops consumed:
;;   G1  `symbol-name-eq'    — tag+len+byte inline literal compare
;;   G4  `sexp-float-unwrap' — f64 bits as i64 from Sexp::Float
;;   G5  `bits-to-f64'       — i64 bits → f64-class (MOVQ xmm0, rax)
;;   G5  `f64-to-i64-trunc'  — CVTTSD2SI rax, xmm0
;;   G6  `i64-to-f64'        — signed int → f64 (CVTSI2SD)
;;       `f64-call'           — PLT32 to libm floor/ceil/round/trunc
;;       `extern-call'        — PLT32 to nl_bi_f64_trunc_div_bits
;;       `sexp-int-make'      — write Sexp::Int(n) into *out
;;
;; Linux-x86_64 only — `f64-to-i64-trunc' / `bits-to-f64' / `i64-to-f64'
;; emit paths are x86_64-specific at MVP scope.  `extern-call' for
;; aarch64 ships in the aarch64 f64 follow-up doc.

;;; Code:

(defconst nelisp-cc-bi-f64-trunc--source
  ;; The AOT `(seq ...)' form compiles multiple defuns into a
  ;; single `.o'.  The first defun is the public entry point
  ;; `nl_bi_f64_trunc_impl'; it forwards to the mode-specific tail
  ;; via `symbol-name-eq' dispatch.
  ;;
  ;; One defun handles all 4 modes + the unknown-mode sentinel (= 1)
  ;; in a single `cond' chain.  `sexp-int-make' returns out-ptr in
  ;; rax for the success arms; the final else arm returns 1 so the
  ;; Rust shim can signal Internal error.
  '(defun nl_bi_f64_trunc_impl (mode-ptr num-ptr denom-ptr out-ptr)
     (cond
      ;; "truncate" — CVTTSD2SI of raw quotient (no libm call).
      ((= (symbol-name-eq mode-ptr "truncate") 1)
       (sexp-int-make out-ptr
         (f64-to-i64-trunc
           (bits-to-f64
             (extern-call nl_bi_f64_trunc_div_bits num-ptr denom-ptr)))))
      ;; "floor" — libm floor(q) then truncate.
      ((= (symbol-name-eq mode-ptr "floor") 1)
       (sexp-int-make out-ptr
         (f64-to-i64-trunc
           (f64-call floor
             (bits-to-f64
               (extern-call nl_bi_f64_trunc_div_bits num-ptr denom-ptr))))))
      ;; "ceiling" — libm ceil(q) then truncate.
      ((= (symbol-name-eq mode-ptr "ceiling") 1)
       (sexp-int-make out-ptr
         (f64-to-i64-trunc
           (f64-call ceil
             (bits-to-f64
               (extern-call nl_bi_f64_trunc_div_bits num-ptr denom-ptr))))))
      ;; "round" — libm round(q) then truncate.
      ((= (symbol-name-eq mode-ptr "round") 1)
       (sexp-int-make out-ptr
         (f64-to-i64-trunc
           (f64-call round
             (bits-to-f64
               (extern-call nl_bi_f64_trunc_div_bits num-ptr denom-ptr))))))
      ;; Unknown mode — return 1 so Rust shim signals Internal error.
      (t 1)))
  "AOT source for the Doc 118 `nelisp--f64-trunc' computation swap.

4-argument gp-class function — AOT SysV AMD64 prologue spills
the four `*const Sexp' / `*mut Sexp' pointers into rbp-relative
slots 0-3 (= args[0]=mode-ptr, args[1]=num-ptr, args[2]=denom-ptr,
args[3]=out-ptr).

The body is a 5-arm `cond' chain.  Arms 1-4 each:
  1. `symbol-name-eq MODE-PTR LITERAL' — inline tag+len+byte check.
  2. `extern-call nl_bi_f64_trunc_div_bits NUM-PTR DENOM-PTR' —
     calls the Rust `nl_bi_f64_trunc_div_bits' helper (= sexp→f64
     for each operand + IEEE 754 division + `.to_bits() as i64').
     Returns i64 quotient bits in rax.
  3. `bits-to-f64 (rax)' — MOVQ xmm0, rax (f64-leaf bridge, G5).
  4. For floor/ceil/round: `f64-call LIBM-FN xmm0' — PLT32 CALL to
     libm floor / ceil / round; result back in xmm0.
  5. `f64-to-i64-trunc xmm0' — CVTTSD2SI rax, xmm0 (G5).
  6. `sexp-int-make out-ptr rax' — write Sexp::Int(rax) into *out-ptr;
     return out-ptr in rax (= success sentinel).
Arm 5 (unknown mode): emit `mov rax, 1' — Rust shim checks rc != 0.

Rust `sexp_to_f64' private helper + `bi_f64_trunc' body are
deleted from `build-tool/src/eval/builtins.rs' (net -2 LOC on that
file).  `nl_bi_f64_trunc_div_bits' is a new 1-LOC `#[no_mangle]
pub extern \"C\"' in builtins.rs that the Rust linker resolves as a
PLT32 target from this `.o'.")

(provide 'nelisp-cc-bi-f64-trunc)

;;; nelisp-cc-bi-f64-trunc.el ends here
