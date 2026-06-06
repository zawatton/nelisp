;;; nelisp-cc-jit-float.el --- Doc 110 §110.E float.rs swap sources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 110 §110.E.2.a — first batch of `jit/float.rs' trampoline
;; swaps for the f64 ABI rollout.  Each `defconst' below carries the
;; canonical elisp source for one `nl_jit_float_*' symbol; the build
;; chain (`scripts/compile-elisp-objects.el' →
;; `nelisp-aot-compile-to-object' → ET_REL `.o' → static archive
;; → cargo link) compiles each source into a single C-callable
;; symbol that replaces the corresponding Rust trampoline in
;; `build-tool/src/jit/float.rs'.
;;
;; Stage 2.a covers the 4 arithmetic trampolines (add / sub / mul /
;; div).  The 5 comparison trampolines (eq_eps / lt / gt / le / ge)
;; need §110.C compiler integration first (= UCOMISD parse / emit)
;; and ship in §110.E.2.b.
;;
;; Naming: each elisp defun matches the existing Rust trampoline
;; name (= `nl_jit_float_add' etc.) so the bridge dispatch in
;; `build-tool/src/jit/bridge.rs::unified_fn_ptr' resolves the same
;; string to the elisp-emitted symbol on linux-x86_64 (= Stage 2.a
;; target) and to the Rust trampoline on aarch64 / macOS until
;; §110.D ships aarch64 f64 emit.
;;
;; The function bodies (`(f64-add a b)' etc.) compile to the canonical
;; 40-byte layout pinned by
;; `test/nelisp-asm-x86_64-f64-test.el::defun-f64-add-canonical-bytes':
;;
;;   Prologue (21 bytes):  push rbp; mov rbp, rsp; sub rsp, 16;
;;                         spill xmm0 / xmm1 to [rbp - 8] / [rbp - 16]
;;   Body     (14 bytes):  movsd xmm1, [rbp - 16];
;;                         movsd xmm0, [rbp - 8];
;;                         addsd / subsd / mulsd / divsd xmm0, xmm1
;;   Epilogue (5 bytes):   mov rsp, rbp; pop rbp; ret

;;; Code:

(defconst nelisp-cc-jit-float-add--source
  '(defun nl_jit_float_add ((a :type f64) (b :type f64))
     (f64-add a b))
  "AOT source for `nl_jit_float_add' f64 trampoline.
Replaces `build-tool/src/jit/float.rs::nl_jit_float_add'.  ABI:
`extern \"C\" fn(f64, f64) -> f64' (= xmm0, xmm1 → xmm0).")

(defconst nelisp-cc-jit-float-sub--source
  '(defun nl_jit_float_sub ((a :type f64) (b :type f64))
     (f64-sub a b))
  "AOT source for `nl_jit_float_sub' f64 trampoline.")

(defconst nelisp-cc-jit-float-mul--source
  '(defun nl_jit_float_mul ((a :type f64) (b :type f64))
     (f64-mul a b))
  "AOT source for `nl_jit_float_mul' f64 trampoline.")

(defconst nelisp-cc-jit-float-div--source
  '(defun nl_jit_float_div ((a :type f64) (b :type f64))
     (f64-div a b))
  "AOT source for `nl_jit_float_div' f64 trampoline.
IEEE 754 division-by-zero produces ±inf / NaN, not a trap.")

;; Doc 110 §110.C.2.a — 4 ordered-comparison trampolines.  Each
;; returns i64 0 / 1 in rax (= `extern \"C\" fn(f64, f64) -> i64'
;; shape).  NaN semantics: NaN comparisons return 0 to match Rust
;; `<' / `>' / `<=' / `>=' (= operand-swap trick + SETA/SETAE
;; naturally masks unordered case, see emit-f64-cmp docstring).
;; EQ-EPS comes in §110.C.2.b once rodata for the 1e-15 literal +
;; ANDPD abs-mask lands.

(defconst nelisp-cc-jit-float-lt--source
  '(defun nl_jit_float_lt ((a :type f64) (b :type f64))
     (f64-lt a b))
  "AOT source for `nl_jit_float_lt' f64 trampoline.
Matches Rust `(a < b) as i64' including NaN → 0.")

(defconst nelisp-cc-jit-float-gt--source
  '(defun nl_jit_float_gt ((a :type f64) (b :type f64))
     (f64-gt a b))
  "AOT source for `nl_jit_float_gt' f64 trampoline.
Matches Rust `(a > b) as i64' including NaN → 0.")

(defconst nelisp-cc-jit-float-le--source
  '(defun nl_jit_float_le ((a :type f64) (b :type f64))
     (f64-le a b))
  "AOT source for `nl_jit_float_le' f64 trampoline.
Matches Rust `(a <= b) as i64' including NaN → 0.")

(defconst nelisp-cc-jit-float-ge--source
  '(defun nl_jit_float_ge ((a :type f64) (b :type f64))
     (f64-ge a b))
  "AOT source for `nl_jit_float_ge' f64 trampoline.
Matches Rust `(a >= b) as i64' including NaN → 0.")

;; Doc 110 §110.C.2.b — EQ-EPS (= `(a-b).abs() < 1e-15') with
;; NaN-aware Rust semantics.  Uses the multi-instruction sequence
;; emitted by `--emit-f64-eq-eps' (= SUBSD + ANDPD abs-mask +
;; UCOMISD against 1e-15 + SETB/SETNP/AND mask + MOVZX) so the
;; defun body fits the single `:kind f64-cmp :op f64-eq-eps' IR
;; shape that the §110.C.2 dispatch already accepts.

(defconst nelisp-cc-jit-float-eq-eps--source
  '(defun nl_jit_float_eq_eps ((a :type f64) (b :type f64))
     (f64-eq-eps a b))
  "AOT source for `nl_jit_float_eq_eps' f64 trampoline.
Matches Rust `if (a - b).abs() < 1e-15 { 1 } else { 0 }'
including NaN → 0.  The 1e-15 bit pattern lives in
`nelisp-aot-compiler--f64-1e-15-bits' so it stays
cross-checkable against Rust's own `1e-15_f64.to_bits()'.")

(provide 'nelisp-cc-jit-float)

;;; nelisp-cc-jit-float.el ends here
