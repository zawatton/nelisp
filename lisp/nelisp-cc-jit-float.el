;;; nelisp-cc-jit-float.el --- Doc 110 §110.E float.rs swap sources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 110 §110.E.2.a — first batch of `jit/float.rs' trampoline
;; swaps for the f64 ABI rollout.  Each `defconst' below carries the
;; canonical elisp source for one `nl_jit_float_*' symbol; the build
;; chain (`scripts/compile-elisp-objects.el' →
;; `nelisp-phase47-compile-to-object' → ET_REL `.o' → static archive
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
  "Phase 47 source for `nl_jit_float_add' f64 trampoline.
Replaces `build-tool/src/jit/float.rs::nl_jit_float_add'.  ABI:
`extern \"C\" fn(f64, f64) -> f64' (= xmm0, xmm1 → xmm0).")

(defconst nelisp-cc-jit-float-sub--source
  '(defun nl_jit_float_sub ((a :type f64) (b :type f64))
     (f64-sub a b))
  "Phase 47 source for `nl_jit_float_sub' f64 trampoline.")

(defconst nelisp-cc-jit-float-mul--source
  '(defun nl_jit_float_mul ((a :type f64) (b :type f64))
     (f64-mul a b))
  "Phase 47 source for `nl_jit_float_mul' f64 trampoline.")

(defconst nelisp-cc-jit-float-div--source
  '(defun nl_jit_float_div ((a :type f64) (b :type f64))
     (f64-div a b))
  "Phase 47 source for `nl_jit_float_div' f64 trampoline.
IEEE 754 division-by-zero produces ±inf / NaN, not a trap.")

(provide 'nelisp-cc-jit-float)

;;; nelisp-cc-jit-float.el ends here
