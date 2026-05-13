;;; nelisp-cc-jit-arith.el --- Doc 100 §100.D jit/arith.rs swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 100 §100.D — Phase-47-compiled replacements for the 12 plain
;; Rust trampolines that lived in `build-tool/src/jit/arith.rs'
;; (= `nl_jit_arith_add2' / `sub2' / `mul2' / 5 signed comparisons /
;; 3 bitwise binops / `ash').  Each `defconst' below holds a single
;; `(defun nelisp_jit_NAME (a b) BODY)' form; the build orchestrator
;; (`scripts/compile-elisp-objects.el') runs each form through
;; `nelisp-phase47-compile-to-object', producing an ET_REL `.o' file
;; that exports the `nelisp_jit_NAME' symbol.  Cargo's
;; `build.rs::link_elisp_cc_spike' picks up the `.o' files and adds
;; them to the static archive linked into the final binary, where
;; `build-tool/src/jit/bridge.rs' (= `unified_fn_ptr') resolves the
;; symbols at link time.
;;
;; Body shape: every function uses only grammar forms already
;; supported by Phase 47 §100.D:
;;
;;   ADD / SUB / MUL  — existing `(+ - *)' inline arith
;;   EQ / LT / GT / LE / GE — existing `(= < > <= >=)' inline cmp
;;                            (materialises 0/1 in rax via setCC + movzx)
;;   IOR / IAND / IXOR — new `(logior logand logxor)' inline arith
;;                       (= MR-form opcodes 0x09 / 0x21 / 0x31)
;;   ASH               — new `(shl sar)' shift IR kind, composed via
;;                       `(if (< c 0) (sar n (- 0 c)) (shl n c))'
;;
;; The result for every function lives in rax at return.  No allocator,
;; no Sexp marshalling — these are raw i64 arithmetic primitives the
;; elisp dispatchers in `nelisp-jit-strategy.el' / `nelisp-stdlib.el'
;; reach through the `nl-jit-call-i64-i64' bridge after unwrapping the
;; `Sexp::Int' payload.
;;
;; Linux-x86_64 only.  `build-tool/src/jit/mod.rs' gates the legacy
;; Rust `arith' module on `#[cfg(not(all(target_os = "linux",
;; target_arch = "x86_64")))]'; other targets keep the Rust trampolines
;; until ARM64 / non-Linux Phase 47 codegen lands (= Stage 2 of the
;; arith.rs swap).

;;; Code:

;; ---- 3 wrapping arithmetic ops ----

(defconst nelisp-cc-jit-arith-add2--source
  '(defun nelisp_jit_add2 (a b) (+ a b))
  "Phase 47 source for `nl_jit_arith_add2' — wrapping i64 add.
Emits `add rax, r10' inline (= 3 bytes).  The wrapping semantics
match the Rust trampoline because x86_64 ADD already wraps on
2's-complement overflow.")

(defconst nelisp-cc-jit-arith-sub2--source
  '(defun nelisp_jit_sub2 (a b) (- a b))
  "Phase 47 source for `nl_jit_arith_sub2' — wrapping i64 subtract.")

(defconst nelisp-cc-jit-arith-mul2--source
  '(defun nelisp_jit_mul2 (a b) (* a b))
  "Phase 47 source for `nl_jit_arith_mul2' — wrapping i64 multiply.
Emits `imul rax, r10' (= 4 bytes, signed two-operand form).")

;; ---- 5 signed integer comparisons (returning 0/1 i64) ----

(defconst nelisp-cc-jit-arith-eq2--source
  '(defun nelisp_jit_eq2 (a b) (= a b))
  "Phase 47 source for `nl_jit_arith_eq2' — returns 1 if A == B else 0.
Phase 47 `emit-cmp' materialises the flag via `setCC al + movzx
eax, al', leaving 0 or 1 in rax.")

(defconst nelisp-cc-jit-arith-lt2--source
  '(defun nelisp_jit_lt2 (a b) (< a b))
  "Phase 47 source for `nl_jit_arith_lt2' — 1 if A < B else 0 (signed).")

(defconst nelisp-cc-jit-arith-gt2--source
  '(defun nelisp_jit_gt2 (a b) (> a b))
  "Phase 47 source for `nl_jit_arith_gt2' — 1 if A > B else 0 (signed).")

(defconst nelisp-cc-jit-arith-le2--source
  '(defun nelisp_jit_le2 (a b) (<= a b))
  "Phase 47 source for `nl_jit_arith_le2' — 1 if A <= B else 0 (signed).")

(defconst nelisp-cc-jit-arith-ge2--source
  '(defun nelisp_jit_ge2 (a b) (>= a b))
  "Phase 47 source for `nl_jit_arith_ge2' — 1 if A >= B else 0 (signed).")

;; ---- 3 bitwise ops ----

(defconst nelisp-cc-jit-arith-logior2--source
  '(defun nelisp_jit_logior2 (a b) (logior a b))
  "Phase 47 source for `nl_jit_arith_logior2' — i64 bitwise OR.")

(defconst nelisp-cc-jit-arith-logand2--source
  '(defun nelisp_jit_logand2 (a b) (logand a b))
  "Phase 47 source for `nl_jit_arith_logand2' — i64 bitwise AND.")

(defconst nelisp-cc-jit-arith-logxor2--source
  '(defun nelisp_jit_logxor2 (a b) (logxor a b))
  "Phase 47 source for `nl_jit_arith_logxor2' — i64 bitwise XOR.")

;; ---- arithmetic shift ----

(defconst nelisp-cc-jit-arith-ash--source
  '(defun nelisp_jit_ash (n count)
     (if (< count 0)
         (sar n (- 0 count))
       (shl n count)))
  "Phase 47 source for `nl_jit_arith_ash' — signed arithmetic shift.
Positive COUNT: `shl n count' (= logical-left, sign survives in
high bits as 2's-complement requires).  Negative COUNT: `sar n
(-count)' (= arithmetic-right, sign bit replicates).  Caller bounds-
checks `count ∈ [-62, +62]'; out-of-range counts produce undefined
behaviour, matching the Rust trampoline contract.")

(provide 'nelisp-cc-jit-arith)

;;; nelisp-cc-jit-arith.el ends here
