;;; nelisp-cc-env-shim-set-op.el --- Wave c+: env_shim set-* .o dispatcher -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave c+ — AOT reimplementation of the
;; `nelisp--env-globals-op' set-value / set-function / set-constant
;; dispatch arms.  Replaces 3 Rust match arms in
;; `build-tool/src/eval/env_shim.rs::bi_globals_op' with a thin
;; elisp .o that reads the op symbol, calls the appropriate
;; `_or_insert' AOT helper, then clones the stored value back
;; into the caller-supplied result slot.
;;
;; Handled OPs:
;;   set-value    → slot 7 of scratch → nelisp_mirror_set_value_or_insert
;;   set-function → slot 8 of scratch → nelisp_mirror_set_function_or_insert
;;   set-constant → slot 10 of scratch → nelisp_mirror_set_constant_or_insert
;;
;; Scratch vector layout (built by Rust thin wrapper, same as
;; `build_or_insert_scratch_vec'):
;;   [0]  Nil source
;;   [1]  inner-pair scratch
;;   [2]  outer-cell scratch
;;   [3]  count int scratch
;;   [4]  KEY str scratch
;;   [5]  symbol-entry tag
;;   [6]  entry result
;;   [7]  value cell  (= args[2] for set-value, unbound for others)
;;   [8]  function cell (= args[2] for set-function, unbound for others)
;;   [9]  plist (Nil)
;;   [10] constant flag (Sexp::T or Sexp::Nil for set-constant, Nil for others)
;;
;; Signature of `nelisp_env_shim_set_op':
;;   op-ptr      (arg 0 / rdi): *const Sexp — OP symbol
;;   mirror-ptr  (arg 1 / rsi): *const Sexp — env globals record
;;   sym-ptr     (arg 2 / rdx): *const Sexp — symbol name
;;   scratch-ptr (arg 3 / rcx): *const Sexp — pre-built 11-slot scratch vec
;;   result-slot (arg 4 / r8):  *mut Sexp   — output slot (caller-owned)
;;   _pad        (arg 5 / r9):  i64         — arity alignment pad
;;
;; Return codes:
;;   1  = success (result Sexp cloned from scratch into result-slot)
;;  -3  = unknown OP (should not occur; Rust pre-validates)
;;
;; Arity conventions:
;;   All defuns are even-arity (4 or 6) so body-entry rsp ≡ 0 mod 16.
;;   Each extern-call or vector-ref occupies a distinct CPS step.
;;
;; ABI deps:
;;   nelisp_mirror_set_value_or_insert    — set-value write+vivify
;;   nelisp_mirror_set_function_or_insert — set-function write+vivify
;;   nelisp_mirror_set_constant_or_insert — set-constant write+vivify
;;   nl_sexp_clone_into                   — via vector-ref (refcount-safe copy)
;;   symbol-name-eq                       — inline tag+len+byte compare

;;; Code:

(defconst nelisp-cc-env-shim-set-op--source
  '(seq

    ;; nelisp_env_shim_setop_finish
    ;;
    ;; After a _or_insert call: clone scratch[IDX] into result-slot.
    ;; Returns 1 always.
    ;;
    ;; _rv:         unused i64 return of the _or_insert call.
    ;; scratch-ptr: *const Sexp pointing at the 11-slot scratch vector.
    ;; idx:         i64 — slot index within scratch (7, 8, or 10).
    ;; result-slot: *mut Sexp — output slot.
    ;;
    ;; `vector-ref' clones scratch[idx] refcount-safely into result-slot
    ;; via `nl_sexp_clone_into', then restores dst ptr into rax (= the
    ;; slot pointer, not the return value).  The outer `(and ... 1)'
    ;; discards rax and returns the i64 1.
    ;;
    ;; Arity 4 (even) — body-entry rsp ≡ 0 mod 16.
    ;; `vector-ref' inserts one `push rsi' (= dst save) before the PLT
    ;; call → rsp ≡ 8 mod 16 before call → callee rsp ≡ 0 mod 16. ✓
    (defun nelisp_env_shim_setop_finish (_rv scratch-ptr idx result-slot)
      (and (vector-ref scratch-ptr idx result-slot) 1))

    ;; nelisp_env_shim_set_op
    ;;
    ;; Dispatch on op-ptr for set-value / set-function / set-constant.
    ;; Each arm calls the matching `_or_insert' AOT helper then
    ;; delegates to `nelisp_env_shim_setop_finish' with the correct
    ;; scratch slot index.
    ;;
    ;; Arity 6 (even) — body-entry rsp ≡ 0 mod 16.  `extern-call' with
    ;; 4 args uses registers (rdi/rsi/rdx/rcx) with no stack pushes;
    ;; the `call' instruction itself decrements rsp by 8, giving callee
    ;; entry ≡ 0 mod 16. ✓
    ;;
    ;; The `(extern-call FUNC ...)' is evaluated to completion before
    ;; any `nelisp_env_shim_setop_finish' call arg is pushed, so the
    ;; stack is back at baseline before each defun call. ✓
    (defun nelisp_env_shim_set_op
        (op-ptr mirror-ptr sym-ptr scratch-ptr result-slot _pad)

      ;; set-value: write value (slot 7) via mirror_set_value_or_insert.
      (if (= (symbol-name-eq op-ptr "set-value") 1)
          (nelisp_env_shim_setop_finish
           (extern-call nelisp_mirror_set_value_or_insert
                        mirror-ptr sym-ptr scratch-ptr 0)
           scratch-ptr 7 result-slot)

        ;; set-function: write function cell (slot 8).
        (if (= (symbol-name-eq op-ptr "set-function") 1)
            (nelisp_env_shim_setop_finish
             (extern-call nelisp_mirror_set_function_or_insert
                          mirror-ptr sym-ptr scratch-ptr 0)
             scratch-ptr 8 result-slot)

          ;; set-constant: write constant flag (slot 10).
          (if (= (symbol-name-eq op-ptr "set-constant") 1)
              (nelisp_env_shim_setop_finish
               (extern-call nelisp_mirror_set_constant_or_insert
                            mirror-ptr sym-ptr scratch-ptr 0)
               scratch-ptr 10 result-slot)

            ;; Unknown OP — Rust pre-validates; should not reach here.
            -3))))
    )
  "AOT source for Wave c+ `nelisp_env_shim_set_op'.

Two-defun CPS composition:
  `nelisp_env_shim_set_op' (6 args, even) — op dispatch.
    Three sequential if-arms, each with one extern-call for the
    _or_insert mutation, then delegate to finish helper.
  `nelisp_env_shim_setop_finish' (4 args, even) — post-mutation clone.
    Uses `vector-ref' to copy scratch[idx] to result-slot refcount-safely.

The SCRATCH-PTR arg carries the pre-built 11-slot scratch vector from the
Rust thin wrapper (`build_or_insert_scratch_vec' layout):
  slot 7  = value sexp  (set-value path)
  slot 8  = function sexp (set-function path)
  slot 10 = constant flag T/Nil (set-constant path)
The other slots hold unbound_marker / Nil / symbol-entry tag as required
by the `_or_insert' auto-vivify branch.")

(provide 'nelisp-cc-env-shim-set-op)

;;; nelisp-cc-env-shim-set-op.el ends here
