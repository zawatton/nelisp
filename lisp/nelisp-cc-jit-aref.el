;;; nelisp-cc-jit-aref.el --- Doc 120 §120.D aref swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.D — Phase-47-compiled replacement for the Rust
;; `nl_jit_access_aref' trampoline in `build-tool/src/jit/access.rs'.
;; Same `(*const Sexp, i64, *mut Sexp) -> i64' contract:
;;
;;   1. idx < 0:    TRAMPOLINE_ERR (= 1).  Strategy.el actually
;;      pre-checks this before calling but the trampoline is
;;      defensive (= the pre-Doc 120.D Rust impl had the same
;;      guard).
;;   2. Vector arm: bounds-check idx < vec.value.len.  If in range,
;;      copy slot via refcount-aware `vector-ref' (= delegates to
;;      `nl_sexp_clone_into') → OK.  OOR → ERR.
;;   3. BoolVector arm: delegate to the narrow Rust extern
;;      `nl_jit_access_aref_bool_vector_inner' which performs the
;;      single-bit read + `Sexp::T' / `Sexp::Nil' tag-byte write.
;;      Phase 47 has no bool-vector grammar primitives yet (see
;;      Doc 122 §122.B `bool-vector-{len,bit,set-bit}' blocker
;;      cluster), so the extern is the minimum viable swap path.
;;   4. Anything else: ERR → strategy.el's `condition-case'
;;      dispatcher routes by `type-of'.
;;
;; Tag-byte constants: 8 = `nelisp-sexp--tag-vector', 10 =
;; `nelisp-sexp--tag-bool-vector'.

;;; Code:

(defconst nelisp-cc-jit-aref--source
  '(defun nelisp_jit_aref (arg idx out)
     ;; arg: *const Sexp.  idx: i64.  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK, 1 on ERR.
     (if (< idx 0)
         1
       (if (= (sexp-tag arg) 8)
           (if (< idx (vector-len arg))
               (and (vector-ref arg idx out) 0)
             1)
         (if (= (sexp-tag arg) 10)
             (extern-call nl_jit_access_aref_bool_vector_inner arg idx out)
           1))))
  "Phase 47 source for the §120.D `nl_jit_access_aref' swap.

Three guard arms: (1) `idx >= 0' lower bound, (2) Vector tag-byte
check + inline `vector-len' bounds check + refcount-aware
`vector-ref' slot copy, (3) BoolVector tag-byte check +
delegation to the narrow `nl_jit_access_aref_bool_vector_inner'
Rust extern (= 1-byte-per-bit storage decode + Sexp::T/Nil tag
write).  Vector OOR / non-array-tag → ERR; strategy.el's
`condition-case' dispatcher routes the error by `type-of'.

The `(and SIDE-EFFECT 0)' idiom for the Vector OK arm matches
§120.A / §120.B conventions; the BoolVector arm uses the extern's
i64 return directly so the trampoline status is propagated
without an extra `and' wrapper.")

(provide 'nelisp-cc-jit-aref)

;;; nelisp-cc-jit-aref.el ends here
