;;; nelisp-cc-jit-aset.el --- Doc 120 §120.D aset swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.D — AOT-compiled replacement for the Rust
;; `nl_jit_access_aset' trampoline in `build-tool/src/jit/access.rs'.
;; Same `(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64' contract:
;;
;;   1. idx < 0:     TRAMPOLINE_ERR (= 1).
;;   2. Vector arm:  bounds-check idx < vec.value.len.  If in range:
;;      `vector-slot-set' (= refcount-aware overwrite via
;;      `nl_vector_set_slot') for the mutation, then
;;      `nl_sexp_clone_into' to thread `*val' into `*out' per the
;;      Rust trampoline's `*out = (*val).clone()' contract.  OOR
;;      → ERR.
;;   3. BoolVector arm: delegate to narrow Rust extern
;;      `nl_jit_access_aset_bool_vector_inner' (same blocker as
;;      `aref' BoolVector — no bool-vector grammar yet).
;;   4. Anything else: ERR → strategy.el dispatches.
;;
;; Tag-byte constants: 8 = `nelisp-sexp--tag-vector', 10 =
;; `nelisp-sexp--tag-bool-vector'.

;;; Code:

(defconst nelisp-cc-jit-aset--source
  '(defun nelisp_jit_aset (arg idx val out)
     ;; arg: *const Sexp.  idx: i64.  val: *const Sexp.
     ;; out: *mut Sexp.  Returns: i64 = 0 on OK, 1 on ERR.
     (if (< idx 0)
         1
       (if (= (sexp-tag arg) 8)
           (if (< idx (vector-len arg))
               (and
                (vector-slot-set arg idx val)
                (extern-call nl_sexp_clone_into val out)
                0)
             1)
         (if (= (sexp-tag arg) 10)
             (extern-call nl_jit_access_aset_bool_vector_inner arg idx val out)
           1))))
  "AOT source for the §120.D `nl_jit_access_aset' swap.

Vector arm: inline bounds check + refcount-safe
`vector-slot-set' (= delegates to `nl_vector_set_slot') for the
mutation, then refcount-aware `nl_sexp_clone_into' for the `*out
= clone(*val)' write.  BoolVector arm: extern call to a narrow
Rust helper because AOT has no `bool-vector-set-bit'
grammar primitive yet.  Negative idx + non-array-tag both return
ERR; strategy.el's `condition-case' dispatcher classifies the
error by `type-of'.

The two-op chain inside the Vector OK arm composes
`vector-slot-set' (= rax=1 sentinel) with `nl_sexp_clone_into'
(= void return → rax preserved) and the trailing `0' status
sentinel via the standard `(and ... 0)' idiom (§120.A
`nl_jit_ref_eq', §120.B `nl_jit_record_set').")

(provide 'nelisp-cc-jit-aset)

;;; nelisp-cc-jit-aset.el ends here
