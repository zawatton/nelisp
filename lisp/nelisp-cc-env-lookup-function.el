;;; nelisp-cc-env-lookup-function.el --- Wave a-2: Env::lookup_function AOT .o  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave a-2 — `Env::lookup_function' body migrated to AOT elisp .o.
;; Replaces the 7-LOC Rust body in
;; `build-tool/src/eval/env_helpers.rs::Env::lookup_function'.
;;
;; Algorithm (= literal transcription of the Rust body):
;;
;;   1. Check mirror entry existence via `nelisp_mirror_lookup_entry'.
;;      If miss (= 0), return 1 (= unbound-fn sentinel).
;;
;;   2. If hit: call `nelisp_mirror_lookup_function' to fill out-ptr
;;      with the function Sexp (refcount-aware copy via record-slot-ref
;;      which delegates to `nl_sexp_clone_into' since Doc 111 §111.C v3).
;;      Return 0 (= found).
;;
;; Signature:
;;   (nelisp_env_lookup_function MIRROR-PTR UNBOUND-PTR NAME-PTR OUT-PTR)
;;     MIRROR-PTR  : *const Sexp — Env::globals_record.
;;     UNBOUND-PTR : *const Sexp — Env::unbound_marker (unused, for future
;;                                  use / arity padding to 4 = even).
;;     NAME-PTR    : *const Sexp — Sexp::Symbol name to look up.
;;     OUT-PTR     : *mut Sexp   — 32-byte caller-owned result slot.
;;   Returns: i64.  0 = found (function written to *out-ptr),
;;                  1 = unbound (out-ptr unchanged).
;;
;; ABI:
;;   4 args (even) — body-entry rsp ≡ 0 mod 16 ✓.
;;   Each defun has at most one extern-call in any execution path.
;;
;; ABI deps:
;;   nelisp_mirror_lookup_entry    — hit/miss check (0 = miss)
;;   nelisp_mirror_lookup_function — fills out-ptr (refcount-safe)

;;; Code:

(defconst nelisp-cc-env-lookup-function--source
  '(defun nelisp_env_lookup_function (mirror-ptr unbound-ptr name-ptr out-ptr)
     ;; Main entry: check mirror for the function entry; return 1 on
     ;; miss or fill out-ptr with the function Sexp on hit (= 0).
     ;;
     ;; R11a CSE-hoist: the previous two-defun CPS shape called
     ;; `nelisp_mirror_lookup_entry' for the existence check and then
     ;; `nelisp_mirror_lookup_function' on hit (= 2 FNV-1a hashes).
     ;; The hoisted shape calls `nelisp_mirror_lookup_entry' once via
     ;; `let' (= let-rt frame slot) and reads slot 1 directly via
     ;; `record-slot-ref' — bypassing the `nelisp_mirror_lookup_function'
     ;; wrapper since we already hold the entry pointer.  `record-slot-
     ;; ref' delegates to `nl_sexp_clone_into' (refcount-safe, same
     ;; semantics as the previous wrapper).
     ;;
     ;; Arity 4 (even) ✓.  `(extern-call nelisp_mirror_lookup_entry ...)'
     ;; is arg 0 of the let-binding (= itself the value-form of let-rt,
     ;; satisfying the "extern-call as arg 0" alignment rule).
     (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr name-ptr)))
       (if (= entry 0)
           1
         (and (record-slot-ref entry 1 out-ptr) 0))))
  "AOT source for Wave a-2 `Env::lookup_function' body.

R11a (Doc 49 Wave 9): collapsed two-defun CPS to a single defun
using `let-rt' CSE hoist of the entry pointer + `record-slot-ref'
direct slot-1 read.  Previous shape paid 2 FNV-1a hashes per call
(`lookup_entry' + `lookup_function'); hoisted shape pays 1.

The `unbound-ptr' parameter is unused (retained for call-site symmetry
with `lookup_value' and to keep even arity).  The miss sentinel is 1
(= unbound-fn), found is 0 (= out-ptr filled with function Sexp via
the refcount-safe `nl_sexp_clone_into' invoked by `record-slot-ref').")

(provide 'nelisp-cc-env-lookup-function)

;;; nelisp-cc-env-lookup-function.el ends here
