;;; nelisp-cc-env-lookup-function.el --- Wave a-2: Env::lookup_function Phase 47 .o  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave a-2 — `Env::lookup_function' body migrated to Phase 47 elisp .o.
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
  '(seq
    ;; nelisp_env_lkf_hit
    ;;
    ;; Hit path: fill out-ptr with the function Sexp via
    ;; `nelisp_mirror_lookup_function' (= record-slot-ref slot 1, which
    ;; clones via `nl_sexp_clone_into' — refcount-safe).
    ;; Returns 0 (= found sentinel).
    ;;
    ;; Arity 4 (even) — `sub rsp, 8' NOT needed; rsp ≡ 0 mod 16 at body.
    ;; extern-call `nelisp_mirror_lookup_function' is arg 0 of `and'. ✓
    (defun nelisp_env_lkf_hit (mirror-ptr name-ptr out-ptr _pad)
      (and (extern-call nelisp_mirror_lookup_function mirror-ptr name-ptr out-ptr)
           0))

    ;; nelisp_env_lookup_function
    ;;
    ;; Main entry: check mirror for the function entry; return 1 on miss
    ;; or call `nelisp_env_lkf_hit' to fill out-ptr and return 0 on hit.
    ;;
    ;; Arity 4 (even) ✓.
    ;; `(extern-call nelisp_mirror_lookup_entry mirror-ptr name-ptr)' is
    ;; arg 0 of `='; only one extern-call per code path ✓.
    (defun nelisp_env_lookup_function (mirror-ptr unbound-ptr name-ptr out-ptr)
      (if (= (extern-call nelisp_mirror_lookup_entry mirror-ptr name-ptr) 0)
          1
        (nelisp_env_lkf_hit mirror-ptr name-ptr out-ptr 0))))
  "Phase 47 source for Wave a-2 `Env::lookup_function' body.

Two-defun CPS composition:
  `nelisp_env_lookup_function' — mirror entry check; dispatches to
    hit or miss path (extern-call as arg 0 of `=' ✓).
  `nelisp_env_lkf_hit' — fills out-ptr via refcount-safe
    `nelisp_mirror_lookup_function' (record-slot-ref slot 1, delegates
    to `nl_sexp_clone_into' per Doc 111 §111.C v3).

The `unbound-ptr' parameter is unused (retained for call-site symmetry
with `lookup_value' and to keep even arity).  The miss sentinel is 1
(= unbound-fn), found is 0 (= out-ptr filled with function Sexp).")

(provide 'nelisp-cc-env-lookup-function)

;;; nelisp-cc-env-lookup-function.el ends here
