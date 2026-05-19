;;; nelisp-cc-mirror-is-constant.el --- Doc 111 §111.E #6 mirror_is_constant  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group A helper #6 — `mirror_is_constant'.  Phase 47
;; composition of #1 (`mirror_lookup_entry') + §111.B
;; `record-slot-ref-ptr' (slot 3) + §100 `sexp-tag' equality against
;; `SEXP_TAG_T' (= 1).  Replaces the `Env::mirror_is_constant' Rust
;; impl at `build-tool/src/eval/env_mirror.rs::mirror_is_constant'.
;;
;; Signature:
;;   (nelisp_mirror_is_constant MIRROR-PTR SYM-PTR) -> i64
;;     MIRROR-PTR : *const Sexp — env-mirror Record.
;;     SYM-PTR    : *const Sexp — Sexp::Symbol / Sexp::Str to look up.
;;   Returns: i64.  1 if the entry exists AND slot 3 holds `Sexp::T'
;;   (= `SEXP_TAG_T' / numeric value 1), else 0.  No unbound-marker
;;   parameter — constancy is a single tag-byte check, not a
;;   sentinel-Sexp comparison.

;;; Code:

(defconst nelisp-cc-mirror-is-constant--source
  '(seq
    ;; entry-ptr: result of nelisp_mirror_lookup_entry, pre-fetched as
    ;; register arg 0 (no alignment hazard).
    ;; Arity 4 (even): body-entry rsp ≡ 0 mod 16.
    ;; `record-slot-ref-ptr' and `sexp-tag' are inline ops — no CALL.
    ;; `SEXP_TAG_T' = 1 per `build-tool/src/eval/sexp.rs::SEXP_TAG_T'.
    ;; Guard: if entry-ptr = 0 (symbol not found), return 0 — no slot access.
    ;; `(= entry-ptr 0)' is safe: entry-ptr is a register arg, not an
    ;; extern-call result pushed mid-expression.
    (defun nelisp_mirror_is_constant_check (entry-ptr _mirror-ptr _sym-ptr _pad)
      (if (= entry-ptr 0)
          0
        (if (= (sexp-tag (record-slot-ref-ptr entry-ptr 3)) 1) 1 0)))

    ;; Public entry: nelisp_mirror_is_constant(mirror-ptr, sym-ptr) → i64
    ;; Arity 2 (even): body-entry rsp ≡ 0.
    ;; extern-call `nelisp_mirror_lookup_entry' at position 0 → rsp ≡ 0 ✓.
    (defun nelisp_mirror_is_constant (mirror-ptr sym-ptr)
      (nelisp_mirror_is_constant_check
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       mirror-ptr sym-ptr 0)))
  "Phase 47 source for Doc 111 §111.E #6 `mirror_is_constant'.

Two defuns (seq form).  Alignment-safe CPS structure.

Reads symbol-entry slot 3 (= the constant-flag cell, initialised to
`Sexp::Nil' and set to `Sexp::T' by `intern_constant' / `defconst').
Returns 1 iff the tag byte equals `SEXP_TAG_T' (= 1), matching the
Rust impl's `matches!(slot, Some(Sexp::T))' classification.

Alignment fix: the original single-defun version had
`(= (extern-call nelisp_mirror_lookup_entry ...) 0)' in arity-2
which caused rsp ≡ 8 at the extern-call → SIGSEGV.  The check
helper receives entry-ptr at arg position 0 → safe.")

(provide 'nelisp-cc-mirror-is-constant)

;;; nelisp-cc-mirror-is-constant.el ends here
