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
  '(defun nelisp_mirror_is_constant (mirror-ptr sym-ptr)
     ;; Compose §111.E #1 + slot-3-ptr + sexp-tag tag-byte read.
     ;;
     ;; `SEXP_TAG_T' = 1 per
     ;; `build-tool/src/eval/sexp.rs::SEXP_TAG_T'.  The Rust impl
     ;; matches `Some(Sexp::T)' (= slot 3 = `Sexp::T'); any other
     ;; value (including the default `Sexp::Nil') yields false.
     (if (= (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr) 0)
         0
       (if (= (sexp-tag
               (record-slot-ref-ptr
                (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
                3))
              1)
           1
         0)))
  "Phase 47 source for Doc 111 §111.E #6 `mirror_is_constant'.

Reads symbol-entry slot 3 (= the constant-flag cell, initialised to
`Sexp::Nil' and set to `Sexp::T' by `intern_constant' / `defconst').
Returns 1 iff the tag byte equals `SEXP_TAG_T' (= 1), matching the
Rust impl's `matches!(slot, Some(Sexp::T))' classification.

This helper takes only `(MIRROR-PTR SYM-PTR)' — no unbound-marker
parameter — because constancy is a direct tag-byte check, not a
sentinel comparison.  Slot 3 absent (= record with fewer than 4
slots) is impossible under `mirror_install_entry' which always
allocates 4 slots; if a malformed mirror is ever supplied the
out-of-bounds `record-slot-ref-ptr' is on the caller.")

(provide 'nelisp-cc-mirror-is-constant)

;;; nelisp-cc-mirror-is-constant.el ends here
