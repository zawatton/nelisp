;;; nelisp-cc-mirror-is-bound.el --- Doc 111 §111.E #4 mirror_is_bound  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group A helper #4 — `mirror_is_bound'.  Phase 47
;; composition of #1 (`mirror_lookup_entry') + §111.B
;; `record-slot-ref-ptr' (slot 0) + §101.C `symbol-eq' against the
;; caller-supplied unbound-marker.  Replaces the
;; `Env::mirror_is_bound' Rust impl at
;; `build-tool/src/eval/env_mirror.rs::mirror_is_bound'.
;;
;; Signature:
;;   (nelisp_mirror_is_bound MIRROR-PTR SYM-PTR UNBOUND-PTR) -> i64
;;     MIRROR-PTR  : *const Sexp — env-mirror Record.
;;     SYM-PTR     : *const Sexp — Sexp::Symbol / Sexp::Str to look up.
;;     UNBOUND-PTR : *const Sexp — Sexp::Symbol("nelisp--unbound-marker")
;;                                  (= `Env::unbound_marker', supplied
;;                                  by the caller so this object stays
;;                                  stateless).
;;   Returns: i64.  1 if the symbol is bound (= entry exists AND
;;   slot 0 != unbound-marker), else 0.

;;; Code:

(defconst nelisp-cc-mirror-is-bound--source
  '(defun nelisp_mirror_is_bound (mirror-ptr sym-ptr unbound-ptr)
     ;; Compose §111.E #1 + slot-0-ptr + symbol-eq.
     ;;
     ;; The double `extern-call' into `nelisp_mirror_lookup_entry'
     ;; matches the pattern in #2 / #3: Phase 47 grammar does not yet
     ;; have an i64 `let' binding for raw pointer results, and the
     ;; FNV-1a + bucket walk is fast enough that the duplicate call is
     ;; not measurable in the production hot path (= `defvar' init).
     ;; Folding the duplicate is deferred to the §111.E optimisation
     ;; pass after Group B lands the `let' grammar form.
     (if (= (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr) 0)
         0
       (if (= (symbol-eq
               (record-slot-ref-ptr
                (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
                0)
               unbound-ptr)
              1)
           0
         1)))
  "Phase 47 source for Doc 111 §111.E #4 `mirror_is_bound'.

`boundp' equivalent against the elisp env mirror.  Returns 1 iff the
named entry exists in the mirror AND its value cell does not hold
the caller-supplied unbound-marker sentinel; else 0.

`symbol-eq' returns 0 for tag-mismatched inputs (= the value cell
holds a non-Symbol like `Sexp::Int' / `Sexp::Cons' / etc.), so this
helper correctly classifies any non-Symbol value cell as bound.")

(provide 'nelisp-cc-mirror-is-bound)

;;; nelisp-cc-mirror-is-bound.el ends here
