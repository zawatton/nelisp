;;; nelisp-cc-mirror-is-bound.el --- Doc 111 §111.E #4 mirror_is_bound  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group A helper #4 — `mirror_is_bound'.  AOT
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
     ;; R11a CSE-hoist: bind the entry pointer once via `let' (= let-rt
     ;; frame slot) and reuse for both the existence check and the
     ;; slot-0 unbound-marker comparison.  2 FNV-1a hashes + 2 bucket
     ;; walks → 1 per call; semantics bit-for-bit identical.
     (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)))
       (if (= entry 0)
           0
         (if (= (symbol-eq
                 (record-slot-ref-ptr entry 0)
                 unbound-ptr)
                1)
             0
           1))))
  "AOT source for Doc 111 §111.E #4 `mirror_is_bound'.

`boundp' equivalent against the elisp env mirror.  Returns 1 iff the
named entry exists in the mirror AND its value cell does not hold
the caller-supplied unbound-marker sentinel; else 0.

`symbol-eq' returns 0 for tag-mismatched inputs (= the value cell
holds a non-Symbol like `Sexp::Int' / `Sexp::Cons' / etc.), so this
helper correctly classifies any non-Symbol value cell as bound.

R11a (Doc 49 Wave 9): `let-rt' CSE hoist — single entry lookup,
reused in the miss test and the slot-0 unbound-marker comparison.")

(provide 'nelisp-cc-mirror-is-bound)

;;; nelisp-cc-mirror-is-bound.el ends here
