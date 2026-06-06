;;; nelisp-cc-mirror-lookup-function.el --- Doc 111 §111.E #3 mirror_lookup_function  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group A helper #3 — `mirror_lookup_function'.
;; Identical to #2 except it reads slot 1 (function cell) instead of
;; slot 0 (value cell).  Replaces the `Env::mirror_lookup_function'
;; Rust impl at
;; `build-tool/src/eval/env_mirror.rs::mirror_lookup_function'.
;;
;; Signature:
;;   (nelisp_mirror_lookup_function MIRROR-PTR SYM-PTR RESULT-SLOT)
;;     Same shape as #2; RESULT-SLOT receives the function Sexp on
;;     hit or `Sexp::Nil' on miss.

;;; Code:

(defconst nelisp-cc-mirror-lookup-function--source
  '(defun nelisp_mirror_lookup_function (mirror-ptr sym-ptr result-slot)
     ;; Compose §111.E #1 + §111.B `record-slot-ref' over slot 1.
     ;; CSE-hoisted via `let' — see `mirror-lookup-value' for the full
     ;; rationale; this file mirrors that fix on slot 1 (function cell).
     (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)))
       (if (= entry 0)
           (sexp-write-nil result-slot)
         (record-slot-ref entry 1 result-slot))))
  "AOT source for Doc 111 §111.E #3 `mirror_lookup_function'.

Identical to `mirror_lookup_value' (#2) except it reads slot 1 (=
symbol-entry function cell) instead of slot 0 (= value cell).  The
sentinel-return convention matches #2: `Sexp::Nil' on miss, the
caller-facing dispatcher re-introduces the unbound marker.

R11a (Doc 49 Wave 9): `let-rt' CSE hoist — 2 FNV-1a hashes + 2
bucket walks reduced to 1 per call (= bit-for-bit identical to the
previous double-call shape).")

(provide 'nelisp-cc-mirror-lookup-function)

;;; nelisp-cc-mirror-lookup-function.el ends here
