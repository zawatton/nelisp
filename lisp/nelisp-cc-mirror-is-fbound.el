;;; nelisp-cc-mirror-is-fbound.el --- Doc 111 §111.E #5 mirror_is_fbound  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group A helper #5 — `mirror_is_fbound'.  Identical
;; in shape to #4 except it inspects slot 1 (= function cell) instead
;; of slot 0.  Replaces the `Env::mirror_is_fbound' Rust impl at
;; `build-tool/src/eval/env_mirror.rs::mirror_is_fbound'.
;;
;; Signature:
;;   (nelisp_mirror_is_fbound MIRROR-PTR SYM-PTR UNBOUND-PTR) -> i64
;;     Returns: i64.  1 if the symbol has a function binding (= entry
;;     exists AND slot 1 != unbound-marker), else 0.

;;; Code:

(defconst nelisp-cc-mirror-is-fbound--source
  '(defun nelisp_mirror_is_fbound (mirror-ptr sym-ptr unbound-ptr)
     ;; R11a CSE-hoist — see `mirror-is-bound' for the full rationale;
     ;; this mirrors the same fix on slot 1 (function cell).
     (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)))
       (if (= entry 0)
           0
         (if (= (symbol-eq
                 (record-slot-ref-ptr entry 1)
                 unbound-ptr)
                1)
             0
           1))))
  "Phase 47 source for Doc 111 §111.E #5 `mirror_is_fbound'.

`fboundp' equivalent against the elisp env mirror.  Slot 1 variant
of #4 `mirror_is_bound'; same `symbol-eq' tag-mismatch semantics.

R11a (Doc 49 Wave 9): `let-rt' CSE hoist — single entry lookup
shared across miss test and slot-1 unbound-marker check.")

(provide 'nelisp-cc-mirror-is-fbound)

;;; nelisp-cc-mirror-is-fbound.el ends here
