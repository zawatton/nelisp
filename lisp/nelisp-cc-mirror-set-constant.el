;;; nelisp-cc-mirror-set-constant.el --- Doc 111 §111.E #11 mirror_set_constant  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group B helper #11 — `mirror_set_constant'.  Phase 47
;; reimplementation of the env-mirror entry constant-flag writer (=
;; slot 3 of the `symbol-entry' record).  Mirrors
;; `build-tool/src/eval/env_mirror.rs::mirror_set_constant''s existing-
;; entry fast path:
;;
;;   if let Some(entry) = Env::mirror_lookup_entry(...) {
;;       unsafe { entry.with_slots_mut(|s| s[3] = value) };
;;       return;
;;   }
;;
;; The auto-vivify branch stays in Rust until helper #12
;; `mirror_install_entry' lands.  Callers pass the desired flag value
;; (`Sexp::T' for truthy, `Sexp::Nil' for falsy) as a `*const Sexp'.
;;
;; ABI deps:
;;   §111.E #1  `mirror_lookup_entry'  — locate the symbol-entry record.
;;   §111.B     `record-slot-set'      — refcount-safe slot 3 overwrite.

;;; Code:

(defconst nelisp-cc-mirror-set-constant--source
  '(seq
    (defun nelisp_mirror_set_constant_apply (entry-ptr flag-ptr)
      ;; Inner: dispatch on entry-ptr.  Slot 3 = constant flag.
      ;; `record-slot-set' returns rax=1 (Doc 111 §111.E enabler) so
      ;; the `(and X 1)' chain reliably returns 1 after the write.
      (if (= entry-ptr 0)
          0
        (and (record-slot-set entry-ptr 3 flag-ptr) 1)))
    (defun nelisp_mirror_set_constant (mirror-ptr sym-ptr flag-ptr)
      ;; mirror-ptr: *const Sexp pointing at the env-mirror Record.
      ;; sym-ptr:    *const Sexp pointing at the Sexp::Symbol / Str.
      ;; flag-ptr:   *const Sexp pointing at the new constant flag
      ;;             (= Sexp::T or Sexp::Nil; the Rust caller picks).
      ;;
      ;; Returns: i64.  1 on hit (slot 3 overwritten in place), 0 on
      ;; miss (entry not in mirror — caller may auto-vivify).
      (nelisp_mirror_set_constant_apply
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       flag-ptr)))
  "Phase 47 source for Doc 111 §111.E #11 `mirror_set_constant'.

Compose-on-7 with slot index 3 (= the constant-flag slot).  Returns
i64 1 on hit / 0 on miss; the Rust dispatcher falls back to a
`mirror_prepend_to_bucket' call on miss to mark the symbol constant
before its value is ever set (= `intern_constant' usage pattern).")

(provide 'nelisp-cc-mirror-set-constant)

;;; nelisp-cc-mirror-set-constant.el ends here
