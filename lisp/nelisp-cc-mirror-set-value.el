;;; nelisp-cc-mirror-set-value.el --- Doc 111 §111.E #7 mirror_set_value  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group B helper #7 — `mirror_set_value'.  Phase 47
;; reimplementation of the env-mirror entry value-slot writer.  Mirrors
;; the existing-entry fast path of
;; `build-tool/src/eval/env_mirror.rs::mirror_set_value':
;;
;;   if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
;;       unsafe { entry.with_slots_mut(|s| s[0] = value) };
;;       return;
;;   }
;;
;; The auto-vivify branch (= `mirror_insert_new_entry') stays in Rust;
;; this helper handles only the in-place mutate case.  Callers that
;; need auto-vivify should call this helper, observe its 0 return (=
;; entry not found), and fall back to helper #12 `mirror_install_entry'
;; or the Rust path.
;;
;; ABI deps:
;;   §111.E #1  `mirror_lookup_entry'  — locate the symbol-entry record.
;;   §111.B     `record-slot-set'      — refcount-safe slot 0 overwrite.

;;; Code:

(defconst nelisp-cc-mirror-set-value--source
  '(seq
    (defun nelisp_mirror_set_value_apply (entry-ptr val-ptr)
      ;; Inner: dispatch on entry-ptr being null (= miss) or live.
      ;; Returns: i64 1 on hit (slot 0 was overwritten), 0 on miss.
      ;;
      ;; `record-slot-set' is augmented (Doc 111 §111.E enabler) to
      ;; materialise `rax = 1' after the helper call so it composes
      ;; cleanly with `and' value-form chains.  Dynamic rsp alignment
      ;; inside the emit means this works regardless of body-entry
      ;; alignment (= odd-arity outer with 8 mod 16 entry, or
      ;; even-arity outer with 0 mod 16 entry).
      (if (= entry-ptr 0)
          0
        (and (record-slot-set entry-ptr 0 val-ptr) 1)))
    (defun nelisp_mirror_set_value (mirror-ptr sym-ptr val-ptr)
      ;; mirror-ptr: *const Sexp pointing at the env-mirror Record.
      ;; sym-ptr:    *const Sexp pointing at the Sexp::Symbol / Str.
      ;; val-ptr:    *const Sexp pointing at the new value to install.
      ;;
      ;; Returns: i64.  1 on hit (slot 0 overwritten in place), 0 on
      ;; miss (entry not in mirror — caller falls back to install).
      ;;
      ;; Pre-conditions: same as `mirror_lookup_entry' (= mirror is a
      ;; well-formed `nelisp-env' record with fast-hash-table at slot 0).
      (nelisp_mirror_set_value_apply
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       val-ptr)))
  "Phase 47 source for Doc 111 §111.E #7 `mirror_set_value'.

Composes `mirror_lookup_entry' (§111.E #1) + `record-slot-set'
(§111.B).  Returns i64 1 on hit / 0 on miss; the Rust dispatcher
falls back to `mirror_insert_new_entry' on miss until helper #12
`mirror_install_entry' lands and replaces that path too.")

(provide 'nelisp-cc-mirror-set-value)

;;; nelisp-cc-mirror-set-value.el ends here
