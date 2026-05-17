;;; nelisp-cc-mirror-install-entry.el --- Doc 111 §111.E #12 mirror_install_entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group B helper #12 — `mirror_install_entry'.  Phase
;; 47 reimplementation of the env-mirror full entry installer (=
;; `intern_constant' + `image_decode_v3_into' usage pattern).  Mirrors
;; the existing-entry update fast path of
;; `build-tool/src/eval/env_mirror.rs::mirror_install_entry':
;;
;;   if let Some(entry) = Env::mirror_lookup_entry(...) {
;;       unsafe {
;;           entry.with_slots_mut(|s| {
;;               s[0] = value_slot;
;;               s[1] = function_slot;
;;               s[2] = plist_slot;
;;               s[3] = constant_slot;
;;           });
;;       }
;;       return;
;;   }
;;
;; Miss / auto-vivify branch — allocate fresh symbol-entry record +
;; cons-prepend onto bucket — stays in Rust under the helper-#12
;; dispatcher: this elisp object returns 0 on miss so the Rust caller
;; can route to `mirror_prepend_to_bucket' as today.  Splitting the
;; install path this way keeps the elisp surface small (= 4 record-
;; slot-set calls) and defers the bucket-vector mutation (= the parts
;; that need `nl_alloc_record' + `vector-slot-set' + the bump-count
;; record-slot-set) to a later helper if the LOC win warrants it.
;;
;; The 4 slot-value pointers (value / function / plist / constant) are
;; the caller's responsibility: the Rust caller resolves `Option<Sexp>'
;; arguments to either the actual value or `self.unbound_marker' (=
;; same pre-resolution the existing Rust impl already does internally)
;; before invoking this helper.  This matches the convention helpers
;; #9 / #10 use for the unbound-marker pointer.
;;
;; ABI deps:
;;   §111.E #1  `mirror_lookup_entry'  — locate the symbol-entry record.
;;   §111.B     `record-slot-set'      — refcount-safe slot 0-3 overwrite.

;;; Code:

(defconst nelisp-cc-mirror-install-entry--source
  '(seq
    (defun nelisp_mirror_install_entry_apply
        (entry-ptr value-ptr function-ptr plist-ptr constant-ptr)
      ;; Inner: dispatch on entry-ptr.  Update all 4 slots in place.
      ;;
      ;; The four `record-slot-set' calls compile to four
      ;; `nl_record_set_slot' externs in sequence; each handles its own
      ;; refcount-aware drop of the prior slot value + clone of the
      ;; new one.  No ordering hazards: each slot is independent.
      ;;
      ;; Each `record-slot-set' materialises rax=1 (Doc 111 §111.E
      ;; enabler), so the `and' value-form chain runs all 4 writes and
      ;; returns 1 at the end.
      (if (= entry-ptr 0)
          0
        (and
         (record-slot-set entry-ptr 0 value-ptr)
         (record-slot-set entry-ptr 1 function-ptr)
         (record-slot-set entry-ptr 2 plist-ptr)
         (record-slot-set entry-ptr 3 constant-ptr))))
    (defun nelisp_mirror_install_entry
        (mirror-ptr sym-ptr value-ptr function-ptr plist-ptr constant-ptr)
      ;; mirror-ptr:   *const Sexp pointing at the env-mirror Record.
      ;; sym-ptr:      *const Sexp pointing at the Sexp::Symbol / Str.
      ;; value-ptr:    *const Sexp — new value slot (slot 0).
      ;; function-ptr: *const Sexp — new function slot (slot 1).
      ;; plist-ptr:    *const Sexp — new plist slot (slot 2).
      ;; constant-ptr: *const Sexp — new constant flag (slot 3, T/Nil).
      ;;
      ;; Returns: i64.  1 on hit (all 4 slots overwritten in place), 0
      ;; on miss (entry not in mirror — caller falls back to the Rust
      ;; `mirror_prepend_to_bucket' branch which allocates a fresh
      ;; record + cons-prepends onto the bucket).
      ;;
      ;; The 6-arg shape matches the SysV AMD64 ABI's GP-register limit
      ;; (rdi/rsi/rdx/rcx/r8/r9), so the helper avoids any stack-arg
      ;; complexity.  All `Option<Sexp>' / `bool' resolution happens on
      ;; the Rust side before the call (= explicit unbound-marker /
      ;; Sexp::T / Sexp::Nil pointers).
      (nelisp_mirror_install_entry_apply
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       value-ptr function-ptr plist-ptr constant-ptr)))
  "Phase 47 source for Doc 111 §111.E #12 `mirror_install_entry'.

Implements only the existing-entry update fast path (= 4
`record-slot-set' calls).  Returns i64 1 on hit / 0 on miss; the
Rust dispatcher falls back to `mirror_prepend_to_bucket' on miss
(= same path helpers #7 / #11 currently take on auto-vivify).
Splitting the install path this way keeps the elisp surface
~10 lines instead of ~40 and defers the `nl_alloc_record' /
`vector-slot-set' work (a similar follow-up helper).")

(provide 'nelisp-cc-mirror-install-entry)

;;; nelisp-cc-mirror-install-entry.el ends here
