;;; nelisp-cc-mirror-clear-value.el --- Doc 111 §111.E #9 mirror_clear_value  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group B helper #9 — `mirror_clear_value'.  Phase 47
;; reimplementation of the env-mirror entry value-slot clearer (=
;; `makunbound' fast path).  Mirrors
;; `build-tool/src/eval/env_mirror.rs::mirror_clear_value':
;;
;;   if let Some(entry) = Env::mirror_lookup_entry(...) {
;;       unsafe { entry.with_slots_mut(|s| s[0] = unbound) };
;;   }
;;
;; The Rust impl reads `self.unbound_marker' from the live `Env'.  The
;; Phase 47 helper cannot access Env state, so the caller passes the
;; unbound-marker `*const Sexp' as an explicit argument (= the same
;; pattern used by `mirror_clear_value' callers which all have access
;; to `self.unbound_marker' anyway).
;;
;; ABI deps:
;;   §111.E #1  `mirror_lookup_entry'  — locate the symbol-entry record.
;;   §111.B     `record-slot-set'      — refcount-safe slot 0 overwrite.

;;; Code:

(defconst nelisp-cc-mirror-clear-value--source
  '(seq
    (defun nelisp_mirror_clear_value_apply (entry-ptr unbound-ptr)
      ;; Inner: dispatch on entry-ptr.  Slot 0 = value cell.
      ;; `record-slot-set' returns rax=1 (Doc 111 §111.E enabler) so
      ;; the `(and X 1)' chain reliably returns 1 after the write.
      (if (= entry-ptr 0)
          0
        (and (record-slot-set entry-ptr 0 unbound-ptr) 1)))
    (defun nelisp_mirror_clear_value (mirror-ptr sym-ptr unbound-ptr)
      ;; mirror-ptr:  *const Sexp pointing at the env-mirror Record.
      ;; sym-ptr:     *const Sexp pointing at the Sexp::Symbol / Str.
      ;; unbound-ptr: *const Sexp pointing at the `nelisp--unbound-marker'
      ;;              sentinel (= `Env::unbound_marker' from the Rust
      ;;              caller's side).
      ;;
      ;; Returns: i64.  1 on hit (slot 0 set to unbound), 0 on miss.
      ;; The Rust impl is silent on miss (= the entry doesn't exist so
      ;; nothing to clear); the helper preserves that — miss is not an
      ;; error condition for `makunbound'.
      (nelisp_mirror_clear_value_apply
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       unbound-ptr)))
  "Phase 47 source for Doc 111 §111.E #9 `mirror_clear_value'.

Compose-on-7 with the unbound-marker pointer passed as the new slot
value.  Returns i64 1 on hit / 0 on miss; production callers ignore
the return code (= `makunbound' is silent on absent symbols).")

(provide 'nelisp-cc-mirror-clear-value)

;;; nelisp-cc-mirror-clear-value.el ends here
