;;; nelisp-cc-mirror-alloc-entry.el --- Doc 119 §119.A mirror_alloc_entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 119 §119.A — pure-elisp port of the Rust
;; `mirror_insert_new_entry' helper.  Allocates a fresh
;; `symbol-entry' Record (= 5 slots: value / function / plist /
;; constant / variable redirect) and refcount-safely installs the
;; caller-supplied Sexps plus a nil redirect.  The result
;; `Sexp::Record(symbol-entry)' is
;; written into RESULT-SLOT (= caller-owned `*mut Sexp', pre-set to
;; `Sexp::Nil' by the Rust safe wrapper).
;;
;; Mirrors `Env::mirror_insert_new_entry' (~12 LOC) plus the inline
;; `Sexp::record(...)' allocator call.  Used by Doc 119 §119.A's
;; four `_or_insert' wrappers (= the auto-vivify branch of
;; `mirror_set_value' / `mirror_set_function' / `mirror_install_entry'
;; / `mirror_set_constant').
;;
;; Layout (= `lisp/nelisp-env.el'):
;;   symbol-entry Sexp::Record
;;     slots[0] = Sexp::value     (= the variable value, `unbound_marker' if absent)
;;     slots[1] = Sexp::function  (= the function value, same convention)
;;     slots[2] = Sexp::plist     (= property list, Sexp::Nil if absent)
;;     slots[3] = Sexp::constant  (= Sexp::T iff symbol is constant, else Sexp::Nil)
;;
;; The 6-arg shape matches the SysV AMD64 ABI's GP-register limit
;; (rdi/rsi/rdx/rcx/r8/r9).  Outer arity is even (= 6) so body-entry
;; rsp ≡ 0 mod 16, matching the static rsp-alignment baked into
;; `record-make' / `record-slot-set' (= Doc 124.F-blocker
;; even-arity fix; see `lisp/nelisp-cc-frame-push.el' for the
;; alignment audit).
;;
;; ABI deps satisfied:
;;   §111.B  `record-slot-set'  — refcount-safe slot 0-4 install.
;;   §115.3  `record-make'      — fresh `NlRecord' allocator.

;;; Code:

(defconst nelisp-cc-mirror-alloc-entry--source
  '(defun nelisp_mirror_alloc_entry
       (tag-sym-ptr value-ptr function-ptr plist-ptr constant-ptr result-slot)
     ;; tag-sym-ptr:  *const Sexp pointing at `Sexp::Symbol("symbol-entry")'
     ;;               (= the type-tag for the freshly-allocated record;
     ;;               the Rust safe wrapper materialises this Sexp on the
     ;;               call stack and passes its address).
     ;; value-ptr:    *const Sexp — slot 0 (= the variable value cell).
     ;; function-ptr: *const Sexp — slot 1 (= the function cell).
     ;; plist-ptr:    *const Sexp — slot 2 (= the property list).
     ;; constant-ptr: *const Sexp — slot 3 (= Sexp::T / Sexp::Nil flag).
     ;; result-slot:  *mut Sexp, caller-owned Sexp::Nil — receives the
     ;;               freshly-allocated `Sexp::Record(NlRecordRef)' of
     ;;               the symbol-entry record.
     ;;
     ;; Returns: i64 — 1 on success.  All sub-ops materialise non-zero
     ;; rax sentinels so the `and' value-form chain threads through.
     ;;
     ;; Refcount discipline: `record-make' allocates with all 5 slots
     ;; pre-filled with `Sexp::Nil' (= via `nl_alloc_record').  The
     ;; subsequent five `record-slot-set' calls dispatch to
     ;; `nl_record_set_slot' which `(*val).clone()' the source Sexp
     ;; before writing (= refcount-bumps any box-tagged variant), so
     ;; the caller's stack values stay live across the call.  When the
     ;; caller's frame unwinds, each box's refcount drops back to its
     ;; pre-call value while the record retains its own ref.
     (let ((redirect-ptr (alloc-bytes 32 8)))
       (and
        ;; Slot 4 is the NeLisp variable redirect.  Nil means direct.
        (sexp-write-nil redirect-ptr)
        (record-make tag-sym-ptr 5 result-slot)
        (record-slot-set result-slot 0 value-ptr)
        (record-slot-set result-slot 1 function-ptr)
        (record-slot-set result-slot 2 plist-ptr)
        (record-slot-set result-slot 3 constant-ptr)
        (record-slot-set result-slot 4 redirect-ptr))))
  "AOT source for Doc 119 §119.A `mirror_alloc_entry'.

Allocates a fresh `symbol-entry' Record via `record-make' (§115.3)
+ five refcount-safe `record-slot-set' (§111.B) installs.  Replaces
the ~12 LOC Rust helper `Env::mirror_insert_new_entry' (= the
slot-pre-resolution + `Sexp::record(...)' allocator branch).
The bucket-prepend follow-up lives in `nelisp-cc-mirror-bucket-prepend.el'.")

(provide 'nelisp-cc-mirror-alloc-entry)

;;; nelisp-cc-mirror-alloc-entry.el ends here
