;;; nelisp-cc-cell-ops.el --- Doc 111 §111.D Cell read+write op probes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.D introduces four Phase 47 grammar ops on
;; `Sexp::Cell(_)' / `NlCell':
;;
;;   (cell-value H SLOT)     — copy NlCell.value into caller-owned SLOT.
;;   (cell-set-value H VAL)  — refcount-aware overwrite via the Rust
;;                             extern `nl_cell_set_value'.
;;   (cell-make VAL SLOT)    — allocate a fresh NlCell holding `*VAL'
;;                             and write `Sexp::Cell(box)' into SLOT.
;;                             Uses the Rust extern `nl_alloc_cell'.
;;   (cell-null-p H)         — predicate: 1 iff NlCell.value's tag is
;;                             `Sexp::Nil', else 0.
;;
;; This file packages each op as a standalone Phase 47-compiled
;; `defun' so the `tests/phase47_cell.rs' integration test can probe
;; the round-trip end-to-end without any user-visible swap (Doc 111
;; §3.D explicitly defers swap targets to §111.E env_lexframe).
;;
;; The four `defun's match the extern declarations in
;; `build-tool/src/lib.rs::elisp_cc_spike' and the manifest entries in
;; `scripts/compile-elisp-objects.el'.

;;; Code:

(defconst nelisp-cc-cell-ops--value-source
  '(defun nelisp_cell_value (arg0 result-slot)
     ;; arg0:        *const Sexp pointing at Sexp::Cell(_).
     ;; result-slot: *mut Sexp 32-byte slot for the read value.
     ;;
     ;; MVP refcount note (= mirrors `cons-car' / `cons-cdr' for
     ;; Doc 101 §101.B): the inline 32-byte copy does not bump
     ;; refcounts on nested boxed payloads.  Safe for the §111.E
     ;; env_lexframe use case where the cell outlives the SLOT.
     (cell-value arg0 result-slot))
  "Phase 47 source for the Doc 111 §111.D `cell-value' op.")

(defconst nelisp-cc-cell-ops--set-value-source
  '(defun nelisp_cell_set_value (arg0 val-ptr)
     ;; arg0:    *const Sexp pointing at Sexp::Cell(_).
     ;; val-ptr: *const Sexp pointing at the new value.
     ;;
     ;; Delegates to the Rust `nl_cell_set_value' extern which calls
     ;; `NlCellRef::set_value' (drop-then-write with refcount-aware
     ;; semantics via `Sexp::Drop' + `Sexp::Clone').
     (cell-set-value arg0 val-ptr))
  "Phase 47 source for the Doc 111 §111.D `cell-set-value' op.")

(defconst nelisp-cc-cell-ops--make-source
  '(defun nelisp_cell_make (val-ptr result-slot)
     ;; val-ptr:     *const Sexp — initial value for the new cell.
     ;; result-slot: *mut Sexp 32-byte slot, will hold Sexp::Cell(_).
     ;;
     ;; Calls Rust `nl_alloc_cell(val-ptr)' which refcount-aware-
     ;; clones `*val-ptr' into a new `NlCell { value, refcount = 1 }'
     ;; and returns the raw `*mut NlCell'.
     (cell-make val-ptr result-slot))
  "Phase 47 source for the Doc 111 §111.D `cell-make' op.")

(defconst nelisp-cc-cell-ops--null-p-source
  '(defun nelisp_cell_null_p (arg0)
     ;; arg0: *const Sexp pointing at Sexp::Cell(_).
     ;; Returns i64 1 iff the cell's `value' field currently holds
     ;; `Sexp::Nil', else 0.
     (cell-null-p arg0))
  "Phase 47 source for the Doc 111 §111.D `cell-null-p' op.")

(provide 'nelisp-cc-cell-ops)

;;; nelisp-cc-cell-ops.el ends here
