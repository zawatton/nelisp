;;; nelisp-cc-nlchartable-clone.el --- Doc 124 §124.L+ NlCharTable Clone elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.L+ — mechanical port of §124.A's NlConsBox Clone kernel
;; to NlCharTable.  Identical shape modulo the REFCOUNT_OFFSET literal:
;;
;;   §124.A NlConsBox:  REFCOUNT_OFFSET = 64  (= 2 * size_of::<Sexp>())
;;   §124.L+ NlCharTable: REFCOUNT_OFFSET = 120 (= size_of::<CharTableInner>())
;;
;; `NlCharTable' layout (= `build-tool/src/eval/nlchartable.rs:8-12',
;; `#[repr(C)]'):
;;
;;   offset 0:   `inner: CharTableInner'   (= 120 bytes)
;;   offset 120: `refcount: AtomicUsize'   (= 8 bytes — i64 slot)
;;
;; `CharTableInner' (= `build-tool/src/eval/sexp.rs', `#[repr(C)]'):
;;
;;   subtype     : Sexp                     (= 32 bytes)
;;   default_val : Sexp                     (= 32 bytes)
;;   entries     : Vec<(i64, Sexp)>         (= 24 bytes — Vec header)
;;   parent      : Option<NlCharTableRef>   (=  8 bytes — niche of NonNull)
;;   extra       : Vec<Sexp>                (= 24 bytes — Vec header)
;;   total = 32 + 32 + 24 + 8 + 24 = 120 bytes
;;
;; Layout pinned by `offset_of!(NlCharTable, refcount) ==
;; size_of::<CharTableInner>()' at `nlchartable.rs:75'.
;;
;; Function contract (mirrors §124.A):
;;   box-ptr: raw `*const NlCharTable' coerced to i64.
;;   returns: `box-ptr' unchanged.
;;
;; The body is identical to §124.A modulo the offset literal:
;;
;;   (nelisp_nlchartable_clone_prog2
;;     (atomic-fetch-add (+ box-ptr 120) 1)
;;     box-ptr)
;;
;; Ordering: SeqCst (= §122.E `atomic-fetch-add' contract) ⊃ Relaxed
;; (= the Rust `impl Clone for NlCharTableRef' body at `nlchartable.rs:42'
;; uses `Ordering::Relaxed').
;;
;; Build wiring: same trio of files as §124.A-E.

;;; Code:

(defconst nelisp-cc-nlchartable-clone--source
  '(seq
    ;; Side-effect sequencer — identical to §124.A pattern.
    (defun nelisp_nlchartable_clone_prog2 (_eff val) val)

    ;; Public entry — bump refcount at offset 120 + return input pointer.
    (defun nelisp_nlchartable_clone (box-ptr)
      (nelisp_nlchartable_clone_prog2
       (atomic-fetch-add (+ box-ptr 120) 1)
       box-ptr)))
  "Phase 47 source for the Doc 124 §124.L+ NlCharTable Clone kernel.

Mechanical port of §124.A modulo the REFCOUNT_OFFSET literal (= 120
for `NlCharTable', which trails the 120-byte `CharTableInner' field
— see `nlchartable.rs:8-12' `#[repr(C)]' struct + the
`offset_of!(NlCharTable, refcount) == size_of::<CharTableInner>()'
assertion at `nlchartable.rs:75').

= 32 (Sexp subtype) + 32 (Sexp default_val) + 24 (Vec header entries)
+ 8 (Option<NlCharTableRef> parent) + 24 (Vec header extra) = 120 bytes.

`atomic-fetch-add' lowers to `call nl_atomic_fetch_add@PLT'.  rax on
return holds `box-ptr', matching the trait impl's `Self { ptr,
_marker }' construction (= ABI no-op because `NlCharTableRef' is
`#[repr(transparent)]' over `NonNull<NlCharTable>').")

(provide 'nelisp-cc-nlchartable-clone)

;;; nelisp-cc-nlchartable-clone.el ends here
