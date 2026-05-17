;;; nelisp-cc-nlcell-clone.el --- Doc 124 §124.C NlCell Clone elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.C — mechanical port of §124.A's NlConsBox Clone kernel
;; to NlCell.  Identical shape modulo the REFCOUNT_OFFSET literal:
;;
;;   §124.A NlConsBox: REFCOUNT_OFFSET = 64 (= 2 * size_of::<Sexp>())
;;   §124.C NlCell:    REFCOUNT_OFFSET = 32 (= 1 * size_of::<Sexp>())
;;
;; `NlCell' layout (= `build-tool/src/eval/nlcell.rs:56-64',
;; `#[repr(C)]'):
;;
;;   offset 0:  `value: Sexp'        (= 32 bytes — single tagged slot)
;;   offset 32: `refcount: AtomicUsize'  (= 8 bytes — i64 slot)
;;
;; The single-`Sexp' shape (one slot instead of NlConsBox's two)
;; halves the offset literal but leaves the kernel body otherwise
;; identical.  Layout pinned by `offset_of!(NlCell, refcount) ==
;; size_of::<Sexp>()' at `nlcell.rs:285' (assert in cfg(test)).
;;
;; Function contract (mirrors §124.A):
;;   box-ptr: raw `*const NlCell' coerced to i64.
;;   returns: `box-ptr' unchanged.
;;
;; The body is identical to §124.A modulo the offset literal:
;;
;;   (nelisp_nlcell_clone_prog2
;;     (atomic-fetch-add (+ box-ptr 32) 1)
;;     box-ptr)
;;
;; Ordering: SeqCst (= §122.E `atomic-fetch-add' contract) ⊃ Relaxed
;; (= the Rust `impl Clone for NlCellRef' body at `nlcell.rs:230'
;; uses `Ordering::Relaxed').
;;
;; Build wiring: same trio of files as §124.A/B.

;;; Code:

(defconst nelisp-cc-nlcell-clone--source
  '(seq
    ;; Side-effect sequencer — identical to §124.A pattern.
    (defun nelisp_nlcell_clone_prog2 (_eff val) val)

    ;; Public entry — bump refcount at offset 32 + return input pointer.
    (defun nelisp_nlcell_clone (box-ptr)
      (nelisp_nlcell_clone_prog2
       (atomic-fetch-add (+ box-ptr 32) 1)
       box-ptr)))
  "Phase 47 source for the Doc 124 §124.C NlCell Clone kernel.

Mechanical port of §124.A modulo the REFCOUNT_OFFSET literal (= 32
for `NlCell', which puts an `AtomicUsize' immediately after the
single 32-byte `Sexp' value slot — see `nlcell.rs:56-64' `#[repr(C)]'
struct + the `offset_of!(NlCell, refcount) == size_of::<Sexp>()'
assertion at `nlcell.rs:285').

`atomic-fetch-add' lowers to `call nl_atomic_fetch_add@PLT'.  rax on
return holds `box-ptr', matching the trait impl's `Self { ptr,
_marker }' construction (= ABI no-op because `NlCellRef' is
`#[repr(transparent)]' over `NonNull<NlCell>').")

(provide 'nelisp-cc-nlcell-clone)

;;; nelisp-cc-nlcell-clone.el ends here
