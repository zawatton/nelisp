;;; nelisp-cc-nlcell-clone.el --- Doc 124 §124.C NlCell Clone elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.C — mechanical port of §124.A's NlConsBox Clone kernel
;; to NlCell.  Doc 147 Phase 1 shrinks NlCell, so REFCOUNT_OFFSET = 8:
;;
;;   §124.A NlConsBox: REFCOUNT_OFFSET = 64 (= 2 * size_of::<Sexp>())
;;   §124.C NlCell:    REFCOUNT_OFFSET = 8  (Doc 147: value WORD@0)
;;
;; `NlCell' layout (Doc 147 Phase 1 — container slot shrink):
;;
;;   offset 0:  `value' WORD        (= 8 bytes — tagged: imm or box ptr)
;;   offset 8:  `refcount: AtomicUsize'  (= 8 bytes — i64 slot)
;;   total = 16 bytes, align = 8
;;
;; The value field is now an 8-byte tagged WORD (was a 32-byte inline
;; Sexp), so the refcount trailer sits at offset 8 (was 32).
;;
;; Function contract (mirrors §124.A):
;;   box-ptr: raw `*const NlCell' coerced to i64.
;;   returns: `box-ptr' unchanged.
;;
;; The body is identical to §124.A modulo the offset literal:
;;
;;   (nelisp_nlcell_clone_prog2
;;     (atomic-fetch-add (+ box-ptr 8) 1)
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

    ;; Public entry — bump refcount at offset 8 + return input pointer.
    ;; Doc 147 Phase 1: NlCell shrank to value WORD@0 + rc@8 (size 16),
    ;; so the refcount trailer moved from offset 32 to offset 8.
    (defun nelisp_nlcell_clone (box-ptr)
      (nelisp_nlcell_clone_prog2
       (atomic-fetch-add (+ box-ptr 8) 1)
       box-ptr)))
  "AOT source for the Doc 124 §124.C NlCell Clone kernel.

Mechanical port of §124.A modulo the REFCOUNT_OFFSET literal (= 8
for `NlCell' after the Doc 147 Phase 1 shrink, which puts an
`AtomicUsize' immediately after the single 8-byte tagged value WORD,
giving the 16-byte NlCell layout value-WORD@0 + refcount@8).

`atomic-fetch-add' lowers to `call nl_atomic_fetch_add@PLT'.  rax on
return holds `box-ptr', matching the trait impl's `Self { ptr,
_marker }' construction (= ABI no-op because `NlCellRef' is
`#[repr(transparent)]' over `NonNull<NlCell>').")

(provide 'nelisp-cc-nlcell-clone)

;;; nelisp-cc-nlcell-clone.el ends here
