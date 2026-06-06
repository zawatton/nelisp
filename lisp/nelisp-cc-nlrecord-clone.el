;;; nelisp-cc-nlrecord-clone.el --- Doc 124 §124.D NlRecord Clone elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.D — mechanical port of §124.A's NlConsBox Clone kernel
;; to NlRecord.  Identical shape modulo the REFCOUNT_OFFSET literal:
;;
;;   §124.A NlConsBox: REFCOUNT_OFFSET = 64 (= 2 * size_of::<Sexp>())
;;   §124.D NlRecord:  REFCOUNT_OFFSET = 56 (= size_of::<Sexp>()
;;                                            + size_of::<Vec<Sexp>>())
;;
;; `NlRecord' layout (= `build-tool/src/eval/nlrecord.rs:50-58',
;; `#[repr(C)]'):
;;
;;   offset 0:  `type_tag: Sexp'       (= 32 bytes — single tagged slot)
;;   offset 32: `slots: Vec<Sexp>'     (= 24 bytes — ptr + cap + len triple)
;;   offset 56: `refcount: AtomicUsize' (= 8 bytes — i64 slot)
;;
;; Note: Doc 124 §1's audit table listed NlRecord at 24, but the
;; Rust source at `nlrecord.rs:248-253' shows the actual layout has
;; the refcount trailing *both* the `type_tag: Sexp' and the
;; `slots: Vec<Sexp>' fields:
;;
;;   assert!(
;;     offset_of!(NlRecord, refcount) == size_of::<Sexp>() + size_of::<Vec<Sexp>>()
;;   );
;;
;; = 32 + 24 = 56 bytes.  Layout-pinned by `#[repr(C)]' linear
;; ordering — this kernel uses the byte-accurate offset rather than
;; the audit table's shorthand value.
;;
;; Function contract (mirrors §124.A):
;;   box-ptr: raw `*const NlRecord' coerced to i64.
;;   returns: `box-ptr' unchanged.
;;
;; The body is identical to §124.A modulo the offset literal:
;;
;;   (nelisp_nlrecord_clone_prog2
;;     (atomic-fetch-add (+ box-ptr 56) 1)
;;     box-ptr)
;;
;; Ordering: SeqCst (= §122.E `atomic-fetch-add' contract) ⊃ Relaxed
;; (= the Rust `impl Clone for NlRecordRef' body at `nlrecord.rs:205'
;; uses `Ordering::Relaxed').
;;
;; Build wiring: same trio of files as §124.A-C.

;;; Code:

(defconst nelisp-cc-nlrecord-clone--source
  '(seq
    ;; Side-effect sequencer — identical to §124.A pattern.
    (defun nelisp_nlrecord_clone_prog2 (_eff val) val)

    ;; Public entry — bump refcount at offset 56 + return input pointer.
    (defun nelisp_nlrecord_clone (box-ptr)
      (nelisp_nlrecord_clone_prog2
       (atomic-fetch-add (+ box-ptr 56) 1)
       box-ptr)))
  "AOT source for the Doc 124 §124.D NlRecord Clone kernel.

Mechanical port of §124.A modulo the REFCOUNT_OFFSET literal (= 56
for `NlRecord', which trails the `type_tag: Sexp' + `slots: Vec<Sexp>'
fields — see `nlrecord.rs:50-58' `#[repr(C)]' struct + the
`offset_of!(NlRecord, refcount) == size_of::<Sexp>() +
size_of::<Vec<Sexp>>()' assertion at `nlrecord.rs:248-253').

= 32 (Sexp) + 24 (Vec header) = 56 bytes.  The Doc 124 §1 audit
table's shorthand value of 24 referred to the Vec-header portion
only; the byte-accurate `repr(C)' offset is 56.

`atomic-fetch-add' lowers to `call nl_atomic_fetch_add@PLT'.  rax on
return holds `box-ptr', matching the trait impl's `Self { ptr,
_marker }' construction (= ABI no-op because `NlRecordRef' is
`#[repr(transparent)]' over `NonNull<NlRecord>').")

(provide 'nelisp-cc-nlrecord-clone)

;;; nelisp-cc-nlrecord-clone.el ends here
