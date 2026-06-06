;;; nelisp-cc-nlvector-clone.el --- Doc 124 §124.B NlVector Clone elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.B — mechanical port of §124.A's NlConsBox Clone kernel
;; to NlVector.  Identical shape modulo the REFCOUNT_OFFSET literal:
;;
;;   §124.A NlConsBox: REFCOUNT_OFFSET = 64 (= 2 * size_of::<Sexp>())
;;   §124.B NlVector:  REFCOUNT_OFFSET = 24 (= size_of::<Vec<Sexp>>())
;;
;; `NlVector' layout (= `build-tool/src/eval/nlvector.rs:43-49',
;; `#[repr(C)]'):
;;
;;   offset 0:  `value: Vec<Sexp>'   (= 24 bytes — ptr + cap + len triple)
;;   offset 24: `refcount: AtomicUsize'  (= 8 bytes — i64 slot)
;;
;; The `offset_of!(NlVector, refcount) == size_of::<Vec<Sexp>>()'
;; compile-time assert at `nlvector.rs:233' pins the 24-byte trailer.
;;
;; Function contract (mirrors §124.A):
;;   box-ptr: raw `*const NlVector' coerced to i64.
;;   returns: `box-ptr' unchanged.
;;
;; The body is identical to §124.A modulo the offset literal:
;;
;;   (nelisp_nlvector_clone_prog2
;;     (atomic-fetch-add (+ box-ptr 24) 1)
;;     box-ptr)
;;
;; Ordering: SeqCst (= §122.E `atomic-fetch-add' contract) ⊃ Relaxed
;; (= the Rust `impl Clone for NlVectorRef' body at `nlvector.rs:191'
;; uses `Ordering::Relaxed' for the +1).  Stronger ordering is always
;; valid for an inc-only operation.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' manifest entry +
;; `build-tool/build.rs' source-list entry + Rust extern decl in
;; `build-tool/src/lib.rs::elisp_cc_spike' + safe wrapper +
;; `build-tool/tests/elisp_cc_nlvector_clone_probe.rs' probe.
;;
;; Stage scope: §124.B ships *only* the elisp kernel + wiring + probe.
;; The Rust `impl Clone for NlVectorRef' body in `nlvector.rs:187-198'
;; continues to drive Clone semantics — the dispatch swap lands in
;; §124.F once all 5 PoCs (§124.A NlConsBox + §124.B-E NlVector /
;; NlCell / NlRecord / NlStr) are green.

;;; Code:

(defconst nelisp-cc-nlvector-clone--source
  '(seq
    ;; Side-effect sequencer — identical to §124.A pattern.
    (defun nelisp_nlvector_clone_prog2 (_eff val) val)

    ;; Public entry — bump refcount at offset 24 + return input pointer.
    (defun nelisp_nlvector_clone (box-ptr)
      (nelisp_nlvector_clone_prog2
       (atomic-fetch-add (+ box-ptr 24) 1)
       box-ptr)))
  "AOT source for the Doc 124 §124.B NlVector Clone kernel.

Mechanical port of §124.A modulo the REFCOUNT_OFFSET literal (= 24
for `NlVector', which puts an `AtomicUsize' immediately after the
24-byte `Vec<Sexp>' header — see `nlvector.rs:43-49' `#[repr(C)]'
struct + the `offset_of!(NlVector, refcount) == size_of::<Vec<Sexp>>()'
compile-time assert at `nlvector.rs:233').

`atomic-fetch-add' lowers to `call nl_atomic_fetch_add@PLT' (=
SeqCst i64 fetch_add wrapper in `raw_mem.rs:75').  rax on return
holds `box-ptr', matching the trait impl's `Self { ptr, _marker }'
construction (= ABI no-op because `NlVectorRef' is
`#[repr(transparent)]' over `NonNull<NlVector>').")

(provide 'nelisp-cc-nlvector-clone)

;;; nelisp-cc-nlvector-clone.el ends here
