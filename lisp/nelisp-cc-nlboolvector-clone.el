;;; nelisp-cc-nlboolvector-clone.el --- Doc 124 §124.M NlBoolVector Clone elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.M — mechanical port of §124.B's NlVector Clone kernel
;; to NlBoolVector.  Identical shape modulo the type; the
;; REFCOUNT_OFFSET literal is the same (= 24) because `Vec<bool>' and
;; `Vec<Sexp>' share the same 24-byte Vec header (ptr + len + cap
;; triple — Rust's `Vec<T>' header size is independent of `T').
;;
;; `NlBoolVector' layout (= `build-tool/src/eval/nlboolvector.rs',
;; `#[repr(C)]'):
;;
;;   offset 0:  `value: Vec<bool>'       (= 24 bytes — ptr + len + cap triple)
;;   offset 24: `refcount: AtomicUsize'  (= 8 bytes — i64 slot)
;;
;; The `offset_of!(NlBoolVector, refcount) == size_of::<Vec<bool>>()'
;; compile-time assert at `nlboolvector.rs' pins the 24-byte trailer.
;;
;; Function contract (mirrors §124.B):
;;   box-ptr: raw `*const NlBoolVector' coerced to i64.
;;   returns: `box-ptr' unchanged.
;;
;; The body is identical to §124.B modulo the type:
;;
;;   (nelisp_nlboolvector_clone_prog2
;;     (atomic-fetch-add (+ box-ptr 24) 1)
;;     box-ptr)
;;
;; Ordering: SeqCst (= §122.E `atomic-fetch-add' contract) ⊃ Relaxed
;; (= the Rust `impl Clone for NlBoolVectorRef' body uses
;; `Ordering::Relaxed' for the +1).  Stronger ordering is always
;; valid for an inc-only operation.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' manifest entry +
;; `build-tool/build.rs' rerun-if-changed entry + Rust extern decl in
;; `build-tool/src/lib.rs::elisp_cc_spike' + safe wrapper +
;; `impl Clone for NlBoolVectorRef' dispatch swap in `nlboolvector.rs'.

;;; Code:

(defconst nelisp-cc-nlboolvector-clone--source
  '(seq
    ;; Side-effect sequencer — identical to §124.A/B pattern.
    (defun nelisp_nlboolvector_clone_prog2 (_eff val) val)

    ;; Public entry — bump refcount at offset 24 + return input pointer.
    (defun nelisp_nlboolvector_clone (box-ptr)
      (nelisp_nlboolvector_clone_prog2
       (atomic-fetch-add (+ box-ptr 24) 1)
       box-ptr)))
  "AOT source for the Doc 124 §124.M NlBoolVector Clone kernel.

Mechanical port of §124.B (NlVector) modulo the type name.
REFCOUNT_OFFSET = 24 for `NlBoolVector' because `Vec<bool>' has the
same 24-byte header as `Vec<Sexp>' — see `nlboolvector.rs' `#[repr(C)]'
struct + compile-time `offset_of!' asserts.

`atomic-fetch-add' lowers to `call nl_atomic_fetch_add@PLT' (=
SeqCst i64 fetch_add wrapper).  rax on return holds `box-ptr',
matching the trait impl's `Self { ptr, _marker }' construction
(= `NlBoolVectorRef' is `#[repr(transparent)]' over
`NonNull<NlBoolVector>').")

(provide 'nelisp-cc-nlboolvector-clone)

;;; nelisp-cc-nlboolvector-clone.el ends here
