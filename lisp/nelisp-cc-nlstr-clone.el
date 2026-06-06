;;; nelisp-cc-nlstr-clone.el --- Doc 124 §124.E NlStr Clone elisp kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 124 §124.E — mechanical port of §124.A's NlConsBox Clone kernel
;; to NlStr.  Identical shape modulo the REFCOUNT_OFFSET literal:
;;
;;   §124.A NlConsBox: REFCOUNT_OFFSET = 64 (= 2 * size_of::<Sexp>())
;;   §124.E NlStr:     REFCOUNT_OFFSET = 24 (= size_of::<String>())
;;
;; `NlStr' layout (= `build-tool/src/eval/nlstr.rs:58-68', `#[repr(C)]'):
;;
;;   offset 0:  `value: String'         (= 24 bytes — ptr + cap + len triple)
;;   offset 24: `refcount: AtomicUsize'  (= 8 bytes — i64 slot)
;;
;; `String' is structurally identical to `Vec<u8>' (= 24-byte header)
;; so the offset matches §124.B's NlVector.  Layout pinned by
;; `offset_of!(NlStr, refcount) == size_of::<String>()' at
;; `nlstr.rs:686'.
;;
;; Function contract (mirrors §124.A):
;;   box-ptr: raw `*const NlStr' coerced to i64.
;;   returns: `box-ptr' unchanged.
;;
;; The body is identical to §124.A modulo the offset literal:
;;
;;   (nelisp_nlstr_clone_prog2
;;     (atomic-fetch-add (+ box-ptr 24) 1)
;;     box-ptr)
;;
;; Ordering: SeqCst (= §122.E `atomic-fetch-add' contract) ⊃ Relaxed
;; (= the Rust `impl Clone for NlStrRef' body at `nlstr.rs:180' uses
;; `Ordering::Relaxed').
;;
;; Build wiring: same trio of files as §124.A-D.

;;; Code:

(defconst nelisp-cc-nlstr-clone--source
  '(seq
    ;; Side-effect sequencer — identical to §124.A pattern.
    (defun nelisp_nlstr_clone_prog2 (_eff val) val)

    ;; Public entry — bump refcount at offset 24 + return input pointer.
    (defun nelisp_nlstr_clone (box-ptr)
      (nelisp_nlstr_clone_prog2
       (atomic-fetch-add (+ box-ptr 24) 1)
       box-ptr)))
  "AOT source for the Doc 124 §124.E NlStr Clone kernel.

Mechanical port of §124.A modulo the REFCOUNT_OFFSET literal (= 24
for `NlStr', which puts an `AtomicUsize' immediately after the
24-byte `String' header — see `nlstr.rs:58-68' `#[repr(C)]' struct
+ the `offset_of!(NlStr, refcount) == size_of::<String>()' assertion
at `nlstr.rs:686').

`String' is `#[repr(Rust)]' but its 24-byte size is stable across
all supported targets (ptr + cap + len triple, identical to
`Vec<u8>').  This matches §124.B NlVector's 24-byte offset modulo
the value-slot type.

`atomic-fetch-add' lowers to `call nl_atomic_fetch_add@PLT'.  rax on
return holds `box-ptr', matching the trait impl's `Self { ptr,
_marker }' construction (= ABI no-op because `NlStrRef' is
`#[repr(transparent)]' over `NonNull<NlStr>').")

(provide 'nelisp-cc-nlstr-clone)

;;; nelisp-cc-nlstr-clone.el ends here
