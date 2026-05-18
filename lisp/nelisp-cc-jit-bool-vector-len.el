;;; nelisp-cc-jit-bool-vector-len.el --- Doc 122 §122.B bool-vector-len trampoline  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.B / Doc 120 §120.B — Phase-47-compiled replacement for
;; the Rust `nl_jit_bool_vector_len' trampoline in
;; `build-tool/src/jit/box_accessor.rs'.  Same
;; `(*const Sexp, *mut Sexp) -> i64' contract:
;;
;;   1. Non-BoolVector tag → TRAMPOLINE_ERR (= 1).
;;   2. BoolVector arm: read the NlBoolVector* box pointer from the Sexp
;;      payload at offset 8, then read `Vec<bool>.length' (= the
;;      element count) at offset 16 from the NlBoolVector header, write
;;      `Sexp::Int(len)' to `*out', → TRAMPOLINE_OK (= 0).
;;
;; ABI layout (pinned by compile-time asserts in nlboolvector.rs):
;;
;;   Sexp::BoolVector slot layout:
;;     offset 0:  tag byte (= 10)
;;     offset 8:  NonNull<NlBoolVector> box pointer
;;
;;   NlBoolVector layout (#[repr(C)]):
;;     offset 0:  value.ptr      (= Vec<bool> data pointer)
;;     offset 8:  value.capacity (= Vec<bool> capacity)
;;     offset 16: value.length   (= Vec<bool> element count)
;;     offset 24: refcount AtomicUsize
;;
;; The `nelisp-nlvector--offset-value-length = 16' constant in
;; `lisp/nelisp-sexp-layout.el' pins offset 16 as `Vec<T>.length'
;; for all NlBox types (NlVector, NlBoolVector, NlRecord share the
;; same Vec header layout independent of the element type T).
;;
;; Grammar ops used:
;;   `sexp-tag'     — read the u8 discriminant at offset 0 of a Sexp slot.
;;   `ptr-read-u64' — load a u64 from (base + offset).
;;   `sexp-int-make' — write `Sexp::Int(n)' into `*mut Sexp' out slot.
;;
;; Tag-byte constant: 10 = `nelisp-sexp--tag-bool-vector' (sexp-layout.el).

;;; Code:

(defconst nelisp-cc-jit-bool-vector-len--source
  '(defun nl_jit_bool_vector_len (arg out)
     ;; arg: *const Sexp.  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK, 1 on ERR.
     ;;
     ;; ABI: Sexp payload at offset 8 = NlBoolVector*.
     ;;      NlBoolVector.value.length at NlBoolVector* + 16.
     ;;      Use ptr-read-u64 twice: first to dereference the box
     ;;      pointer, second to load Vec<bool>.length.
     (if (= (sexp-tag arg) 10)
         (and (sexp-int-make out (ptr-read-u64 (ptr-read-u64 arg 8) 16)) 0)
       1))
  "Phase 47 source for the §120.B `nl_jit_bool_vector_len' swap.

Single guard arm: BoolVector tag-byte check (= 10) + double
`ptr-read-u64' dereference to load Vec<bool>.length from the
NlBoolVector box:

  1. `(ptr-read-u64 arg 8)'  — load the NonNull<NlBoolVector> box
     pointer from the Sexp::BoolVector payload at offset 8.
  2. `(ptr-read-u64 BOX 16)' — load Vec<bool>.length from offset 16
     of the NlBoolVector struct (= `nelisp-nlvector--offset-value-length'
     constant, shared by all NlBox Vec<T> headers).
  3. `(sexp-int-make out N)'  — write Sexp::Int(N) to *out.
  4. `(and SIDE-EFFECT 0)'    — return 0 (TRAMPOLINE_OK) in rax.

Non-BoolVector tag → ERR = 1; strategy.el's condition-case
dispatcher signals wrong-type-argument via `nelisp--bool-vector-len'.")

(provide 'nelisp-cc-jit-bool-vector-len)

;;; nelisp-cc-jit-bool-vector-len.el ends here
