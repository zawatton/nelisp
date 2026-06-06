;;; nelisp-cc-jit-access-aref-bool-vector-inner.el --- §120.D aref BoolVector sub-arm  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.D — AOT-compiled replacement for the Rust
;; `nl_jit_access_aref_bool_vector_inner' narrow helper in
;; `build-tool/src/jit/access.rs'.
;;
;; Function contract (matches the Rust `#[no_mangle] pub unsafe extern "C"'):
;;   arg: *const Sexp::BoolVector  — caller has confirmed tag == 10.
;;   idx: i64                      — element index (caller guards >= 0).
;;   out: *mut Sexp                — output slot, pre-initialised to Nil.
;;   returns: i64 = 0 (TRAMPOLINE_OK) or 1 (TRAMPOLINE_ERR).
;;
;; Implementation path:
;;
;;   1. idx < 0 guard -> ERR (defensive; the `nelisp_jit_aref' body
;;      in `nelisp-cc-jit-aref.el' already pre-checks before calling).
;;
;;   2. Read NlBoolVector* from the Sexp payload at offset 8 via
;;      `sexp-payload-ptr'.
;;
;;   3. Bounds check: read Vec<bool>.length at offset 16 of the
;;      NlBoolVector box via `ptr-read-u64 payload 16'.
;;      OOR -> ERR.
;;
;;   4. Read the bool byte: Vec<bool>.ptr lives at offset 8 of the
;;      NlBoolVector box.  Each element is 1 byte (Rust std layout
;;      for Vec<bool>).  `ptr-read-u8 data-ptr idx' -> 0 (false) or
;;      1 (true).
;;
;;   5. Write `Sexp::Nil' (tag=0) or `Sexp::T' (tag=1) to `*out'
;;      via `sexp-write-nil' / `sexp-write-t'.  The `out' slot is
;;      pre-initialised to Nil (= no refcount drop needed on write).
;;
;; NlBoolVector ABI (#[repr(C)], pinned by nlboolvector.rs asserts):
;;   offset  0: value.capacity (= Vec<bool> capacity field)
;;   offset  8: value.ptr      (= Vec<bool> data pointer)
;;   offset 16: value.length   (= Vec<bool> element count)
;;   offset 24: refcount AtomicUsize
;;
;; Tag constants: 10 = `nelisp-sexp--tag-bool-vector', 0 = SEXP_TAG_NIL,
;; 1 = SEXP_TAG_T (from `build-tool/src/eval/sexp.rs').
;;
;; The sub-arm factoring via `nl_jit_access_aref_bv_do' avoids
;; evaluating `sexp-payload-ptr' twice by receiving the NlBoolVector*
;; as a parameter -- AOT `let' supports compile-time constants only.

;;; Code:

(defconst nelisp-cc-jit-access-aref-bool-vector-inner--source
  '(seq
    (defun nl_jit_access_aref_bv_do (payload idx out)
      ;; payload: NlBoolVector* (raw i64).  idx: i64.  out: *mut Sexp.
      ;; NlBoolVector.value.length at offset 16.
      ;; NlBoolVector.value.ptr    at offset 8.
      (if (< idx (ptr-read-u64 payload 16))
          (if (= (ptr-read-u8 (ptr-read-u64 payload 8) idx) 0)
              (and (sexp-write-nil out) 0)
            (and (sexp-write-t out) 0))
        1))
    (defun nl_jit_access_aref_bool_vector_inner (arg idx out)
      ;; arg: *const Sexp::BoolVector.  idx: i64.  out: *mut Sexp.
      ;; Returns: i64 = 0 OK, 1 ERR.
      (if (< idx 0)
          1
        (nl_jit_access_aref_bv_do (sexp-payload-ptr arg) idx out))))
  "AOT source for the §120.D `nl_jit_access_aref_bool_vector_inner' swap.

Two-function package (= `seq' form):

  `nl_jit_access_aref_bv_do' -- inner body that accepts the already-
  dereferenced NlBoolVector* payload pointer.  Performs the bounds
  check against Vec<bool>.length (offset 16) and the `ptr-read-u8'
  data read against Vec<bool>.ptr (offset 8), then writes Nil or T
  to *out via the `sexp-write-nil' / `sexp-write-t' grammar ops.

  `nl_jit_access_aref_bool_vector_inner' -- ABI entry point matching
  the deleted Rust `#[no_mangle]' body.  Guards idx >= 0, then
  delegates to the inner helper with `sexp-payload-ptr arg' to
  obtain NlBoolVector* in one op.

The factoring avoids a double `sexp-payload-ptr' call (= AOT
`let' is compile-time constant only) at the cost of one call frame.
ABI: `(*const Sexp, i64, *mut Sexp) -> i64' matches the former
Rust extern and the `extern-call' invocation in
`lisp/nelisp-cc-jit-aref.el' §120.D BoolVector arm.")

(provide 'nelisp-cc-jit-access-aref-bool-vector-inner)

;;; nelisp-cc-jit-access-aref-bool-vector-inner.el ends here
