;;; nelisp-cc-jit-access-aset-bool-vector-inner.el --- §120.D aset BoolVector sub-arm  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.D — AOT-compiled replacement for the Rust
;; `nl_jit_access_aset_bool_vector_inner' narrow helper in
;; `build-tool/src/jit/access.rs'.
;;
;; Function contract (matches the Rust `#[no_mangle] pub unsafe extern "C"'):
;;   arg: *const Sexp::BoolVector  — caller has confirmed tag == 10.
;;   idx: i64                      — element index (caller guards >= 0).
;;   val: *const Sexp              — new value; truthiness determines the bit.
;;   out: *mut Sexp                — output slot, pre-initialised to Nil.
;;   returns: i64 = 0 (TRAMPOLINE_OK) or 1 (TRAMPOLINE_ERR).
;;
;; Implementation path:
;;
;;   1. idx < 0 guard → ERR.
;;
;;   2. Read NlBoolVector* via `sexp-payload-ptr arg'.
;;
;;   3. Bounds check: `ptr-read-u64 payload 16' = Vec<bool>.length.
;;      OOR → ERR.
;;
;;   4. Compute the bool bit: `(if (= (sexp-tag val) 0) 0 1)'.
;;      Matches Rust's `is_truthy' = `!matches!(v, Sexp::Nil)'.
;;
;;   5. Write the bit: `ptr-write-u8 data-ptr idx bit' where
;;      data-ptr = `ptr-read-u64 payload 8' (= Vec<bool>.ptr,
;;      offset 8 of NlBoolVector).
;;
;;   6. Clone val into *out via `extern-call nl_sexp_clone_into val out'
;;      — same refcount-safe copy used by the Vector arm in
;;      `lisp/nelisp-cc-jit-aset.el'.
;;
;; NlBoolVector ABI (#[repr(C)], pinned by nlboolvector.rs asserts):
;;   offset  0: value.capacity (= Vec<bool> capacity field)
;;   offset  8: value.ptr      (= Vec<bool> data pointer)
;;   offset 16: value.length   (= Vec<bool> element count)
;;   offset 24: refcount AtomicUsize
;;
;; The inner helper factoring avoids a double `sexp-payload-ptr' call
;; — AOT `let' supports compile-time constants only.

;;; Code:

(defconst nelisp-cc-jit-access-aset-bool-vector-inner--source
  '(seq
    (defun nl_jit_access_aset_bv_bitval (off)
      (cond ((= off 0) 1) ((= off 1) 2) ((= off 2) 4) ((= off 3) 8)
            ((= off 4) 16) ((= off 5) 32) ((= off 6) 64) (t 128)))
    (defun nl_jit_access_aset_bv_do (payload idx val out)
      ;; payload: NlBoolVector* (raw i64).  idx: i64.
      ;; val: *const Sexp.  out: *mut Sexp.
      ;; Pure-Elisp box bit length is at offset 0 and packed data at +8.
      (if (< idx (ptr-read-u64 payload 0))
          (and
           (ptr-write-u8
            (ptr-read-u64 payload 8)
            (/ idx 8)
            (if (= (sexp-tag val) 0)
                (logand (ptr-read-u8 (ptr-read-u64 payload 8) (/ idx 8))
                        (- 255 (nl_jit_access_aset_bv_bitval (mod idx 8))))
              (logior (ptr-read-u8 (ptr-read-u64 payload 8) (/ idx 8))
                      (nl_jit_access_aset_bv_bitval (mod idx 8)))))
           (extern-call nl_sexp_clone_into val out)
           0)
        1))
    (defun nl_jit_access_aset_bool_vector_inner (arg idx val out)
      ;; arg: *const Sexp::BoolVector.  idx: i64.
      ;; val: *const Sexp.  out: *mut Sexp.
      ;; Returns: i64 = 0 OK, 1 ERR.
      (if (< idx 0)
          1
        (nl_jit_access_aset_bv_do (sexp-payload-ptr arg) idx val out))))
  "AOT source for the §120.D `nl_jit_access_aset_bool_vector_inner' swap.

Two-function package (= `seq' form):

  `nl_jit_access_aset_bv_do' — inner body.  Bounds-checks
  Vec<bool>.length (offset 16 of the NlBoolVector box), computes the
  bool bit from `val' truthiness via `(if (= (sexp-tag val) 0) 0 1)'
  (= Rust `is_truthy' = `!matches!(v, Sexp::Nil)'), writes the byte
  via `ptr-write-u8' to Vec<bool>.ptr (offset 8), then refcount-clones
  `val' into `*out' via `extern-call nl_sexp_clone_into'.

  `nl_jit_access_aset_bool_vector_inner' — ABI entry point matching
  the deleted Rust `#[no_mangle]' body.  Guards idx >= 0, then
  delegates to the inner helper with `sexp-payload-ptr arg'.

The `(and WRITE CLONE 0)' idiom in the OK arm chains two side-effect
ops and returns 0 (TRAMPOLINE_OK) in rax — same pattern as the Vector
arm in `lisp/nelisp-cc-jit-aset.el'.  ABI:
`(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64' matches the
former Rust extern and the `extern-call' invocation in
`lisp/nelisp-cc-jit-aset.el' §120.D BoolVector arm.")

(provide 'nelisp-cc-jit-access-aset-bool-vector-inner)

;;; nelisp-cc-jit-access-aset-bool-vector-inner.el ends here
