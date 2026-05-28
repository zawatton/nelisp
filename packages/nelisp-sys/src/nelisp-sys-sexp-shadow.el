;;; nelisp-sys-sexp-shadow.el --- nelisp-sys shadow of the NeLisp Sexp ABI -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 133 Phase 1: the NeLisp runtime Sexp value + its heap boxes,
;; expressed as `nelisp-sys' `:repr c' structs.  This is the "shadow"
;; representation — it does NOT replace the Rust `Sexp' yet (Phase 2+
;; cuts over).  Its sole job in Phase 1 is to prove that the nelisp-sys
;; layout engine reproduces the frozen Sexp ABI (`lisp/nelisp-sexp-
;; layout.el' / `make sexp-abi-check') byte-for-byte, so that when the
;; refcount / allocator / dispatch kernel is rewritten in nelisp-sys
;; (Phases 2-8) it shares the exact same memory layout the existing
;; Phase-47 `.o' corpus depends on.
;;
;; Key fact: Rust's `String' / `Vec' are NOT `#[repr(C)]' — their field
;; order is toolchain-pinned, not formally frozen.  The frozen constants
;; in `nelisp-sexp-layout.el' capture the ACTUAL offsets on the repo
;; toolchain.  We reproduce them here by declaring `:repr c' fields in
;; the same offset order (e.g. the `String' header is (capacity, ptr,
;; length); a `Vec' header is (ptr, capacity, length)).
;;
;; The structs are registered into a `nelisp-sys' types env via
;; `nelisp-sys-sexp-shadow-env'; the equivalent `sys:defstruct' source
;; forms are kept in `nelisp-sys-sexp-shadow--source' for documentation
;; and the future 4th `sexp-abi-check' artifact.

;;; Code:

(require 'nelisp-sys-types)

(defconst nelisp-sys-sexp-shadow--source
  '(;; One Sexp value slot: a 1-byte discriminant at +0, an 8-byte
    ;; payload word at +8 (a tagged pointer / inline scalar), padded to
    ;; the 32-byte slot the Rust `enum Sexp' occupies.
    (sys:defstruct sexp (:repr c)
      (tag u8)
      (payload u64)
      (pad (array u8 16)))
    ;; NlConsBox: car / cdr Sexp slots + atomic refcount (size 72).
    (sys:defstruct nlconsbox (:repr c)
      (car (struct sexp))
      (cdr (struct sexp))
      (refcount u64))
    ;; NlVector: inline Vec<Sexp> header (ptr, cap, len) + refcount.
    (sys:defstruct nlvector (:repr c)
      (ptr usize)
      (cap usize)
      (len usize)
      (refcount u64))
    ;; NlCell: one Sexp slot + refcount (size 40).
    (sys:defstruct nlcell (:repr c)
      (value (struct sexp))
      (refcount u64))
    ;; NlRecord: type_tag Sexp slot + inline Vec<Sexp> header + refcount.
    (sys:defstruct nlrecord (:repr c)
      (type-tag (struct sexp))
      (slots-ptr usize)
      (slots-cap usize)
      (slots-len usize)
      (refcount u64))
    ;; Rust `String' header as it sits inside a Sexp::Str / Sexp::Symbol
    ;; payload: (capacity, ptr, length) on the repo toolchain (24 bytes).
    (sys:defstruct nlstringheader (:repr c)
      (capacity usize)
      (ptr usize)
      (length usize)))
  "The Sexp ABI shadow as `sys:defstruct' source forms (Doc 133 P1).
Documentation + future `sexp-abi-check' 4th-artifact source.  The
machine-checked layout is built from the same field specs by
`nelisp-sys-sexp-shadow-env'.")

(defconst nelisp-sys-sexp-shadow--specs
  '((sexp c ((tag u8) (payload u64) (pad (array u8 16))))
    (nlconsbox c ((car (struct sexp)) (cdr (struct sexp)) (refcount u64)))
    (nlvector c ((ptr usize) (cap usize) (len usize) (refcount u64)))
    (nlcell c ((value (struct sexp)) (refcount u64)))
    (nlrecord c ((type-tag (struct sexp)) (slots-ptr usize)
                 (slots-cap usize) (slots-len usize) (refcount u64)))
    (nlstringheader c ((capacity usize) (ptr usize) (length usize))))
  "(NAME REPR FIELDS) specs for the Sexp ABI shadow structs.
Mirrors `nelisp-sys-sexp-shadow--source' field-for-field; consumed by
`nelisp-sys-sexp-shadow-env'.")

(defun nelisp-sys-sexp-shadow-env ()
  "Return a fresh `nelisp-sys' types env holding the Sexp ABI shadow."
  (let ((env (nelisp-sys-types-env-make)))
    (dolist (spec nelisp-sys-sexp-shadow--specs)
      (nelisp-sys-types-env-add env (nth 0 spec) (nth 1 spec) (nth 2 spec)))
    env))

(provide 'nelisp-sys-sexp-shadow)

;;; nelisp-sys-sexp-shadow.el ends here
