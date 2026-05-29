;;; nelisp-cc-nlvector-set-slot.el --- nl_vector_set_slot Phase 47 swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_vector_set_slot' body with a Phase 47-compiled
;; elisp object.  The function writes the 32-byte Sexp at `val' into
;; element `n' of the NlVector pointed to by `vec-ptr'.
;;
;; Rust signature:
;;   unsafe extern "C" fn nl_vector_set_slot(
;;       vec_ptr: *mut NlVector,   // rdi
;;       n:       i64,             // rsi — zero-based element index
;;       val:     *const Sexp,     // rdx
;;   )
;;
;; NlVector layout (pinned by `#[repr(C)]' + compile-time asserts in
;; `build-tool/src/eval/mod.rs'):
;;
;;   value: Vec<Sexp> @ 0   (24 bytes)
;;     Vec.capacity  @ 0    (8 bytes — element capacity count)
;;     Vec.data_ptr  @ 8    (8 bytes — *mut Sexp, heap data buffer)
;;     Vec.len       @ 16   (8 bytes — element count)
;;   refcount        @ 24   (8 bytes — AtomicUsize)
;;   total = 32 bytes, align = 8
;;
;; Confirmed by `nelisp-nlvector--offset-value-ptr = 8' (= data pointer
;; is at Vec header offset 8 within NlVector, i.e. NlVector + 8) in
;; `lisp/nelisp-sexp-layout.el', and cross-checked against the Phase 47
;; `--emit-vector-slot-ptr-core' implementation in
;; `nelisp-phase47-compiler.el' which adds `[NlVector* + 8]' to `idx*32'.
;;
;; The Rust body indexes into Vec<Sexp> as:
;;   (&mut (*vec_ptr).value)[n as usize] = (*val).clone()
;;
;; In Phase 47 spike scope (raw copy, no drop/clone):
;;   data_ptr = *(u64*)(vec_ptr + 8)    // Vec.data_ptr
;;   dst      = data_ptr + n * 32       // element n; n*32 = n << 5
;;
;;   word 0: dst+0  <- val+0
;;   word 1: dst+8  <- val+8
;;   word 2: dst+16 <- val+16
;;   word 3: dst+24 <- val+24
;;
;; Arithmetic: `(shl n 5)' computes n*32 in Phase 47 (= `shl rax, 5'
;; on x86_64); `+' adds the data pointer.
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *mut NlVector
;;   rsi = i64 — element index n
;;   rdx = *const Sexp — source value
;;   return: void (rax = 1 sentinel from last ptr-write-u64)
;;
;; NOTE: Raw 4×u64 copy without refcount-safe drop of the slot's
;; previous occupant.  Matches the Phase 47 cutover spike scope.

;;; Code:

(defconst nelisp-cc-nlvector-set-slot--source
  '(seq
    ;; Peek data_ptr from Vec header at vec-ptr+8, compute element
    ;; address (data_ptr + n*32 = data_ptr + (shl n 5)), then copy
    ;; 4 × u64 words from val into that slot.
    (defun nl_vector_set_slot (vec-ptr n val)
      (let ((dst (+ (ptr-read-u64 vec-ptr 8) (shl n 5))))
        (and (ptr-write-u64 dst 0 (ptr-read-u64 val 0))
             (ptr-write-u64 dst 8 (ptr-read-u64 val 8))
             (ptr-write-u64 dst 16 (ptr-read-u64 val 16))
             (ptr-write-u64 dst 24 (ptr-read-u64 val 24))))))
  "Phase 47 source for the `nl_vector_set_slot' cutover spike.

Single-entry `(seq DEFUN)' manifest:
- `nl_vector_set_slot (vec-ptr n val) -> i64' — reads Vec.data_ptr
  from VEC-PTR+8, computes DST = data_ptr + (N << 5) = data_ptr + N*32,
  copies the 32-byte Sexp at VAL into DST via four word-copy pairs.

Phase 47 ops consumed:
  `ptr-read-u64'   — load Vec.data_ptr from NlVector+8.
  `shl'            — N << 5 = N*32 (element byte offset).
  `+'              — data_ptr + element offset.
  `ptr-write-u64'  — four word stores into the element slot.
  `let'            — bind the computed destination pointer.")

(provide 'nelisp-cc-nlvector-set-slot)

;;; nelisp-cc-nlvector-set-slot.el ends here
