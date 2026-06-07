;;; nelisp-cc-nlvector-set-slot.el --- nl_vector_set_slot AOT swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_vector_set_slot' body with a AOT-compiled
;; elisp object.  Doc 147 Phase 2: stores the value at `val' as a
;; single 8-byte tagged WORD into element `n' of the NlVector at
;; `vec-ptr' (was: a 32-byte Sexp copy into a 32B-stride slot).
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
;;     Vec.data_ptr  @ 8    (8 bytes — *mut Sexp-word, heap data buffer)
;;     Vec.len       @ 16   (8 bytes — element count)
;;   refcount        @ 24   (8 bytes — AtomicUsize)
;;   total = 32 bytes, align = 8 (HEADER UNCHANGED by Doc 147 Phase 2)
;;
;; Confirmed by `nelisp-nlvector--offset-value-ptr = 8' (= data pointer
;; is at Vec header offset 8 within NlVector, i.e. NlVector + 8) in
;; `lisp/nelisp-sexp-layout.el'.
;;
;; Doc 147 Phase 2: the data buffer now holds one 8-byte tagged WORD
;; per element (was a 32B inline Sexp).  The body delegates to the
;; Doc 147 Phase 0 keystone `nl_val_clone_into(src_slot, dst_word_ptr)':
;;   data_ptr = *(u64*)(vec_ptr + 8)    // Vec.data_ptr
;;   dst      = data_ptr + n * 8        // element n (stride 8, was 32)
;;   nl_val_clone_into(val, dst):
;;     - immediate VAL (low bit 1): writes the 8B word straight to dst.
;;     - boxed/string VAL (low bit 0): deep-clones the child into a
;;       FRESH 32B box and stores its 8-aligned pointer as the WORD at
;;       dst (never a transient scratch slot).
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *mut NlVector
;;   rsi = i64 — element index n
;;   rdx = *const Sexp — source value 32B-slot view
;;   return: i64 word (the stored value word, from `nl_val_clone_into').
;;
;; NOTE: refcount-correct via `nl_val_clone_into' (boxed children are
;; cloned into fresh 32B boxes; immediates passthrough).  Doc 147
;; supersedes the prior raw 4-word cutover-spike copy.

;;; Code:

(defconst nelisp-cc-nlvector-set-slot--source
  '(seq
    ;; Doc 147 Phase 2: peek data_ptr from Vec header at vec-ptr+8,
    ;; compute element WORD address (data_ptr + n*8, stride 8 — was 32),
    ;; then install VAL as a single 8B tagged WORD via the keystone
    ;; `nl_val_clone_into(src_slot, dst_word_ptr)' (immediate direct;
    ;; boxed child deep-cloned into a fresh 32B box, its 8-aligned ptr
    ;; stored as the word).  The prior occupant was either a Nil WORD
    ;; (from alloc fill) or a previously-cloned WORD; immediates need no
    ;; drop, and re-cloning a pointer slot is refcount-correct because
    ;; nl_val_clone_into deep-clones into a fresh box (no aliasing).
    (defun nl_vector_set_slot (vec-ptr n val)
      (let ((dst (+ (ptr-read-u64 vec-ptr 8) (* n 8))))
        (extern-call nl_val_clone_into val dst))))
  "AOT source for the `nl_vector_set_slot' cutover spike.

Single-entry `(seq DEFUN)' manifest:
- `nl_vector_set_slot (vec-ptr n val) -> i64' — reads Vec.data_ptr
  from VEC-PTR+8, computes DST = data_ptr + N*8 (Doc 147 Phase 2
  stride shrink 32 -> 8), stores VAL as an 8-byte tagged WORD at DST
  via the `nl_val_clone_into' keystone (immediate direct; boxed child
  deep-cloned into a fresh 32B box, its 8-aligned ptr stored).

AOT ops consumed:
  `ptr-read-u64'   — load Vec.data_ptr from NlVector+8.
  `*'              — N * 8 (element WORD byte offset).
  `+'              — data_ptr + element offset.
  `extern-call nl_val_clone_into' — store VAL as a value WORD at DST.
  `let'            — bind the computed destination pointer.")

(provide 'nelisp-cc-nlvector-set-slot)

;;; nelisp-cc-nlvector-set-slot.el ends here
