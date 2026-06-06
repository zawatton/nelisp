;;; nelisp-cc-nlrecord-set-slot.el --- nl_record_set_slot AOT swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_record_set_slot' body with a AOT-compiled
;; elisp object.  The function writes the 32-byte Sexp at `val' into
;; element `n' of the `slots' Vec inside the NlRecord pointed to by
;; `record'.
;;
;; Rust signature:
;;   unsafe extern "C" fn nl_record_set_slot(
;;       record: *mut NlRecord,   // rdi
;;       n:      usize,           // rsi — zero-based slot index (usize = u64)
;;       val:    *const Sexp,     // rdx
;;   )
;;
;; NlRecord layout (pinned by `#[repr(C)]' + compile-time asserts in
;; `build-tool/src/eval/mod.rs'):
;;
;;   type_tag: Sexp    @ 0   (32 bytes — sizeof(Sexp))
;;   slots: Vec<Sexp>  @ 32  (24 bytes)
;;     Vec.capacity    @ 32  (8 bytes — element capacity count)
;;     Vec.data_ptr    @ 40  (8 bytes — *mut Sexp, heap data buffer)
;;     Vec.len         @ 48  (8 bytes — element count)
;;   refcount          @ 56  (8 bytes — AtomicUsize)
;;   total = 64 bytes, align = 8
;;
;; Confirmed by `nelisp-nlrecord--offset-slots-ptr = 40' (= absolute
;; offset of Vec.data_ptr within NlRecord) in `lisp/nelisp-sexp-layout.el',
;; and by the `define_nlbox!' layout_asserts in the pre-deletion
;; `build-tool/src/eval/mod.rs':
;;   assert!(offset_of!(NlRecord, slots) == size_of::<Sexp>())  // = 32
;; Vec.data_ptr is at slots + 8 = NlRecord + 40.
;;
;; The Rust body indexes into Vec<Sexp>:
;;   (&mut (*record).slots)[n] = (*val).clone()
;;
;; In AOT spike scope (raw copy, no drop/clone):
;;   data_ptr = *(u64*)(record + 40)    // Vec.data_ptr inside slots
;;   dst      = data_ptr + n * 32       // slot n; n*32 = n << 5
;;
;;   word 0: dst+0  <- val+0
;;   word 1: dst+8  <- val+8
;;   word 2: dst+16 <- val+16
;;   word 3: dst+24 <- val+24
;;
;; `usize' in C-ABI on x86_64 is a 64-bit integer (same as i64 in rsi).
;; AOT `shl' computes n*32 = n<<5 as an i64.
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *mut NlRecord
;;   rsi = usize — slot index n (passed as i64-width register)
;;   rdx = *const Sexp — source value
;;   return: void (rax = 1 sentinel from last ptr-write-u64)
;;
;; NOTE: Raw 4×u64 copy without refcount-safe drop of the slot's
;; previous occupant.  Matches the AOT cutover spike scope.

;;; Code:

(defconst nelisp-cc-nlrecord-set-slot--source
  '(seq
    ;; Peek Vec.data_ptr from slots header at record+40, compute
    ;; element address (data_ptr + n*32 = data_ptr + (shl n 5)),
    ;; then copy 4 × u64 words from val into that slot.
    (defun nl_record_set_slot (record n val)
      ;; Doc 135 cutover fix: refcount-SAFE install -- clone VAL into the slot
      ;; (rc-bump for boxed variants, deep-copy for strings) via
      ;; `nl_sexp_clone_into' instead of a raw 4xu64 move.  Without this the
      ;; bootstrap mirror's fast-hash-table / backing-vector boxes were
      ;; MOVE-aliased between the Rust scratch `Sexp::Vector' and
      ;; globals.slot[0] / frames.slot[0]; dropping the scratch then
      ;; over-freed a still-owned box (= the §9.2 / §9.6.4 bootstrap UAF).
      (let ((dst (+ (ptr-read-u64 record 40) (shl n 5))))
        (extern-call nl_sexp_clone_into val dst))))
  "AOT source for the `nl_record_set_slot' cutover spike.

Single-entry `(seq DEFUN)' manifest:
- `nl_record_set_slot (record n val) -> i64' — reads Vec.data_ptr
  from RECORD+40 (= slots Vec header offset 32 + Vec.data_ptr offset 8),
  computes DST = data_ptr + (N << 5) = data_ptr + N*32, copies the
  32-byte Sexp at VAL into DST via four word-copy pairs.

AOT ops consumed:
  `ptr-read-u64'   — load Vec.data_ptr from NlRecord+40.
  `shl'            — N << 5 = N*32 (slot byte offset).
  `+'              — data_ptr + slot offset.
  `ptr-write-u64'  — four word stores into the slot.
  `let'            — bind the computed destination pointer.")

(provide 'nelisp-cc-nlrecord-set-slot)

;;; nelisp-cc-nlrecord-set-slot.el ends here
