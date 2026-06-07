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
;;   type_tag: Sexp    @ 0   (32 bytes — STAYS INLINE 32B)
;;   slots: Vec<Sexp>  @ 32  (24 bytes)
;;     Vec.capacity    @ 32  (8 bytes — element capacity count)
;;     Vec.data_ptr    @ 40  (8 bytes — *mut Sexp-word, heap data buffer)
;;     Vec.len         @ 48  (8 bytes — element count)
;;   refcount          @ 56  (8 bytes — AtomicUsize)
;;   total = 64 bytes, align = 8 (HEADER UNCHANGED by Doc 147 Phase 2)
;;
;; Confirmed by `nelisp-nlrecord--offset-slots-ptr = 40' (= absolute
;; offset of Vec.data_ptr within NlRecord) in `lisp/nelisp-sexp-layout.el'.
;; Vec.data_ptr is at slots + 8 = NlRecord + 40.
;;
;; Doc 147 Phase 2: the slots buffer now holds one 8-byte tagged WORD
;; per slot (was a 32B inline Sexp).  The body delegates to the Doc 147
;; Phase 0 keystone `nl_val_clone_into(src_slot, dst_word_ptr)':
;;   data_ptr = *(u64*)(record + 40)    // Vec.data_ptr inside slots
;;   dst      = data_ptr + n * 8        // slot n (stride 8, was 32)
;;   nl_val_clone_into(val, dst):
;;     - immediate VAL (low bit 1): writes the 8B word straight to dst.
;;     - boxed/string VAL (low bit 0): deep-clones the child into a
;;       FRESH 32B box and stores its 8-aligned pointer as the WORD at
;;       dst (never a transient scratch slot).
;;
;; `usize' in C-ABI on x86_64 is a 64-bit integer (same as i64 in rsi).
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *mut NlRecord
;;   rsi = usize — slot index n (passed as i64-width register)
;;   rdx = *const Sexp — source value 32B-slot view
;;   return: i64 word (the stored value word, from `nl_val_clone_into').
;;
;; NOTE: refcount-correct via `nl_val_clone_into' (boxed children are
;; cloned into fresh 32B boxes; immediates passthrough).  The Doc 135
;; cutover fix (refcount-safe install via clone, not a raw move) is
;; preserved — `nl_val_clone_into' deep-clones boxed children into a
;; fresh box, so no MOVE-aliasing of a still-owned box.

;;; Code:

(defconst nelisp-cc-nlrecord-set-slot--source
  '(seq
    ;; Doc 147 Phase 2: peek Vec.data_ptr from slots header at record+40,
    ;; compute element WORD address (data_ptr + n*8, stride 8 — was 32),
    ;; then install VAL as a single 8B tagged WORD via the keystone
    ;; `nl_val_clone_into'.
    (defun nl_record_set_slot (record n val)
      ;; Doc 135 cutover fix (preserved): refcount-SAFE install -- clone VAL
      ;; into the slot (rc-bump for boxed variants, deep-copy for strings) via
      ;; `nl_val_clone_into' instead of a raw move.  Without this the bootstrap
      ;; mirror's fast-hash-table / backing-vector boxes were MOVE-aliased
      ;; between the Rust scratch `Sexp::Vector' and globals.slot[0] /
      ;; frames.slot[0]; dropping the scratch then over-freed a still-owned box
      ;; (= the §9.2 / §9.6.4 bootstrap UAF).  Doc 147 Phase 2: the slot is now
      ;; an 8B WORD, so `nl_val_clone_into' (immediate direct; boxed child
      ;; deep-cloned into a fresh 32B box, its 8-aligned ptr stored) replaces
      ;; the 32B-into-32B-slot `nl_sexp_clone_into'.
      (let ((dst (+ (ptr-read-u64 record 40) (* n 8))))
        (extern-call nl_val_clone_into val dst))))
  "AOT source for the `nl_record_set_slot' cutover spike.

Single-entry `(seq DEFUN)' manifest:
- `nl_record_set_slot (record n val) -> i64' — reads Vec.data_ptr
  from RECORD+40 (= slots Vec header offset 32 + Vec.data_ptr offset 8),
  computes DST = data_ptr + N*8 (Doc 147 Phase 2 stride shrink 32 -> 8),
  stores VAL as an 8-byte tagged WORD at DST via the `nl_val_clone_into'
  keystone (immediate direct; boxed child deep-cloned into a fresh 32B
  box, its 8-aligned ptr stored).

AOT ops consumed:
  `ptr-read-u64'   — load Vec.data_ptr from NlRecord+40.
  `*'              — N * 8 (slot WORD byte offset).
  `+'              — data_ptr + slot offset.
  `extern-call nl_val_clone_into' — store VAL as a value WORD at DST.
  `let'            — bind the computed destination pointer.")

(provide 'nelisp-cc-nlrecord-set-slot)

;;; nelisp-cc-nlrecord-set-slot.el ends here
