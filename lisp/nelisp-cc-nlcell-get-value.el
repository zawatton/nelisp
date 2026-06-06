;;; nelisp-cc-nlcell-get-value.el --- nl_cell_get_value AOT swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_cell_get_value' body with a AOT-compiled
;; elisp object.
;;
;; Rust signature:
;;   unsafe extern "C" fn nl_cell_get_value(
;;       cell_ptr: *const Sexp,   // rdi — a Sexp::Cell(_) wrapper
;;       out:      *mut Sexp,     // rsi — caller-owned 32-byte output slot
;;   ) -> i64                     // rax — 0 = success, 1 = type mismatch
;;
;; The Rust body pattern-matches on Sexp::Cell(c) and clones c.value
;; into *out.  In the AOT spike scope we perform a raw 4×u64 copy
;; (no refcount-safe clone) using the two-hop pointer indirection:
;;
;; Step 1: Read the NlCell* from the Sexp payload.
;;
;;   The Sexp::Cell variant stores a NonNull<NlCell> (= 8-byte raw pointer)
;;   at payload offset 8 inside the 32-byte Sexp slot.  The `sexp_box_ptr_
;;   accessor!' macro in the pre-deletion mod.rs reads this via
;;   `(self as *const Sexp as *const u8).add(SEXP_PAYLOAD_OFFSET)' where
;;   SEXP_PAYLOAD_OFFSET = 8.  So:
;;
;;     nlcell_ptr = *(u64*)(cell_ptr + 8)   // = NonNull<NlCell>.as_ptr()
;;
;; Step 2: Copy NlCell.value (offset 0, 32 bytes) into *out.
;;
;;   NlCell layout (pinned by `#[repr(C)]'):
;;     value    @ 0   (32 bytes — sizeof(Sexp))
;;     refcount @ 32  (8 bytes)
;;
;;   The four u64 words of the value field are at nlcell_ptr + 0/8/16/24.
;;
;; Step 3: Return 0 (= success; the Rust body returns 0 for the Cell arm
;;   and 1 for the fallthrough `_ => 1' arm).  In the spike we always
;;   return 0 (callers guarantee the Sexp::Cell tag before calling).
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *const Sexp pointing at a Sexp::Cell(_) value
;;   rsi = *mut Sexp — output slot (32 bytes, caller-owned)
;;   return: i64 = 0 (success)
;;
;; NOTE: Raw 4×u64 copy without refcount-safe clone.
;; Matches the AOT cutover spike scope.

;;; Code:

(defconst nelisp-cc-nlcell-get-value--source
  '(seq
    ;; Read NlCell* from Sexp payload at cell-ptr+8 (two-hop), then
    ;; copy 32 bytes (4 × u64) from NlCell.value (@0) into *out.
    ;; Return 0 (= success).
    (defun nl_cell_get_value (cell-ptr out)
      (let ((nlcell-ptr (ptr-read-u64 cell-ptr 8)))
        (and (ptr-write-u64 out 0 (ptr-read-u64 nlcell-ptr 0))
             (ptr-write-u64 out 8 (ptr-read-u64 nlcell-ptr 8))
             (ptr-write-u64 out 16 (ptr-read-u64 nlcell-ptr 16))
             (ptr-write-u64 out 24 (ptr-read-u64 nlcell-ptr 24)))
        0)))
  "AOT source for the `nl_cell_get_value' cutover spike.

Single-entry `(seq DEFUN)' manifest:
- `nl_cell_get_value (cell-ptr out) -> i64' — extracts the NlCell*
  from the Sexp::Cell payload at CELL-PTR+8, copies the 32-byte value
  field (offset 0) into OUT via four word-copy pairs, returns 0.

AOT ops consumed:
  `ptr-read-u64'   — two-hop: sexp payload load + NlCell.value load.
  `ptr-write-u64'  — four word stores into OUT.
  `let'            — bind the intermediate NlCell pointer.
  literal `0'      — i64 return value.")

(provide 'nelisp-cc-nlcell-get-value)

;;; nelisp-cc-nlcell-get-value.el ends here
