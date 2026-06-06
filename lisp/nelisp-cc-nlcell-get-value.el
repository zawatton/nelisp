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
;; into *out.  Doc 147 Phase 1: NlCell.value is now an 8-byte tagged
;; WORD @ box+0 (not a 32B inline Sexp).  Two-hop + keystone clone:
;;
;; Step 1: Read the NlCell* from the Sexp payload.
;;
;;   The Sexp::Cell variant stores a NonNull<NlCell> (= 8-byte raw pointer)
;;   at payload offset 8 inside the 32-byte Sexp slot.  So:
;;
;;     nlcell_ptr = *(u64*)(cell_ptr + 8)   // = NonNull<NlCell>.as_ptr()
;;
;; Step 2: Load the value WORD and materialise a 32B-slot view into *out.
;;
;;   NlCell layout (Doc 147 Phase 1):
;;     value    @ 0   (8 bytes — tagged WORD)
;;     refcount @ 8   (8 bytes)
;;
;;   word = *(u64*)(nlcell_ptr + 0).  `nl_sexp_clone_into(word, out)':
;;     - immediate WORD (low bit 1): store_imm materialises into OUT.
;;     - box-ptr WORD  (low bit 0): tag-dispatch deep-clone the child
;;       32B box into OUT (rc-bump on boxed/string variants).
;;   This preserves the "OUT holds a refcount-correct 32B-slot view of
;;   the value" consumer contract (env-lookup-value / eval-inner read OUT).
;;
;; Step 3: Return 0 (= success; the Rust body returns 0 for the Cell arm
;;   and 1 for the fallthrough `_ => 1' arm).  Callers guarantee the
;;   Sexp::Cell tag before calling.
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *const Sexp pointing at a Sexp::Cell(_) value
;;   rsi = *mut Sexp — output slot (32 bytes, caller-owned)
;;   return: i64 = 0 (success)
;;
;; NOTE: refcount-correct via `nl_sexp_clone_into' (boxed children are
;; rc-bumped / string children deep-copied; immediates materialise).

;;; Code:

(defconst nelisp-cc-nlcell-get-value--source
  '(seq
    ;; Read NlCell* from Sexp payload at cell-ptr+8 (two-hop), then
    ;; copy 32 bytes (4 × u64) from NlCell.value (@0) into *out.
    ;; Return 0 (= success).
    ;; Doc 147 Phase 1: the NlCell value field is now an 8-byte tagged
    ;; WORD @ box+0 (not a 32B inline Sexp).  Read the NlCell* from the
    ;; Sexp::Cell payload at cell-ptr+8, load the value WORD at box+0,
    ;; then materialise a 32B-slot VIEW of it into the caller-owned OUT
    ;; via `nl_sexp_clone_into(word, out)' — the keystone that accepts a
    ;; value word: immediate (low bit 1) -> store_imm into OUT; box ptr
    ;; (low bit 0) -> tag-dispatch deep-clone the child into OUT (rc-bump
    ;; on boxed/string variants).  Preserves the "returns a 32B-slot
    ;; view in OUT" consumer contract refcount-correctly.  Return 0.
    (defun nl_cell_get_value (cell-ptr out)
      (let ((nlcell-ptr (ptr-read-u64 cell-ptr 8)))
        (if (or (extern-call nl_sexp_clone_into (ptr-read-u64 nlcell-ptr 0) out) 1)
            0
          0))))
  "AOT source for the `nl_cell_get_value' cutover spike.

Single-entry `(seq DEFUN)' manifest:
- `nl_cell_get_value (cell-ptr out) -> i64' — extracts the NlCell*
  from the Sexp::Cell payload at CELL-PTR+8, loads the 8-byte value
  WORD (offset 0), materialises a refcount-correct 32B-slot view of it
  into OUT via `nl_sexp_clone_into', returns 0.

AOT ops consumed:
  `ptr-read-u64'   — two-hop: sexp payload load + NlCell.value-word load.
  `extern-call nl_sexp_clone_into' — value WORD -> 32B-slot view in OUT.
  `let'            — bind the intermediate NlCell pointer.
  literal `0'      — i64 return value.")

(provide 'nelisp-cc-nlcell-get-value)

;;; nelisp-cc-nlcell-get-value.el ends here
