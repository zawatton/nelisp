;;; nelisp-cc-nlcell-set-value.el --- nl_cell_set_value AOT swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_cell_set_value' body with a AOT-compiled
;; elisp object.  Doc 147 Phase 1: stores the value at `val' as a single
;; 8-byte tagged WORD in the `value' field of the NlCell at `cell'.
;;
;; NlCell layout (Doc 147 Phase 1 — container slot shrink):
;;
;;   value    @ 0   (8 bytes — tagged WORD: imm or 8-aligned box ptr)
;;   refcount @ 8   (8 bytes — AtomicUsize)
;;   total = 16 bytes, align = 8
;;
;; Confirmed by `nelisp-nlcell--offset-value = 0' and
;; `nelisp-nlcell--offset-refcount = 8' in `lisp/nelisp-sexp-layout.el'.
;;
;; The body delegates to the Doc 147 Phase 0 keystone
;; `nl_val_clone_into(src_slot, dst_word_ptr)':
;;   - immediate VAL (low bit 1): writes the 8B word straight to cell+0.
;;   - boxed/string VAL (low bit 0): deep-clones the child into a FRESH
;;     32B box and stores its 8-aligned pointer as the word at cell+0
;;     (never a transient scratch slot).
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *mut NlCell   (= raw cell pointer; the value-WORD dst)
;;   rsi = *const Sexp   (= pointer to the new value 32B slot view)
;;   return: i64 word (the stored value word, from `nl_val_clone_into').
;;
;; NOTE: refcount-correct via `nl_val_clone_into' (boxed children are
;; cloned into fresh 32B boxes; immediates passthrough).  Doc 147
;; supersedes the prior raw 4-word cutover-spike copy.

;;; Code:

(defconst nelisp-cc-nlcell-set-value--source
  '(seq
    ;; Doc 147 Phase 1: store VAL as a single 8-byte tagged WORD @ cell+0
    ;; (the value field).  `nl_val_clone_into(src_slot, dst_word_ptr)':
    ;;   - immediate VAL (low bit 1): writes the 8B word straight to cell+0.
    ;;   - boxed/string VAL (low bit 0): deep-clones the child into a FRESH
    ;;     32B box and stores its 8-aligned pointer as the word at cell+0.
    ;; NOT a 4xu64 inline copy — the value field is now 8 bytes.
    (defun nl_cell_set_value (cell val)
      (extern-call nl_val_clone_into val cell)))
  "AOT source for the `nl_cell_set_value' cutover spike.

Single-entry `(seq DEFUN)' manifest:
- `nl_cell_set_value (cell val) -> i64' — stores VAL as an 8-byte
  tagged WORD in the value field (offset 0) of the NlCell at CELL via
  the Doc 147 `nl_val_clone_into' keystone (immediate direct; boxed
  child deep-cloned into a fresh 32B box, its 8-aligned ptr stored).

AOT ops consumed:
  `extern-call nl_val_clone_into' — store VAL as a value WORD at CELL+0.")

(provide 'nelisp-cc-nlcell-set-value)

;;; nelisp-cc-nlcell-set-value.el ends here
