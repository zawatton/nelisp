;;; nelisp-cc-nlcell-set-value.el --- nl_cell_set_value AOT swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_cell_set_value' body with a AOT-compiled
;; elisp object.  The function copies the 32-byte Sexp at `val' into the
;; `value' field of the NlCell pointed to by `cell'.
;;
;; NlCell layout (pinned by `#[repr(C)]' + compile-time asserts):
;;
;;   value    @ 0   (32 bytes — sizeof(Sexp))
;;   refcount @ 32  (8 bytes  — AtomicUsize)
;;   total = 40 bytes, align = 8
;;
;; Confirmed by `nelisp-nlcell--offset-value = 0' and
;; `nelisp-nlcell--offset-refcount = 32' in `lisp/nelisp-sexp-layout.el',
;; and by the `layout_asserts{ assert!(offset_of!(NlCell, value) == 0) }'
;; in the pre-deletion `build-tool/src/eval/mod.rs'.
;;
;; The value slot occupies bytes 0..31 of the NlCell.  The body copies
;; four u64 words from val to cell+0:
;;
;;   word 0: cell+0  <- val+0
;;   word 1: cell+8  <- val+8
;;   word 2: cell+16 <- val+16
;;   word 3: cell+24 <- val+24
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *mut NlCell   (= raw cell pointer)
;;   rsi = *const Sexp   (= pointer to the new value)
;;   return: void (rax holds 1 sentinel from the last ptr-write-u64)
;;
;; NOTE: Raw 4×u64 copy without refcount-safe drop/clone.
;; Matches the AOT cutover spike scope (same rationale as
;; `nelisp-cc-nlconsbox-set-car.el').

;;; Code:

(defconst nelisp-cc-nlcell-set-value--source
  '(seq
    ;; Copy 32 bytes (4 × u64) from val into cell+0..+31 (= value slot).
    (defun nl_cell_set_value (cell val)
      (and (ptr-write-u64 cell 0 (ptr-read-u64 val 0))
           (ptr-write-u64 cell 8 (ptr-read-u64 val 8))
           (ptr-write-u64 cell 16 (ptr-read-u64 val 16))
           (ptr-write-u64 cell 24 (ptr-read-u64 val 24)))))
  "AOT source for the `nl_cell_set_value' cutover spike.

Single-entry `(seq DEFUN)' manifest:
- `nl_cell_set_value (cell val) -> i64' — copies the 32-byte Sexp
  at VAL into the value slot (offset 0) of the NlCell at CELL via
  four `ptr-write-u64' / `ptr-read-u64' word-copy pairs.

AOT ops consumed:
  `ptr-read-u64'   — `*(u64*)(val + offset)' load (offsets 0/8/16/24).
  `ptr-write-u64'  — `*(u64*)(cell + offset) = v' store (same offsets).")

(provide 'nelisp-cc-nlcell-set-value)

;;; nelisp-cc-nlcell-set-value.el ends here
