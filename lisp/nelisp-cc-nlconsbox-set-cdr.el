;;; nelisp-cc-nlconsbox-set-cdr.el --- nl_consbox_set_cdr AOT swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_consbox_set_cdr' body with a AOT-compiled
;; elisp object.  The function copies the 32-byte Sexp at `val' (rsi)
;; into the cdr slot of the NlConsBox pointed to by `box' (rdi).
;;
;; NlConsBox layout (Doc 147 Phase 3 — container slot shrink):
;;
;;   car      @ 0   (8 bytes — tagged WORD: imm or 8-aligned box ptr)
;;   cdr      @ 8   (8 bytes — tagged WORD: imm or 8-aligned box ptr)
;;   refcount @ 16  (8 bytes — AtomicUsize)
;;   total = 24 bytes, align = 8
;;
;; Confirmed by `nelisp-nlconsbox--offset-cdr = 8' in
;; `lisp/nelisp-sexp-layout.el'.
;;
;; The cdr field is now a single 8-byte tagged WORD @ box+8; the body
;; stores it via the `nl_val_clone_into' keystone (immediate direct;
;; boxed child deep-cloned into a fresh 32B box, its 8-aligned ptr
;; stored).
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *mut NlConsBox (= raw box pointer)
;;   rsi = *const Sexp    (= pointer to the new cdr value 32B slot view)
;;   return: i64 word (the stored value word, from `nl_val_clone_into').

;;; Code:

(defconst nelisp-cc-nlconsbox-set-cdr--source
  '(seq
    ;; Doc 147 Phase 3: store VAL as a single 8-byte tagged WORD @ box+8
    ;; (the cdr field) via the keystone `nl_val_clone_into(src_slot,
    ;; dst_word_ptr)' — immediate direct, boxed child deep-cloned into a
    ;; fresh 32B box and its 8-aligned ptr stored.  cdr offset 32 -> 8;
    ;; NOT a 4xu64 inline copy (the cdr field is now 8 bytes).
    (defun nl_consbox_set_cdr (box val)
      (extern-call nl_val_clone_into val (+ box 8))))
  "AOT source for the `nl_consbox_set_cdr' swap.

Single-entry `(seq DEFUN)' manifest:
- `nl_consbox_set_cdr (box val) -> i64' — stores VAL as an 8-byte
  tagged WORD in the cdr field (offset 8) of the NlConsBox at BOX via
  the Doc 147 `nl_val_clone_into' keystone (immediate direct; boxed
  child deep-cloned into a fresh 32B box, its 8-aligned ptr stored).

AOT ops consumed:
  `extern-call nl_val_clone_into' — store VAL as a cdr WORD at box+8.")

(provide 'nelisp-cc-nlconsbox-set-cdr)

;;; nelisp-cc-nlconsbox-set-cdr.el ends here
