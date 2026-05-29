;;; nelisp-cc-nlconsbox-set-cdr.el --- nl_consbox_set_cdr Phase 47 swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_consbox_set_cdr' body with a Phase 47-compiled
;; elisp object.  The function copies the 32-byte Sexp at `val' (rsi)
;; into the cdr slot of the NlConsBox pointed to by `box' (rdi).
;;
;; NlConsBox layout (pinned by `#[repr(C)]' + compile-time asserts in
;; `build-tool/src/eval/nlconsbox.rs'):
;;
;;   car      @ 0   (32 bytes — sizeof(Sexp))
;;   cdr      @ 32  (32 bytes — sizeof(Sexp))
;;   refcount @ 64  (8 bytes  — AtomicUsize)
;;   total = 72 bytes, align = 8
;;
;; Confirmed by `nelisp-nlconsbox--offset-cdr = 32' in
;; `lisp/nelisp-sexp-layout.el' and by the `define_nlbox!' layout_asserts
;; in the pre-deletion `build-tool/src/eval/mod.rs'.
;;
;; The cdr slot occupies bytes 32..63 of the NlConsBox.  A Sexp is
;; 32 bytes = 4 × u64 words.  The body copies these four words in order
;; using `ptr-write-u64' + `ptr-read-u64' pairs:
;;
;;   word 0: box+32 <- val+0
;;   word 1: box+40 <- val+8
;;   word 2: box+48 <- val+16
;;   word 3: box+56 <- val+24
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *mut NlConsBox (= raw box pointer)
;;   rsi = *const Sexp    (= pointer to the new cdr value)
;;   return: void (rax holds 1 sentinel from the last ptr-write-u64)
;;
;; NOTE: Raw 4×u64 copy without refcount-safe drop of the previous cdr
;; value.  Matches the Phase 47 cutover spike scope (same rationale as
;; `nelisp-cc-nlconsbox-set-car.el').

;;; Code:

(defconst nelisp-cc-nlconsbox-set-cdr--source
  '(seq
    ;; Copy 32 bytes (4 × u64) from val into box+32..+63 (= cdr slot).
    (defun nl_consbox_set_cdr (box val)
      (and (ptr-write-u64 box 32 (ptr-read-u64 val 0))
           (ptr-write-u64 box 40 (ptr-read-u64 val 8))
           (ptr-write-u64 box 48 (ptr-read-u64 val 16))
           (ptr-write-u64 box 56 (ptr-read-u64 val 24)))))
  "Phase 47 source for the `nl_consbox_set_cdr' cutover spike.

Single-entry `(seq DEFUN)' manifest:
- `nl_consbox_set_cdr (box val) -> i64' — copies the 32-byte Sexp
  at VAL into the cdr slot (offset 32) of the NlConsBox at BOX via
  four `ptr-write-u64' / `ptr-read-u64' word-copy pairs.

Phase 47 ops consumed:
  `ptr-read-u64'   — `*(u64*)(val + offset)' load (offsets 0/8/16/24).
  `ptr-write-u64'  — `*(u64*)(box + offset) = v' store (offsets 32/40/48/56).")

(provide 'nelisp-cc-nlconsbox-set-cdr)

;;; nelisp-cc-nlconsbox-set-cdr.el ends here
