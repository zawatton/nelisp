;;; nelisp-cc-nlconsbox-set-car.el --- nl_consbox_set_car Phase 47 swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_consbox_set_car' body in
;; `build-tool/src/eval/nlconsbox.rs' with a Phase 47-compiled elisp
;; object.  The function copies the 32-byte Sexp at `val' (a raw
;; `*const Sexp' in rsi) into the car slot of the NlConsBox pointed to
;; by `box' (a raw `*mut NlConsBox' in rdi).
;;
;; NlConsBox layout (pinned by `#[repr(C)]' + compile-time asserts in
;; `build-tool/src/eval/nlconsbox.rs'):
;;
;;   car      @ 0   (32 bytes — sizeof(Sexp))
;;   cdr      @ 32  (32 bytes — sizeof(Sexp))
;;   refcount @ 64  (8 bytes  — AtomicUsize)
;;   total = 72 bytes, align = 8
;;
;; The car slot occupies bytes 0..31 of the NlConsBox.  A Sexp is
;; 32 bytes = 4 × u64 words.  The body copies these four words in
;; order using `ptr-write-u64' + `ptr-read-u64' pairs:
;;
;;   word 0: box+0  <- val+0
;;   word 1: box+8  <- val+8
;;   word 2: box+16 <- val+16
;;   word 3: box+24 <- val+24
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *mut NlConsBox (= raw box pointer, already extracted from
;;         the Sexp payload by the `cons-set-car' grammar op emitter)
;;   rsi = *const Sexp    (= pointer to the new car value)
;;   return: void (rax holds 1 sentinel from the last ptr-write-u64)
;;
;; NOTE: This implementation performs a raw 4×u64 copy without a
;; refcount-safe drop of the previous car value.  This matches the
;; scope agreed for this Phase 47 cutover spike (= prove the wiring
;; path; the refcount-safe semantics are preserved by the Rust shim
;; above the Phase 47 boundary for production callers).
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nl_consbox_set_car.o' and archives it into `libnelisp_elisp_spike.a'
;; which the linker resolves against the `nl_consbox_set_car' PLT
;; reference emitted by the `cons-set-car' grammar op.

;;; Code:

(defconst nelisp-cc-nlconsbox-set-car--source
  '(seq
    ;; Public entry — copy 32 bytes (4 × u64) from val into box+0..+31.
    ;; Phase 47 `ptr-write-u64 PTR OFFSET VAL' stores a u64 word;
    ;; `ptr-read-u64 PTR OFFSET' loads a u64 word.  Both use literal
    ;; byte offsets.  The `and' chain sequences the four stores
    ;; left-to-right; each `ptr-write-u64' returns 1 (non-nil) so the
    ;; chain never short-circuits.  The final store's 1-sentinel becomes
    ;; the function return in rax (void from the caller's perspective).
    (defun nl_consbox_set_car (box val)
      (and (ptr-write-u64 box 0 (ptr-read-u64 val 0))
           (ptr-write-u64 box 8 (ptr-read-u64 val 8))
           (ptr-write-u64 box 16 (ptr-read-u64 val 16))
           (ptr-write-u64 box 24 (ptr-read-u64 val 24)))))
  "Phase 47 source for the `nl_consbox_set_car' cutover spike.

Single-entry `(seq DEFUN)' manifest:
- `nl_consbox_set_car (box val) -> i64' — copies the 32-byte Sexp
  at VAL into the car slot (offset 0) of the NlConsBox at BOX via
  four `ptr-write-u64' / `ptr-read-u64' word-copy pairs.

Phase 47 ops consumed:
  `ptr-read-u64'   — `*(u64*)(val + offset)' load (offsets 0/8/16/24).
  `ptr-write-u64'  — `*(u64*)(box + offset) = v' store (same offsets).

Net Rust delta: deletes the `nl_consbox_set_car' body from
`build-tool/src/eval/nlconsbox.rs'.")

(provide 'nelisp-cc-nlconsbox-set-car)

;;; nelisp-cc-nlconsbox-set-car.el ends here
