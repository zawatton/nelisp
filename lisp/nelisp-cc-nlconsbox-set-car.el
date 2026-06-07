;;; nelisp-cc-nlconsbox-set-car.el --- nl_consbox_set_car AOT swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_consbox_set_car' body in
;; `build-tool/src/eval/nlconsbox.rs' with a AOT-compiled elisp
;; object.  The function copies the 32-byte Sexp at `val' (a raw
;; `*const Sexp' in rsi) into the car slot of the NlConsBox pointed to
;; by `box' (a raw `*mut NlConsBox' in rdi).
;;
;; NlConsBox layout (Doc 147 Phase 3 — container slot shrink):
;;
;;   car      @ 0   (8 bytes — tagged WORD: imm or 8-aligned box ptr)
;;   cdr      @ 8   (8 bytes — tagged WORD: imm or 8-aligned box ptr)
;;   refcount @ 16  (8 bytes — AtomicUsize)
;;   total = 24 bytes, align = 8
;;
;; The car field is now a single 8-byte tagged WORD; the body stores
;; it via the `nl_val_clone_into' keystone (immediate direct; boxed
;; child deep-cloned into a fresh 32B box, its 8-aligned ptr stored).
;;
;; C-ABI contract (SysV AMD64):
;;   rdi = *mut NlConsBox (= raw box pointer, already extracted from
;;         the Sexp payload by the `cons-set-car' grammar op emitter)
;;   rsi = *const Sexp    (= pointer to the new car value)
;;   return: void (rax holds 1 sentinel from the last ptr-write-u64)
;;
;; NOTE: This implementation performs a raw 4×u64 copy without a
;; refcount-safe drop of the previous car value.  This matches the
;; scope agreed for this AOT cutover spike (= prove the wiring
;; path; the refcount-safe semantics are preserved by the Rust shim
;; above the AOT boundary for production callers).
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
    ;; AOT `ptr-write-u64 PTR OFFSET VAL' stores a u64 word;
    ;; `ptr-read-u64 PTR OFFSET' loads a u64 word.  Both use literal
    ;; byte offsets.  The `and' chain sequences the four stores
    ;; left-to-right; each `ptr-write-u64' returns 1 (non-nil) so the
    ;; chain never short-circuits.  The final store's 1-sentinel becomes
    ;; the function return in rax (void from the caller's perspective).
    ;; Doc 147 Phase 3: store VAL as a single 8-byte tagged WORD @ box+0
    ;; (the car field) via the keystone `nl_val_clone_into(src_slot,
    ;; dst_word_ptr)':
    ;;   - immediate VAL (low bit 1): writes the 8B word straight to box+0.
    ;;   - boxed/string VAL (low bit 0): deep-clones the child into a FRESH
    ;;     32B box and stores its 8-aligned pointer as the word at box+0
    ;;     (never a transient scratch slot).
    ;; NOT a 4xu64 inline copy — the car field is now 8 bytes.
    (defun nl_consbox_set_car (box val)
      (extern-call nl_val_clone_into val box)))
  "AOT source for the `nl_consbox_set_car' swap.

Single-entry `(seq DEFUN)' manifest:
- `nl_consbox_set_car (box val) -> i64' — stores VAL as an 8-byte
  tagged WORD in the car field (offset 0) of the NlConsBox at BOX via
  the Doc 147 `nl_val_clone_into' keystone (immediate direct; boxed
  child deep-cloned into a fresh 32B box, its 8-aligned ptr stored).

AOT ops consumed:
  `extern-call nl_val_clone_into' — store VAL as a car WORD at box+0.")

(provide 'nelisp-cc-nlconsbox-set-car)

;;; nelisp-cc-nlconsbox-set-car.el ends here
