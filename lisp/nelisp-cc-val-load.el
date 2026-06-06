;;; nelisp-cc-val-load.el --- AOT nl_val_load / nl_val_clone_into  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 147 Phase 0 keystone — the word<->32B-slot boundary helpers that
;; let the ~302 container-slot consumer sites keep their "32B Sexp slot
;; view" contract while the underlying CONTAINER slots shrink from a
;; fat 32-byte inline `Sexp' to an 8-byte tagged WORD.
;;
;; A tagged WORD is 8 bytes:
;;   low bit 1 = immediate  (Int = (n<<2)|1, Nil = 3, T = 7)
;;   low bit 0 = 8-aligned pointer to a 32-byte `Sexp' box (unchanged).
;; Child Sexp boxes stay 32B; only the container SLOTS shrink.
;;
;; `Sexp' is a 32-byte `#[repr(C, u8)]' slot: tag (u8) @ 0, payload @ 8.
;;
;; Two C-ABI exports:
;;
;;   nl_val_load(word, scratch32) -> *const Sexp
;;     - (logand word 1) == 0  (already a 32B-slot pointer): return WORD.
;;     - else (immediate): nl_sci_store_imm(word, scratch32) materialises
;;       the immediate as a 32-byte storage Sexp at SCRATCH32; return
;;       SCRATCH32.  The caller owns SCRATCH32 (a transient 32B slot);
;;       the returned VIEW is valid only for the consumer's read.  This
;;       is the inverse of the Doc 146 producer (storage Sexp -> word)
;;       in the arena glue, which Phase 1 renames `nl_val_store_word' to
;;       free the `nl_val_load' name for this consumer-side contract.
;;     `nl_sci_store_imm' is provided by `nl_sexp_clone_into.o'
;;     (lisp/nelisp-cc-sexp-clone-into.el).
;;
;;   nl_val_clone_into(src_slot, dst_word_ptr) -> word
;;     Construction side: turn a 32B-slot SRC into a tagged WORD stored
;;     at *DST_WORD_PTR (8 bytes), returning that word.
;;     - immediate SRC (low bit 1): write the 8B word straight to
;;       DST_WORD_PTR and return it.
;;     - slot-pointer SRC (low bit 0): the child must OUTLIVE this call,
;;       so we NEVER store a pointer to a transient scratch slot.  We
;;       alloc a FRESH 32-byte box (`alloc-bytes 32 8'), deep-clone the
;;       child into it via `nl_sexp_clone_into(src, box)' (= reads *SRC,
;;       writes the clone into *BOX; bumps rc on boxed/string variants,
;;       copies pod bits otherwise), and store the
;;       8-aligned box pointer as the word at DST_WORD_PTR.  The box
;;       pointer is naturally 8-aligned (alloc-bytes align = 8) so its
;;       low bit is 0 (= a slot-pointer word, as required).
;;
;; AOT `let' is FOLD-ONLY (cannot bind a runtime value); both entries
;; thread runtime values purely as function parameters, matching the
;; `nl_sci_*' helper-chain style in `nelisp-cc-sexp-clone-into.el'.
;;
;; Helper structure (mirrors the clone-into seq manifest):
;;   nl_vl_prog2     — effect-sequencer (evaluate both, return 2nd).
;;   nl_val_load     — public C-ABI entry (word -> 32B-slot view).
;;   nl_vci_box      — clone SRC into a fresh 32B box, store its pointer
;;                     word at DST_WORD_PTR, return that word.
;;   nl_val_clone_into — public C-ABI entry (src slot -> word @ dst).
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest (so `-meta.el' picks it up via the shared manifest);
;; `build-tool/build.rs' compiles the source into `nl_val_load.o' and
;; archives it; the standalone link manifest in
;; `scripts/nelisp-standalone-build.el' adds it as a unit so the
;; standalone reader resolves the `nl_val_load' / `nl_val_clone_into'
;; PLT references emitted by the Phase 1+ container emit fns.

;;; Code:

(defconst nelisp-cc-val-load--source
  '(seq
    ;; effect-sequencer: evaluate both args (so a void effect can precede
    ;; a value), return the 2nd.  Same idiom as nl_sci_prog2.
    (defun nl_vl_prog2 (_eff val) val)

    ;; Public C-ABI entry: nl_val_load(word, scratch32) -> *const Sexp.
    ;; Slot-pointer words (low bit 0) pass straight through (already a
    ;; 32B slot view).  Immediate words (low bit 1) are materialised as a
    ;; 32-byte storage Sexp at SCRATCH32 via nl_sci_store_imm, and the
    ;; SCRATCH32 view is returned.
    (defun nl_val_load (word scratch32)
      (if (= (logand word 1) 0)
          word
        (extern-call nl_sci_store_imm word scratch32)))

    ;; Boxed/string construction path: clone the 32B-slot child SRC into a
    ;; FRESH 32-byte box (alloc-bytes 32 8 -> 8-aligned, low bit 0), then
    ;; store that box pointer as the WORD at DST_WORD_PTR.  Returns the
    ;; word.  `box' must outlive this call, so we never reference a
    ;; transient scratch slot.  nl_sexp_clone_into(SRC, DST) reads *SRC and
    ;; writes the clone into *DST (= the call order in `nl_alloc_cell_init':
    ;; (nl_sexp_clone_into initial box-ptr)); BOX is treated as
    ;; uninitialised — safe, freshly alloced — and SRC is deep-copied /
    ;; rc-bumped per its tag.
    (defun nl_vci_box (box src dst_word_ptr)
      (nl_vl_prog2
       (and (or (extern-call nl_sexp_clone_into src box) 1)
            (ptr-write-u64 dst_word_ptr 0 box))
       box))

    ;; Public C-ABI entry: nl_val_clone_into(src_slot, dst_word_ptr) -> word.
    ;; Immediate SRC (low bit 1): write the 8B word straight to
    ;; DST_WORD_PTR, return it.  Slot-pointer SRC (low bit 0): clone the
    ;; child into a fresh 32B box and store the box-pointer word.
    (defun nl_val_clone_into (src_slot dst_word_ptr)
      (if (= (logand src_slot 1) 1)
          (nl_vl_prog2 (ptr-write-u64 dst_word_ptr 0 src_slot) src_slot)
        (nl_vci_box (alloc-bytes 32 8) src_slot dst_word_ptr))))
  "AOT source for the Doc 147 Phase 0 word<->slot keystone helpers.

Four-entry `(seq DEFUN ...)' manifest:
- `nl_vl_prog2 (_eff val) -> val'    — effect-sequencer (eval both, 2nd).
- `nl_val_load (word scratch32) -> *const Sexp' — public C-ABI entry;
  slot-pointer words pass through, immediates materialise into SCRATCH32
  via `nl_sci_store_imm' (from `nl_sexp_clone_into.o').
- `nl_vci_box (box src dst_word_ptr) -> word' — clone SRC into a fresh
  32B box via `nl_sexp_clone_into', store the box-pointer word at DST.
- `nl_val_clone_into (src_slot dst_word_ptr) -> word' — public C-ABI
  entry; immediate SRC stores its word directly, slot SRC clones into a
  fresh box (never a transient scratch slot).

AOT ops consumed:
  `logand'                 — low-bit tag test on the value word.
  `extern-call nl_sci_store_imm'   — immediate WORD -> 32B Sexp at scratch.
  `extern-call nl_sexp_clone_into' — deep-clone child SRC into fresh box.
  `alloc-bytes'            — `nl_alloc_bytes(32, 8)' fresh 8-aligned box.
  `ptr-write-u64'          — store the 8B value word at `dst_word_ptr+0'.")

(provide 'nelisp-cc-val-load)

;;; nelisp-cc-val-load.el ends here
