;;; nelisp-cc-nlvector-slot-ptr.el --- nl_vector_slot_ptr AOT helper  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 147 Phase 2 — the word->32B-slot materialiser for the Vector
;; `vector-ref-ptr' / `vector-slot-ptr-core' read path.
;;
;; Background: after the Phase 2 shrink the NlVector DATA BUFFER holds
;; one 8-byte tagged WORD per element (was a 32B inline `Sexp').  But
;; every post-Phase-1.5 consumer of `vector-ref-ptr' (= the IR op that
;; returns the element's `*const Sexp') is READ-ONLY and reads a 32B
;; `Sexp' from the returned pointer (tag @ +0, payload @ +8).  So the
;; emit must MATERIALISE a 32B-slot VIEW of the slot's 8B word and
;; return a pointer to THAT, not the raw 8B-buffer pointer.
;;
;; SCRATCH-LIFETIME HAZARD: some consumers hold TWO (or more)
;; slot-ptr results live at once — e.g.
;;   `(extern-call nelisp_meta_should_rebuild (vector-ref-ptr srcs i)
;;                 (vector-ref-ptr outs i) scratch)'  (meta-walk)
;;   `(nelisp_frame_bind frames name cell-slot (vector-ref-ptr s 1)
;;     (vector-ref-ptr s 2) (vector-ref-ptr s 3))'   (env-bind-local)
;; A single SHARED scratch slot would be clobbered by the 2nd / 3rd
;; materialise before the 1st is read.  To be hazard-free for ANY
;; number of concurrently-live results, this helper allocates a FRESH
;; 32B box PER immediate slot (on the per-eval arena, bulk-reclaimed),
;; so two results never alias.  A pointer slot needs NO box — its WORD
;; already points at a distinct live 32B child box, returned as-is.
;;
;; nl_vector_slot_ptr(sexp_ptr, idx) -> *const Sexp:
;;   box_ptr  = *(u64*)(sexp_ptr + 8)       // Sexp::Vector payload (NlVector*)
;;   data_ptr = *(u64*)(box_ptr + 8)        // Vec.data_ptr (box header @+8)
;;   word     = *(u64*)(data_ptr + idx*8)   // the 8B tagged WORD slot
;;   - pointer WORD (low bit 0): return WORD directly (already a 32B
;;     child-box pointer; the consumer reads tag/payload from it).
;;   - immediate WORD (low bit 1): alloc a FRESH 32B box, materialise
;;     the immediate into it via `nl_val_load(word, box)' (= the Phase 0
;;     keystone: `nl_sci_store_imm' into the scratch), return the box.
;;
;; `nl_val_load' is the arity-2 keystone provided by `val-load.o'
;; (lisp/nelisp-cc-val-load.el); branching on the low bit here keeps the
;; fresh-box alloc OFF the hot pointer-slot path.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' manifest entry +
;; the standalone link manifest in `scripts/nelisp-standalone-build.el'.

;;; Code:

(defconst nelisp-cc-nlvector-slot-ptr--source
  '(seq
    ;; nl_vector_slot_ptr(sexp-ptr, idx) -> *const Sexp.  SEXP-PTR points
    ;; at a `Sexp::Vector' slot whose payload@+8 is the NlVector box;
    ;; data_ptr lives at box+8.  Read the slot's 8B tagged WORD from the
    ;; data buffer at data_ptr + idx*8; for a pointer WORD return it
    ;; directly (already a 32B box), for an immediate WORD materialise a
    ;; 32B view into a FRESH box (one per call -> no shared-scratch
    ;; clobber when results are held live).
    (defun nl_vector_slot_ptr (sexp-ptr idx)
      (let ((word (ptr-read-u64
                   (+ (ptr-read-u64 (ptr-read-u64 sexp-ptr 8) 8) (* idx 8))
                   0)))
        (if (= (logand word 1) 0)
            word
          (extern-call nl_val_load word (alloc-bytes 32 8))))))
  "AOT source for the `nl_vector_slot_ptr' Doc 147 Phase 2 helper.

Single-entry `(seq DEFUN)' manifest:
- `nl_vector_slot_ptr (sexp-ptr idx) -> *const Sexp' — derefs the
  `Sexp::Vector' payload at SEXP-PTR+8 to the NlVector box, reads
  Vec.data_ptr from box+8, loads the 8B tagged WORD at data_ptr + IDX*8,
  and returns a 32B-slot VIEW pointer: a pointer WORD passes through
  (already a 32B child box); an immediate WORD is materialised into a
  FRESH 32B box via the arity-2 `nl_val_load' keystone (one box per call
  avoids the shared-scratch clobber when a consumer holds two+ slot-ptr
  results live).

AOT ops consumed:
  `ptr-read-u64'   — three-hop: Sexp payload -> NlVector box -> data_ptr,
                     then the slot WORD load.
  `*' / `+'        — IDX*8 element WORD offset + data_ptr base.
  `logand'         — low-bit immediate/pointer tag test.
  `alloc-bytes'    — fresh 32B scratch box for the immediate path.
  `extern-call nl_val_load' — immediate WORD -> 32B Sexp view in box.
  `let'            — bind the loaded WORD.")

(provide 'nelisp-cc-nlvector-slot-ptr)

;;; nelisp-cc-nlvector-slot-ptr.el ends here
