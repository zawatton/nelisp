;;; nelisp-cc-nlrecord-slot-ptr.el --- nl_record_slot_ptr AOT helper  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 147 Phase 2 — the word->32B-slot materialiser for the Record
;; `record-slot-ref-ptr' / `record-slot-ptr-core' read path.  Sibling
;; of `nelisp-cc-nlvector-slot-ptr.el'; identical shape modulo the
;; Vec.data_ptr offset (NlRecord + 40, vs NlVector + 8) — the NlRecord
;; slots Vec header lives at box+32 and its data_ptr field at +40.
;;
;; After the Phase 2 shrink the NlRecord SLOTS BUFFER holds one 8-byte
;; tagged WORD per slot (was a 32B inline `Sexp'); the inline
;; `type_tag' Sexp @ box+0 STAYS 32B (header offsets unchanged).  Every
;; post-Phase-1.5 consumer of `record-slot-ref-ptr' is READ-ONLY and
;; reads a 32B `Sexp' from the returned pointer, so the emit must
;; MATERIALISE a 32B-slot VIEW of the slot's 8B word.
;;
;; SCRATCH-LIFETIME HAZARD (see the Vector sibling for the full note):
;; consumers may hold two+ slot-ptr results live at once
;; (e.g. `nl_jit_sxhash' / `nl_fmt_sexp_dispatch' recursion, frame-pop's
;; `(record-slot-ref-ptr frames-ptr 1)' reads), so this helper allocs a
;; FRESH 32B box PER immediate slot (per-eval arena, bulk-reclaimed) —
;; two results never alias.  A pointer slot needs no box.
;;
;; nl_record_slot_ptr(sexp_ptr, idx) -> *const Sexp:
;;   box_ptr  = *(u64*)(sexp_ptr + 8)       // Sexp::Record payload (NlRecord*)
;;   data_ptr = *(u64*)(box_ptr + 40)       // slots Vec.data_ptr (box+40)
;;   word     = *(u64*)(data_ptr + idx*8)   // the 8B tagged WORD slot
;;   - pointer WORD (low bit 0): return WORD directly (32B child box).
;;   - immediate WORD (low bit 1): alloc a FRESH 32B box, materialise
;;     via `nl_val_load(word, box)' (Phase 0 keystone), return the box.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' manifest entry +
;; the standalone link manifest in `scripts/nelisp-standalone-build.el'.

;;; Code:

(defconst nelisp-cc-nlrecord-slot-ptr--source
  '(seq
    ;; nl_record_slot_ptr(sexp-ptr, idx) -> *const Sexp.  SEXP-PTR points
    ;; at a `Sexp::Record' slot whose payload@+8 is the NlRecord box; the
    ;; slots Vec.data_ptr lives at box+40 (slots Vec header @ box+32,
    ;; data_ptr @ +8).  Read the slot's 8B tagged WORD from the slots
    ;; buffer at data_ptr + idx*8; pointer WORD passes through, immediate
    ;; WORD materialises into a FRESH box (one per call -> no
    ;; shared-scratch clobber).
    (defun nl_record_slot_ptr (sexp-ptr idx)
      (let ((word (ptr-read-u64
                   (+ (ptr-read-u64 (ptr-read-u64 sexp-ptr 8) 40) (* idx 8))
                   0)))
        (if (= (logand word 1) 0)
            word
          (extern-call nl_val_load word (alloc-bytes 32 8))))))
  "AOT source for the `nl_record_slot_ptr' Doc 147 Phase 2 helper.

Single-entry `(seq DEFUN)' manifest:
- `nl_record_slot_ptr (sexp-ptr idx) -> *const Sexp' — derefs the
  `Sexp::Record' payload at SEXP-PTR+8 to the NlRecord box, reads slots
  Vec.data_ptr from box+40, loads the 8B tagged WORD at data_ptr + IDX*8,
  and returns a 32B-slot VIEW pointer: a pointer WORD passes through
  (already a 32B child box); an immediate WORD is materialised into a
  FRESH 32B box via the arity-2 `nl_val_load' keystone (one box per call
  avoids the shared-scratch clobber when a consumer holds two+ slot-ptr
  results live).

AOT ops consumed:
  `ptr-read-u64'   — three-hop: Sexp payload -> NlRecord box -> data_ptr,
                     then the slot WORD load.
  `*' / `+'        — IDX*8 slot WORD offset + data_ptr base.
  `logand'         — low-bit immediate/pointer tag test.
  `alloc-bytes'    — fresh 32B scratch box for the immediate path.
  `extern-call nl_val_load' — immediate WORD -> 32B Sexp view in box.
  `let'            — bind the loaded WORD.")

(provide 'nelisp-cc-nlrecord-slot-ptr)

;;; nelisp-cc-nlrecord-slot-ptr.el ends here
