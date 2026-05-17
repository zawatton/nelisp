;;; nelisp-cc-frame-push.el --- Doc 111 §111.E #21 frame_push_rust_direct  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #21 — `frame_push_rust_direct'.  Doc
;; 115 §115.3 — full pure-elisp implementation of the lexframe push.
;; Replaces the prior Rust shim `nl_frame_push' (= deleted from
;; `build-tool/src/eval/env_lexframe_phase47_shims.rs').
;;
;; Algorithm (= literal transcription of the Rust `nl_frame_push'
;; impl):
;;
;;   1. Allocate fresh fast-hash-table record (3 slots) via
;;      `record-make' (§115.3 new grammar op).
;;   2. Allocate 16-bucket vector via `vector-make' (§115.1).
;;   3. Install buckets vector into ht.slot 1.
;;   4. Materialise `Sexp::Int(16)' and install into ht.slot 0
;;      (bucket-count, used by `mirror_fnv1a' bucket-index).
;;   5. Materialise `Sexp::Int(0)' and install into ht.slot 2 (size).
;;   6. Allocate fresh `nelisp-lexframe' record (1 slot) via
;;      `record-make'.
;;   7. Install fast-hash-table into frame.slot 0.
;;   8. Call `nelisp_frame_stack_ensure_capacity' to grow backing if
;;      needed (= depth+1 capacity).
;;   9. Install the fresh frame into backing[depth] via
;;      `vector-slot-set' (refcount-aware clone).
;;  10. Materialise `Sexp::Int(depth+1)' and install into
;;      frames-ptr.slot 1 (depth bump).
;;
;; Scratch-slot layout: the Rust safe wrapper in
;; `build-tool/src/lib.rs::frame_push' allocates a Sexp::Vector of 7
;; slots and passes a pointer to it as `scratch-vec-ptr':
;;
;;   slot 0 — `Sexp::Symbol("nelisp-lexframe")'   (type-tag for frame).
;;   slot 1 — `Sexp::Symbol("fast-hash-table")'   (type-tag for ht).
;;   slot 2 — Sexp::Nil — scratch for `ensure_capacity'.
;;   slot 3 — Sexp::Nil — scratch for the bucket Sexp::Vector.
;;   slot 4 — Sexp::Nil — scratch for the fast-hash-table Sexp::Record.
;;   slot 5 — Sexp::Nil — scratch for the lexframe Sexp::Record.
;;   slot 6 — Sexp::Nil — reusable scratch for Sexp::Int writes
;;                        (bucket-count, size, new depth).
;;
;; All scratches that end up holding box-tagged variants
;; (Sexp::Vector / Sexp::Record) are refcount-balanced when the
;; scratch vector is dropped at safe-wrapper return: each box has
;; refcount = 2 by then (one ref from the scratch slot, one ref from
;; the published location inside `frames-ptr'); dropping the scratch
;; vector decrements to 1, leaving the published owner intact.  Slot
;; 6 only holds Sexp::Int values (no box), so refcount discipline is
;; trivially preserved when it is overwritten between int writes.
;;
;; Outer-defun arity is 3 (frames-ptr / scratch-vec-ptr / _pad) — odd
;; arity keeps post-prologue rsp = 8 (mod 16), which matches the
;; static alignment assumption baked into `vector-make' /
;; `record-make' (= 2 net live pushes at the call site → call rsp = 0
;; mod 16).  `record-slot-set' / `vector-slot-set' / `extern-call'
;; aligns rsp dynamically and thus tolerate either body-entry
;; alignment.  `_pad' is unused but its presence is what flips body
;; alignment to the odd-arity branch.
;;
;; ABI deps satisfied:
;;   §111.B  `record-slot-set'     — refcount-safe slot install.
;;   §111.C  `vector-ref-ptr'      — extract slot ptr from scratch vec.
;;   §111.E  `vector-slot-set'     — refcount-safe backing[depth] write.
;;   §100    `sexp-int-make'       — i64 → `Sexp::Int' materialise.
;;   §100    `sexp-int-unwrap'     — i64 payload read for depth.
;;   §115.1  `vector-make'         — fresh `NlVector' allocator (16 buckets).
;;   §115.3  `record-make'         — fresh `NlRecord' allocator (NEW).
;;   §100.A  `extern-call'         — `nelisp_frame_stack_ensure_capacity'.

;;; Code:

(defconst nelisp-cc-frame-push--source
  '(defun nelisp_frame_push (frames-ptr scratch-vec-ptr _pad)
     ;; frames-ptr:      *const Sexp pointing at Env::frames_record (=
     ;;                  Sexp::Record(`nelisp-lexframe-stack')).
     ;; scratch-vec-ptr: *const Sexp pointing at a Sexp::Vector with 7
     ;;                  slots (= layout above).  Safe wrapper owns the
     ;;                  Sexp; this op only reads slot pointers via
     ;;                  `vector-ref-ptr'.
     ;; _pad:            unused — kept for outer-defun odd-arity
     ;;                  alignment (see commentary).
     ;;
     ;; Returns: i64 — 1 on push (= `(and ...)' threads all sub-ops
     ;; through to the final `record-slot-set' which materialises rax=1).
     (and
      ;; Step 1: allocate fresh fast-hash-table record (3 slots).
      (record-make (vector-ref-ptr scratch-vec-ptr 1) ; ht-sym
                   3
                   (vector-ref-ptr scratch-vec-ptr 4))
      ;; Step 2: allocate 16-bucket Sexp::Vector.
      (vector-make 16 (vector-ref-ptr scratch-vec-ptr 3))
      ;; Step 3: install buckets into ht.slot 1.
      (record-slot-set (vector-ref-ptr scratch-vec-ptr 4)
                       1
                       (vector-ref-ptr scratch-vec-ptr 3))
      ;; Step 4: ht.slot 0 = Sexp::Int(16) — bucket-count.
      (sexp-int-make (vector-ref-ptr scratch-vec-ptr 6) 16)
      (record-slot-set (vector-ref-ptr scratch-vec-ptr 4)
                       0
                       (vector-ref-ptr scratch-vec-ptr 6))
      ;; Step 5: ht.slot 2 = Sexp::Int(0) — size.
      (sexp-int-make (vector-ref-ptr scratch-vec-ptr 6) 0)
      (record-slot-set (vector-ref-ptr scratch-vec-ptr 4)
                       2
                       (vector-ref-ptr scratch-vec-ptr 6))
      ;; Step 6: allocate fresh nelisp-lexframe record (1 slot).
      (record-make (vector-ref-ptr scratch-vec-ptr 0) ; lex-sym
                   1
                   (vector-ref-ptr scratch-vec-ptr 5))
      ;; Step 7: install fast-hash-table into frame.slot 0.
      (record-slot-set (vector-ref-ptr scratch-vec-ptr 5)
                       0
                       (vector-ref-ptr scratch-vec-ptr 4))
      ;; Step 8: ensure backing capacity >= depth+1.  Side-effect only;
      ;; threaded through `and' via the truthy i64 return.
      (extern-call nelisp_frame_stack_ensure_capacity
                   frames-ptr
                   (+ (sexp-int-unwrap
                       (record-slot-ref-ptr frames-ptr 1))
                      1)
                   (vector-ref-ptr scratch-vec-ptr 2))
      ;; Step 9: install fresh frame into backing[depth].
      (vector-slot-set
       (record-slot-ref-ptr frames-ptr 0) ; backing vector
       (sexp-int-unwrap (record-slot-ref-ptr frames-ptr 1)) ; depth
       (vector-ref-ptr scratch-vec-ptr 5)) ; frame
      ;; Step 10: depth bump — frames.slot 1 = Sexp::Int(depth+1).
      (sexp-int-make (vector-ref-ptr scratch-vec-ptr 6)
                     (+ (sexp-int-unwrap
                         (record-slot-ref-ptr frames-ptr 1))
                        1))
      (record-slot-set frames-ptr
                       1
                       (vector-ref-ptr scratch-vec-ptr 6))))
  "Phase 47 source for Doc 111 §111.E #21 / Doc 115 §115.3
`frame_push_rust_direct'.

Pure-elisp lexframe push.  Composes `record-make' (§115.3 new),
`vector-make' (§115.1), `record-slot-set' / `record-slot-ref-ptr'
(§111.B), `vector-slot-set' / `vector-ref-ptr' (§111.C / §111.E),
`sexp-int-make' / `sexp-int-unwrap' (§100), and an `extern-call' to
`nelisp_frame_stack_ensure_capacity' (the §115.1 elisp port).

Replaces the ~60 LOC Rust shim `nl_frame_push' which has been removed
from `env_lexframe_phase47_shims.rs'.  The safe wrapper in
`build-tool/src/lib.rs::Spike::frame_push' allocates the 7-slot
scratch vector and the two type-tag symbol Sexps before calling
this helper; see the file commentary for the slot layout and refcount
discipline.")

(provide 'nelisp-cc-frame-push)

;;; nelisp-cc-frame-push.el ends here
