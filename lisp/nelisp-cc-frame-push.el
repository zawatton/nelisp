;;; nelisp-cc-frame-push.el --- Doc 111 §111.E #21 frame_push_rust_direct  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #21 — `frame_push_rust_direct'.  Doc
;; 115 §115.3 — full pure-elisp implementation of the lexframe push.
;; Replaces the prior Rust shim `nl_frame_push' (= deleted from
;; `build-tool/src/eval/env_lexframe_aot_shims.rs').
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
;;   slot 3 — (Doc 147 P1.5) no longer written; bucket Vector now in a
;;            fresh stack slot (`buckets-slot').
;;   slot 4 — (Doc 147 P1.5) no longer written; ht Record now in a fresh
;;            stack slot (`ht-slot').
;;   slot 5 — (Doc 147 P1.5) no longer written; lexframe Record now in a
;;            fresh stack slot (`frame-slot').
;;   slot 6 — (Doc 147 P1.5) no longer written; Sexp::Int writes now in a
;;            fresh stack slot (`int-slot').
;;
;; Doc 147 P1.5 refcount note: the published box-tagged variants
;; (Sexp::Vector / Sexp::Record) are no longer staged in scratch
;; slots 3/4/5 — they are built into fresh stack slots, then installed
;; into the published location inside `frames-ptr' by the refcount-safe
;; publish ops (`record-slot-set' clone+own / `vector-slot-set'
;; raw-copy own).  Each published box therefore has exactly ONE live
;; owner reachable from `frames-ptr' after the call; the stack staging
;; slot adds no owner-count (it is arena-reclaimed, never
;; refcount-dropped — exactly as the prior Vector-interior slot was
;; never iterated/dropped in this substrate).  See the per-step
;; re-derivation in the defun body.
;;
;; Outer-defun arity is 2 (frames-ptr / scratch-vec-ptr) — even arity
;; keeps post-prologue rsp ≡ 0 mod 16, which matches the static
;; alignment assumption baked into `vector-make' / `record-make' /
;; `vector-slot-set' (= 2 net live pushes at the call site → call rsp
;; = 0 mod 16).  `record-slot-set' aligns rsp dynamically (= `and rsp,
;; -16' pattern) and thus tolerates either body-entry alignment;
;; `extern-call' adds a runtime `sub rsp, 8' when the outer defun's
;; arity is odd via the `nelisp-aot-compiler--current-defun-arity'
;; dynvar.
;;
;; HISTORY: the original Doc 115 §115.3 ship used arity 3 with a `_pad'
;; arg.  That commentary mis-reasoned the alignment: odd-arity body
;; entry is rsp ≡ 8 mod 16 (= NOT 0), and `vector-make' / `record-make'
;; do 2 pushes from body-entry which keeps the mod-16 parity
;; (8 - 16 ≡ 8 mod 16, still misaligned).  The resulting SysV ABI
;; violation manifested as a SIGSEGV inside `nl_alloc_record' /
;; `nl_alloc_vector' (= those callees use `movaps' for their stack
;; locals, which require 16-byte aligned rsp).  Doc 124.F-blocker.
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
  '(defun nelisp_frame_push (frames-ptr scratch-vec-ptr)
     ;; frames-ptr:      *const Sexp pointing at Env::frames_record (=
     ;;                  Sexp::Record(`nelisp-lexframe-stack')).
     ;; scratch-vec-ptr: *const Sexp pointing at a Sexp::Vector with 7
     ;;                  slots (= layout above).  Safe wrapper owns the
     ;;                  Sexp; this op only reads slot pointers via
     ;;                  `vector-ref-ptr'.
     ;;
     ;; Returns: i64 — 1 on push (= `(and ...)' threads all sub-ops
     ;; through to the final `record-slot-set' which materialises rax=1).
     ;; Doc 147 Phase 1.5 Group S — the four scratch slots that received
     ;; a freshly-built 32B Sexp via a write-through-interior-pointer now
     ;; land in FRESH 32B stack slots (`alloc-bytes 32 8' on the per-eval
     ;; arena, OUTSIDE the scratch Vector buffer):
     ;;   slot 4 -> ht-slot      (`record-make' fast-hash-table Record)
     ;;   slot 3 -> buckets-slot (`vector-make' 16-bucket Vector)
     ;;   slot 5 -> frame-slot   (`record-make' nelisp-lexframe Record)
     ;;   slot 6 -> int-slot     (`sexp-int-make' bucket-count/size/depth)
     ;; `record-make' / `vector-make' / `sexp-int-make' wrote a full 32B
     ;; Sexp at `data_ptr + N*32'; once the Phase-2 Vector buffer stride
     ;; shrinks to 8B those 32B writes would address the wrong slot and
     ;; overrun neighbours.  Every later READ of each value now uses the
     ;; same stack slot, so the scratch Vector interior is never a WRITE
     ;; destination.  Scratch slots 0/1 (type-tag symbols, pre-filled by
     ;; the safe wrapper) and slot 2 (ensure_capacity pad) stay
     ;; `(vector-ref-ptr scratch-vec-ptr N)' — READ sources / callee pad
     ;; only.
     ;;
     ;; Refcount re-derivation (the published Vectors/Records keep their
     ;; refcount; only the throwaway STAGING moves to stack):
     ;;   `record-make' / `vector-make' return a fresh rc=1 box and write
     ;;   it into the destination slot WITHOUT bumping rc (raw 32B store).
     ;;   Whether that destination is a Vector interior slot or a stack
     ;;   slot is irrelevant to rc — neither is refcount-dropped (the
     ;;   Vector interior was arena-reclaimed too; this substrate never
     ;;   iterated the scratch slots to drop them).  The published owner
     ;;   is materialised by the refcount-SAFE publish ops:
     ;;     Step 3 `record-slot-set ht.slot1 <- buckets'  (clone+own buckets)
     ;;     Step 7 `record-slot-set frame.slot0 <- ht'     (clone+own ht)
     ;;     Step 9 `vector-slot-set backing[depth] <- frame' (raw-copy own)
     ;;   These remain the sole owner-count source, so each published box
     ;;   has exactly one live owner reachable from `frames-ptr' after the
     ;;   call.  The stack staging slot adds NO owner-count (it is not a
     ;;   counted reference), identical to the prior Vector-interior slot
     ;;   which was likewise never decremented.  Step-6 (`int-slot') holds
     ;;   only `Sexp::Int' immediates (no box, no rc).  No new owner, no
     ;;   leak, no double-free; invariant preserved.
     (let ((ht-slot (alloc-bytes 32 8))
           (buckets-slot (alloc-bytes 32 8))
           (frame-slot (alloc-bytes 32 8))
           (int-slot (alloc-bytes 32 8)))
       (and
        ;; Step 1: allocate fresh fast-hash-table record (3 slots).
        (record-make (vector-ref-ptr scratch-vec-ptr 1) ; ht-sym
                     3
                     ht-slot)
        ;; Step 2: allocate 16-bucket Sexp::Vector.
        (vector-make 16 buckets-slot)
        ;; Step 3: install buckets into ht.slot 1.
        (record-slot-set ht-slot
                         1
                         buckets-slot)
        ;; Step 4: ht.slot 0 = Sexp::Int(16) — bucket-count.
        (sexp-int-make int-slot 16)
        (record-slot-set ht-slot
                         0
                         int-slot)
        ;; Step 5: ht.slot 2 = Sexp::Int(0) — size.
        (sexp-int-make int-slot 0)
        (record-slot-set ht-slot
                         2
                         int-slot)
        ;; Step 6: allocate fresh nelisp-lexframe record (1 slot).
        (record-make (vector-ref-ptr scratch-vec-ptr 0) ; lex-sym
                     1
                     frame-slot)
        ;; Step 7: install fast-hash-table into frame.slot 0.
        (record-slot-set frame-slot
                         0
                         ht-slot)
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
         frame-slot) ; frame
        ;; Step 10: depth bump — frames.slot 1 = Sexp::Int(depth+1).
        (sexp-int-make int-slot
                       (+ (sexp-int-unwrap
                           (record-slot-ref-ptr frames-ptr 1))
                          1))
        (record-slot-set frames-ptr
                         1
                         int-slot))))
  "AOT source for Doc 111 §111.E #21 / Doc 115 §115.3
`frame_push_rust_direct'.

Pure-elisp lexframe push.  Composes `record-make' (§115.3 new),
`vector-make' (§115.1), `record-slot-set' / `record-slot-ref-ptr'
(§111.B), `vector-slot-set' / `vector-ref-ptr' (§111.C / §111.E),
`sexp-int-make' / `sexp-int-unwrap' (§100), and an `extern-call' to
`nelisp_frame_stack_ensure_capacity' (the §115.1 elisp port).

Replaces the ~60 LOC Rust shim `nl_frame_push' which has been removed
from `env_lexframe_aot_shims.rs'.  The safe wrapper in
`build-tool/src/lib.rs::Spike::frame_push' allocates the 7-slot
scratch vector and the two type-tag symbol Sexps before calling
this helper; see the file commentary for the slot layout and refcount
discipline.")

(provide 'nelisp-cc-frame-push)

;;; nelisp-cc-frame-push.el ends here
