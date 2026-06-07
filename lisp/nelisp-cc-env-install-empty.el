;;; nelisp-cc-env-install-empty.el --- Wave h env install_empty globals+frames  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave h — pure-elisp port of the three Rust helpers that initialise
;; an empty `Env' mirror + frame stack:
;;
;;   • `Env::make_fast_hash_table(bucket_count)' (private, 6 LOC)
;;   • `Env::install_empty_mirror_rust_direct' body (8 LOC, formerly
;;     called from `install_stage0')
;;   • `Env::install_empty_frames_record_rust_direct' (8 LOC, formerly
;;     called from `install_empty_mirror_rust_direct')
;;
;; All three bodies are deleted; `install_empty_mirror_rust_direct'
;; becomes a 5-LOC thin wrapper that sets `unbound_marker' in Rust and
;; then calls this AOT-compiled object.
;;
;; Algorithm implemented by `nelisp_env_install_empty_globals_frames':
;;
;;   1. Allocate a fresh `fast-hash-table' Record (3 slots) via
;;      `record-make' into scratch[3].
;;   2. Allocate a 1024-bucket Sexp::Vector via `vector-make' into
;;      scratch[4].
;;   3. Install buckets into ht.slot 1 via `record-slot-set'.
;;   4. Install Sexp::Int(1024) into ht.slot 0 (bucket-count).
;;   5. Install Sexp::Int(0) into ht.slot 2 (size).
;;   6. Allocate a fresh `nelisp-env' Record (3 slots) directly into
;;      *globals-out via `record-make'.
;;   7. Install the fast-hash-table into globals.slot 0.
;;      (slots 1, 2 stay Sexp::Nil = auto-set by record-make.)
;;   8. Allocate a fresh 8-element backing Sexp::Vector (Doc 147 P1.5:
;;      into its own fresh stack slot `backing-slot', NOT a raw-overwrite
;;      of a shared scratch slot — so the old 32KB scratch-ref leak is
;;      gone too).
;;   9. Allocate a fresh `nelisp-lexframe-stack' Record (2 slots)
;;      directly into *frames-out via `record-make'.
;;  10. Install the backing vector into frames.slot 0.
;;  11. Install Sexp::Int(0) into frames.slot 1 (depth).
;;
;; Scratch-slot layout: the Rust safe wrapper
;; `env_install_empty_globals_frames' in `build-tool/src/lib.rs'
;; allocates a Sexp::Vector of 6 slots and passes a pointer to it as
;; `scratch-ptr':
;;
;;   slot 0 — `Sexp::Symbol("nelisp-env")'            (globals type-tag)
;;   slot 1 — `Sexp::Symbol("fast-hash-table")'        (ht type-tag)
;;   slot 2 — `Sexp::Symbol("nelisp-lexframe-stack")'  (frames type-tag)
;;   slot 3 — (Doc 147 P1.5) no longer written; ht Record now in a fresh
;;            stack slot (`ht-slot').
;;   slot 4 — (Doc 147 P1.5) no longer written; buckets / backing Vectors
;;            now in fresh stack slots (`buckets-slot' / `backing-slot').
;;   slot 5 — (Doc 147 P1.5) no longer written; Sexp::Int writes now in a
;;            fresh stack slot (`int-slot').
;;
;; Refcount discipline (Doc 147 P1.5): the published box-tagged variants
;; (fast-ht Record / buckets+backing Vectors) are no longer staged in
;; scratch slots 3/4 — they are built into fresh stack slots, then
;; installed into globals-out / frames-out by the refcount-safe
;; `record-slot-set' (clone+own).  Each published box therefore has
;; exactly ONE live owner reachable from globals-out / frames-out after
;; the call; the stack staging slots add no owner-count (arena-reclaimed,
;; never refcount-dropped — exactly as the prior Vector-interior slots
;; were never iterated/dropped in this substrate).  See the per-step
;; re-derivation in the defun body.
;;
;; Alignment: outer defun arity = 4 (even) → post-prologue
;; rsp ≡ 0 mod 16, satisfying the `vector-make' / `record-make'
;; alignment requirement.
;;
;; ABI deps satisfied:
;;   §115.3  `record-make'        — fresh NlRecord allocator.
;;   §115.1  `vector-make'        — fresh NlVector allocator.
;;   §111.B  `record-slot-set'    — refcount-safe slot install.
;;   §100    `sexp-int-make'      — i64 → Sexp::Int materialise.
;;   §100    `vector-ref-ptr'     — extract slot ptr from scratch vec.

;;; Code:

(defconst nelisp-cc-env-install-empty--source
  '(defun nelisp_env_install_empty_globals_frames
       (globals-out frames-out scratch-ptr _pad)
     ;; globals-out: *mut Sexp — Env::globals_record field address.
     ;; frames-out:  *mut Sexp — Env::frames_record field address.
     ;; scratch-ptr: *const Sexp pointing at a Sexp::Vector with 6
     ;;              slots (layout above).  Rust safe wrapper owns it.
     ;; _pad:        i64 = 0 — alignment pad (keeps outer arity even).
     ;;
     ;; Returns: i64 from final `record-slot-set' (= 1 sentinel).
     ;; Doc 147 Phase 1.5 Group S — the scratch slots that received a
     ;; freshly-built 32B Sexp via a write-through-interior-pointer now
     ;; land in FRESH 32B stack slots (`alloc-bytes 32 8' on the per-eval
     ;; arena, OUTSIDE the scratch Vector buffer):
     ;;   slot 3 -> ht-slot       (`record-make' fast-hash-table Record)
     ;;   slot 4 -> buckets-slot  (`vector-make' 1024-bucket Vector)
     ;;             backing-slot   (`vector-make' 8-elem backing Vector;
     ;;                             now a SEPARATE slot — the old Step-8
     ;;                             raw-overwrite of scratch[4] is gone,
     ;;                             so the documented 32KB scratch-ref
     ;;                             "leak" cannot occur)
     ;;   slot 5 -> int-slot      (`sexp-int-make' 1024 / 0 / depth)
     ;; `record-make' / `vector-make' / `sexp-int-make' wrote a full 32B
     ;; Sexp at `data_ptr + N*32'; once the Phase-2 Vector buffer stride
     ;; shrinks to 8B those 32B writes would address the wrong slot and
     ;; overrun neighbours.  Scratch slots 0/1/2 (type-tag symbols,
     ;; pre-filled by the safe wrapper) stay `(vector-ref-ptr ...)' —
     ;; READ sources only, never WRITE destinations.
     ;;
     ;; Refcount re-derivation (published Records/Vectors keep their
     ;; refcount; only the throwaway STAGING moves to stack):
     ;;   `record-make' / `vector-make' return a fresh rc=1 box and store
     ;;   it raw into the destination slot (no rc bump).  The published
     ;;   owner is then materialised by the refcount-SAFE `record-slot-set'
     ;;   (= `nl_record_set_slot' -> `nl_sexp_clone_into', rc-bump for
     ;;   boxed variants):
     ;;     Step 3 ht.slot1     <- buckets-slot  (clone+own buckets vec)
     ;;     Step 7 globals.slot0 <- ht-slot       (clone+own ht record)
     ;;     Step 10 frames.slot0 <- backing-slot  (clone+own backing vec)
     ;;   The stack staging slots are arena-reclaimed and never
     ;;   refcount-dropped, so they add NO owner-count — exactly as the
     ;;   prior Vector-interior staging was never iterated/dropped in this
     ;;   substrate.  Each published box therefore has exactly ONE live
     ;;   owner reachable from globals-out / frames-out after the call.
     ;;   int-slot holds only `Sexp::Int' immediates (no box, no rc).  No
     ;;   new owner, no leak, no double-free; the original rc invariant
     ;;   (and its Step-12 neutraliser removal) is preserved, and the
     ;;   Step-8 scratch-overwrite leak is additionally eliminated.
     (let ((ht-slot (alloc-bytes 32 8))
           (buckets-slot (alloc-bytes 32 8))
           (backing-slot (alloc-bytes 32 8))
           (int-slot (alloc-bytes 32 8)))
       (and
        ;; Step 1: allocate fresh fast-hash-table record (3 slots).
        (record-make (vector-ref-ptr scratch-ptr 1)  ; ht type-tag sym
                     3
                     ht-slot)
        ;; Step 2: allocate 1024-bucket Sexp::Vector.
        (vector-make 1024 buckets-slot)
        ;; Step 3: ht.slot 1 = buckets vector.
        (record-slot-set ht-slot
                         1
                         buckets-slot)
        ;; Step 4: ht.slot 0 = Sexp::Int(1024) — bucket-count.
        (sexp-int-make int-slot 1024)
        (record-slot-set ht-slot
                         0
                         int-slot)
        ;; Step 5: ht.slot 2 = Sexp::Int(0) — size.
        (sexp-int-make int-slot 0)
        (record-slot-set ht-slot
                         2
                         int-slot)
        ;; Step 6: allocate nelisp-env record (3 slots) into *globals-out.
        (record-make (vector-ref-ptr scratch-ptr 0)  ; nelisp-env type-tag sym
                     3
                     globals-out)
        ;; Step 7: globals.slot 0 = fast-hash-table.
        ;; (slots 1, 2 stay Sexp::Nil; set by record-make.)
        (record-slot-set globals-out
                         0
                         ht-slot)
        ;; Step 8: allocate 8-element backing Sexp::Vector into backing-slot
        ;; (its own stack slot; no scratch-overwrite, no leak).
        (vector-make 8 backing-slot)
        ;; Step 9: allocate nelisp-lexframe-stack record (2 slots) into *frames-out.
        (record-make (vector-ref-ptr scratch-ptr 2)  ; nelisp-lexframe-stack type-tag
                     2
                     frames-out)
        ;; Step 10: frames.slot 0 = backing vector.
        (record-slot-set frames-out
                         0
                         backing-slot)
        ;; Step 11: frames.slot 1 = Sexp::Int(0) — depth.
        (sexp-int-make int-slot 0)
        (record-slot-set frames-out
                         1
                         int-slot))))
  "AOT source for Wave h `nelisp_env_install_empty_globals_frames'.

Builds a fresh empty globals mirror (nelisp-env Record with a 1024-bucket
fast-hash-table) and empty frame stack (nelisp-lexframe-stack Record with
8-element backing vector) in a single AOT-compiled call.

Replaces three deleted Rust helpers:
  - `Env::make_fast_hash_table' (6 LOC)
  - `Env::install_empty_mirror_rust_direct' body (8 LOC)
  - `Env::install_empty_frames_record_rust_direct' (8 LOC)

Net Rust delta: -22 LOC deleted + 5 LOC thin-wrapper additions = -17 LOC.

AOT ops consumed:
  `record-make'      — §115.3 fresh NlRecord allocator (3 calls).
  `vector-make'      — §115.1 fresh NlVector allocator (2 calls).
  `record-slot-set'  — §111.B refcount-safe slot install (6 calls).
  `sexp-int-make'    — §100 Sexp::Int materialise (3 calls).
  `vector-ref-ptr'   — §111.C scratch slot pointer reads.")

(provide 'nelisp-cc-env-install-empty)

;;; nelisp-cc-env-install-empty.el ends here
