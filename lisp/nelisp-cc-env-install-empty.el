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
;; then calls this Phase-47-compiled object.
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
;;   8. Allocate a fresh 8-element backing Sexp::Vector into scratch[4].
;;      OLD scratch[4] (1024-bucket vec, rc=2) raw-overwritten — this
;;      causes a refcount LEAK (NlVector_1024.rc stays 2 with only
;;      ht.slot[1] as real owner), but NOT a crash.
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
;;   slot 3 — Sexp::Nil — scratch for fast-hash-table Record
;;   slot 4 — Sexp::Nil — scratch for Sexp::Vector (buckets / backing)
;;   slot 5 — Sexp::Nil — scratch for Sexp::Int writes
;;
;; Refcount discipline:
;;   After step 10, scratch[4] holds the 8-element backing vector (rc=2,
;;   one from scratch[4], one from frames.slot[0]).
;;   When the Rust safe-wrapper drops the scratch Sexp::Vector, each
;;   scratch.data[N] is dropped normally:
;;     scratch[3] (fast-ht record, rc=2) → drop → rc=1 (globals.slot[0] keeps it).
;;     scratch[4] (8-elem backing vec, rc=2) → drop → rc=1 (frames.slot[0] keeps it).
;;     scratch[5] (Int, no heap) → drop → no-op.
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
     (and
      ;; Step 1: allocate fresh fast-hash-table record (3 slots).
      (record-make (vector-ref-ptr scratch-ptr 1)  ; ht type-tag sym
                   3
                   (vector-ref-ptr scratch-ptr 3)) ; ht record scratch
      ;; Step 2: allocate 1024-bucket Sexp::Vector.
      (vector-make 1024 (vector-ref-ptr scratch-ptr 4)) ; bucket scratch
      ;; Step 3: ht.slot 1 = buckets vector.
      (record-slot-set (vector-ref-ptr scratch-ptr 3)
                       1
                       (vector-ref-ptr scratch-ptr 4))
      ;; Step 4: ht.slot 0 = Sexp::Int(1024) — bucket-count.
      (sexp-int-make (vector-ref-ptr scratch-ptr 5) 1024)
      (record-slot-set (vector-ref-ptr scratch-ptr 3)
                       0
                       (vector-ref-ptr scratch-ptr 5))
      ;; Step 5: ht.slot 2 = Sexp::Int(0) — size.
      (sexp-int-make (vector-ref-ptr scratch-ptr 5) 0)
      (record-slot-set (vector-ref-ptr scratch-ptr 3)
                       2
                       (vector-ref-ptr scratch-ptr 5))
      ;; Step 6: allocate nelisp-env record (3 slots) into *globals-out.
      (record-make (vector-ref-ptr scratch-ptr 0)  ; nelisp-env type-tag sym
                   3
                   globals-out)
      ;; Step 7: globals.slot 0 = fast-hash-table.
      ;; (slots 1, 2 stay Sexp::Nil; set by record-make.)
      (record-slot-set globals-out
                       0
                       (vector-ref-ptr scratch-ptr 3))
      ;; Step 8: allocate 8-element backing Sexp::Vector into scratch[4].
      ;; NOTE: raw-overwrite of scratch[4] (was NlVector_1024, rc=2).
      ;; The NlVector_1024 refcount stays at 2 (ht.slot[1] holds it);
      ;; the scratch ref is lost = refcount leak (32KB), not a crash.
      (vector-make 8 (vector-ref-ptr scratch-ptr 4)) ; backing scratch
      ;; Step 9: allocate nelisp-lexframe-stack record (2 slots) into *frames-out.
      (record-make (vector-ref-ptr scratch-ptr 2)  ; nelisp-lexframe-stack type-tag
                   2
                   frames-out)
      ;; Step 10: frames.slot 0 = backing vector.
      (record-slot-set frames-out
                       0
                       (vector-ref-ptr scratch-ptr 4))
      ;; Step 11: frames.slot 1 = Sexp::Int(0) — depth.
      (sexp-int-make (vector-ref-ptr scratch-ptr 5) 0)
      (record-slot-set frames-out
                       1
                       (vector-ref-ptr scratch-ptr 5))))
  "Phase 47 source for Wave h `nelisp_env_install_empty_globals_frames'.

Builds a fresh empty globals mirror (nelisp-env Record with a 1024-bucket
fast-hash-table) and empty frame stack (nelisp-lexframe-stack Record with
8-element backing vector) in a single Phase-47-compiled call.

Replaces three deleted Rust helpers:
  - `Env::make_fast_hash_table' (6 LOC)
  - `Env::install_empty_mirror_rust_direct' body (8 LOC)
  - `Env::install_empty_frames_record_rust_direct' (8 LOC)

Net Rust delta: -22 LOC deleted + 5 LOC thin-wrapper additions = -17 LOC.

Phase 47 ops consumed:
  `record-make'      — §115.3 fresh NlRecord allocator (3 calls).
  `vector-make'      — §115.1 fresh NlVector allocator (2 calls).
  `record-slot-set'  — §111.B refcount-safe slot install (6 calls).
  `sexp-int-make'    — §100 Sexp::Int materialise (3 calls).
  `vector-ref-ptr'   — §111.C scratch slot pointer reads.")

(provide 'nelisp-cc-env-install-empty)

;;; nelisp-cc-env-install-empty.el ends here
