;;; nelisp-cc-env-bind-local.el --- Wave b: Env::bind_local AOT .o  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave b — `Env::bind_local' body migrated to AOT elisp .o.
;; Replaces the 9-LOC Rust body in
;; `build-tool/src/eval/env_helpers.rs::Env::bind_local'.
;;
;; Algorithm (= literal transcription of the Rust body):
;;
;;   1. Read DEPTH from Env::frames_record slot 1 via
;;      `(sexp-int-unwrap (record-slot-ref-ptr frames-ptr 1))'.
;;      If 0 (= no active lexical frame): go to mirror path (step 3).
;;
;;   2. Frame path (DEPTH > 0):
;;      a. `cell-make val-ptr (vector-ref-ptr scratch-ptr 0)' —
;;         allocate a fresh NlCell holding `*val-ptr' (= refcount-safe
;;         clone) and write `Sexp::Cell(NlCellRef)' into scratch slot 0.
;;         Grammar op (not extern-call); uses `nl_alloc_cell' internally.
;;      b. `extern-call nelisp_frame_bind ...' — bind NAME → CELL in
;;         the innermost frame.  Uses scratch slots 0-3 as the four
;;         scratchpad pointers (cell/pair/outer/count).  Scratch slot 0
;;         now holds Sexp::Cell after cell-make; slots 1-3 are Nil.
;;      Returns 1.
;;
;;   3. Mirror path (DEPTH == 0):
;;      `extern-call nelisp_mirror_set_value_or_insert
;;         mirror-ptr name-ptr scratch-ptr 0'
;;      Scratch-ptr is the FULL 11-slot standard scratch starting at
;;      slot 0 (= Nil source).  Slot 7 = VAL_PARAM pre-filled by
;;      the Rust wrapper via `build_or_insert_scratch_vec'.
;;
;; *** Scratch reuse contract ***
;;
;; The frame and mirror paths are mutually exclusive, so the same
;; 11-slot scratch vector is used for BOTH:
;;
;;   Frame path writes:
;;     (Doc 147 P1.5) Sexp::Cell now built into a FRESH stack slot, not
;;       scratch slot 0 (was a 32B write-through-interior-pointer).
;;     slots 1-3 (pair/outer/count by nelisp_frame_bind)
;;   Mirror path reads:
;;     slot 5  Symbol("symbol-entry") — pre-filled by Rust wrapper
;;     slot 7  value                  — pre-filled by Rust wrapper
;;     slot 8  unbound_marker         — pre-filled by Rust wrapper
;;
;;   No conflict: slot 0 overwrite by cell-make is invisible to mirror
;;   path (never executed on the same call).  Slots 1-4 collision is
;;   also moot since frame path doesn't reach mirror_set_value_or_insert.
;;
;; *** ABI constraints ***
;;
;; `nelisp_env_bind_local' — main entry; uses only grammar ops for the
;; depth check; delegates to two intra-seq helpers (regular `call').
;; Zero `extern-call's ✓.
;;
;; `nelisp_env_bl_frame' — one `extern-call nelisp_frame_bind'; preceded
;; by `cell-make' (grammar op, not extern-call).  Exactly one extern-call
;; per execution path ✓.
;;
;; `nelisp_env_bl_mirror' — one `extern-call nelisp_mirror_set_value_or_insert' ✓.
;;
;; Signature:
;;   (nelisp_env_bind_local MIRROR-PTR FRAMES-PTR NAME-PTR VAL-PTR
;;                          SCRATCH-PTR _PAD)
;;     MIRROR-PTR  : *const Sexp — Env::globals_record.
;;     FRAMES-PTR  : *const Sexp — Env::frames_record.
;;     NAME-PTR    : *const Sexp — Sexp::Symbol name to bind.
;;     VAL-PTR     : *const Sexp — value Sexp to bind.
;;     SCRATCH-PTR : *const Sexp — pointer to an 11-slot Sexp::Vector
;;                                  (standard layout from
;;                                   `build_or_insert_scratch_vec').
;;     _PAD        : i64         — alignment padding (even arity).
;;   Returns: i64.  Always 1 (bind completed).
;;
;; Standard 11-slot scratch layout (= `build_or_insert_scratch_vec'):
;;   slot 0   Nil source     (→ Sexp::Cell on frame path via cell-make)
;;   slot 1   inner-pair     (→ pair slot for nelisp_frame_bind)
;;   slot 2   outer-cell     (→ outer slot for nelisp_frame_bind)
;;   slot 3   count scratch  (→ count slot for nelisp_frame_bind)
;;   slot 4   KEY scratch    (mirror path only)
;;   slot 5   Symbol("symbol-entry")  (mirror path — pre-filled)
;;   slot 6   entry result   (mirror path only)
;;   slot 7   value          (mirror path — pre-filled)
;;   slot 8   unbound_marker (mirror path — pre-filled)
;;   slot 9   plist (Nil)
;;   slot 10  constant (Nil)
;;
;; ABI deps:
;;   §111.B  `record-slot-ref-ptr' — depth read from frames_record.
;;   §100    `sexp-int-unwrap'     — i64 depth payload.
;;   §111.D  `cell-make'           — fresh NlCell allocation (grammar op).
;;   §111.C  `vector-ref-ptr'      — scratch slot pointer extraction.
;;   nelisp_frame_bind             — frame-side bind (extern-call to .o).
;;   Doc 119 nelisp_mirror_set_value_or_insert — mirror write+vivify.

;;; Code:

(defconst nelisp-cc-env-bind-local--source
  '(seq
    ;; nelisp_env_bl_frame
    ;;
    ;; Frame path: allocate NlCell containing VAL-PTR, then bind the
    ;; resulting Sexp::Cell in the innermost lexical frame.
    ;;
    ;; frames-ptr:  *const Sexp — Env::frames_record.
    ;; name-ptr:    *const Sexp — symbol name.
    ;; val-ptr:     *const Sexp — initial value.
    ;; scratch-ptr: *const Sexp — 11-slot standard scratch vector.
    ;;
    ;; Doc 147 Phase 1.5 Group S — the freshly-made Sexp::Cell no longer
    ;; lands in scratch slot 0 via a write-through-interior-pointer.
    ;; `cell-make' wrote a 32B `Sexp::Cell' straight through
    ;; `(vector-ref-ptr scratch-ptr 0)' (= data_ptr + 0*32); once the
    ;; Phase-2 Vector buffer stride shrinks 32B->8B, that 32B write would
    ;; stomp scratch slots 1-3 (the very pad slots `nelisp_frame_bind'
    ;; needs).  Fix: `cell-make' writes into a fresh 32B stack slot
    ;; CELL-SLOT (= `alloc-bytes 32 8' on the per-eval arena, NOT inside
    ;; the shrinking Vector buffer), and that same CELL-SLOT is passed as
    ;; the `cell-ptr' arg to `nelisp_frame_bind' (which only READS it =
    ;; clones the cell into the innermost frame).  Scratch slots 1-3 stay
    ;; `(vector-ref-ptr scratch-ptr N)' — `nelisp_frame_bind' OWNS those
    ;; writes (separate consumer; decoupled in its own file), so they are
    ;; out of scope here.
    ;;
    ;; Refcount: unchanged.  `cell-make' clones `*val-ptr' into a fresh
    ;; rc=1 NlCell exactly as before; the destination slot (stack vs
    ;; Vector interior) does not affect the cell's refcount — the stack
    ;; slot is arena-reclaimed, never refcount-dropped, and the cell's
    ;; sole counted owner remains the frame binding installed by
    ;; `nelisp_frame_bind'.  No new owner, no leak, no double-free.
    ;;
    ;; Arity 4 (even) ✓.
    ;; One extern-call in the single execution path ✓.
    (defun nelisp_env_bl_frame (frames-ptr name-ptr val-ptr scratch-ptr)
      (let ((cell-slot (alloc-bytes 32 8)))
        (and
         (cell-make val-ptr cell-slot)
         (extern-call nelisp_frame_bind
                      frames-ptr name-ptr
                      cell-slot
                      (vector-ref-ptr scratch-ptr 1)
                      (vector-ref-ptr scratch-ptr 2)
                      (vector-ref-ptr scratch-ptr 3))
         1)))

    ;; nelisp_env_bl_mirror
    ;;
    ;; Mirror path: write VAL to the symbol's global binding via
    ;; `nelisp_mirror_set_value_or_insert', auto-vivifying if needed.
    ;; Passes scratch-ptr (= 11-slot standard vector) directly; slot 7
    ;; = value and slot 8 = unbound_marker were pre-filled by Rust.
    ;;
    ;; mirror-ptr:  *const Sexp — Env::globals_record.
    ;; name-ptr:    *const Sexp — symbol name.
    ;; scratch-ptr: *const Sexp — 11-slot standard scratch.
    ;; _pad:        i64 — even-arity padding.
    ;;
    ;; Arity 4 (even) ✓.
    ;; One extern-call per execution path ✓.
    (defun nelisp_env_bl_mirror (mirror-ptr name-ptr scratch-ptr _pad)
      (and (extern-call nelisp_mirror_set_value_or_insert
                        mirror-ptr name-ptr scratch-ptr 0)
           1))

    ;; nelisp_env_bind_local
    ;;
    ;; Main entry: depth check → frame or mirror path.
    ;;
    ;; Arity 6 (even) ✓.
    ;; No extern-call in this defun ✓.
    (defun nelisp_env_bind_local
        (mirror-ptr frames-ptr name-ptr val-ptr scratch-ptr _pad)
      (if (= (sexp-int-unwrap (record-slot-ref-ptr frames-ptr 1)) 0)
          (nelisp_env_bl_mirror mirror-ptr name-ptr scratch-ptr 0)
        (nelisp_env_bl_frame frames-ptr name-ptr val-ptr scratch-ptr))))
  "AOT source for Wave b `Env::bind_local' body.

Three-defun CPS composition (all in same seq = intra-seq calls):
  `nelisp_env_bind_local' — depth check + dispatch (6 args, even).
  `nelisp_env_bl_frame' — cell-make + extern-call nelisp_frame_bind
    (4 args, even); scratch slots 0-3 used for frame_bind.
  `nelisp_env_bl_mirror' — extern-call nelisp_mirror_set_value_or_insert
    (4 args, even); standard 11-slot scratch passed verbatim.

Scratch reuse: one 11-slot standard vector (= build_or_insert_scratch_vec)
serves both paths.  Frame path overwrites slots 0-3; mirror path reads
slots 5/7/8.  Paths are mutually exclusive — no collision.

nelisp_frame_bind is a compiled elisp symbol from `nelisp_frame_bind.o'.")

(provide 'nelisp-cc-env-bind-local)

;;; nelisp-cc-env-bind-local.el ends here
