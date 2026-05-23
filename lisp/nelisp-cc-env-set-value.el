;;; nelisp-cc-env-set-value.el --- Wave a-2: Env::set_value Phase 47 .o  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave a-2 — `Env::set_value' body migrated to Phase 47 elisp .o.
;; Replaces the 10-LOC Rust body in
;; `build-tool/src/eval/env_helpers.rs::Env::set_value'.
;;
;; Algorithm (= literal transcription of the Rust body):
;;
;;   1. Constant guard: `nelisp_mirror_is_constant(mirror-ptr, name-ptr)'
;;      returns 1 if the symbol is marked constant.  If so, return 1
;;      (= setting-constant error sentinel).
;;
;;   2. Check frame stack: `nelisp_frame_stack_find(frames-ptr, name-ptr)'
;;      → i64 cell-ptr.  If non-zero (= lexical binding exists):
;;      use `cell-set-value(cell-ptr, val-ptr)' to overwrite it
;;      (refcount-safe via `nl_cell_set_value' Rust helper), return 0.
;;
;;   3. Mirror write (global variable path):
;;      `nelisp_mirror_set_value_or_insert(mirror-ptr, name-ptr,
;;       scratch-vec-ptr, 0)' — auto-vivifies if not present.
;;      Return 0.
;;
;; Signature:
;;   (nelisp_env_set_value MIRROR-PTR FRAMES-PTR NAME-PTR
;;                         VAL-PTR SCRATCH-PTR _PAD)
;;     MIRROR-PTR  : *const Sexp — Env::globals_record.
;;     FRAMES-PTR  : *const Sexp — Env::frames_record.
;;     NAME-PTR    : *const Sexp — Sexp::Symbol name to set.
;;     VAL-PTR     : *const Sexp — new value Sexp.
;;     SCRATCH-PTR : *const Sexp — 11-slot scratch vector (pre-built by
;;                                  the Rust thin wrapper, slot 7 = value,
;;                                  slot 8 = unbound_marker, slot 5 =
;;                                  symbol-entry tag).
;;     _PAD        : i64         — alignment padding (even arity).
;;   Returns: i64.  0 = ok, 1 = error (setting-constant).
;;
;; ABI:
;;   All defun arities are even (2, 4, or 6) — rsp ≡ 0 mod 16 ✓.
;;   Each defun has at most one extern-call per execution path.
;;
;; ABI deps:
;;   §111.E #6  nelisp_mirror_is_constant        — constant flag check
;;   §111.E #24 nelisp_frame_stack_find          — lexical frame walk
;;   §111.D     cell-set-value                   — NlCell refcount-safe write
;;   Doc 119    nelisp_mirror_set_value_or_insert — mirror write+vivify

;;; Code:

(defconst nelisp-cc-env-set-value--source
  '(seq
    ;; nelisp_env_setv_cell_hit
    ;;
    ;; Frame hit path: write val-ptr into the lexical NlCell at cell-ptr.
    ;;
    ;; cell-ptr: i64 = *const Sexp pointing at Sexp::Cell(_) (from
    ;;           nelisp_frame_stack_find — the CDR slot of the inner
    ;;           (NAME . CELL) pair).
    ;; val-ptr:  *const Sexp — new value.
    ;;
    ;; `cell-set-value' delegates to `nl_cell_set_value' on the Rust
    ;; side which calls `NlCellRef::set_value' (= refcount-aware drop +
    ;; write via `Sexp::clone' of the new value).  Always returns 0 (ok).
    ;;
    ;; Arity 2 (even) ✓.
    (defun nelisp_env_setv_cell_hit (cell-ptr val-ptr)
      (and (cell-set-value cell-ptr val-ptr) 0))

    ;; nelisp_env_setv_mirror
    ;;
    ;; Mirror write path (frame miss): write or vivify via
    ;; `nelisp_mirror_set_value_or_insert'.
    ;;
    ;; mirror-ptr:  *const Sexp.
    ;; name-ptr:    *const Sexp.
    ;; scratch-ptr: *const Sexp — pre-built scratch vector.
    ;; _pad:        i64.
    ;;
    ;; Arity 4 (even) ✓; extern-call is arg 0 of `and' ✓.
    (defun nelisp_env_setv_mirror (mirror-ptr name-ptr scratch-ptr _pad)
      (and (extern-call nelisp_mirror_set_value_or_insert
                        mirror-ptr name-ptr scratch-ptr 0)
           0))

    ;; nelisp_env_set_value
    ;;
    ;; Main entry: constant guard → frame check → mirror write.
    ;;
    ;; R11a CSE-hoist: bind `frame_stack_find' result once via `let'
    ;; (= let-rt frame slot) so both the miss-test `(= cell-ptr 0)' and
    ;; the hit-branch `cell-set-value' reuse the same i64 cell pointer.
    ;; Previous shape called `frame_stack_find' twice on the hit path
    ;; (= 2 FNV-1a hashes + 2 stack walks); hoisted shape pays 1.
    ;;
    ;; Arity 6 (even) ✓.
    ;; Execution paths:
    ;;   constant path: one extern-call (is_constant = 1), return 1.
    ;;   frame-miss path: two sequential extern-calls (is_constant=0,
    ;;     frame_stack_find=0), then delegate to mirror helper.
    ;;   frame-hit path: two sequential extern-calls (is_constant=0,
    ;;     frame_stack_find=non-zero bound via let-rt), then
    ;;     cell-set-value in helper consuming the bound cell-ptr.
    ;; In all paths the extern-calls are arg 0 of `=' resp. of the
    ;; let-binding value-form, consuming each result before the next
    ;; extern-call executes ✓.
    (defun nelisp_env_set_value
        (mirror-ptr frames-ptr name-ptr val-ptr scratch-ptr _pad)
      (if (= (extern-call nelisp_mirror_is_constant mirror-ptr name-ptr) 1)
          1
        (let ((cell-ptr (extern-call nelisp_frame_stack_find frames-ptr name-ptr)))
          (if (= cell-ptr 0)
              ;; Frame miss: write to mirror.
              (nelisp_env_setv_mirror mirror-ptr name-ptr scratch-ptr 0)
            ;; Frame hit: overwrite lexical cell (refcount-safe).
            (nelisp_env_setv_cell_hit cell-ptr val-ptr))))))
  "Phase 47 source for Wave a-2 `Env::set_value' body.

R11a (Doc 49 Wave 9): `let-rt' CSE hoist on the `frame_stack_find'
result.  Frame-hit path now pays 1 FNV-1a hash + 1 stack walk
instead of 2 (= second walk was the established repeat-on-hit
pattern, now obsoleted by the let-rt frame slot).  Semantics
bit-for-bit identical — both calls observed the same stack memory
and returned the same cell pointer.

Three-defun composition:
  `nelisp_env_set_value' — constant guard + let-bound frame check
    (6 args, even).  Sequential extern-calls in the non-constant
    path: `is_constant' (arg 0 of `='), then `frame_stack_find'
    (arg 0 of let-binding value-form); each result consumed before
    the next extern-call executes.
  `nelisp_env_setv_mirror' — mirror write (4 args, even); one
    extern-call into `nelisp_mirror_set_value_or_insert' ✓.
  `nelisp_env_setv_cell_hit' — cell overwrite (2 args, even);
    `cell-set-value' delegates to refcount-safe `nl_cell_set_value' ✓.

The SCRATCH-PTR arg carries the pre-built 11-slot scratch vector prepared
by the Rust thin wrapper (= `build_or_insert_scratch_vec(value, unbound,
Nil, Nil)' layout: slot 7 = value, slot 5 = symbol-entry tag-sym,
slot 8 = unbound_marker function cell).  `nelisp_mirror_set_value_or_insert'
reads slot 7 on both hit and miss paths.")

(provide 'nelisp-cc-env-set-value)

;;; nelisp-cc-env-set-value.el ends here
