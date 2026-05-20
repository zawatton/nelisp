;;; nelisp-cc-env-lookup-value.el --- Wave a-2: Env::lookup_value Phase 47 .o  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave a-2 — `Env::lookup_value' body migrated to Phase 47 elisp .o.
;; Replaces the 13-LOC Rust body in
;; `build-tool/src/eval/env_helpers.rs::Env::lookup_value'.
;;
;; Algorithm (= literal transcription of the Rust body):
;;
;;   1. Check frame stack first (innermost lexical binding wins):
;;      `nelisp_frame_stack_find(frames-ptr, name-ptr)' → i64 cell-ptr.
;;      If non-zero (= frame hit): extract value via `nl_cell_get_value'
;;      (= refcount-safe `c.value.clone()'), write to out-ptr, return 0.
;;
;;   2. Check mirror entry existence via `nelisp_mirror_lookup_entry'.
;;      If miss (= 0), return 1 (= unbound-var sentinel).
;;
;;   3. If mirror hit: call `nelisp_mirror_lookup_value' to fill out-ptr
;;      (refcount-safe via record-slot-ref → nl_sexp_clone_into, per
;;      Doc 111 §111.C v3 fix).  Return 0.
;;
;; *** Critical fix vs Wave a ***
;; Wave a used `(cell-value cell-ptr out-ptr)' — a raw 32-byte SIMD
;; copy WITHOUT incrementing refcounts.  This caused double-free /
;; SIGABRT when the lexical cell was later mutated (old value dropped,
;; caller still held the raw-copied Rc pointer → use-after-free).
;; This wave uses `(extern-call nl_cell_get_value cell-ptr out-ptr)'
;; which calls the Rust `nl_cell_get_value' helper that does
;; `c.value.clone()' — correct refcount increment.
;;
;; Signature:
;;   (nelisp_env_lookup_value MIRROR-PTR FRAMES-PTR NAME-PTR OUT-PTR)
;;     MIRROR-PTR  : *const Sexp — Env::globals_record.
;;     FRAMES-PTR  : *const Sexp — Env::frames_record.
;;     NAME-PTR    : *const Sexp — Sexp::Symbol name to look up.
;;     OUT-PTR     : *mut Sexp   — 32-byte caller-owned result slot.
;;   Returns: i64.  0 = found (value written to *out-ptr),
;;                  1 = unbound (out-ptr unchanged).
;;
;; ABI:
;;   All defun arities are even (2 or 4) — rsp ≡ 0 mod 16 at body ✓.
;;   Each defun has at most one extern-call in any execution path.
;;
;; ABI deps:
;;   nelisp_frame_stack_find      — lexical frame walk (0 = miss)
;;   nl_cell_get_value            — refcount-safe cell value clone (Rust)
;;   nelisp_mirror_lookup_entry   — mirror hit/miss check (0 = miss)
;;   nelisp_mirror_lookup_value   — mirror value fill (refcount-safe)

;;; Code:

(defconst nelisp-cc-env-lookup-value--source
  '(seq
    ;; nelisp_env_lkv_cell_hit
    ;;
    ;; Frame hit path: extract value from the lexical cell at cell-ptr
    ;; into out-ptr using `nl_cell_get_value' — a refcount-aware clone.
    ;;
    ;; cell-ptr: i64 = *const Sexp pointing at Sexp::Cell(_) (returned
    ;;           by `nelisp_frame_stack_find' as the cdr slot of the
    ;;           inner (NAME . CELL) pair in the frame's hash bucket).
    ;; out-ptr:  *mut Sexp — 32-byte result slot.
    ;;
    ;; `nl_cell_get_value' calls `match &*cell_ptr { Sexp::Cell(c) =>
    ;; { ptr::write(out, c.value.clone()); 0 } _ => 1 }' on the Rust
    ;; side — the clone() increments the Rc before writing into out-ptr,
    ;; preventing double-free when the cell is later mutated.
    ;;
    ;; Returns: i64 result of nl_cell_get_value (0=ok, 1=not-a-cell).
    ;; In practice cell-ptr was obtained from nelisp_frame_stack_find
    ;; which only returns non-zero for real Sexp::Cell-CDR pointers,
    ;; so the 1=not-a-cell path is unreachable in production.
    ;;
    ;; Arity 2 (even) ✓; extern-call is the top-level expression ✓.
    (defun nelisp_env_lkv_cell_hit (cell-ptr out-ptr)
      (extern-call nl_cell_get_value cell-ptr out-ptr))

    ;; nelisp_env_lkv_mirror
    ;;
    ;; Mirror path (called on frame miss): check entry existence then
    ;; fill out-ptr with value Sexp.  Returns 1 if unbound, 0 if found.
    ;;
    ;; mirror-ptr: *const Sexp — Env::globals_record.
    ;; name-ptr:   *const Sexp — symbol name sexp.
    ;; out-ptr:    *mut Sexp   — result slot.
    ;; _pad:       i64         — alignment padding (even arity).
    ;;
    ;; Arity 4 (even) ✓.
    ;; Two extern-calls, but each is in a different branch — only one
    ;; executes per path.  extern-call is arg 0 of `=' resp. arg 0 of
    ;; `and' ✓.
    (defun nelisp_env_lkv_mirror (mirror-ptr name-ptr out-ptr _pad)
      (if (= (extern-call nelisp_mirror_lookup_entry mirror-ptr name-ptr) 0)
          1
        (and (extern-call nelisp_mirror_lookup_value mirror-ptr name-ptr out-ptr)
             0)))

    ;; nelisp_env_lookup_value
    ;;
    ;; Main entry: check frame stack first (lexical scoping), then fall
    ;; through to the mirror (global scope).
    ;;
    ;; The `nelisp_frame_stack_find' call is duplicated in the false
    ;; branch (= frame hit path) to avoid the `let' binding constraint;
    ;; the second call traverses the same stack, hits the same bucket,
    ;; and returns the identical cell pointer.  This is the established
    ;; Phase 47 repeat-on-hit pattern (= identical to the double extern-
    ;; call in `nelisp_frame_stack_find_descend' and `nelisp_mirror_is_bound').
    ;;
    ;; Arity 4 (even) ✓.
    ;; Each execution path passes through at most one extern-call site in
    ;; this defun before delegating to a helper.  extern-call is arg 0
    ;; of `=' in both branches ✓.
    (defun nelisp_env_lookup_value (mirror-ptr frames-ptr name-ptr out-ptr)
      (if (= (extern-call nelisp_frame_stack_find frames-ptr name-ptr) 0)
          ;; Frame miss: check mirror.
          (nelisp_env_lkv_mirror mirror-ptr name-ptr out-ptr 0)
        ;; Frame hit: read cell value (refcount-safe).
        (nelisp_env_lkv_cell_hit
         (extern-call nelisp_frame_stack_find frames-ptr name-ptr)
         out-ptr))))
  "Phase 47 source for Wave a-2 `Env::lookup_value' body.

Three-defun CPS composition:
  `nelisp_env_lookup_value' — frame check (4 args, even); dispatches to
    cell-hit or mirror path.  extern-call `nelisp_frame_stack_find' is
    duplicated in the hit branch (same result, no let-binding needed).
  `nelisp_env_lkv_mirror' — mirror hit/miss (4 args, even); exactly one
    extern-call per branch ✓.
  `nelisp_env_lkv_cell_hit' — cell value extract (2 args, even);
    delegates to `nl_cell_get_value' for refcount-aware clone ✓.

Critical fix vs Wave a: `cell-value' (raw SIMD copy) replaced by
`(extern-call nl_cell_get_value ...)' which calls `c.value.clone()'
on the Rust side — refcount-safe, eliminates the double-free SIGABRT.")

(provide 'nelisp-cc-env-lookup-value)

;;; nelisp-cc-env-lookup-value.el ends here
