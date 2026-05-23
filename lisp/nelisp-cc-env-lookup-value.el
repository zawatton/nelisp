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
    ;; nelisp_env_lkv_mirror
    ;;
    ;; Mirror path (called on frame miss): check entry existence then
    ;; fill out-ptr with value Sexp.  Returns 1 if unbound, 0 if found.
    ;;
    ;; R11a CSE-hoist: bind the entry pointer once via `let' (= let-rt
    ;; frame slot) and read slot 0 directly via `record-slot-ref' on
    ;; the hit path — bypassing the `nelisp_mirror_lookup_value'
    ;; wrapper (which would re-hash).  2 hashes → 1 per call on hit;
    ;; semantics identical (= `record-slot-ref' uses the same refcount-
    ;; safe `nl_sexp_clone_into' as the wrapper).
    (defun nelisp_env_lkv_mirror (mirror-ptr name-ptr out-ptr _pad)
      (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr name-ptr)))
        (if (= entry 0)
            1
          (and (record-slot-ref entry 0 out-ptr) 0))))

    ;; nelisp_env_lookup_value
    ;;
    ;; Main entry: check frame stack first (lexical scoping), then fall
    ;; through to the mirror (global scope).
    ;;
    ;; R11a CSE-hoist: bind `frame_stack_find' result once via `let' so
    ;; both the miss-test `(= cell-ptr 0)' and the hit-path
    ;; `nl_cell_get_value' reuse the same i64 cell pointer.  Previous
    ;; shape called `frame_stack_find' twice (once for the if-test,
    ;; once for the cell-hit dispatch).  On frame-hit: 1 hash instead
    ;; of 2.  On frame-miss + mirror-hit: 2 hashes instead of 3.
    (defun nelisp_env_lookup_value (mirror-ptr frames-ptr name-ptr out-ptr)
      (let ((cell-ptr (extern-call nelisp_frame_stack_find frames-ptr name-ptr)))
        (if (= cell-ptr 0)
            ;; Frame miss: check mirror.
            (nelisp_env_lkv_mirror mirror-ptr name-ptr out-ptr 0)
          ;; Frame hit: read cell value (refcount-safe).
          (extern-call nl_cell_get_value cell-ptr out-ptr)))))
  "Phase 47 source for Wave a-2 `Env::lookup_value' body.

R11a (Doc 49 Wave 9): two-tier `let-rt' CSE hoist:
  1. `nelisp_env_lookup_value' binds the `frame_stack_find' result
     once; the miss-test and the cell-hit `nl_cell_get_value' share
     the same i64 pointer (= 2 hashes → 1 on frame-hit path).
  2. `nelisp_env_lkv_mirror' binds the `mirror_lookup_entry' result
     once and reads slot 0 directly via `record-slot-ref' (= bypasses
     the `nelisp_mirror_lookup_value' wrapper, 2 hashes → 1 on
     mirror-hit).  `record-slot-ref' delegates to `nl_sexp_clone_into'
     for the same refcount-safe clone semantics as the wrapper.

The previous CPS `nelisp_env_lkv_cell_hit' helper was inlined since
the only call site now consumes a let-bound cell pointer that's
already on the stack — no need for a separate hop.

Critical fix vs Wave a (retained): `cell-value' (raw SIMD copy) is
still replaced by `(extern-call nl_cell_get_value ...)' which calls
`c.value.clone()' on the Rust side — refcount-safe, eliminates the
double-free SIGABRT.")

(provide 'nelisp-cc-env-lookup-value)

;;; nelisp-cc-env-lookup-value.el ends here
