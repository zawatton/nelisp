;;; nelisp-cc-env-lookup-value.el --- Wave a-2: Env::lookup_value AOT .o  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave a-2 — `Env::lookup_value' body migrated to AOT elisp .o.
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
;;   3. On a nonalias mirror hit, clone slot 0 directly. On a genuine
;;      slot-4 alias, authenticate the Env/table/entry tags, resolve the
;;      bounded chain, recheck the requested frame class for the terminal
;;      symbol, then read its frame or mirror value. Legacy four-slot
;;      entries retain the nonalias path.
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
;;   nelisp_mirror_lookup_value   — retained mirror value helper

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
    (defun nelisp_env_alias_slot_candidate (entry-ptr _pad)
      ;; Allocation-free classification for ordinary value reads/writes.
      ;; 0=real alias, 1=malformed, 3=nonalias or legacy four-slot entry.
      (if (/= (sexp-tag entry-ptr) 12)
          1
        (if (<= (record-slot-count entry-ptr) 4)
            3
          (let ((target-ptr (record-slot-ref-ptr entry-ptr 4)))
            (if (= (sexp-tag target-ptr) 0) 3
              (if (= (sexp-tag target-ptr) 4) 0 1))))))

    (defun nelisp_env_alias_canonicalize
        (mirror-ptr entry-ptr name-ptr result-address-ptr)
      ;; RESULT-ADDRESS-PTR is raw u64 storage. Status 3 is the fast path:
      ;; no alias slot, including a legacy four-slot symbol-entry. On a real
      ;; alias, authenticate record tags against fixed names before invoking
      ;; the shared borrowed resolver. No Sexp clone escapes this helper.
      (ptr-write-u64 result-address-ptr 0 name-ptr)
      (if (/= (sexp-tag entry-ptr) 12)
          1
        (if (<= (record-slot-count entry-ptr) 4)
            3
          (let ((target-ptr (record-slot-ref-ptr entry-ptr 4)))
            (if (= (sexp-tag target-ptr) 0)
                3
              (if (/= (sexp-tag target-ptr) 4)
                  1
                (let ((env-tag (alloc-bytes 32 8))
                      (table-tag (alloc-bytes 32 8))
                      (entry-tag (alloc-bytes 32 8))
                      (table-ptr 0)
                      (status 1))
                  (sexp-write-nil env-tag)
                  (sexp-write-nil table-tag)
                  (sexp-write-nil entry-tag)
                  (if (and (= (sexp-tag mirror-ptr) 12)
                           (> (record-slot-count mirror-ptr) 0))
                      (seq
                       (record-type-tag mirror-ptr env-tag)
                       (if (= (nl_sp_eq_lit env-tag 10
                                           7290607012774962542 30318) 1)
                           (seq
                            (setq table-ptr
                                  (record-slot-ref-ptr mirror-ptr 0))
                            (if (and (= (sexp-tag table-ptr) 12)
                                     (> (record-slot-count table-ptr) 1))
                                (seq
                                 (record-type-tag table-ptr table-tag)
                                 (if (= (nl_sp_eq_lit table-tag 15
                                                     8314040931539181926
                                                     28548142445374824) 1)
                                     (seq
                                      (record-type-tag entry-ptr entry-tag)
                                      (if (= (nl_sp_eq_lit entry-tag 12
                                                          7290602597431212403
                                                          2037544046) 1)
                                          (setq status
                                                (extern-call
                                                 nelisp_mirror_alias_resolve_borrowed
                                                 mirror-ptr name-ptr env-tag
                                                 table-tag entry-tag
                                                 result-address-ptr))
                                        0))
                                   0))
                              0))
                         0))
                    0)
                  (dealloc-bytes entry-tag 32 8)
                  (dealloc-bytes table-tag 32 8)
                  (dealloc-bytes env-tag 32 8)
                  status)))))))

    (defun nelisp_env_variable_canonicalize
        (mirror-ptr frames-ptr entry-ptr name-ptr address _pad)
      ;; Alias resolution precedes the buffer-local redirect. Slot 5, when
      ;; present, is [CURRENT-BUFFER-SYMBOL ((BUFFER . CELL-SYMBOL) ...)].
      ;; Only borrowed object access occurs here; Lisp owns registration.
      (let ((status (nelisp_env_alias_canonicalize
                     mirror-ptr entry-ptr name-ptr address)))
        (if (or (= status 0) (= status 3))
            (let* ((canonical (ptr-read-u64 address 0))
                   (entry (extern-call nelisp_mirror_lookup_entry mirror-ptr canonical)))
              (if (and (/= entry 0) (> (record-slot-count entry) 5))
                  (let ((redirect (record-slot-ref-ptr entry 5)))
                    (if (and (= (sexp-tag redirect) 8) (= (vector-len redirect) 2))
                        (let ((buffer (alloc-bytes 32 8))
                              (locals (vector-ref-ptr redirect 1)) (found 0))
                          (sexp-write-nil buffer)
                          (if (= (nelisp_env_lookup_value
                                  mirror-ptr frames-ptr (vector-ref-ptr redirect 0) buffer) 0)
                              (while (and (= (sexp-tag locals) 7) (= found 0))
                                (let* ((pair (nl_cons_car_ptr locals))
                                       (key (nl_cons_car_ptr pair)))
                                  (if (and (= (sexp-tag key) (sexp-tag buffer))
                                           (= (ptr-read-u64 key 8) (ptr-read-u64 buffer 8)))
                                      (seq (ptr-write-u64 address 0 (nl_cons_cdr_ptr pair))
                                           (setq found 1))
                                    (setq locals (nl_cons_cdr_ptr locals))))) 0)
                          (dealloc-bytes buffer 32 8)) 0)) 0)
              0)
          status)))

    (defun nelisp_env_lkv_mirror_with_frames
        (mirror-ptr frames-ptr name-ptr out-ptr frame-mode _pad _pad2 _pad3)
      (let ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr name-ptr)))
        (if (= entry 0) 1
          (if (and (= (nelisp_env_alias_slot_candidate entry 0) 3)
                   (if (> (record-slot-count entry) 5)
                       (= (sexp-tag (record-slot-ref-ptr entry 5)) 0) 1))
              (and (record-slot-ref entry 0 out-ptr) 0)
          (let ((address (alloc-bytes 8 8)) (status 1))
            (setq status (nelisp_env_variable_canonicalize
                          mirror-ptr frames-ptr entry name-ptr address 0))
            (if (= status 0)
                (let* ((canonical (ptr-read-u64 address 0))
                       (cell (if (= frames-ptr 0) 0
                               (if (= frame-mode 1)
                                   (extern-call nelisp_frame_stack_find_kind frames-ptr canonical 1 0)
                                 (extern-call nelisp_frame_stack_find frames-ptr canonical)))))
                  (if (= cell 0)
                      (let ((terminal (extern-call nelisp_mirror_lookup_entry mirror-ptr canonical)))
                        (if (= terminal 0) (setq status 1)
                          (setq status (and (record-slot-ref terminal 0 out-ptr) 0))))
                    (setq status (extern-call nl_cell_get_value cell out-ptr)))) 0)
            (dealloc-bytes address 8 8) status)))))

    ;; Preserve the established four-argument global-mirror ABI. It cannot
    ;; inspect frames, so frame-aware callers use the eight-argument entry.
    (defun nelisp_env_lkv_mirror (mirror-ptr name-ptr out-ptr _pad)
      (nelisp_env_lkv_mirror_with_frames mirror-ptr 0 name-ptr out-ptr 0 0 0 0))

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
      (let* ((entry (extern-call nelisp_mirror_lookup_entry mirror-ptr name-ptr))
             (local-p (if (= entry 0) 0
                        (if (> (record-slot-count entry) 5)
                            (= (sexp-tag (record-slot-ref-ptr entry 5)) 8) 0)))
             ;; Lexical cells shadow locals. A default dynamic binding does
             ;; not shadow an existing local cell in another buffer.
             (cell-ptr (if (= local-p 1)
                           (extern-call nelisp_frame_stack_find_kind frames-ptr name-ptr 0 0)
                         (extern-call nelisp_frame_stack_find frames-ptr name-ptr))))
        (if (= cell-ptr 0)
            ;; Frame miss: check mirror.
            (nelisp_env_lkv_mirror_with_frames
             mirror-ptr frames-ptr name-ptr out-ptr 0 0 0 0)
          ;; Frame hit: read cell value (refcount-safe).
          (extern-call nl_cell_get_value cell-ptr out-ptr)))))
  "AOT source for Wave a-2 `Env::lookup_value' body.

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
