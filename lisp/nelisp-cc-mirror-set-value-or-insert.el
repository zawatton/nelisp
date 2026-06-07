;;; nelisp-cc-mirror-set-value-or-insert.el --- Doc 119 §119.A mirror_set_value_or_insert  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 119 §119.A — pure-elisp port of `Env::mirror_set_value' that
;; absorbs the miss-path (= no fallback to Rust
;; `mirror_insert_new_entry').  Replaces helper #7
;; `nelisp_mirror_set_value' (hit-only) at the Rust call site once
;; §119.A ships.
;;
;; Algorithm:
;;   1. `mirror_lookup_entry' to locate the existing symbol-entry.
;;   2. On hit (entry-ptr != 0): `record-slot-set entry 0 value' and
;;      return 1.  Same fast-path as helper #7.
;;   3. On miss (entry-ptr = 0): allocate a fresh symbol-entry record
;;      via `nelisp_mirror_alloc_entry' (= 4 record-slot-set ops),
;;      then prepend `(KEY-STR . ENTRY-RECORD)' onto the appropriate
;;      bucket via `nelisp_mirror_bucket_prepend' (= hash + cons +
;;      vector-slot-set + count bump).  Return 1.
;;
;; Caller-owned scratch vector layout (= shared across all four Doc
;; 119 §119.A `_or_insert' wrappers, pre-filled by the Rust safe
;; wrapper):
;;
;;   slot 0   Sexp::Nil source                 — reused throughout.
;;   slot 1   inner-pair scratch (Nil)         — for bucket_prepend.
;;   slot 2   outer-cell scratch (Nil)         — for bucket_prepend.
;;   slot 3   count int scratch (Nil)          — for bucket_prepend.
;;   slot 4   KEY str scratch (Nil)            — for bucket_prepend.
;;   slot 5   Sexp::Symbol("symbol-entry")     — pre-filled tag.
;;   slot 6   entry record result (Nil)        — for alloc_entry.
;;   slot 7   value Sexp (= VAL_PARAM)         — pre-filled.
;;   slot 8   function Sexp (= unbound_marker) — pre-filled.
;;   slot 9   plist Sexp (= Sexp::Nil)         — pre-filled.
;;   slot 10  constant Sexp (= Sexp::Nil)      — pre-filled.
;;
;; For `set_value_or_insert' specifically: slot 7 holds VAL_PARAM and
;; slot 8 holds the `unbound_marker' (= the symbol's function-cell
;; default when auto-vivifying); slots 9/10 are `Sexp::Nil' (= no
;; plist, not a constant).
;;
;; Outer arity is 4 (even ✓) so body-entry rsp ≡ 0 mod 16, matching
;; the static rsp-alignment of `record-make' / `cons-make' /
;; `record-slot-set' (= Doc 124.F-blocker even-arity fix; see
;; `nelisp-cc-frame-push.el').  The `_dispatch' sub-defun is 5-arg
;; (= odd) so the runtime `sub rsp, 8' rsp-alignment kicks in via
;; the `nelisp-aot-compiler--current-defun-arity' dynvar; see
;; `nelisp-cc-frame-push.el' commentary for the pattern.  Doc 124.F-
;; blocker even-arity fix applies to defuns that *issue* alignment-
;; sensitive ops (= `record-make' / `cons-make'); the outer defun
;; here only `extern-call's into other helpers and stays clean.
;;
;; ABI deps satisfied:
;;   §100.A  `extern-call'  — `nelisp_mirror_lookup_entry' (helper #1),
;;                            `nelisp_mirror_alloc_entry' (§119.A),
;;                            `nelisp_mirror_bucket_prepend' (§119.A).
;;   §111.B  `record-slot-set' — hit-path slot 0 install.
;;   §111.C  `vector-ref-ptr'  — scratch slot pointer extraction.

;;; Code:

(defconst nelisp-cc-mirror-set-value-or-insert--source
  '(seq
    (defun nelisp_mirror_set_value_or_insert_dispatch
        (entry-ptr mirror-ptr sym-ptr scratch-vec-ptr _pad _pad2)
      ;; Dispatch on ENTRY-PTR being null (= miss) or live (= hit).
      ;; Returns: i64 — 1 on success.  The `_pad' / `_pad2' params keep
      ;; outer arity even (= 6) for body-entry rsp alignment.
      ;;
      ;; On hit: refcount-safe overwrite of the value slot (slot 0 of
      ;; the symbol-entry record).  `record-slot-set' materialises
      ;; rax = 1 (Doc 111 §111.E enabler) so the trailing `1' guarantees
      ;; the chain returns 1 regardless of `record-slot-set' sentinel
      ;; semantics.
      ;;
      ;; On miss: `nelisp_mirror_alloc_entry' builds the fresh
      ;; `symbol-entry' Record into ENTRY-SLOT, then
      ;; `nelisp_mirror_bucket_prepend' hashes NAME and prepends the
      ;; `(KEY-STR . ENTRY-RECORD)' bucket cell.  Both ops return 1.
      ;;
      ;; Doc 147 Phase 2 decouple: the entry result was written THROUGH
      ;; `(vector-ref-ptr scratch-vec-ptr 6)' (a Vector interior pointer)
      ;; and read back the same way.  Post-Phase-2 the Vector slot is an
      ;; 8B WORD and `vector-ref-ptr' returns a TRANSIENT materialised
      ;; view of the slot's word (a fresh box for the Nil slot), so the
      ;; `record-make' 32B write landed in a throwaway box and the
      ;; read-back saw Nil -> null entry -> SIGSEGV.  Fix: stage the new
      ;; entry in a fresh 32B STACK slot ENTRY-SLOT (`alloc-bytes 32 8'
      ;; on the per-eval arena), written by alloc_entry and read by
      ;; bucket_prepend (which CLONES it into the bucket cons via
      ;; `cons-set-cdr' -> `nl_val_clone_into', so the stack slot is
      ;; never a counted owner).  Mirrors the env-bind-local cell-slot
      ;; decoupling.  Scratch slot 6 is now unused on this path.
      (if (= entry-ptr 0)
          (let ((entry-slot (alloc-bytes 32 8)))
            (and
             (extern-call nelisp_mirror_alloc_entry
                          (vector-ref-ptr scratch-vec-ptr 5)  ; tag-sym
                          (vector-ref-ptr scratch-vec-ptr 7)  ; value
                          (vector-ref-ptr scratch-vec-ptr 8)  ; function
                          (vector-ref-ptr scratch-vec-ptr 9)  ; plist
                          (vector-ref-ptr scratch-vec-ptr 10) ; constant
                          entry-slot)                         ; result (stack)
             (extern-call nelisp_mirror_bucket_prepend
                          mirror-ptr sym-ptr
                          entry-slot                          ; entry (stack)
                          scratch-vec-ptr)))                  ; scratch
        (and (record-slot-set entry-ptr 0
                              (vector-ref-ptr scratch-vec-ptr 7))
             1)))
    (defun nelisp_mirror_set_value_or_insert
        (mirror-ptr sym-ptr scratch-vec-ptr _pad)
      ;; mirror-ptr:      *const Sexp pointing at the env-mirror Record.
      ;; sym-ptr:         *const Sexp pointing at Sexp::Symbol / Sexp::Str.
      ;; scratch-vec-ptr: *const Sexp pointing at the 11-slot scratch
      ;;                  vector (= layout in file commentary).
      ;; _pad:            unused — keeps outer arity even (= 4).
      ;;
      ;; Returns: i64 — 1 on success (hit-update OR miss-insert).
      (nelisp_mirror_set_value_or_insert_dispatch
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       mirror-ptr sym-ptr scratch-vec-ptr 0 0)))
  "AOT source for Doc 119 §119.A `mirror_set_value_or_insert'.

Composes `mirror_lookup_entry' (= helper #1) + on-hit `record-slot-set'
(= helper #7's fast path) + on-miss `mirror_alloc_entry' +
`mirror_bucket_prepend' (= §119.A's new helpers).  Absorbs the
Rust `mirror_insert_new_entry' + `mirror_prepend_to_bucket' miss
path into pure elisp.")

(provide 'nelisp-cc-mirror-set-value-or-insert)

;;; nelisp-cc-mirror-set-value-or-insert.el ends here
