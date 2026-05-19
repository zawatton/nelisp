;;; nelisp-cc-nlrecord-alloc.el --- nl_alloc_record Phase 47 migration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_alloc_record' body in
;; `build-tool/src/eval/nlrecord.rs' with a Phase 47-compiled elisp
;; object.  Mechanical sibling of `nelisp-cc-nlvector-alloc.el' with
;; an additional type_tag Sexp clone step.
;;
;; NlRecord layout (pinned by `#[repr(C)]' + compile-time asserts):
;;
;;   type_tag: Sexp    @ 0   (32 bytes)
;;   slots: Vec<Sexp>  @ 32  (24 bytes)
;;     Vec.capacity    @ 32  (8 bytes — element count)
;;     Vec.data_ptr    @ 40  (8 bytes — *mut Sexp buffer)
;;     Vec.len         @ 48  (8 bytes — element count)
;;   refcount          @ 56  (8 bytes — AtomicUsize)
;;   total = 64 bytes, align = 8
;;
;; Vec field order is `(capacity, data_ptr, len)' — same convention
;; as NlVector.  `nelisp-nlrecord--offset-slots-capacity = 40' is the
;; data pointer field (named from a pre-merge `(ptr, cap, len)'
;; assumption; the Phase 47 compiler comment in
;; `--emit-record-slot-ptr-core' documents the corrected layout).
;;
;; Three-helper + public entry structure mirrors `nelisp-cc-nlvector-alloc.el':
;;
;;   nl_alloc_record_fill (data-ptr i n) — recursive Nil-fill loop for
;;     the slots Vec element buffer.
;;
;;   nl_alloc_record_build (box-ptr data-ptr tag-ptr n) — writes all
;;     five words of the NlRecord struct:
;;       [box-ptr +  0..32] = clone of *tag-ptr  (type_tag Sexp)
;;       [box-ptr + 32] = n     (Vec.capacity count)
;;       [box-ptr + 40] = data-ptr (Vec.data_ptr)
;;       [box-ptr + 48] = n     (Vec.len)
;;       [box-ptr + 56] = 1     (refcount)
;;
;;   nl_alloc_record_with_data (box-ptr data-ptr tag-ptr n) — chains
;;     fill + build.
;;
;;   nl_alloc_record (type-tag-ptr slot-count) — public entry.
;;
;; `extern-call nl_sexp_clone_into tag-ptr box-ptr' writes a
;; refcount-aware clone of *tag-ptr into box-ptr+0 (= the 32-byte
;; type_tag field).  The destination is uninitialized (= freshly
;; allocated), matching `nl_sexp_clone_into''s contract ("*dst treated
;; as uninit").
;;
;; The `(or ... 1)' wrapper around the extern-call guarantees the `and'
;; chain sees a non-zero value even if rax is 0 after the Rust-unit
;; return (= same idiom as `nelisp-cc-nlcell-alloc.el').
;;
;; Build wiring: `scripts/compile-elisp-objects.el' manifest entry;
;; `build.rs' compiles into `nl_alloc_record.o' and archives it.
;; The Rust `nlrecord.rs' body is deleted entirely — integration tests
;; use `NlRecordRef::new', not the raw extern.

;;; Code:

(defconst nelisp-cc-nlrecord-alloc--source
  '(seq
    ;; Recursive Nil-fill loop for the slots buffer.
    ;; Identical shape to `nl_alloc_vector_fill'; modulo variable names.
    (defun nl_alloc_record_fill (data-ptr i n)
      (if (< i n)
          (and (sexp-write-nil (+ data-ptr (* i 32)))
               (nl_alloc_record_fill data-ptr (+ i 1) n))
        data-ptr))

    ;; Write all NlRecord fields and return box-ptr.
    ;;
    ;;   [box-ptr +  0..32] = clone of *tag-ptr  (type_tag Sexp, 32 bytes)
    ;;   [box-ptr + 32] = n        — Vec<Sexp>.capacity (element count)
    ;;   [box-ptr + 40] = data-ptr — Vec<Sexp>.data_ptr
    ;;   [box-ptr + 48] = n        — Vec<Sexp>.len
    ;;   [box-ptr + 56] = 1        — NlRecord.refcount
    ;;
    ;; `ptr-write-u64' returns 1, so the `and' chain never short-circuits.
    ;; The `(or ... 1)' around extern-call guards against undefined rax
    ;; from the Rust-unit `nl_sexp_clone_into' return.
    (defun nl_alloc_record_build (box-ptr data-ptr tag-ptr n)
      (and (or (extern-call nl_sexp_clone_into tag-ptr box-ptr) 1)
           (ptr-write-u64 box-ptr 32 n)
           (ptr-write-u64 box-ptr 40 data-ptr)
           (ptr-write-u64 box-ptr 48 n)
           (ptr-write-u64 box-ptr 56 1)
           box-ptr))

    ;; Bridge: fill slots buffer, then build the struct.
    (defun nl_alloc_record_with_data (box-ptr data-ptr tag-ptr n)
      (nl_alloc_record_build
       box-ptr
       (nl_alloc_record_fill data-ptr 0 n)
       tag-ptr
       n))

    ;; Public entry.  Clamps negative slot-count to 0, allocates the
    ;; 64-byte NlRecord struct and the n*32-byte slot element buffer,
    ;; then delegates to `nl_alloc_record_with_data'.
    (defun nl_alloc_record (type-tag-ptr slot-count)
      (nl_alloc_record_with_data
       (alloc-bytes 64 8)
       (alloc-bytes (* (if (< slot-count 0) 0 slot-count) 32) 8)
       type-tag-ptr
       (if (< slot-count 0) 0 slot-count))))
  "Phase 47 source for the `nl_alloc_record' allocator swap.

Four-entry `(seq DEFUN ...)' manifest:
- `nl_alloc_record_fill (data-ptr i n) -> data-ptr' — recursive
  Nil-fill loop for the slots Vec element buffer.
- `nl_alloc_record_build (box-ptr data-ptr tag-ptr n) -> box-ptr' —
  writes type_tag (via nl_sexp_clone_into) + Vec header + refcount.
- `nl_alloc_record_with_data (box-ptr data-ptr tag-ptr n) -> box-ptr'
  — chains fill + build.
- `nl_alloc_record (type-tag-ptr slot-count) -> *mut NlRecord' —
  public entry.

Phase 47 ops consumed:
  `alloc-bytes'                    — 64B struct + n*32B slot buffer.
  `extern-call nl_sexp_clone_into' — clones type_tag into box-ptr+0.
  `sexp-write-nil'                 — tags each slot with SEXP_TAG_NIL.
  `ptr-write-u64'                  — 4 writes for Vec header + refcount.
  `if' / `<' / `*' / `+'          — control flow + arithmetic.

Net Rust delta: deletes the 15-LOC `nl_alloc_record' body + safety
comment from `build-tool/src/eval/nlrecord.rs'.  No Rust shim needed
because `nlrecord_integration.rs' uses `NlRecordRef::new', not the raw
`nl_alloc_record' extern.")

(provide 'nelisp-cc-nlrecord-alloc)

;;; nelisp-cc-nlrecord-alloc.el ends here
