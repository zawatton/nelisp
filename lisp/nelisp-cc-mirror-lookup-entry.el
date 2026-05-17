;;; nelisp-cc-mirror-lookup-entry.el --- Doc 111 §111.E #1 mirror_lookup_entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group A helper #1 — `mirror_lookup_entry'.  Phase 47
;; reimplementation of the env-mirror hash-bucket walk that currently
;; lives in Rust at `build-tool/src/eval/env_mirror.rs::mirror_lookup_entry'.
;; This object replaces the inner walk; the Rust impl is kept alive
;; alongside until all Group A/B helpers ship (= the extern wrapper
;; that dispatches into the Phase 47 `.o' replaces the Rust function
;; body in a follow-up commit after the full Group lands).
;;
;; Layout (= `lisp/nelisp-env.el' + `nelisp-stdlib-fast-hash.el'):
;;   globals_record = Sexp::Record(`nelisp-env')
;;     slots[0] = Sexp::Record(`fast-hash-table')
;;       slots[0] = Sexp::Int (= bucket count, power-of-2)
;;       slots[1] = Sexp::Vector (= buckets, each elt an alist)
;;       slots[2] = Sexp::Int (= entry count)
;;   Bucket alist cell: (Sexp::Cons (Sexp::Cons (Sexp::Str . Sexp::Record(`symbol-entry'))) . NEXT)
;;
;; ABI deps satisfied:
;;   §111.B  `record-slot-ref-ptr'  — slot pointer through env-record → ht-record.
;;   §111.C  `vector-ref-ptr'       — bucket pointer at FNV1a-hashed index.
;;   §101.B  `cons-walk' primitives — `sexp-payload-ptr' (= bucket / pair box-ptr extraction)
;;                                  + `cons-cdr-raw-from-box' (= next-bucket walk).
;;   §101.C  `str-eq'               — byte-payload equality between bucket KEY (Sexp::Str)
;;                                    and the lookup symbol (Sexp::Symbol).
;;   §100.A  `extern-call'          — `nl_mirror_fnv1a_sexp' for the hash (the bare
;;                                    `mirror_fnv1a' tight loop stays in Rust per
;;                                    Doc 111 §3.E Group C).
;;
;; Power-of-2 bucket-count assumption: the bootstrap mirror created by
;; `install_empty_mirror_rust_direct' always allocates 1024 buckets and
;; `mirror_prepend_to_bucket' never resizes, so the elisp body uses the
;; cheap `(h & (count-1))' mask matching the Rust fast path.

;;; Code:

(defconst nelisp-cc-mirror-lookup-entry--source
  '(seq
    (defun nelisp_mirror_walk_bucket (box-ptr sym-ptr)
      ;; Tail-recursive walk over a bucket's NlConsBox* chain.
      ;;
      ;;   box-ptr: i64.  0 means end-of-bucket.  Otherwise a live
      ;;            `NlConsBox*' (= bucket cell whose car is the
      ;;            (KEY . ENTRY) pair, cdr is the next bucket cell).
      ;;   sym-ptr: `*const Sexp' pointing at the Sexp::Symbol /
      ;;            Sexp::Str being looked up.
      ;;
      ;; Returns: i64.  On hit, the `*const Sexp' of the matching
      ;; ENTRY (= the Sexp slot one NlConsBox-pair-cdr deep, holding
      ;; `Sexp::Record(NlRecordRef)').  On miss / end-of-bucket, 0.
      ;;
      ;; `box-ptr' itself, treated as `*const Sexp', addresses the
      ;; bucket cell's CAR slot — the PAIR Sexp.  `sexp-payload-ptr'
      ;; checks PAIR's tag and returns the inner NlConsBox* of the
      ;; (KEY . ENTRY) pair, or 0 for any non-Cons tag (= malformed
      ;; bucket entry, treated as end-of-list).  The inner pair box
      ;; b2 starts with the KEY Sexp at offset 0 and the ENTRY Sexp
      ;; at offset 32, so `(+ b2 32)' is the entry's `*const Sexp'.
      (if (= box-ptr 0)
          0
        (if (= (str-eq (sexp-payload-ptr box-ptr) sym-ptr) 1)
            (+ (sexp-payload-ptr box-ptr) 32)
          (nelisp_mirror_walk_bucket
           (cons-cdr-raw-from-box box-ptr)
           sym-ptr))))
    (defun nelisp_mirror_lookup_entry (mirror-ptr sym-ptr)
      ;; mirror-ptr: *const Sexp pointing at the env-mirror Record
      ;;             (= `globals_record', tag `nelisp-env').
      ;; sym-ptr:    *const Sexp pointing at the Sexp::Symbol /
      ;;             Sexp::Str to look up.
      ;;
      ;; Returns: i64.  On hit, the `*const Sexp' of the symbol-entry
      ;; Record (slot 1 of the bucket's inner (KEY . ENTRY) cons).
      ;; On miss / empty mirror, 0.
      ;;
      ;; Pre-conditions (= caller / dispatcher responsibility, mirrors
      ;; the Rust impl's early-`return None' arms):
      ;;   - mirror-ptr.tag = Sexp::Record.
      ;;   - mirror-ptr.slots[0].tag = Sexp::Record (= fast-hash-table).
      ;;   - ht_rec.slots[0].tag = Sexp::Int (= bucket-count, power of 2).
      ;;   - ht_rec.slots[1].tag = Sexp::Vector (= buckets).
      (nelisp_mirror_walk_bucket
       (sexp-payload-ptr
        (vector-ref-ptr
         (record-slot-ref-ptr (record-slot-ref-ptr mirror-ptr 0) 1)
         (logand
          (extern-call nl_mirror_fnv1a_sexp sym-ptr)
          (- (sexp-int-unwrap
              (record-slot-ref-ptr (record-slot-ref-ptr mirror-ptr 0) 0))
             1))))
       sym-ptr)))
  "Phase 47 source for Doc 111 §111.E #1 `mirror_lookup_entry'.

Composes record-slot-ref-ptr (§111.B) + vector-ref-ptr (§111.C) +
cons-walk primitives (§101.B) + str-eq (§101.C) + extern-call into
`nl_mirror_fnv1a_sexp' (§100.A) to walk the env-mirror fast-hash-
table without materialising any intermediate Sexp slot (= every
hop is a raw `*const Sexp' / NlConsBox* pointer in rax).")

(provide 'nelisp-cc-mirror-lookup-entry)

;;; nelisp-cc-mirror-lookup-entry.el ends here
