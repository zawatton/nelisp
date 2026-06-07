;;; nelisp-cc-mirror-lookup-entry.el --- Doc 111 §111.E #1 mirror_lookup_entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group A helper #1 — `mirror_lookup_entry'.  AOT
;; reimplementation of the env-mirror hash-bucket walk that currently
;; lives in Rust at `build-tool/src/eval/env_mirror.rs::mirror_lookup_entry'.
;; This object replaces the inner walk; the Rust impl is kept alive
;; alongside until all Group A/B helpers ship (= the extern wrapper
;; that dispatches into the AOT `.o' replaces the Rust function
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
;;   §100.A  `extern-call'          — `nelisp_fnv1a' for the hash (Doc 115
;;                                    §115.7 pure-elisp 32-bit FNV-1a;
;;                                    the previous `nl_mirror_fnv1a_sexp'
;;                                    Rust extern + `mirror_fnv1a' free fn
;;                                    have been deleted).
;;
;; Power-of-2 bucket-count assumption: the bootstrap mirror created by
;; `install_empty_mirror_rust_direct' always allocates 1024 buckets and
;; `mirror_prepend_to_bucket' never resizes, so the elisp body uses the
;; cheap `(h & (count-1))' mask matching the Rust fast path.

;;; Code:

(defconst nelisp-cc-mirror-lookup-entry--source
  '(seq
    (defun nelisp_mirror_walk_bucket (cell-ptr sym-ptr)
      ;; Tail-recursive walk over a bucket's cons chain.
      ;;
      ;; Doc 147 Phase 3 — the NlConsBox car / cdr are now 8-byte tagged
      ;; WORDS (not 32B inline Sexps), so the bucket cell can no longer be
      ;; read as a raw box with car @ box+0 / cdr @ box+32.  The walk now
      ;; carries the bucket cell's 32B-slot Sexp VIEW and uses the Phase 3
      ;; materialising accessors `nl_cons_car_ptr' / `nl_cons_cdr_ptr':
      ;;
      ;;   cell-ptr: `*const Sexp' — a Cons (tag 7) bucket cell whose car
      ;;             is the (KEY . ENTRY) PAIR, cdr is the next bucket
      ;;             cell (or Nil at end-of-bucket).  0 / non-Cons = end.
      ;;   sym-ptr:  `*const Sexp' for the Sexp::Symbol / Sexp::Str key.
      ;;
      ;; Returns: i64.  On hit, the `*const Sexp' 32B-slot view of the
      ;; matching ENTRY (= the PAIR's cdr, `Sexp::Record(NlRecordRef)').
      ;; On miss / end-of-bucket, 0.
      ;;
      ;;   pair-view = nl_cons_car_ptr(cell)   // the (KEY . ENTRY) PAIR
      ;;   key-view  = nl_cons_car_ptr(pair)   // KEY -> str-eq vs sym-ptr
      ;;   hit       -> nl_cons_cdr_ptr(pair)  // ENTRY view (live box)
      ;;   miss      -> recurse nl_cons_cdr_ptr(cell)  // next bucket cell
      ;;
      ;; The materialising accessors return the live 32B child box for a
      ;; pointer slot (bucket cells / pairs / entries are all boxed), and
      ;; a fresh-per-call 32B box only for the rare immediate slot, so two
      ;; concurrently-live views never alias (no shared-scratch clobber
      ;; under the recursive descent).
      (if (= cell-ptr 0)
          0
        (if (= (sexp-tag cell-ptr) 7)
            (let ((pair-view (extern-call nl_cons_car_ptr cell-ptr)))
              (if (= (str-eq (extern-call nl_cons_car_ptr pair-view) sym-ptr) 1)
                  (extern-call nl_cons_cdr_ptr pair-view)
                (nelisp_mirror_walk_bucket
                 (extern-call nl_cons_cdr_ptr cell-ptr)
                 sym-ptr)))
          0)))
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
      ;; Doc 147 Phase 3: seed the walk with the bucket-head Sexp VIEW
      ;; (`vector-ref-ptr' = the 32B-slot view of the bucket slot — a
      ;; Cons for a non-empty bucket, Nil for an empty one), NOT the raw
      ;; `sexp-payload-ptr' box (the walker now reads car/cdr WORDS via
      ;; the materialising accessors, so it needs the Sexp VIEW).
      (nelisp_mirror_walk_bucket
       (vector-ref-ptr
        (record-slot-ref-ptr (record-slot-ref-ptr mirror-ptr 0) 1)
        (logand
         (extern-call nelisp_fnv1a sym-ptr)
         (- (sexp-int-unwrap
             (record-slot-ref-ptr (record-slot-ref-ptr mirror-ptr 0) 0))
            1)))
       sym-ptr)))
  "AOT source for Doc 111 §111.E #1 `mirror_lookup_entry'.

Composes record-slot-ref-ptr (§111.B) + vector-ref-ptr (§111.C) +
cons-walk primitives (§101.B) + str-eq (§101.C) + extern-call into
`nelisp_fnv1a' (Doc 115 §115.7) to walk the env-mirror fast-hash-
table without materialising any intermediate Sexp slot (= every
hop is a raw `*const Sexp' / NlConsBox* pointer in rax).")

(provide 'nelisp-cc-mirror-lookup-entry)

;;; nelisp-cc-mirror-lookup-entry.el ends here
