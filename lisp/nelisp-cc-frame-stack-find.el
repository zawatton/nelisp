;;; nelisp-cc-frame-stack-find.el --- Doc 111 §111.E #24 frame_stack_find  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #24 — `frame_stack_find_rust_direct'
;; with the §111.E #25 private helpers `frame_lookup_rust_direct' +
;; `frame_lookup_in' folded in.  Doc 115 §115.6 — full pure-elisp
;; implementation of the innermost-first stack walk.  Replaces the
;; ~78 LOC Rust shim `nl_frame_stack_find' (+ private
;; `lookup_in_frame' helper) that previously lived in
;; `build-tool/src/eval/env_lexframe_phase47_shims.rs'.
;;
;; Algorithm (= literal transcription of the Rust impl):
;;
;;   1. Read depth via `sexp-int-unwrap' on slots[1] of the
;;      frames-record.
;;   2. Read backing-vector pointer via slots[0].
;;   3. Recursive descend `i' from depth-1 down to 0:
;;        a. Fetch frame = backing[i] via `vector-ref-ptr'.
;;        b. Look up NAME in frame's hash table (= slot 0 of frame).
;;        c. If hit (= non-zero ptr), return the cell pointer.
;;        d. Else recurse with i-1.
;;   4. Return 0 when i < 0 (= no frame matched).
;;
;; Per-frame lookup is structurally identical to
;; `mirror_lookup_entry' (= §111.E #1) — same `(KEY . VALUE)' bucket
;; cons-pair shape — except one extra `record-slot-ref-ptr' hop
;; through the outer `nelisp-lexframe' record to reach the inner
;; `fast-hash-table'.  We reuse the same bucket-walk primitive shape;
;; here it walks the frame's bucket and returns `(+ box-ptr 32)' on
;; hit (= the cdr slot of the (NAME . CELL) inner pair, holding the
;; `Sexp::Cell').
;;
;; ABI deps satisfied:
;;   §111.B  `record-slot-ref-ptr'  — frame.slots[0] → ht_rec, ht_rec.slots[0/1].
;;   §111.C  `vector-ref-ptr'       — backing[i], buckets[idx].
;;   §101.B  `cons-walk' primitives — `sexp-payload-ptr' + `cons-cdr-raw-from-box'.
;;   §101.C  `str-eq'               — byte-payload equality on bucket KEY.
;;   §100.A  `extern-call'          — `nelisp_fnv1a' (Doc 115 §115.7
;;                                    pure-elisp 32-bit FNV-1a).
;;   §100    `sexp-int-unwrap'      — depth payload + bucket-count payload.
;;   §111.E  `logand'               — `(h & (count - 1))' fast-path mask.

;;; Code:

(defconst nelisp-cc-frame-stack-find--source
  '(seq
    (defun nelisp_frame_stack_find_walk_bucket (box-ptr name-ptr)
      ;; Tail-recursive walk over one bucket's NlConsBox* chain.
      ;; Identical in shape to `nelisp_mirror_walk_bucket' (§111.E #1);
      ;; duplicated here so this object stands alone for Phase 47
      ;; compile (= each helper module owns its tight loop).
      ;;
      ;;   box-ptr:  i64.  0 means end-of-bucket.  Otherwise a live
      ;;             `NlConsBox*' for the outer bucket cell whose CAR
      ;;             is the inner (KEY . CELL) pair, CDR is the next
      ;;             bucket cell.
      ;;   name-ptr: `*const Sexp' pointing at Sexp::Symbol /
      ;;             Sexp::Str being looked up.
      ;;
      ;; Returns: i64.  On hit, `(+ inner-ptr 32)' where inner-ptr is
      ;; the inner (KEY . CELL) pair's NlConsBox* — that's the address
      ;; of the CELL slot.  On miss, 0.
      ;;
      ;; R11b Wave 9 CSE-hoist: the inner (KEY . CELL) pair pointer is
      ;; read twice on the hit path (= once for `str-eq' against KEY,
      ;; once for the `(+ ... 32)' cell-slot offset).  Hoisting
      ;; `(sexp-payload-ptr box-ptr)' into `inner-ptr' via let-rt frame
      ;; slot eliminates the redundant memory read on every bucket
      ;; comparison.
      (if (= box-ptr 0)
          0
        (let ((inner-ptr (sexp-payload-ptr box-ptr)))
          (if (= (str-eq inner-ptr name-ptr) 1)
              (+ inner-ptr 32)
            (nelisp_frame_stack_find_walk_bucket
             (cons-cdr-raw-from-box box-ptr)
             name-ptr)))))
    (defun nelisp_frame_stack_find_in_frame (frame-ptr name-ptr)
      ;; Look up NAME in a single frame's hash table.  frame-ptr
      ;; points at the outer `Sexp::Record(`nelisp-lexframe')'; its
      ;; slot 0 is the `Sexp::Record(`fast-hash-table')' whose
      ;; slots[0]/[1] are bucket-count/buckets-vector.
      ;;
      ;; Returns: i64 — `*const Sexp' of the matching CELL slot, or
      ;; 0 on miss.
      ;;
      ;; Bucket-count is assumed power-of-2 (= `nl_frame_push'
      ;; allocates 16 buckets, no resize), so the index mask is the
      ;; cheap `(h & (count - 1))' fast path matching the Rust impl.
      (nelisp_frame_stack_find_walk_bucket
       (sexp-payload-ptr
        (vector-ref-ptr
         (record-slot-ref-ptr (record-slot-ref-ptr frame-ptr 0) 1)
         (logand
          (extern-call nelisp_fnv1a name-ptr)
          (- (sexp-int-unwrap
              (record-slot-ref-ptr (record-slot-ref-ptr frame-ptr 0) 0))
             1))))
       name-ptr))
    (defun nelisp_frame_stack_find_descend (backing-ptr i name-ptr)
      ;; Innermost-first walk: descend from i = depth-1 down to 0.
      ;; Returns the first non-zero hit's cell pointer, or 0 if every
      ;; frame missed (= the i < 0 base case).
      ;;
      ;; backing-ptr: `*const Sexp' = frames-record.slots[0] (= the
      ;;              backing `Sexp::Vector').
      ;; i:           i64 — current index being probed; recursion
      ;;              starts at depth-1.
      ;; name-ptr:    `*const Sexp' for the lookup key.
      ;;
      ;; R11b Wave 9 CSE-hoist: the pre-R11b version re-evaluated
      ;; `nelisp_frame_stack_find_in_frame' on the hit branch (= second
      ;; call site) because the original Phase 47 source elided the
      ;; let-binding.  This paid a redundant FNV-1a hash + bucket walk
      ;; per hit — non-negligible since lookup hit rate dominates.
      ;; We now hoist the first call's result into `found' via let-rt
      ;; and return it directly on the non-zero branch, saving one
      ;; full hash+walk per successful frame lookup.
      (if (< i 0)
          0
        (let ((found (nelisp_frame_stack_find_in_frame
                      (vector-ref-ptr backing-ptr i)
                      name-ptr)))
          (if (= found 0)
              (nelisp_frame_stack_find_descend backing-ptr (- i 1) name-ptr)
            found))))
    (defun nelisp_frame_stack_find (frames-ptr name-ptr)
      ;; frames-ptr: *const Sexp pointing at Env::frames_record (=
      ;;             Sexp::Record(`nelisp-lexframe-stack')).
      ;; name-ptr:   *const Sexp pointing at Sexp::Str / Sexp::Symbol.
      ;;
      ;; Returns: i64 — the `*const Sexp' of the matching (NAME . CELL)
      ;; pair's CDR slot (= the cell), or 0 on miss / empty stack.
      ;; The returned pointer borrows the bucket-pair's slot owned by
      ;; `*frames-ptr'; callers must not outlive that ownership (= same
      ;; contract as `nelisp_mirror_lookup_entry').
      ;;
      ;; Pre-condition (= matches the Rust impl's early-return arms):
      ;;   - frames-ptr.tag = Sexp::Record.
      ;;   - slots[0] = Sexp::Vector(BACKING).
      ;;   - slots[1] = Sexp::Int(DEPTH).
      ;; The `Sexp::Nil' pre-bootstrap case routes through Rust until
      ;; the dispatcher rewires, so no tag-check here.  Empty stack
      ;; (= depth 0) yields i = -1 which hits the `(< i 0)' base case
      ;; immediately and returns 0.
      (nelisp_frame_stack_find_descend
       (record-slot-ref-ptr frames-ptr 0)
       (- (sexp-int-unwrap (record-slot-ref-ptr frames-ptr 1)) 1)
       name-ptr)))
  "Phase 47 source for Doc 111 §111.E #24 / Doc 115 §115.6
`frame_stack_find_rust_direct' + folded `lookup_in_frame'.

Pure-elisp innermost-first stack walk.  Composes `record-slot-ref-
ptr' (§111.B), `vector-ref-ptr' (§111.C), `sexp-payload-ptr' /
`cons-cdr-raw-from-box' (§101.B), `str-eq' (§101.C), `logand'
(§111.E), `sexp-int-unwrap' (§100), and `extern-call' into
`nelisp_fnv1a' (Doc 115 §115.7 pure-elisp 32-bit FNV-1a)
+ three recursive helper-function calls for the inner loops (=
bucket walk + per-frame lookup + stack descend).

Replaces the ~78 LOC Rust shim `nl_frame_stack_find' (+ private
`lookup_in_frame' helper) which has been removed from
`env_lexframe_phase47_shims.rs'.")

(provide 'nelisp-cc-frame-stack-find)

;;; nelisp-cc-frame-stack-find.el ends here
