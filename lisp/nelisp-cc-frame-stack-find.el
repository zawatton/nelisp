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
;;   §100.A  `extern-call'          — `nl_mirror_fnv1a_sexp' (hash; rewired to
;;                                    pure elisp in §115.7).
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
      ;; Returns: i64.  On hit, `(+ b2 32)' where b2 is the inner
      ;; (KEY . CELL) pair's NlConsBox* — that's the address of the
      ;; CELL slot.  On miss, 0.
      (if (= box-ptr 0)
          0
        (if (= (str-eq (sexp-payload-ptr box-ptr) name-ptr) 1)
            (+ (sexp-payload-ptr box-ptr) 32)
          (nelisp_frame_stack_find_walk_bucket
           (cons-cdr-raw-from-box box-ptr)
           name-ptr))))
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
          (extern-call nl_mirror_fnv1a_sexp name-ptr)
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
      ;; The hit branch re-evaluates `nelisp_frame_stack_find_in_frame'
      ;; (= second call site) because Phase 47 has no `let' binding
      ;; for the intermediate i64 result; the second call observes the
      ;; same memory + recomputes the same hash + walk, returning the
      ;; identical pointer.  Cost is constant — one redundant hash +
      ;; bucket-walk per hit — vs. zero on miss path.
      (if (< i 0)
          0
        (if (= (nelisp_frame_stack_find_in_frame
                (vector-ref-ptr backing-ptr i)
                name-ptr)
               0)
            (nelisp_frame_stack_find_descend backing-ptr (- i 1) name-ptr)
          (nelisp_frame_stack_find_in_frame
           (vector-ref-ptr backing-ptr i)
           name-ptr))))
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
`nl_mirror_fnv1a_sexp' (§100.A, rewired to pure elisp in §115.7)
+ three recursive helper-function calls for the inner loops (=
bucket walk + per-frame lookup + stack descend).

Replaces the ~78 LOC Rust shim `nl_frame_stack_find' (+ private
`lookup_in_frame' helper) which has been removed from
`env_lexframe_phase47_shims.rs'.")

(provide 'nelisp-cc-frame-stack-find)

;;; nelisp-cc-frame-stack-find.el ends here
