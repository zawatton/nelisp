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
       name-ptr))

    ;; ============================================================
    ;; Doc 49 Wave 10.1d-retry — capture-to-depth Phase 47 native
    ;; ============================================================
    ;;
    ;; `nelisp-lexframe-stack-capture-to-depth' (= R9 95% cumulative
    ;; chain driver, invoked from sf-lambda closure capture) — Phase
    ;; 47 fast path co-located in this .o so the static archive linker
    ;; pulls it in via the same chain that already keeps
    ;; `nelisp_frame_stack_find' alive (= `extern-call' from
    ;; `nelisp-cc-env-lookup-value.el').  No Rust LOC delta required.
    ;;
    ;; Public extern: `nl_capture_descend_native(in-vec, out) -> i64'.
    ;; Reached from elisp via `(nl-jit-call-out-1 \"nl_capture_descend_native\"
    ;; in-vec)' (= 1-arg + out call-out variant, bridges fn signature
    ;; `extern \"C\" fn(*const Sexp, *mut Sexp) -> i64').
    ;;
    ;; in-vec layout (caller-owned Sexp::Vector of 3 slots, pre-populated
    ;; by the elisp wrapper before each call):
    ;;   in-vec[0] = stack record (`Sexp::Record(`nelisp-lexframe-stack')')
    ;;   in-vec[1] = max-depth (`Sexp::Int' — caller's pre-apply depth)
    ;;   in-vec[2] = pair-slot scratch (`Sexp::Nil' on entry — reused as
    ;;               destination for each (NAME . CELL) inner cons cell
    ;;               built during the walk; refcount-safe because every
    ;;               `cons-make-with-clone' clones the previous slot
    ;;               value into the new cell before overwriting)
    ;; out:        result slot (`Sexp::Nil' on entry per `out_call!'
    ;;             convention; receives the final alist on exit).
    ;;
    ;; Algorithm: walk lexframe stack innermost-first (= i from depth-1
    ;; down to 0), and within each frame iterate every hash-table bucket
    ;; chain, prepending each (NAME . CELL) onto `*out' via two fused
    ;; `cons-make-with-clone' ops.  The walk is dedup-free — duplicate
    ;; keys are intentional: the alist consumer
    ;; (`nelisp-lexframe-stack-push-captured!') iterates head-first and
    ;; binds via `fast-hash-put' which UPDATES existing keys, so walking
    ;; inner-first + prepending puts OUTER entries at the head end and
    ;; INNER entries at the tail end — consumer processes outer first,
    ;; inner overwrites it, matching the original "inner shadows outer"
    ;; contract.  This trades a few extra `bind' calls for hot-path
    ;; bindings (rare in practice — most names appear in one frame) for
    ;; eliminating the W10.1d/d-retry blocker (= no `hash-table-make' /
    ;; `hash-table-put' / `hash-table-contains-p' needed, fewer scratch
    ;; slots, no W11.2 `nelisp_ht_walk' splice).
    ;;
    ;; Refcount discipline: every cons cell goes through
    ;; `cons-make-with-clone' which calls `nl_sexp_clone_into' on both
    ;; src ptrs (= refcount bump on box-tagged variants) BEFORE writing
    ;; the dst slot.  Aliasing dst with one of the src ptrs (= the
    ;; `(cons-make-with-clone pair-slot out out)' chain prepend) is safe
    ;; because the clones complete before the dst overwrite drops the
    ;; old `*out' value.
    ;;
    ;; ABI deps satisfied (all existing Phase 47 ops — no new primitives):
    ;;   §111.B  `record-slot-ref-ptr' — stack.slots[0] → backing,
    ;;                                    frame.slots[0] → ht, ht.slots[0/1]
    ;;                                    → bc/buckets.
    ;;   §111.C  `vector-ref-ptr'      — backing[i] → frame, buckets[j]
    ;;                                    → bucket-head slot.
    ;;   §100    `sexp-int-unwrap'     — depth + bucket-count payload reads.
    ;;   §101.B  `sexp-payload-ptr'    — Sexp::Cons → NlConsBox* extract.
    ;;   §101.B  `cons-cdr-raw-from-box' — bucket chain walk.
    ;;   §120.E  `cons-make-with-clone' — fused alloc + clone + tag/payload
    ;;                                    write for inner pair + outer prepend.

    (defun nl_capture_emit_one (name-ptr cell-ptr pair-slot out)
      ;; Build inner (NAME . CELL) cons cell into pair-slot, then prepend
      ;; it onto the *out alist accumulator.  Returns 1 unconditionally
      ;; (= keeps the parent `and' chain alive).
      ;;
      ;; Slot reuse: pair-slot holds the prior iteration's inner pair (or
      ;; Sexp::Nil on first call).  `cons-make-with-clone' overwrites it
      ;; with the new pair box — the prior pair's refcount was already
      ;; bumped by the prior outer cons-make-with-clone that cloned it
      ;; into the *out chain, so dropping the prior pair-slot value here
      ;; is balanced (= unique-owner invariant preserved).
      (and (cons-make-with-clone name-ptr cell-ptr pair-slot)
           (cons-make-with-clone pair-slot out out)
           1))

    (defun nl_capture_walk_bucket (box-ptr pair-slot out)
      ;; Walk one bucket's NlConsBox* chain emitting each entry.  Mirrors
      ;; `nelisp_frame_stack_find_walk_bucket' above but emits instead of
      ;; comparing.  box-ptr = 0 means end-of-bucket.
      ;;
      ;; Bucket cell layout (= identical to `nelisp_frame_stack_find_in_frame'):
      ;;   outer box  = `NlConsBox' whose car = Sexp::Cons(inner-box),
      ;;                cdr = next bucket cell or Sexp::Nil.
      ;;   inner box  = `NlConsBox' whose car = Sexp::Str (NAME),
      ;;                cdr = Sexp::Cell (or any value — opaque to walk).
      ;;
      ;; `sexp-payload-ptr box-ptr' = inner-box (= NlConsBox* of the
      ;; (NAME . CELL) pair).  `+ inner-box 0' = NAME slot ptr,
      ;; `+ inner-box 32' = CELL slot ptr (= second 32-byte slot inside
      ;; the box, matching the `nelisp_frame_stack_find_walk_bucket'
      ;; (`+ inner-ptr 32') convention for the CELL position).
      (if (= box-ptr 0)
          1
        (let ((pair-box (sexp-payload-ptr box-ptr)))
          (and (nl_capture_emit_one pair-box (+ pair-box 32) pair-slot out)
               (nl_capture_walk_bucket (cons-cdr-raw-from-box box-ptr)
                                       pair-slot out)))))

    (defun nl_capture_walk_buckets (buckets-ptr j bc pair-slot out)
      ;; Iterate the bucket array slots j = 0..bc-1.  buckets-ptr is a
      ;; `*const Sexp' to the buckets-vector slot inside the
      ;; `fast-hash-table' record (= obtained via
      ;; `record-slot-ref-ptr ht-ptr 1' by the caller — note that
      ;; `vector-ref-ptr' deref'es this slot to reach the underlying
      ;; `Sexp::Vector' before indexing).
      ;;
      ;; Each bucket slot holds `Sexp::Cons(outer-box)' on non-empty
      ;; buckets and `Sexp::Nil' on empty ones.  `sexp-payload-ptr' on
      ;; Sexp::Nil yields 0 (= tag-payload field is 0 for the all-zero
      ;; Nil representation), which the inner bucket walker handles via
      ;; the `(= box-ptr 0)' base case.
      (if (>= j bc)
          1
        (and (nl_capture_walk_bucket
              (sexp-payload-ptr (vector-ref-ptr buckets-ptr j))
              pair-slot out)
             (nl_capture_walk_buckets buckets-ptr (+ j 1) bc
                                      pair-slot out))))

    (defun nl_capture_walk_frame (frame-ptr pair-slot out)
      ;; Walk one `nelisp-lexframe' record by reading its inner
      ;; `fast-hash-table' (= slot 0) then iterating every bucket.
      ;;
      ;; Layout per `nelisp-lexframe.el' commentary:
      ;;   frame-record.slots[0] = Sexp::Record(`fast-hash-table')
      ;;     ht-record.slots[0] = Sexp::Int (bucket count, power-of-2)
      ;;     ht-record.slots[1] = Sexp::Vector (bucket array)
      ;;     ht-record.slots[2] = Sexp::Int (live entry count — unused
      ;;                                      here; we walk every bucket
      ;;                                      regardless of population).
      (let ((ht-ptr (record-slot-ref-ptr frame-ptr 0)))
        (nl_capture_walk_buckets
         (record-slot-ref-ptr ht-ptr 1)
         0
         (sexp-int-unwrap (record-slot-ref-ptr ht-ptr 0))
         pair-slot out)))

    (defun nl_capture_walk_frames (backing-ptr i pair-slot out)
      ;; Innermost-first descent: walk i = depth-1 down to 0.  Each
      ;; iteration's frame entries get PREPENDED to *out, so the final
      ;; alist has OUTER entries at the head end and INNER entries
      ;; nearer the tail — which lets the alist consumer's head-first
      ;; iteration + `fast-hash-put' UPDATE-existing semantics implement
      ;; the "inner shadows outer" contract without an explicit dedup
      ;; pass here.
      ;;
      ;; backing-ptr: `*const Sexp' = stack-record.slots[0] (= the
      ;;              backing `Sexp::Vector' slot).  `vector-ref-ptr'
      ;;              deref's this slot to reach the underlying vector
      ;;              before indexing — same pattern as
      ;;              `nelisp_frame_stack_find_descend' above.
      (if (< i 0)
          1
        (and (nl_capture_walk_frame (vector-ref-ptr backing-ptr i)
                                    pair-slot out)
             (nl_capture_walk_frames backing-ptr (- i 1)
                                     pair-slot out))))

    (defun nl_capture_descend_native (in-vec out)
      ;; Public entry, dispatched from elisp via:
      ;;   (nl-jit-call-out-1 \"nl_capture_descend_native\" IN-VEC)
      ;; which the §99.B bridge wraps as
      ;;   extern \"C\" fn(*const Sexp, *mut Sexp) -> i64
      ;; with `out' pre-initialised to `Sexp::Nil' (per `out_call!').
      ;;
      ;; in-vec is a 3-slot caller-owned `Sexp::Vector':
      ;;   [0] = stack record (lexframe-stack)
      ;;   [1] = max-depth Sexp::Int
      ;;   [2] = pair-slot scratch (Sexp::Nil on entry — reused inside)
      ;;
      ;; max-depth = 0 → early return; `out' stays `Sexp::Nil', mirroring
      ;; the elisp `(cond ((= max-depth 0) nil) ...)' fast path.
      ;;
      ;; Returns i64 0 = TRAMPOLINE_OK so the `out_call!' bridge yields
      ;; the populated `*out' Sexp value back to the elisp caller.
      ;; Phase 47 `let' is single-binding only; nest each var.
      (let ((depth (sexp-int-unwrap (vector-ref-ptr in-vec 1))))
        (if (= depth 0)
            0
          (let ((stack-ptr (vector-ref-ptr in-vec 0)))
            (let ((pair-slot (vector-ref-ptr in-vec 2)))
              (and (nl_capture_walk_frames
                    (record-slot-ref-ptr stack-ptr 0)
                    (- depth 1)
                    pair-slot
                    out)
                   0)))))))
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
