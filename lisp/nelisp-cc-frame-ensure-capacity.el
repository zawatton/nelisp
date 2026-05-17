;;; nelisp-cc-frame-ensure-capacity.el --- Doc 111 §111.E #20 ensure_capacity  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #20 — `frame_stack_ensure_capacity'.
;; Doc 115 §115.1 — full pure-elisp implementation of the lexframe-
;; stack capacity-doubling grow.  Replaces the prior Rust shim
;; `nl_frame_stack_ensure_capacity' (= deleted from
;; `build-tool/src/eval/env_lexframe_phase47_shims.rs').
;;
;; Algorithm (= literal transcription of the Rust impl):
;;
;;   1. Read current backing capacity via `vector-len' on slot 0 of
;;      the frames-record.
;;   2. If cap >= needed → no-op fast path, return cap.
;;   3. Else compute new-cap = max(cap, 1) doubled until >= needed.
;;   4. Allocate a fresh `NlVector' of capacity new-cap via the new
;;      §115.1 `vector-make' grammar op (= `nl_alloc_vector' +
;;      `Sexp::Vector' tag write), into the caller-provided
;;      scratch-slot.
;;   5. Copy elements [0, depth) from the old backing into the new
;;      vector via `vector-slot-set' + `vector-ref-ptr' (= refcount-
;;      aware via the underlying `nl_vector_set_slot' /
;;      `nl_sexp_clone_into' helpers).
;;   6. Install the new vector into frames-record slot 0 via
;;      refcount-aware `record-slot-set'.
;;   7. Return new-cap.
;;
;; Recursion is used for the two loops (compute-new-cap and copy-
;; elem) because Phase 47 has no mutable local variables — only
;; immutable let-bound integer constants.  Each recursive call passes
;; the iteration state through arg registers.
;;
;; Signature change: the function now takes a `scratch-slot' 3rd
;; parameter (= caller-owned `*mut Sexp' initialised to `Sexp::Nil')
;; to hold the new Sexp::Vector before installing it into slots[0].
;; The safe Rust wrapper in `build-tool/src/lib.rs' allocates this
;; slot on the call stack and passes its pointer, so the probe tests
;; continue to call with the original 2-arg signature.
;;
;; ABI deps satisfied:
;;   §111.B  `record-slot-ref-ptr' — slot 0 / slot 1 pointer.
;;   §111.B  `record-slot-set'     — refcount-safe slot 0 install.
;;   §111.C  `vector-len'          — current backing capacity.
;;   §111.C  `vector-ref-ptr'      — element pointer for copy loop.
;;   §111.E  `vector-slot-set'     — refcount-safe slot overwrite.
;;   §115.1  `vector-make'         — fresh `NlVector' allocator.
;;   §100    `sexp-int-unwrap'     — i64 payload read for depth.

;;; Code:

(defconst nelisp-cc-frame-ensure-capacity--source
  '(seq
    (defun nelisp_frame_stack_ensure_capacity_compute_cap (c n)
      ;; Capacity-doubling helper.  Returns the smallest power-of-two
      ;; multiple of c that is >= n.  c is assumed >= 1.
      ;;
      ;; Mirrors Rust:
      ;;   while new_cap < needed { new_cap *= 2; }
      (if (< c n)
          (nelisp_frame_stack_ensure_capacity_compute_cap (* c 2) n)
        c))
    (defun nelisp_frame_stack_ensure_capacity_copy (src dst i n)
      ;; Element-by-element copy from src vector (= `*const Sexp'
      ;; pointing at the OLD `Sexp::Vector') into dst vector (= `*mut
      ;; Sexp' pointing at the NEW `Sexp::Vector' in the scratch
      ;; slot).  Indices [i, n).  Returns 1 on completion (= a
      ;; non-zero sentinel so the outer `and' threads through without
      ;; short-circuiting on the rax = 0 condition).
      ;;
      ;; `vector-slot-set' delegates to `nl_vector_set_slot' which is
      ;; refcount-aware (= clones the source Sexp into the dst slot,
      ;; bumping refcount on box-tagged variants).  When the OLD
      ;; backing is dropped at step 6 (= `record-slot-set'), those
      ;; refcounts drop back to the original count so no leak.  Doc
      ;; 115 §115.1 augments `--emit-vector-slot-set' to materialise
      ;; rax = 1 after the helper call so this `and' threading works
      ;; — matching the convention `record-slot-set' established in
      ;; §111.B.
      (if (< i n)
          (and (vector-slot-set dst i (vector-ref-ptr src i))
               (nelisp_frame_stack_ensure_capacity_copy
                src dst (+ i 1) n))
        1))
    (defun nelisp_frame_stack_ensure_capacity_grow
        (frames-ptr scratch-slot new-cap)
      ;; Allocate new vector of capacity new-cap into scratch-slot,
      ;; copy depth live elements over, install into slots[0], and
      ;; return new-cap.
      ;;
      ;; Order matters: copy must happen BEFORE `record-slot-set'
      ;; because (record-slot-ref-ptr frames-ptr 0) gives the slot
      ;; pointer — after install, that slot points at the new
      ;; vector, so we'd read from the new (empty Nil) backing
      ;; instead of the OLD one.
      (and (vector-make new-cap scratch-slot)
           (nelisp_frame_stack_ensure_capacity_copy
            (record-slot-ref-ptr frames-ptr 0)
            scratch-slot
            0
            (sexp-int-unwrap (record-slot-ref-ptr frames-ptr 1)))
           (record-slot-set frames-ptr 0 scratch-slot)
           new-cap))
    (defun nelisp_frame_stack_ensure_capacity_initial (cap)
      ;; Initial c for compute_cap: max(cap, 1).  cap may be 0 on
      ;; freshly-bootstrapped Sexp::Nil-backed stacks, but the doubling
      ;; loop must start at 1 to actually grow.
      (if (< cap 1) 1 cap))
    (defun nelisp_frame_stack_ensure_capacity (frames-ptr needed scratch-slot)
      ;; frames-ptr:   *const Sexp pointing at Env::frames_record (=
      ;;               Sexp::Record(`nelisp-lexframe-stack')).
      ;; needed:       i64 — required minimum capacity.
      ;; scratch-slot: *mut Sexp — caller-owned 32-byte slot
      ;;               initialised to Sexp::Nil; used to hold the
      ;;               fresh Sexp::Vector before installation.
      ;;
      ;; Returns: i64 — the resulting backing capacity after the call
      ;; (= current cap on the no-grow fast path, new-cap on grow).
      ;;
      ;; Caller's pre-condition: same as `nelisp_frame_stack_depth' —
      ;; frames-ptr.tag = Sexp::Record AND slots[0..1] are the
      ;; bootstrap-installed Sexp::Vector(BACKING) / Sexp::Int(DEPTH)
      ;; shape.  No tag-check here because the only legal frames-
      ;; record shape meets the precondition.
      (if (< (vector-len (record-slot-ref-ptr frames-ptr 0)) needed)
          (nelisp_frame_stack_ensure_capacity_grow
           frames-ptr
           scratch-slot
           (nelisp_frame_stack_ensure_capacity_compute_cap
            (nelisp_frame_stack_ensure_capacity_initial
             (vector-len (record-slot-ref-ptr frames-ptr 0)))
            needed))
        (vector-len (record-slot-ref-ptr frames-ptr 0)))))
  "Phase 47 source for Doc 111 §111.E #20 / Doc 115 §115.1
`frame_stack_ensure_capacity'.

Pure-elisp capacity-doubling grow.  Composes `record-slot-ref-ptr'
(§111.B), `record-slot-set' (§111.B), `vector-len' / `vector-ref-
ptr' / `vector-slot-set' (§111.C / §111.E), `vector-make' (§115.1
new), `sexp-int-unwrap' (§100), and recursive helper-function calls
for the two inner loops (= compute-new-cap doubling + copy-elem).

Replaces the 47 LOC Rust shim `nl_frame_stack_ensure_capacity'
which has been removed from `env_lexframe_phase47_shims.rs'.")

(provide 'nelisp-cc-frame-ensure-capacity)

;;; nelisp-cc-frame-ensure-capacity.el ends here
