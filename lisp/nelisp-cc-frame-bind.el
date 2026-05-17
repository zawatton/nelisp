;;; nelisp-cc-frame-bind.el --- Doc 111 §111.E #23 frame_bind_rust_direct  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #23 — `frame_bind_rust_direct' with
;; the §111.E #25 private helper `frame_bind_into' folded in.
;; Doc 115 §115.5 — full pure-elisp implementation of the innermost-
;; frame name → cell bind.  Replaces the prior Rust shim
;; `nl_frame_bind' (+ private `bind_into_frame', ~95 LOC) deleted from
;; `build-tool/src/eval/env_lexframe_phase47_shims.rs'.
;;
;; Layout (= bootstrap-installed by `nl_frame_push'):
;;   frames-record  = Sexp::Record(`nelisp-lexframe-stack')
;;     slots[0] = Sexp::Vector(BACKING)
;;     slots[1] = Sexp::Int(DEPTH)
;;   BACKING[depth-1] = Sexp::Record(`nelisp-lexframe')
;;     slots[0] = Sexp::Record(`fast-hash-table')
;;       slots[0] = Sexp::Int (= bucket count, power-of-2, 16 default)
;;       slots[1] = Sexp::Vector (= buckets, each elt either Sexp::Nil
;;                                  or Sexp::Cons of bucket chain)
;;       slots[2] = Sexp::Int (= entry count)
;;   Bucket cell:  Sexp::Cons(NlConsBox)
;;     box.car = Sexp::Cons(INNER-PAIR)  (= the (KEY . CELL) pair)
;;     box.cdr = next bucket cell        (= Sexp::Cons / Sexp::Nil)
;;   Inner pair:  NlConsBox
;;     car = Sexp::Str(NAME)
;;     cdr = Sexp::Cell                  (= the binding's cell slot)
;;
;; Algorithm (= literal transcription of the Rust impl):
;;
;;   1. Read DEPTH from frames-record slot 1.  If 0 → return 0 (no-op).
;;   2. Read FRAME-PTR = vector-ref-ptr BACKING (depth-1).
;;   3. Read HT-PTR    = record-slot-ref-ptr FRAME 0.
;;   4. Read BUCKET-COUNT = sexp-int-unwrap (record-slot-ref-ptr HT 0)
;;      and BUCKETS-PTR  = record-slot-ref-ptr HT 1.
;;   5. Hash NAME via extern-call nl_mirror_fnv1a_sexp.
;;      idx = hash & (bucket-count - 1).  (Doc 115 §115.7 will rewire
;;      to a pure-elisp FNV-1a helper.)
;;   6. Walk the bucket chain.  If a pair's KEY matches NAME, replace
;;      that pair's cdr via `cons-set-cdr' (= in-place refcount-safe
;;      drop+write of the bucket-cell-box's CAR Sexp's payload-box's
;;      cdr — = the inner pair box's cdr field).  Return 1.
;;   7. Else (= miss): allocate INNER-PAIR via `cons-make name cell',
;;      allocate OUTER-CELL via `cons-make INNER OLD-HEAD', install via
;;      `vector-slot-set' on BUCKETS-PTR/idx, bump slot 2 entry-count.
;;      Return 1.
;;
;; Three caller-owned scratch slots are needed:
;;   scratch-pair-slot  = holds the new inner (NAME . CELL) Sexp::Cons
;;   scratch-outer-slot = holds the new outer bucket-cell Sexp::Cons
;;   scratch-count-slot = holds Sexp::Int(new-count) for the slot-2 bump
;;
;; The safe Rust wrapper in `build-tool/src/lib.rs' allocates these
;; slots on the call stack as `Sexp::Nil' and passes their pointers, so
;; the public 3-arg API is preserved for existing callers / probes.
;;
;; ABI deps satisfied:
;;   §111.B  `record-slot-ref-ptr' — frame/ht record slot pointer.
;;   §111.B  `record-slot-set'     — refcount-safe HT count overwrite.
;;   §111.C  `vector-ref-ptr'      — innermost frame + bucket-slot ptr.
;;   §111.E  `vector-slot-set'     — refcount-safe bucket-head install.
;;   §100    `sexp-int-unwrap'     — i64 payload read for depth/count/cap.
;;   §100    `sexp-int-make'       — Sexp::Int writer for count bump.
;;   §101.B  `sexp-payload-ptr'    — outer box-ptr extraction.
;;   §101.B  `cons-cdr-raw-from-box' — next-bucket walk.
;;   §101.C  `str-eq'              — KEY (Sexp::Str) ↔ NAME byte compare.
;;   §101.D  `cons-make'           — fresh NlConsBox allocator.
;;   §101.D  `cons-set-cdr'        — in-place pair-cdr replace.
;;   §100.A  `extern-call'         — `nl_mirror_fnv1a_sexp' (§111.E grp C,
;;                                   to be rewired in Doc 115 §115.7).

;;; Code:

(defconst nelisp-cc-frame-bind--source
  '(seq
    (defun nelisp_frame_bind_walk_update (box-ptr name-ptr cell-ptr)
      ;; Tail-recursive walk over a bucket's NlConsBox* chain looking
      ;; for an existing (KEY . CELL) pair whose KEY matches NAME-PTR.
      ;; On hit: `cons-set-cdr' replaces the inner pair's cdr with
      ;; CELL-PTR (= refcount-safe drop+write via `nl_consbox_set_cdr')
      ;; and returns 1.  On miss / end-of-bucket (box-ptr = 0): returns 0
      ;; so the caller can route to the prepend branch.
      ;;
      ;; Pointer-walk shape (= mirror of `nelisp_mirror_walk_bucket' in
      ;; `nelisp-cc-mirror-lookup-entry.el'):
      ;;   box-ptr            i64.  0 = end-of-bucket.  Otherwise a
      ;;                      live `NlConsBox*' of the bucket cell.
      ;;   sexp-payload-ptr   reads the box's CAR Sexp; for the bucket
      ;;                      cell this is the inner (KEY . CELL) pair's
      ;;                      `NlConsBox*' (= b2).  b2 starts with the
      ;;                      KEY Sexp at offset 0, so b2 itself is the
      ;;                      KEY `*const Sexp' for `str-eq'.
      ;;   cons-set-cdr       takes box-ptr treated as `Sexp::Cons' ptr.
      ;;                      Internally reads `[box-ptr + 8]' = the CAR
      ;;                      Sexp's payload = inner-pair box ptr, then
      ;;                      calls `nl_consbox_set_cdr(inner-pair, ...)'
      ;;                      to overwrite the pair's cdr (= the cell
      ;;                      slot).  Net effect: the (NAME . OLD-CELL)
      ;;                      pair becomes (NAME . CELL-PTR).
      (if (= box-ptr 0)
          0
        (if (= (str-eq (sexp-payload-ptr box-ptr) name-ptr) 1)
            (and (cons-set-cdr box-ptr cell-ptr) 1)
          (nelisp_frame_bind_walk_update
           (cons-cdr-raw-from-box box-ptr) name-ptr cell-ptr))))
    (defun nelisp_frame_bind_install
        (buckets-ptr idx scratch-pair-slot scratch-outer-slot)
      ;; Miss-path tail: populate the empty outer bucket cell in
      ;; SCRATCH-OUTER-SLOT (= `(Nil . Nil)' from the caller's `cons-make
      ;; nil-src nil-src') with car = SCRATCH-PAIR-SLOT and cdr = OLD
      ;; bucket head, then refcount-safely install at BUCKETS-PTR[idx]
      ;; via `vector-slot-set' (= `nl_vector_set_slot' clones the value
      ;; into the slot then drops the old one).  Returns 1.
      ;;
      ;; `cons-set-car' and `cons-set-cdr' call `nl_consbox_set_car'
      ;; / `nl_consbox_set_cdr' which clone `*val_ptr' before writing
      ;; (= refcount bump on box-tagged variants).  This is the
      ;; refcount-safe alternative to building the outer cell with
      ;; raw `cons-make' byte copies — see the `nelisp_frame_bind_
      ;; prepend' docstring for the discipline analysis.
      (and (cons-set-car scratch-outer-slot scratch-pair-slot)
           (cons-set-cdr scratch-outer-slot
                         (vector-ref-ptr buckets-ptr idx))
           (vector-slot-set buckets-ptr idx scratch-outer-slot)))
    (defun nelisp_frame_bind_prepend
        (ht-ptr name-ptr cell-ptr
                scratch-pair-slot scratch-outer-slot scratch-count-slot)
      ;; Miss path: build a fresh (NAME . CELL) inner pair into SCRATCH-
      ;; PAIR-SLOT, then cons it onto the bucket head via the
      ;; `..._install' helper (= keeps this defun's GP-reg parameter
      ;; count at the SysV AMD64 limit of 6), then bump HT-PTR.slots[2]
      ;; (= the entry count).  Returns 1.
      ;;
      ;; Refcount discipline (= the critical invariant for crash-free
      ;; cross-`fn nl_frame_bind' boundary handoff):
      ;;
      ;;   The Phase 47 `cons-make' op uses MVP byte-copy semantics (=
      ;;   it does NOT bump refcounts on box-tagged car/cdr inputs, per
      ;;   the §101.D `nelisp-cc-cons-construct.el' commentary).  Using
      ;;   `cons-make' with the live `name-ptr' / `cell-ptr' caller
      ;;   inputs would leave the caller's stack values and the inner
      ;;   pair's car/cdr fields sharing the same `NlStrRef' / `NlCellRef'
      ;;   handles at refcount = 1 — when Rust unwinds the call frame,
      ;;   the stack drop decrements to 0 and frees the `NlStr' / `NlCell'
      ;;   while the bucket still references them.  Result: SIGSEGV /
      ;;   double-free on subsequent `frame_stack_find'.
      ;;
      ;;   The fix: allocate the inner pair as `(Nil . Nil)' first, then
      ;;   use `cons-set-car' / `cons-set-cdr' which call the refcount-
      ;;   aware `nl_consbox_set_car' / `nl_consbox_set_cdr' (= they
      ;;   `(*val).clone()' before writing).  This bumps the caller's
      ;;   `NlStrRef' / `NlCellRef' refcount from 1 to 2, so the stack
      ;;   drop only takes it back to 1, leaving the bucket as the sole
      ;;   owner — matches the steady-state ownership invariant.
      ;;
      ;;   Same logic applies to the outer cell: build empty via `cons-
      ;;   make' with Nil/Nil, then `cons-set-car' / `cons-set-cdr' (=
      ;;   refcount-bumps the inner pair Sexp::Cons handle stored in
      ;;   SCRATCH-PAIR-SLOT, and the bucket-head Sexp at BUCKETS[idx]).
      ;;
      ;;   The `cons-make' Nil/Nil seed needs a `*const Sexp' pointing
      ;;   at a `Sexp::Nil' (= tag byte 0); SCRATCH-COUNT-SLOT is the
      ;;   caller-owned 32-byte slot pre-initialised to `Sexp::Nil' by
      ;;   the Rust safe wrapper, so we reuse it as the Nil source
      ;;   between alloc and the later `sexp-int-make' count bump (=
      ;;   no overlap because the count bump runs strictly after both
      ;;   `cons-make' invocations).
      ;;
      ;; Order: build inner pair → build outer cell → install in bucket
      ;; → bump entry count.  All intermediate ops materialise non-zero
      ;; pointers so the `and' chain threads through to the final 1.
      (and (cons-make scratch-count-slot scratch-count-slot scratch-pair-slot)
           (cons-set-car scratch-pair-slot name-ptr)
           (cons-set-cdr scratch-pair-slot cell-ptr)
           (cons-make scratch-count-slot scratch-count-slot scratch-outer-slot)
           (nelisp_frame_bind_install
            (record-slot-ref-ptr ht-ptr 1)
            (logand
             (extern-call nl_mirror_fnv1a_sexp name-ptr)
             (- (sexp-int-unwrap (record-slot-ref-ptr ht-ptr 0)) 1))
            scratch-pair-slot scratch-outer-slot)
           (sexp-int-make scratch-count-slot
                          (+ (sexp-int-unwrap
                              (record-slot-ref-ptr ht-ptr 2))
                             1))
           (record-slot-set ht-ptr 2 scratch-count-slot)))
    (defun nelisp_frame_bind_in_ht
        (ht-ptr name-ptr cell-ptr
                scratch-pair-slot scratch-outer-slot scratch-count-slot)
      ;; Inside the innermost frame's `fast-hash-table' record, compute
      ;; the bucket index from FNV-1a(NAME) & (BUCKET-COUNT - 1) (=
      ;; BUCKET-COUNT is always a power of 2 per `nl_frame_push'), then
      ;; dispatch to the walk-update path; on miss route to prepend.
      ;;
      ;; The two `record-slot-ref-ptr ht-ptr 1' calls evaluate to the
      ;; same Sexp slot pointer (= the buckets `Sexp::Vector' slot in
      ;; the HT record); no caching needed because the pointer is
      ;; cheap to recompute and the Phase 47 grammar lacks let-binding
      ;; for pointer values across calls anyway.
      (if (= (nelisp_frame_bind_walk_update
              (sexp-payload-ptr
               (vector-ref-ptr
                (record-slot-ref-ptr ht-ptr 1)
                (logand
                 (extern-call nl_mirror_fnv1a_sexp name-ptr)
                 (- (sexp-int-unwrap (record-slot-ref-ptr ht-ptr 0))
                    1))))
              name-ptr cell-ptr)
             1)
          1
        (nelisp_frame_bind_prepend
         ht-ptr name-ptr cell-ptr
         scratch-pair-slot scratch-outer-slot scratch-count-slot)))
    (defun nelisp_frame_bind
        (frames-ptr name-ptr cell-ptr
                    scratch-pair-slot scratch-outer-slot scratch-count-slot)
      ;; frames-ptr:         *const Sexp pointing at Env::frames_record.
      ;; name-ptr:           *const Sexp pointing at Sexp::Str / Sexp::Symbol.
      ;; cell-ptr:           *const Sexp pointing at the new cell value
      ;;                     (commonly Sexp::Cell(NlCellRef)).
      ;; scratch-pair-slot:  *mut Sexp, caller-owned Sexp::Nil — holds
      ;;                     the freshly-allocated inner pair.
      ;; scratch-outer-slot: *mut Sexp, caller-owned Sexp::Nil — holds
      ;;                     the freshly-allocated outer bucket cell.
      ;; scratch-count-slot: *mut Sexp, caller-owned Sexp::Nil — holds
      ;;                     `Sexp::Int(new-count)' for the slot-2 bump.
      ;;
      ;; Returns: i64 — 1 on bind, 0 on no-op (= empty stack).  The
      ;; "malformed name" arm of the Rust impl is left implicit: the
      ;; `str-eq' op tag-checks each side and falls through to the
      ;; prepend path on non-Str/Symbol NAME, mirroring the Rust impl's
      ;; conservative "treat as miss" semantics.
      ;;
      ;; The 6-arg shape matches the SysV AMD64 ABI's GP-register limit
      ;; (rdi/rsi/rdx/rcx/r8/r9), so the helper avoids any stack-arg
      ;; complexity.  The safe Rust wrapper allocates the 3 scratch
      ;; Sexp::Nil slots on the call stack and passes their pointers.
      (if (= (sexp-int-unwrap (record-slot-ref-ptr frames-ptr 1)) 0)
          0
        (nelisp_frame_bind_in_ht
         (record-slot-ref-ptr
          (vector-ref-ptr
           (record-slot-ref-ptr frames-ptr 0)
           (- (sexp-int-unwrap (record-slot-ref-ptr frames-ptr 1)) 1))
          0)
         name-ptr cell-ptr
         scratch-pair-slot scratch-outer-slot scratch-count-slot))))
  "Phase 47 source for Doc 111 §111.E #23 / Doc 115 §115.5
`frame_bind_rust_direct'.

Pure-elisp innermost-frame bind.  Composes `record-slot-ref-ptr'
(§111.B), `record-slot-set' (§111.B), `vector-ref-ptr' / `vector-
slot-set' (§111.C / §111.E), `sexp-int-unwrap' / `sexp-int-make'
(§100), `sexp-payload-ptr' / `cons-cdr-raw-from-box' (§101.B),
`str-eq' (§101.C), `cons-make' / `cons-set-cdr' (§101.D), and an
`extern-call' to `nl_mirror_fnv1a_sexp' for the FNV-1a hash
(Doc 115 §115.7 rewires to a pure-elisp FNV-1a helper).

Replaces the ~95 LOC Rust shim `nl_frame_bind' + private helper
`bind_into_frame' which have been removed from `env_lexframe_phase47_shims.rs'.")

(provide 'nelisp-cc-frame-bind)

;;; nelisp-cc-frame-bind.el ends here
