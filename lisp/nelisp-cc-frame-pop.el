;;; nelisp-cc-frame-pop.el --- Doc 111 §111.E #22 frame_pop_rust_direct  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group E helper #22 — `frame_pop_rust_direct'.
;; Doc 115 §115.2 — full pure-elisp implementation of the lexframe
;; pop.  Replaces the prior Rust shim `nl_frame_pop' (= deleted from
;; `build-tool/src/eval/env_lexframe_phase47_shims.rs').
;;
;; Algorithm (= literal transcription of the Rust impl):
;;
;;   1. Read current depth via `sexp-int-unwrap' on slot 1 of the
;;      frames-record.
;;   2. If depth == 0 → no-op fast path, return 0 (empty stack).
;;   3. Else compute new-depth = depth - 1.
;;   4. The caller-provided scratch-slot starts as `Sexp::Nil' (= the
;;      lib.rs safe wrapper initialises it via `let mut scratch =
;;      Sexp::Nil').  Use `vector-slot-set' with scratch as VAL to
;;      refcount-safely overwrite backing[new-depth] with Nil — this
;;      drops the popped frame's `nelisp-lexframe' record (= bucket
;;      vector + entry-count Int are released by the underlying
;;      `nl_vector_set_slot' helper's drop-then-write discipline).
;;   5. Overwrite scratch-slot in place with `Sexp::Int(new-depth)'
;;      via `sexp-int-make' (= safe: scratch currently holds
;;      `Sexp::Nil' which has no heap, so the unconditional tag-byte
;;      + payload write does not leak).
;;   6. Install the new depth into frames-record slot 1 via refcount-
;;      aware `record-slot-set' (= drops the old `Sexp::Int' depth and
;;      clones the new one).  `Sexp::Int' has no heap so no refcount
;;      bumps actually occur.
;;   7. Return 1.
;;
;; Recursion is not needed here (no inner loops); the helper splits
;; the two-step write into a sub-defun so the `if depth==0' guard
;; stays in the entry point.
;;
;; Signature change: the function now takes a `scratch-slot' 2nd
;; parameter (= caller-owned `*mut Sexp' initialised to `Sexp::Nil')
;; to hold the new depth `Sexp::Int' before installing it into
;; slots[1].  The safe Rust wrapper in `build-tool/src/lib.rs'
;; allocates this slot on the call stack and passes its pointer, so
;; the probe tests continue to call with the original 1-arg
;; signature.
;;
;; Tag-check posture: same as §115.1 / §111.E #19 — no `sexp-tag'
;; gate on the frames-record because the bootstrap-installed shape
;; (= `Sexp::Record(`nelisp-lexframe-stack')') is the only legal
;; one.  Callers that may see `Sexp::Nil' (= mirror unbuilt) route
;; through other paths.
;;
;; ABI deps satisfied:
;;   §111.B  `record-slot-ref-ptr' — slot 1 pointer.
;;   §111.B  `record-slot-set'     — refcount-safe slot 1 install.
;;   §111.E  `vector-slot-set'     — refcount-safe backing[i] = Nil.
;;   §100    `sexp-int-unwrap'     — depth i64 read.
;;   §100    `sexp-int-make'       — scratch slot Sexp::Int writer.

;;; Code:

(defconst nelisp-cc-frame-pop--source
  '(seq
    (defun nelisp_frame_pop_inner (frames-ptr scratch-slot new-depth _pad)
      ;; frames-ptr:   *const Sexp pointing at Env::frames_record.
      ;; scratch-slot: *mut Sexp — currently holds `Sexp::Nil'.
      ;; new-depth:    i64 — the post-pop depth (= old-depth - 1).
      ;; _pad:         unused — Doc 124.F-blocker fix.  Outer arity is
      ;;               kept *even* (= 4) so body-entry rsp ≡ 0 mod 16,
      ;;               which matches the static rsp-alignment of the
      ;;               `vector-slot-set' emit (= 3 push + 3 pop = net
      ;;               zero, so call site inherits body-entry parity).
      ;;
      ;; Two-step refcount-safe write: backing[new-depth] := Nil,
      ;; then depth := Int(new-depth).  Order matters because
      ;; backing is reached via slot 0 which we do not touch; depth
      ;; is reached via slot 1 which we overwrite last.
      ;;
      ;; The `and' threads the rax = 1 sentinel from each side-effect
      ;; op (= the Phase 47 idiom for value-form sequencing).
      ;; `vector-slot-set' (= §111.E + §115.1 rax = 1 epilogue),
      ;; `sexp-int-make' (= returns slot pointer in rax which is
      ;; non-zero hence truthy), and `record-slot-set' (= §111.B
      ;; rax = 1 epilogue) all produce truthy values, so the chain
      ;; reaches the final `1' constant.
      (and (vector-slot-set (record-slot-ref-ptr frames-ptr 0)
                            new-depth
                            scratch-slot)
           (sexp-int-make scratch-slot new-depth)
           (record-slot-set frames-ptr 1 scratch-slot)
           1))
    (defun nelisp_frame_pop (frames-ptr scratch-slot)
      ;; frames-ptr:   *const Sexp pointing at Env::frames_record (=
      ;;               Sexp::Record(`nelisp-lexframe-stack')).
      ;; scratch-slot: *mut Sexp — caller-owned 32-byte slot
      ;;               initialised to `Sexp::Nil'; reused as the
      ;;               Nil-source for vector-slot-set then overwritten
      ;;               to hold the new depth `Sexp::Int' before
      ;;               record-slot-set.
      ;;
      ;; Returns: i64 — 1 on pop, 0 on no-op (= empty stack).
      ;;
      ;; Caller's pre-condition: same as `nelisp_frame_stack_depth' —
      ;; frames-ptr.tag = Sexp::Record AND slots[0..1] are the
      ;; bootstrap-installed Sexp::Vector(BACKING) / Sexp::Int(DEPTH)
      ;; shape.  No tag-check here because the only legal frames-
      ;; record shape meets the precondition.
      (if (< 0 (sexp-int-unwrap (record-slot-ref-ptr frames-ptr 1)))
          (nelisp_frame_pop_inner
           frames-ptr
           scratch-slot
           (- (sexp-int-unwrap (record-slot-ref-ptr frames-ptr 1)) 1)
           0) ; _pad — Doc 124.F-blocker even-arity fix
        0)))
  "Phase 47 source for Doc 111 §111.E #22 / Doc 115 §115.2
`frame_pop_rust_direct'.

Pure-elisp lexframe pop.  Composes `record-slot-ref-ptr' (§111.B),
`record-slot-set' (§111.B), `vector-slot-set' (§111.E),
`sexp-int-unwrap' / `sexp-int-make' (§100), and a single-level
helper-function call for the two-step refcount-safe write
(backing[depth-1] := Nil + depth := Int(depth-1)).

Replaces the ~30 LOC Rust shim `nl_frame_pop' which has been
removed from `env_lexframe_phase47_shims.rs'.")

(provide 'nelisp-cc-frame-pop)

;;; nelisp-cc-frame-pop.el ends here
