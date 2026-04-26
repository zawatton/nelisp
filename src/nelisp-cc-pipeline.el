;;; nelisp-cc-pipeline.el --- T158 SSA pass pipeline integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T158 — wires the Phase 7.7.x SSA optimisation passes (Doc 42) into the
;; codegen pipeline so they actually run on the SSA function before
;; linear-scan + backend.  Pre-T158 the four passes shipped as standalone
;; libraries with full ERT coverage but were *not* invoked by
;; `nelisp-cc-runtime-compile-and-allocate'; T158 closes that gap by
;; introducing one driver `nelisp-cc-pipeline-run-7.7-passes' that the
;; runtime calls between `build-ssa-from-ast' and `linear-scan'.
;;
;; The integration point is deliberately the *runtime layer*, NOT
;; `nelisp-cc-build-ssa-from-ast' itself.  Reasons:
;;
;;   1. Many ERT tests assert exact post-build SSA opcode shapes
;;      (e.g. `(closure return)' for `(lambda () (lambda (x) x))').
;;      Mutating SSA inside the frontend would force a sweeping test
;;      rewrite for no functional gain — the optimisations only matter
;;      when the bytes are about to execute.
;;
;;   2. Keeping the frontend "pure SSA build" preserves the testing
;;      pyramid Doc 28 §3.1 prescribes: frontend tests stay structural,
;;      pipeline tests stay behavioural.
;;
;; Pass order (per task spec, mirroring Doc 42 §3 phase numbering):
;;
;;   1. `nelisp-cc-escape-analyze'      — verdict produced fresh per pass
;;   2. `nelisp-cc-inline-pass'         — T147 simple leaf inliner
;;   3. `nelisp-cc-rec-inline-pass'     — T151 self-recursive unroller
;;   4. `nelisp-cc-lift-pass'           — T155 lambda → top-level lift
;;
;; The escape verdict is rebuilt between passes because each prior pass
;; can mutate the SSA in ways that change escape conclusions
;; (e.g. inlining drops a `:call' use, freeing the operand from
;; `:escapes-via-call'; lifting rewrites `:call-indirect' → `:call' which
;; reshapes the use graph entirely).
;;
;; Registry construction — the inliner needs `((NAME . SSA-FUNCTION) ...)'
;; to resolve `:fn' meta on `:call' instructions.  T158 walks every
;; `:closure' instruction inside CALLER (using the same depth-first walk
;; as `nelisp-cc-callees--collect-inner-functions') and indexes each
;; `:inner-function' under its `:letrec-name' meta when present.  This
;; gives the inliner access to letrec-bound recursive lambdas without
;; requiring the caller to maintain a separate registry.
;;
;; Bench-form caveat (recorded so the negative result has provenance):
;; the 3-axis bench `fib` form uses `(letrec ((fib (lambda (n) ...)))
;; (funcall fib 30))'.  Inside `fib`'s body the recursion goes
;; `(funcall fib ...)' which lowers to `:call-indirect`, NOT a named
;; `:call`.  The simple/recursive inliner only sees `:call` opcodes
;; (named callee, `:fn' meta) — so neither pass fires on the
;; bench-form recursion sites.  Lambda-lift COULD rewrite the
;; `call-indirect' sites to direct `:call' (its lift-decide gate
;; targets `(:closure …)' instructions used purely as callees), but
;; the bench's `fib' closure escapes via `store-var' (the letrec
;; placeholder write) which the lift gate rejects with
;; `:escapes-besides-callee'.  T158 surfaces this as the realistic
;; outcome — the wire infra is correct, the bench-form shape is the
;; blocker, and the next phase is callee-resolution from
;; `:call-indirect' sites whose target traces back to a known
;; `:closure'.

;;; Code:

(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-escape)
(require 'nelisp-cc-inline)
(require 'nelisp-cc-recursive-inline)
(require 'nelisp-cc-lambda-lift)
(require 'nelisp-cc-rewrite)

;;; Feature flag ---------------------------------------------------

(defcustom nelisp-cc-enable-7.7-passes nil
  "Enable Phase 7.7 inliner passes in the codegen pipeline.
When non-nil, `nelisp-cc-runtime-compile-and-allocate' runs the four
Phase 7.7 passes (escape + simple inline + recursive inline +
lambda lift) between `nelisp-cc-build-ssa-from-ast' and the linear-
scan register allocator.

T161 default flip: the SSA-level passes are correct (full ERT
coverage) but the lambda-lift pass synthesises `:call :fn
<CALLER>$lift$N' targets that the current backend's
`nelisp-cc--link-unresolved-calls' cannot resolve (the synthetic
name is not in the trampoline table → fixup left at 0 → segfault on
exec).  Keep this flag nil until the Phase 7.7.5+ backend learns to
emit + resolve synthetic callee bodies in the same translation unit.
Tests that need the SSA-level outcome bind the variable locally —
see `test/nelisp-cc-pipeline-test.el' and
`test/nelisp-cc-rewrite-test.el'."
  :type 'boolean
  :group 'nelisp-cc)

(defcustom nelisp-cc-pipeline-rec-inline-depth-limit 2
  "Pipeline-effective depth-limit override for the recursive inliner.
Bound around `nelisp-cc-rec-inline-pass' invocations from the pipeline
driver to avoid the exponential block growth (= snapshot bug — each
self-call splice clones the *grown* function, not the pre-pass body).
Default 2 keeps fib-30 unroll at 7 inlined sites / ~700 blocks
(~10 ms compile latency).  Raise this once
`nelisp-cc--rec-inline-clone-blocks' learns to snapshot the original
callee body; for now the depth-limit is the conservative gate."
  :type 'integer
  :group 'nelisp-cc)

;;; Statistics container -------------------------------------------

(cl-defstruct (nelisp-cc-pipeline-stats
               (:constructor nelisp-cc-pipeline-stats-make)
               (:copier nil))
  "Aggregated counts produced by `nelisp-cc-pipeline-run-7.7-passes'.
SIMPLE-INLINED is the total number of `:call' instructions splice-
replaced by the simple inliner (T147).  REC-INLINED is the count of
self-recursive splice events (T151) — possibly larger than the
number of source-level call sites because the unroller iterates up
to `nelisp-cc-rec-inline-depth-limit'.  LIFTED is the count of
`:closure' instructions rewritten to top-level direct `:call' (T155).
REWROTE-CALL-INDIRECT is the count of `:call-indirect' instructions
converted to `:call' by the T161 pre-pass (= load-var callee whose
name is in the registry).  REGISTRY-SIZE is the number of
`(NAME . FN)' entries the driver synthesised from walking the SSA's
nested `:closure' instructions."
  (simple-inlined 0)
  (rec-inlined 0)
  (lifted 0)
  (rewrote-call-indirect 0)
  (registry-size 0))

;;; Registry construction ------------------------------------------

(defun nelisp-cc-pipeline--collect-letrec-callee-registry (function)
  "Walk FUNCTION + every nested `:closure' inner-function, return registry alist.

The result is `((NAME . INNER-SSA) ...)' where NAME is the
`:letrec-name' meta carried on the parent `:closure' instruction.
Closures without a `:letrec-name' (= anonymous lambdas not bound by
letrec) are skipped — they have no callable name to match against
`:call' meta `:fn' anyway.

Walk order matches `nelisp-cc-callees--collect-inner-functions' so
the registry composition is deterministic across builds."
  (let ((acc nil))
    (cl-labels ((walk (fn)
                  (dolist (blk (nelisp-cc--ssa-function-blocks fn))
                    (dolist (instr (nelisp-cc--ssa-block-instrs blk))
                      (when (eq (nelisp-cc--ssa-instr-opcode instr) 'closure)
                        (let* ((meta (nelisp-cc--ssa-instr-meta instr))
                               (inner (plist-get meta :inner-function))
                               (name (plist-get meta :letrec-name)))
                          (when (and inner name)
                            ;; Stamp the inner function's name so the
                            ;; recursive-inline pass's self-name match
                            ;; (`(eq callee-name self-name)`) succeeds.
                            (unless (nelisp-cc--ssa-function-name inner)
                              (setf (nelisp-cc--ssa-function-name inner) name))
                            (push (cons name inner) acc))
                          (when inner (walk inner))))))))
      (walk function))
    (nreverse acc)))

;;; Per-function pass driver ---------------------------------------

(defun nelisp-cc-pipeline--run-passes-on-fn (fn registry stats)
  "Run the 4 passes on FN with REGISTRY (inliner registry alist).

Mutates FN in place; bumps the corresponding fields of STATS.
Returns FN."
  ;; (0) T161 — rewrite eligible `:call-indirect' sites whose
  ;; load-var callee name appears in REGISTRY.  Run *before* the
  ;; inliner so the simple/recursive inliner can resolve the callee
  ;; via the new `:fn' meta.
  (let* ((rewrite-result (nelisp-cc-rewrite-call-indirect-to-call fn registry))
         (rewrite-count (cdr rewrite-result)))
    (cl-incf (nelisp-cc-pipeline-stats-rewrote-call-indirect stats)
             rewrite-count))
  ;; (1) escape verdict (fresh per pass run — see file commentary).
  (let* ((escape-info (nelisp-cc-escape-analyze fn))
         ;; (2) simple inline (T147).
         (inline-result (nelisp-cc-inline-pass fn escape-info registry))
         (after-simple (car inline-result))
         (simple-count (cdr inline-result)))
    (cl-incf (nelisp-cc-pipeline-stats-simple-inlined stats) simple-count)
    ;; (3) re-build escape verdict — simple-inline mutated the use graph.
    (let* ((escape-info-2 (nelisp-cc-escape-analyze after-simple))
           (rec-result (nelisp-cc-rec-inline-pass
                        after-simple escape-info-2 registry
                        nelisp-cc-pipeline-rec-inline-depth-limit))
           (after-rec (car rec-result))
           (rec-count (cdr rec-result)))
      (cl-incf (nelisp-cc-pipeline-stats-rec-inlined stats) rec-count)
      ;; (4) re-build escape verdict again — rec-inline cloned blocks.
      (let* ((escape-info-3 (nelisp-cc-escape-analyze after-rec))
             (lift-result (nelisp-cc-lift-pass after-rec escape-info-3 registry))
             (after-lift (car lift-result))
             (lift-count (cdr lift-result)))
        (cl-incf (nelisp-cc-pipeline-stats-lifted stats) lift-count)
        after-lift))))

;;; Top-level entry ------------------------------------------------

;;;###autoload
(defun nelisp-cc-pipeline-run-7.7-passes (function)
  "Run Doc 42 §3 SSA passes on FUNCTION; return `(FUNCTION . STATS)'.

FUNCTION is mutated in place when any pass fires; the returned
FUNCTION is `eq' to the input.  STATS is a `nelisp-cc-pipeline-stats'
struct tallying per-pass action counts (useful for the bench
report's findings narrative — when all counts are zero the bench
result regresses to the T96 12x baseline + the negligible cost of
the verdict re-builds).

The function is a *no-op* when `nelisp-cc-enable-7.7-passes' is nil
— STATS comes back zero-filled and no pass library is invoked.
This is the A/B-toggle entry the perf reports use to attribute the
delta to the wire vs other contemporaneous changes.

The driver runs the passes on every nested inner-function first
(reachable through `:closure' instructions, indexed by registry),
then on FUNCTION itself.  Inner-first ordering matters because the
outer's lambda-lift step renames a hoisted closure's inner SSA
function to `<CALLER>$lift$N' — running inner passes after that
rename would cause the recursive inliner's `(eq callee-name
self-name)' self-detection to fail (call sites still carry the
original letrec name in their `:fn' meta).  Inner passes get their
own escape verdict + their own pass sequence, keeping per-function
complexity bounded at O(N) over each function's instruction count,
exactly as Doc 42 §4.2 specifies."
  (let ((stats (nelisp-cc-pipeline-stats-make)))
    (when nelisp-cc-enable-7.7-passes
      (let ((registry (nelisp-cc-pipeline--collect-letrec-callee-registry
                       function)))
        (setf (nelisp-cc-pipeline-stats-registry-size stats)
              (length registry))
        ;; (a) every inner — they may themselves contain calls that
        ;; benefit from the same passes (= the bench-form `fib'
        ;; recursion sites).  Run inner passes BEFORE the outer to
        ;; preserve the inner's `:letrec-name'-derived self-name for
        ;; the recursive inliner's self-detection.
        (dolist (cell registry)
          (let ((inner (cdr cell)))
            (nelisp-cc-pipeline--run-passes-on-fn inner registry stats)))
        ;; (b) outer.
        (nelisp-cc-pipeline--run-passes-on-fn function registry stats)))
    (cons function stats)))

(provide 'nelisp-cc-pipeline)

;;; nelisp-cc-pipeline.el ends here
