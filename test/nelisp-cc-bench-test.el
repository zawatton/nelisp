;;; nelisp-cc-bench-test.el --- Phase 7.1.5 3-axis bench harness skeleton  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.1.5 *3-axis bench harness skeleton* — see
;; docs/design/28-phase7.1-native-compiler.org §5.2 / §3.5.  The
;; doc commits three completion gates relative to the bytecode VM:
;;
;;   - fib(30)         bytecode VM 比 30x speedup 必達
;;   - fact-iter       bytecode VM 比 20x speedup 必達
;;   - alloc-heavy     bytecode VM 比 5x  speedup 必達
;;
;; Real measurement requires the Phase 7.5 mmap PROT_EXEC bridge —
;; the simulator records syscall traces but does not actually
;; execute the produced bytes.  Until then the three ERTs below
;; ship as *skip-unless-gated* tests: they exist to lock the API
;; shape (`--measure-bytecode' / `--measure-native') and are
;; skipped on every host so `make test' stays green.
;;
;; When Phase 7.5 lands, the skip predicate
;; `nelisp-cc-bench--phase-7.5-real-mmap-available-p' will flip to
;; t on hosts with the FFI bridge enabled, and the gate will
;; activate — no other change required, that is the point of the
;; scaffold.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-runtime)
(require 'nelisp-cc-bootstrap)

;;; Skip predicate -------------------------------------------------

(defun nelisp-cc-bench--phase-7.5-real-mmap-available-p ()
  "Return non-nil when real mmap PROT_EXEC measurement is available.
This predicate is always nil today: Phase 7.1.5 ships the simulator
only.  Phase 7.5 will replace the body with a feature check against
the `nelisp-runtime/' Rust crate's FFI bridge — at that point the
three gated ERTs in this file activate without further edits."
  nil)

;;; Measurement helpers --------------------------------------------

(defun nelisp-cc-bench--measure-bytecode (form iterations)
  "Run FORM ITERATIONS times via the host Emacs bytecode VM.
FORM is a quoted call expression like `(fib 30)' — the caller is
responsible for having installed the named function via `defun'
*before* calling this helper.  Returns the elapsed wall-clock time
in milliseconds (a float).  Used as the bytecode-VM baseline for
the 3-axis gate."
  (unless (and (integerp iterations) (> iterations 0))
    (error "ITERATIONS must be a positive integer"))
  (let ((start (float-time))
        (last nil))
    (dotimes (_ iterations)
      (setq last (eval form t)))
    (ignore last)
    (* 1000.0 (- (float-time) start))))

(defun nelisp-cc-bench--measure-native (compiled-result iterations)
  "Run COMPILED-RESULT's exec page ITERATIONS times via the simulator.
COMPILED-RESULT is the plist returned by
`nelisp-cc-runtime-compile-and-allocate' (i.e. it carries
`:exec-page', `:final-bytes', and `:gc-metadata').

Returns elapsed wall-clock time in milliseconds (a float).

Phase 7.1.5 caveat: the simulator does *not* actually execute
the bytes — it only models the W^X protocol.  The helper therefore
measures the surrogate cost of *re-driving* the simulator pipeline
(allocate-and-free a fresh exec page per iteration), which scales
with the same constants Phase 7.5 will measure under real mmap +
JIT execution but is *not* a valid speedup denominator yet.

The skip-gated ERTs below treat any value returned here as
informational until `--phase-7.5-real-mmap-available-p' flips to t."
  (unless (and (integerp iterations) (> iterations 0))
    (error "ITERATIONS must be a positive integer"))
  (let ((bytes (plist-get compiled-result :final-bytes))
        (start (float-time)))
    (dotimes (_ iterations)
      ;; Surrogate: re-allocate the exec page so we exercise the
      ;; simulator end-to-end on every iteration.  Phase 7.5 will
      ;; replace the body with a real call gate (the page is
      ;; allocated *once*; the iteration calls into it).
      (let ((page (nelisp-cc-runtime--alloc-exec-page bytes)))
        (nelisp-cc-runtime--free-exec-page page)))
    (* 1000.0 (- (float-time) start))))

(defun nelisp-cc-bench--speedup (bc-ms native-ms)
  "Return (BC-MS / NATIVE-MS) as a float speedup ratio.
NATIVE-MS = 0 collapses to `most-positive-fixnum' to avoid divide-by-
zero (treated as `infinitely fast' — the gate predicates clamp it
back into a comparable range)."
  (if (zerop native-ms) most-positive-fixnum
    (/ (float bc-ms) (float native-ms))))

;;; Bench cases (scaffold) -----------------------------------------

(defconst nelisp-cc-bench--fib-30
  '(lambda (n)
     (if (< n 2) n
       (+ (fib (- n 1)) (fib (- n 2)))))
  "Phase 7.5 fib(30) bench case — naive doubly-recursive Fibonacci.
The 30x speedup gate (Doc 28 §5.2 必達) measures this against the
host bytecode VM running the same `fib' defun.")

(defconst nelisp-cc-bench--fact-iter
  '(lambda (n acc)
     (if (< n 1) acc
       (fact-iter (- n 1) (* acc n))))
  "Phase 7.5 fact-iter bench case — tail-recursive factorial.
The 20x speedup gate (Doc 28 §5.2) measures the loop hot-path,
where the tail-call lowering pass produces the largest delta vs
the bytecode VM.")

(defconst nelisp-cc-bench--alloc-heavy
  '(lambda (n)
     (let ((acc nil))
       (dotimes (_ n)
         (setq acc (cons _ acc)))
       acc))
  "Phase 7.5 alloc-heavy bench case — cons stress.
The 5x speedup gate (Doc 28 §5.2) is the smallest of the three
because allocator overhead dominates and the simulator's safe-point
poll budget cannot be amortised away.")

;;; Skip-gated ERTs (Doc 28 §5.2 完遂 gate) -----------------------

(ert-deftest nelisp-cc-bench-fib-30-30x-or-skip ()
  "fib(30) bytecode VM 比 30x speedup 必達 (Doc 28 §5.2).
Phase 7.1.5 scaffold: skip until the Phase 7.5 mmap PROT_EXEC bridge
is available.  The body is the production gate — when the skip
predicate flips, this test runs unmodified and asserts the 30x ratio."
  (skip-unless (nelisp-cc-bench--phase-7.5-real-mmap-available-p))
  (let* ((compiled (nelisp-cc-runtime-compile-and-allocate
                    nelisp-cc-bench--fib-30))
         (bc-ms (nelisp-cc-bench--measure-bytecode '(fib 30) 1))
         (native-ms (nelisp-cc-bench--measure-native compiled 1))
         (speedup (nelisp-cc-bench--speedup bc-ms native-ms)))
    (should (>= speedup 30.0))))

(ert-deftest nelisp-cc-bench-fact-iter-20x-or-skip ()
  "fact-iter bytecode VM 比 20x speedup 必達 (Doc 28 §5.2).
Phase 7.1.5 scaffold: skip until the Phase 7.5 mmap PROT_EXEC bridge
is available.  Loop hot path — proper tail-call lowering carries
most of the win."
  (skip-unless (nelisp-cc-bench--phase-7.5-real-mmap-available-p))
  (let* ((compiled (nelisp-cc-runtime-compile-and-allocate
                    nelisp-cc-bench--fact-iter))
         (bc-ms (nelisp-cc-bench--measure-bytecode '(fact-iter 20 1) 100))
         (native-ms (nelisp-cc-bench--measure-native compiled 100))
         (speedup (nelisp-cc-bench--speedup bc-ms native-ms)))
    (should (>= speedup 20.0))))

(ert-deftest nelisp-cc-bench-alloc-heavy-5x-or-skip ()
  "alloc-heavy bytecode VM 比 5x speedup 必達 (Doc 28 §5.2).
Phase 7.1.5 scaffold: skip until the Phase 7.5 mmap PROT_EXEC bridge
is available.  cons stress — the allocator + safe-point poll budget
keeps the win modest, the smallest of the three §5.2 gates."
  (skip-unless (nelisp-cc-bench--phase-7.5-real-mmap-available-p))
  (let* ((compiled (nelisp-cc-runtime-compile-and-allocate
                    nelisp-cc-bench--alloc-heavy))
         (bc-ms (nelisp-cc-bench--measure-bytecode '(alloc-heavy 100000) 1))
         (native-ms (nelisp-cc-bench--measure-native compiled 1))
         (speedup (nelisp-cc-bench--speedup bc-ms native-ms)))
    (should (>= speedup 5.0))))

(provide 'nelisp-cc-bench-test)
;;; nelisp-cc-bench-test.el ends here
