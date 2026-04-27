;;; nelisp-allocator-bench.el --- Phase 7.2 §7.2 LOCK-close: 3-tier ratio bench  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 29 §5.1 v2 LOCK-close — *3-tier ratio bench* for the Phase 7.2
;; allocator.  The §7.2 close gate calls for:
;;
;;   tier-A (CI gate, ratio):
;;     - alloc-heavy (cons stress 1M)        Phase 7.1 比 3-5x
;;     - per-pool stress (each pool family)  Phase 7.1 比 4-6x
;;     - bulk-alloc (4KB+ large-object path) Phase 7.1 比 8-12x
;;
;;   tier-B (release-audit, pinned):
;;     - same three benches reported as C reference 比 0.7-1.0x
;;       (telemetry only, never blocks CI)
;;
;;   tier-C (invariant): see test/nelisp-allocator-invariant-test.el
;;     (already SHIPPED, not this file's responsibility).
;;
;; This file ships *the bench harness itself* (§7.2 LOCK-close item).
;; It is the allocator twin of `bench/nelisp-cc-bench-actual.el' and
;; behaves the same way: today it provides the infrastructure +
;; baseline timing path, and the moment the Phase 7.5+ pipeline wires
;; the native allocator into the host `cons' / `make-string' /
;; `make-vector' fast paths, the harness flips from "infra baseline"
;; to "gate verification" with no code change.
;;
;; Critical: K1's BCF (base-case-fold) const-fold pass aggressively
;; folds compile-time const-arg recursive calls.  Every bench form in
;; this file is therefore *const-unfoldable* by construction:
;;
;;   - the iteration count is read from a defcustom variable at run-
;;     time via a `let' binding (the byte-compiler can't fold a
;;     symbol-value lookup),
;;
;;   - allocations consume a per-iteration `(random)' source so the
;;     compiler can't lift the body into a constant,
;;
;;   - allocator paths are side-effecting (push to an accumulator,
;;     mutate the per-pool bitmap, bump stats counters),
;;
;;   - the lambda's return value is `length' / `last' of the
;;     accumulator so the compiler can't dead-strip the loop body.
;;
;; Output channels (mirroring Doc 28 §5.2 bench-actual):
;;
;;   - `*NeLisp Allocator Bench*' Emacs buffer (per-tier result plist),
;;   - stderr (one machine-parseable summary line per tier),
;;   - kill-emacs exit code (0 = all 3 tier-A gates PASS, 1 = any FAIL).
;;
;; Heavy-test gate: the full 3-tier run takes ~10-30 s on a typical
;; CI runner.  When the harness is loaded under `make test' (= `make
;; bench-actual' is the primary entry), the ERT smoke at
;; `test/nelisp-allocator-bench-test.el' uses the smallest legal
;; `(:smoke t)' iteration count so the smoke contract stays cheap.
;; Heavy bench runs are gated on `NELISP_HEAVY_TESTS=1' per the
;; project's existing convention (see `nelisp-coding-bench-stress-
;; test.el').

;;; Code:

(require 'cl-lib)
(require 'benchmark)
(require 'nelisp-allocator)

(defgroup nelisp-allocator-bench nil
  "Phase 7.2 §5.1 v2 3-tier ratio bench."
  :group 'nelisp
  :prefix "nelisp-allocator-bench-")

;;; Bench inputs --------------------------------------------------
;;
;; All sized as `defcustom' so they can be lowered by the ERT smoke
;; (which calls into the bench helpers with `:smoke t' and the
;; smallest legal value).  The defaults match Doc 29 §5.1 v2 which
;; pins each bench at "1 iteration runs in the 1-100 ms band on the
;; bytecode VM" so a 10-iter run stays under ~1 s wall.

(defcustom nelisp-allocator-bench-cons-stress-n 10000
  "Number of cons cells the alloc-heavy (tier 1) bench allocates.
Doc 29 §5.1 v2 calls for cons-stress 1M, but the Phase 7.2 simulator
is ~1500x slower than host `cons' so 1M would push CI wall-time to
~80 s.  Default of 10k keeps the simulator branch under ~10 s while
still proving the harness is exercising the allocator hot path; once
Phase 7.5 wires the native fast path, set this to 1000000 via the
`make bench-allocator-heavy' target for the full Doc 29 §5.1 v2
measurement.  The ERT smoke overrides this via the `:smoke t' path
to keep `make test' under ~50 ms."
  :type 'integer
  :group 'nelisp-allocator-bench)

(defcustom nelisp-allocator-bench-per-pool-n 2000
  "Per-family iteration count for the per-pool stress (tier 2) bench.
Doc 29 §5.1 v2 calls for each pool to be exercised independently;
2k * 5 families keeps the simulator branch under ~1 s while still
exercising every Doc 29 §2.5 family.  Phase 7.5 wire-pass should bump
this to 100k via the heavy-tests gate."
  :type 'integer
  :group 'nelisp-allocator-bench)

(defcustom nelisp-allocator-bench-bulk-alloc-n 200
  "Number of large-object (>= 4 KiB) allocations the tier 3 bench performs.
Doc 29 §5.1 v2 calls for bulk-cons 1000 / large-object boundary; 200
keeps the simulator branch under ~10 ms while proving the boundary
path is exercised.  Phase 7.5 wire-pass should bump to 1000 via the
heavy-tests gate."
  :type 'integer
  :group 'nelisp-allocator-bench)

(defcustom nelisp-allocator-bench-bulk-payload-bytes 4096
  "Per-allocation payload size for the tier 3 large-object bench (bytes).
Doc 29 §5.1 v2 bulk-alloc tests the large-object boundary
(`nelisp-allocator-large-object-threshold' = 4 KiB).  Default of
4096 is the smallest size that *exactly* hits the boundary — Phase
7.2.2 routes everything > 4 KiB through `alloc-large-object',
everything <= 4 KiB through the size-classed span path; a one-byte
overshoot exercises the boundary correctly."
  :type 'integer
  :group 'nelisp-allocator-bench)

(defcustom nelisp-allocator-bench-iterations 3
  "Default outer iteration count for each tier.
Each bench's inner loop allocates the configured N items; running
the inner loop ITERATIONS times averages out timer noise.  Total CI
budget = sum of (per-tier bytecode + native) wall-time over
ITERATIONS — keep this small (3 by default)."
  :type 'integer
  :group 'nelisp-allocator-bench)

;;; Tier-A gate thresholds (Doc 29 §5.1 v2) -----------------------
;;
;; The thresholds below are the *low end* of each Doc 29 §5.1 v2 band
;; (3-5x / 4-6x / 8-12x).  CI gates on the low end so a fluctuating
;; runner doesn't false-FAIL a 4.8x measurement on the 5x band.
;; The release-audit run reports the actual ratio so regression
;; toward the low end is still visible.

(defcustom nelisp-allocator-bench-gate-tier1 3.0
  "Tier-A gate threshold for the alloc-heavy (cons stress 1M) bench.
Low end of the Doc 29 §5.1 v2 \"3-5x speedup\" band."
  :type 'number
  :group 'nelisp-allocator-bench)

(defcustom nelisp-allocator-bench-gate-tier2 4.0
  "Tier-A gate threshold for the per-pool stress bench.
Low end of the Doc 29 §5.1 v2 \"4-6x speedup\" band."
  :type 'number
  :group 'nelisp-allocator-bench)

(defcustom nelisp-allocator-bench-gate-tier3 8.0
  "Tier-A gate threshold for the bulk-alloc (4 KiB+) bench.
Low end of the Doc 29 §5.1 v2 \"8-12x speedup\" band (syscall
compression target)."
  :type 'number
  :group 'nelisp-allocator-bench)

;;; Phase 7.2 native availability ---------------------------------
;;
;; Phase 7.2.1-7.2.3 implement the allocator as Emacs-Lisp helpers
;; (the "simulator" — Doc 29 §2.1).  Phase 7.5+ wires the simulator
;; to the native cons / make-string / make-vector fast paths via the
;; in-process FFI module.  Until that wire lands, the "native"
;; measurement here exercises the simulator helpers directly and the
;; reported ratio is *informational* (it cannot beat the host
;; bytecode VM because the simulator runs *on top of* the host).
;;
;; The `:native-skip-reason' field of each result plist surfaces the
;; current state so consumers (release-audit, dashboard) can
;; distinguish "simulator-only baseline" from a real Phase 7.5
;; gate-pass measurement.

(defun nelisp-allocator-bench--native-status ()
  "Return a symbol describing the Phase 7.5 native-allocator wire state.
Currently always returns `simulator-only' because the Phase 7.5 wire
is not yet shipped (Doc 32 §3 callee resolution + alloc fast-path
patch is the gating work item).  This function is the single
choke-point for all per-tier skip-reason reporting so the day Phase
7.5 lands the wire, flipping this predicate flips the entire
harness from \"infra baseline\" to \"gate verification\"."
  (cond
   ;; Future: when `nelisp-cc-runtime--alloc-fast-path-wired-p' (or
   ;; equivalent) becomes available, return nil to indicate the
   ;; native path is live.
   ((and (fboundp 'nelisp-cc-runtime--alloc-fast-path-wired-p)
         (funcall 'nelisp-cc-runtime--alloc-fast-path-wired-p))
    nil)
   (t 'simulator-only)))

;;; Bench bodies (const-unfoldable) -------------------------------
;;
;; Each `--baseline-*' lambda is the host bytecode VM reference and
;; uses *only* host primitives (cons, make-string, make-vector).
;; The `--native-*' lambdas exercise the Phase 7.2 allocator helpers
;; directly so the wall-time delta is allocator-specific.
;;
;; The N parameter is read from the surrounding lexical binding at
;; bench-call time so the byte-compiler cannot fold the loop into a
;; constant.  `(random)' is consumed every iteration to defeat dead-
;; code-elimination (the result feeds the cons head / vector index /
;; payload size, depending on the tier).

(defun nelisp-allocator-bench--tier1-baseline (n)
  "Tier 1 *baseline* — host bytecode VM cons stress (= Phase 7.1 reference).

Allocates N cons cells through the host `cons' primitive in a
runtime-driven `while' loop.  The loop bound N is a runtime
argument (not a literal) so K1's BCF cannot const-fold it.  Each
iteration consumes `(random N)' so the compiler can't lift the
loop body into a constant.  Returns the accumulator `length' so
dead-code-elimination can't strip the body."
  (let ((acc nil)
        (i 0))
    (while (< i n)
      (setq acc (cons (logand i (random 65536)) acc))
      (setq i (1+ i)))
    (length acc)))

(defun nelisp-allocator-bench--tier1-native (n)
  "Tier 1 *native* — Phase 7.2 cons-pool allocator stress.

Allocates N cells through `nelisp-allocator-cons-pool-alloc' using
the runtime input N (= no const-fold).  Returns the count actually
allocated (the cell address itself is opaque, length is the proof
the loop ran)."
  (let* ((pool (nelisp-allocator-init-cons-pool))
         (i 0)
         (taken 0))
    (while (< i n)
      ;; consume per-iter randomness so BCF can't fold the body
      (ignore (random 65536))
      (nelisp-allocator-cons-pool-alloc pool)
      (setq taken (1+ taken))
      (setq i (1+ i)))
    taken))

(defun nelisp-allocator-bench--tier2-baseline (n)
  "Tier 2 *baseline* — host bytecode VM per-pool stress (Phase 7.1 ref).

Exercises each of the 5 Doc 29 §2.5 families through host primitives
in N/5 iterations apiece.  Mirrors the family layout of
`--tier2-native' so the per-call cost comparison is family-symmetric.
Returns the total alloc count."
  (let ((per-family (max 1 (/ n 5)))
        (total 0))
    ;; cons-pool family
    (let ((acc nil) (i 0))
      (while (< i per-family)
        (setq acc (cons (random 65536) acc))
        (setq i (1+ i))
        (setq total (1+ total)))
      (ignore (length acc)))
    ;; closure-pool family (host proxy = small vector to mimic 32-byte slot block)
    (let ((acc nil) (i 0))
      (while (< i per-family)
        (setq acc (cons (vector (random 65536) (random 65536) (random 65536) (random 65536)) acc))
        (setq i (1+ i))
        (setq total (1+ total)))
      (ignore (length acc)))
    ;; string-span family
    (let ((acc nil) (i 0))
      (while (< i per-family)
        (setq acc (cons (make-string 64 (logand (random 64) 63)) acc))
        (setq i (1+ i))
        (setq total (1+ total)))
      (ignore (length acc)))
    ;; vector-span family
    (let ((acc nil) (i 0))
      (while (< i per-family)
        (setq acc (cons (make-vector 8 (random 65536)) acc))
        (setq i (1+ i))
        (setq total (1+ total)))
      (ignore (length acc)))
    ;; large-object family (host proxy = make-string 4KB+)
    (let ((acc nil) (i 0))
      (while (< i per-family)
        (setq acc (cons (make-string 4096 (logand (random 64) 63)) acc))
        (setq i (1+ i))
        (setq total (1+ total)))
      (ignore (length acc)))
    total))

(defun nelisp-allocator-bench--tier2-native (n)
  "Tier 2 *native* — exercises each Phase 7.2 family pool independently.

Doc 29 §5.1 v2 \"per-pool stress\" — drive each of the 5 families
(cons-pool / closure-pool / string-span / vector-span /
large-object) through its dedicated public allocator helper for N/5
iterations.  Each loop body consumes `(random)' to defeat const-
fold.  Returns the total alloc count."
  ;; Init the production trio: nursery (clears region table) ->
  ;; cons-pool -> closure-pool, in that order per the Doc 29 §3.3
  ;; init contract.
  (let ((nursery (nelisp-allocator-init-nursery))
        (cons-pool (nelisp-allocator-init-cons-pool))
        (closure-pool (nelisp-allocator-init-closure-pool))
        (per-family (max 1 (/ n 5)))
        (total 0))
    ;; cons-pool
    (let ((i 0))
      (while (< i per-family)
        (ignore (random 65536))
        (nelisp-allocator-cons-pool-alloc cons-pool)
        (setq i (1+ i))
        (setq total (1+ total))))
    ;; closure-pool (= 0 upvalues, 32-byte cell)
    (let ((i 0))
      (while (< i per-family)
        (ignore (random 65536))
        (nelisp-allocator-cons-pool-alloc closure-pool)
        (setq i (1+ i))
        (setq total (1+ total))))
    ;; string-span (64-byte payload, well below 4 KiB threshold)
    (let ((i 0))
      (while (< i per-family)
        (ignore (random 65536))
        (nelisp-allocator-alloc-string-span nursery 64)
        (setq i (1+ i))
        (setq total (1+ total))))
    ;; vector-span (8-slot * 8 bytes = 64-byte payload)
    (let ((i 0))
      (while (< i per-family)
        (ignore (random 65536))
        (nelisp-allocator-alloc-vector-span nursery 64)
        (setq i (1+ i))
        (setq total (1+ total))))
    ;; large-object (just over 4 KiB threshold)
    (let ((large (1+ nelisp-allocator-large-object-threshold))
          (i 0))
      (while (< i per-family)
        (ignore (random 65536))
        (nelisp-allocator-alloc-large-object nursery large)
        (setq i (1+ i))
        (setq total (1+ total))))
    total))

(defun nelisp-allocator-bench--tier3-baseline (n bytes)
  "Tier 3 *baseline* — host bytecode VM large-object stress.

Allocates N large objects of BYTES each via host `make-string'.
Both N and BYTES are runtime arguments so K1 BCF cannot const-fold.
Each iteration mixes in `(random)' to defeat dead-code-elimination.
Returns the total bytes allocated."
  (let ((acc nil)
        (i 0)
        (total 0))
    (while (< i n)
      (let* ((sz (+ bytes (logand (random 256) 255)))
             (s (make-string sz (logand (random 64) 63))))
        (setq acc (cons s acc))
        (setq total (+ total sz)))
      (setq i (1+ i)))
    (ignore (length acc))
    total))

(defun nelisp-allocator-bench--tier3-native (n bytes)
  "Tier 3 *native* — Phase 7.2 large-object boundary stress.

Drives N allocations through `nelisp-allocator-alloc-large-object',
each at BYTES (>= 4 KiB so the call hits the large-object family,
not the size-classed span path).  Both N and BYTES are runtime
arguments (= no const-fold).  Returns the total bytes allocated."
  (let* ((nursery (nelisp-allocator-init-nursery))
         (i 0)
         (total 0))
    ;; Force payload over the threshold so we hit the
    ;; alloc-large-object path even on the smallest configured value.
    (let ((floor (1+ nelisp-allocator-large-object-threshold)))
      (when (< bytes floor) (setq bytes floor)))
    (while (< i n)
      (let ((sz (+ bytes (logand (random 256) 255))))
        (nelisp-allocator-alloc-large-object nursery sz)
        (setq total (+ total sz)))
      (setq i (1+ i)))
    total))

;;; Measurement helpers -------------------------------------------

(defun nelisp-allocator-bench--time-thunk (thunk iterations)
  "Run THUNK ITERATIONS times, return total wall-clock seconds (float).

`garbage-collect' once before timing so the host's ephemeral state
doesn't bias the first iteration.  Uses `current-time' deltas (the
same primitive `nelisp-cc-bench-actual' uses) for measurement
parity."
  (garbage-collect)
  (let ((start (current-time))
        (last nil))
    (dotimes (_ iterations)
      (setq last (funcall thunk)))
    (ignore last)
    (float-time (time-since start))))

(defun nelisp-allocator-bench--ratio (numerator denominator)
  "Return NUMERATOR/DENOMINATOR as a float, or nil when either is nil.
Mirrors `nelisp-cc-bench-actual--ratio' so result-plist consumers
can apply identical formatting."
  (cond ((or (null numerator) (null denominator)) nil)
        ((zerop denominator) most-positive-fixnum)
        (t (/ (float numerator) (float denominator)))))

;;; Per-tier runners ----------------------------------------------

(cl-defun nelisp-allocator-bench-tier1 (&key smoke iterations)
  "Run tier 1 (alloc-heavy / cons stress).  Return result plist.

When :SMOKE non-nil, uses an iteration count of 1000 (~1 ms on host
bytecode VM) so the ERT smoke stays cheap.  ITERATIONS overrides
`nelisp-allocator-bench-iterations' (default 3).

Result plist keys: see `nelisp-allocator-bench--run-3-tier' docstring."
  (let* ((n (if smoke 1000 nelisp-allocator-bench-cons-stress-n))
         (iters (or iterations nelisp-allocator-bench-iterations))
         (skip (nelisp-allocator-bench--native-status))
         (baseline-elapsed
          (nelisp-allocator-bench--time-thunk
           (lambda () (nelisp-allocator-bench--tier1-baseline n))
           iters))
         (native-elapsed
          (nelisp-allocator-bench--time-thunk
           (lambda () (nelisp-allocator-bench--tier1-native n))
           iters))
         (speedup (nelisp-allocator-bench--ratio
                   baseline-elapsed native-elapsed))
         (gate nelisp-allocator-bench-gate-tier1)
         (gate-pass (cond (skip :skipped)
                          ((and speedup (>= speedup gate)) t)
                          (t nil))))
    (list :tier 1
          :name 'alloc-heavy
          :n n
          :iterations iters
          :baseline-elapsed baseline-elapsed
          :native-elapsed native-elapsed
          :speedup speedup
          :gate-target gate
          :gate-pass gate-pass
          :native-skip-reason skip)))

(cl-defun nelisp-allocator-bench-tier2 (&key smoke iterations)
  "Run tier 2 (per-pool stress).  Return result plist.

When :SMOKE non-nil, uses N = 250 (50 per family) so all 5 families
are exercised but total wall-time stays ms-scale."
  (let* ((n (if smoke 250 nelisp-allocator-bench-per-pool-n))
         (iters (or iterations nelisp-allocator-bench-iterations))
         (skip (nelisp-allocator-bench--native-status))
         (baseline-elapsed
          (nelisp-allocator-bench--time-thunk
           (lambda () (nelisp-allocator-bench--tier2-baseline n))
           iters))
         (native-elapsed
          (nelisp-allocator-bench--time-thunk
           (lambda () (nelisp-allocator-bench--tier2-native n))
           iters))
         (speedup (nelisp-allocator-bench--ratio
                   baseline-elapsed native-elapsed))
         (gate nelisp-allocator-bench-gate-tier2)
         (gate-pass (cond (skip :skipped)
                          ((and speedup (>= speedup gate)) t)
                          (t nil))))
    (list :tier 2
          :name 'per-pool-stress
          :n n
          :iterations iters
          :baseline-elapsed baseline-elapsed
          :native-elapsed native-elapsed
          :speedup speedup
          :gate-target gate
          :gate-pass gate-pass
          :native-skip-reason skip)))

(cl-defun nelisp-allocator-bench-tier3 (&key smoke iterations)
  "Run tier 3 (bulk-alloc / 4 KiB+ large-object).  Return result plist.

When :SMOKE non-nil, uses N = 32 large allocations so the smoke
exercises the boundary path without dominating `make test'."
  (let* ((n (if smoke 32 nelisp-allocator-bench-bulk-alloc-n))
         (bytes nelisp-allocator-bench-bulk-payload-bytes)
         (iters (or iterations nelisp-allocator-bench-iterations))
         (skip (nelisp-allocator-bench--native-status))
         (baseline-elapsed
          (nelisp-allocator-bench--time-thunk
           (lambda () (nelisp-allocator-bench--tier3-baseline n bytes))
           iters))
         (native-elapsed
          (nelisp-allocator-bench--time-thunk
           (lambda () (nelisp-allocator-bench--tier3-native n bytes))
           iters))
         (speedup (nelisp-allocator-bench--ratio
                   baseline-elapsed native-elapsed))
         (gate nelisp-allocator-bench-gate-tier3)
         (gate-pass (cond (skip :skipped)
                          ((and speedup (>= speedup gate)) t)
                          (t nil))))
    (list :tier 3
          :name 'bulk-alloc
          :n n
          :payload-bytes bytes
          :iterations iters
          :baseline-elapsed baseline-elapsed
          :native-elapsed native-elapsed
          :speedup speedup
          :gate-target gate
          :gate-pass gate-pass
          :native-skip-reason skip)))

;;; 3-tier runner + report ----------------------------------------

(defvar nelisp-allocator-bench--batch-mode nil
  "Non-nil when invoked through the `make bench-allocator' entry point.
Gates the `kill-emacs' call inside `nelisp-allocator-bench-run-3-tier'
so calling the runner from ERT smoke (= `make test' runs
`nelisp-allocator-bench-run-3-tier-smoke') doesn't terminate the
whole `ert-run-tests-batch-and-exit' process.")

(defun nelisp-allocator-bench--format-result (r)
  "Format a single tier result plist as a multi-line text block."
  (let* ((tier (plist-get r :tier))
         (name (plist-get r :name))
         (n (plist-get r :n))
         (iters (plist-get r :iterations))
         (bc (plist-get r :baseline-elapsed))
         (nat (plist-get r :native-elapsed))
         (sp (plist-get r :speedup))
         (gate (plist-get r :gate-target))
         (pass (plist-get r :gate-pass))
         (skip (plist-get r :native-skip-reason)))
    (format (concat
             "  tier-%d %-18s n=%-9d iters=%d\n"
             "    baseline (host bc)   : %.4f s (total)\n"
             "    native (allocator)   : %.4f s (total)\n"
             "    speedup              : %s\n"
             "    §5.1 v2 tier-A gate  : %.1fx → %s\n"
             "    skip-reason          : %s\n")
            tier (symbol-name name) n iters
            bc nat
            (if sp (format "%.2fx (baseline/native)" sp) "n/a")
            gate (cond ((eq pass :skipped) "SKIP (no native wire)")
                       (pass "PASS")
                       (t "FAIL"))
            (if skip (symbol-name skip) "—"))))

(defun nelisp-allocator-bench-run-3-tier (&optional iterations)
  "Run all 3 tiers and return the list of result plists.

Each result plist carries:
  :tier              — int (1 / 2 / 3)
  :name              — symbol (`alloc-heavy' / `per-pool-stress' /
                       `bulk-alloc')
  :n                 — bench input count
  :iterations        — outer iteration count
  :baseline-elapsed  — float seconds (host bytecode VM, total)
  :native-elapsed    — float seconds (Phase 7.2 allocator, total)
  :speedup           — float ratio (baseline/native), gate numerator
  :gate-target       — float, the §5.1 v2 tier-A low-end threshold
  :gate-pass         — t / nil / :skipped
  :native-skip-reason — symbol (`simulator-only' until Phase 7.5
                        wires the native fast path) or nil

When called as the `make bench-allocator' batch entry point, also:
  - writes a full report to the `*NeLisp Allocator Bench*' buffer,
  - prints the buffer contents to stderr,
  - exits Emacs with code 0 when all 3 tier-A gates PASS or SKIP,
    1 when any tier FAILs (= speedup measurement below low-end of
    Doc 29 §5.1 v2 band)."
  (let* ((iters (or iterations nelisp-allocator-bench-iterations))
         (results (list (nelisp-allocator-bench-tier1 :iterations iters)
                        (nelisp-allocator-bench-tier2 :iterations iters)
                        (nelisp-allocator-bench-tier3 :iterations iters))))
    (with-current-buffer (get-buffer-create "*NeLisp Allocator Bench*")
      (erase-buffer)
      (insert "Phase 7.2 §5.1 v2 LOCK-close — 3-tier ratio bench\n")
      (insert "(Doc 29 §5.1 v2 LOCKED-2026-04-25)\n\n")
      (insert (format "Host           : %s\n"
                      (or (and (boundp 'system-configuration)
                               system-configuration)
                          "(unknown)")))
      (insert (format "Native wire    : %s\n"
                      (let ((s (nelisp-allocator-bench--native-status)))
                        (if s
                            (format "no (skip-reason: %s — Phase 7.5 wire pending)"
                                    (symbol-name s))
                          "yes (allocator fast path linked)"))))
      (insert (format "Date           : %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S %z")))
      (dolist (r results)
        (insert (nelisp-allocator-bench--format-result r))
        (insert "\n"))
      ;; Aggregator: same semantics as bench-actual.
      (let* ((classify (lambda ()
                         (let ((vs (mapcar (lambda (r) (plist-get r :gate-pass))
                                           results)))
                           (cond ((memq nil vs) 'fail)
                                 ((memq :skipped vs) 'skip)
                                 (t 'pass)))))
             (overall (funcall classify))
             (label (cond ((eq overall 'pass) "PASS")
                          ((eq overall 'skip) "SKIP")
                          (t "FAIL"))))
        (insert (format "OVERALL §5.1 v2 3-tier ratio gate : %s\n" label))
        (when nelisp-allocator-bench--batch-mode
          (princ (buffer-string) #'external-debugging-output)
          (when noninteractive
            (kill-emacs (if (eq overall 'fail) 1 0))))))
    results))

;;;###autoload
(defun nelisp-allocator-bench-batch ()
  "`make bench-allocator' entry point — runs the §5.1 v2 3-tier bench.
Sets `nelisp-allocator-bench--batch-mode' so the runner emits the
buffer to stderr and exits with the gate-derived exit code."
  (let ((nelisp-allocator-bench--batch-mode t))
    (nelisp-allocator-bench-run-3-tier)))

(provide 'nelisp-allocator-bench)

;;; nelisp-allocator-bench.el ends here
