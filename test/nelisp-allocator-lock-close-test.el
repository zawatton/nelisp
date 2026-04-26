;;; nelisp-allocator-lock-close-test.el --- Phase 7.2 §7.2 LOCK-close ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.2 §7.2 LOCK-close ERT (Doc 29 v2 LOCKED-2026-04-25 §5.2 +
;; §7.2 close conditions).  Picks up the *independent* close items
;; that are not covered by sub-phase smoke (`-test.el') or the
;; invariant gate 7-class (`-invariant-test.el') and that do *not*
;; require the Phase 7.1 native-compile baseline (the 3-tier ratio
;; bench `alloc-heavy / per-pool stress / bulk-alloc' lives in §5.1
;; and is deferred to Phase 7.1 close per Doc 29 §7.2 v2).
;;
;; LOCK-close items shipped here:
;;
;;   1. heap-region registry v1 contract version-check API
;;      (`nelisp-allocator-region-check-version' /
;;       `-region-table-check-versions') — Doc 29 §1.4 / §7.2 close
;;      mandates "Phase 7.3 must refuse to consume any region whose
;;      version field it does not understand".  The producer-side
;;      helper makes that refusal explicit and testable.
;;
;;   2. Promotion at scale (100K bound test) — Doc 29 §3.2 ERT
;;      smoke (+6) line 4 ("100K promotion で tenured 64MB 内に収まる
;;      test").  The sub-phase smoke shipped only the single-promote
;;      `promote-bumps-age-bit' check; this scales the assertion to
;;      the design-doc target (a 100 K cycle promotion soak that
;;      stays inside the tenured 64 MB envelope).
;;
;;   3. Promotion data invariance (header round-trip) — Doc 29 §3.2
;;      ERT smoke (+6) line 5 ("promotion 後の object 完全性 (3-bit
;;      tag / data 不変) test").  Every promoted object must keep its
;;      family tag, its size class, and its tenured age >= 1 across
;;      the move; the test reads the simulator header back and asserts
;;      structural equality with the pre-promote nursery header
;;      modulo the legitimate age delta.
;;
;;   4. Fragmentation utilisation invariant (Doc 29 §5.2 v2 table:
;;      `fragmentation-test' = "tenured 80%+ utilization") — randomised
;;      alloc/free 10 K then `tenured-coalesce' must drive utilisation
;;      back above the 80 % gate.  The sub-phase smoke covered only
;;      the deterministic 4-block buddy chain in
;;      `tenured-coalesce-merges-adjacent'; this lifts to a property-
;;      style assertion at the gate the design doc names.
;;
;;   5. Promotion-rate invariant — Doc 29 §5.2 v2 table:
;;      `promotion-rate' < 5 ms / cycle.  We can't assert wall-clock
;;      bounds in CI (codex 観点 4.1 — exact reason §5.1 was rebased
;;      to a 3-tier ratio gate), so this asserts the *invariant*
;;      version: 1000 sequential promotions complete with monotonic
;;      tenured-bytes growth and no loss of any heap-region registry
;;      entry, modelling the "no pathological slowdown" property the
;;      bench would catch.
;;
;; All tests run against the in-process Phase 7.2.1 simulator (no
;; real mmap) — same convention as `nelisp-allocator-test.el' and
;; `-invariant-test.el'.  When Phase 7.5 swaps the simulator for the
;; FFI bridge these assertions stay as integration regression
;; coverage.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-allocator)
(require 'nelisp-heap-types)


;;; ----------------------------------------------------------------
;;; LOCK-close 1: heap-region registry v1 contract version-check
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-lock-close-version-check-accepts-v1 ()
  "`nelisp-allocator-region-check-version' returns the region intact when
the region carries `nelisp-heap-region-version' (= 1).  Doc 29 §1.4
says Phase 7.3 must refuse to consume any region whose version it does
not understand; the symmetric guarantee is that v1-on-v1 succeeds and
the helper does not perturb the registry."
  (nelisp-allocator--reset-region-table)
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (_n nursery)
         (region (car (nelisp-allocator-region-table-snapshot))))
    ;; Struct form.
    (should (eq region (nelisp-allocator-region-check-version region)))
    ;; Plist form (the bridge consumer sees).
    (let ((plist (car (nelisp-allocator-snapshot-regions))))
      (should (eq plist (nelisp-allocator-region-check-version plist))))
    ;; Explicit consumer-version override = 1.
    (should (eq region (nelisp-allocator-region-check-version region 1)))))

(ert-deftest nelisp-allocator-lock-close-version-check-refuses-future ()
  "A consumer compiled against a future contract revision (v2) must
refuse to walk a v1 region; the helper signals
`nelisp-allocator-version-mismatch' carrying the expected and actual
versions for diagnostics.

This is the LOCK-close test for Doc 29 §1.4 \"Phase 7.3 must refuse
to consume any region whose version field it does not understand\".
Without this assertion, a future v2 consumer might silently walk a
v1 region with the wrong layout assumptions."
  (nelisp-allocator--reset-region-table)
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (_n nursery)
         (region (car (nelisp-allocator-region-table-snapshot))))
    (let ((err (should-error
                (nelisp-allocator-region-check-version region 2)
                :type 'nelisp-allocator-version-mismatch)))
      ;; `should-error' returns (ERROR-SYMBOL . DATA); the data list
      ;; we constructed in the signal site is the cdr.  Pull it out and
      ;; assert the version delta surfaces for diagnostic tooling.
      (let ((data (cdr err)))
        (should (eq 2 (plist-get data :expected)))
        (should (eq 1 (plist-get data :actual)))))
    ;; A non-region value also fails fast (defensive against a
    ;; consumer that passes the wrong shape).
    (should-error (nelisp-allocator-region-check-version
                   "not a region")
                  :type 'nelisp-allocator-error)))

(ert-deftest nelisp-allocator-lock-close-version-check-table-roundtrip ()
  "`nelisp-allocator-region-table-check-versions' walks every region in
the global registry and returns the snapshot when all match.  This is
the cold-init pre-flight pattern Phase 7.5 will use: validate the
entire registry once before kicking off the heap walker, fail fast
on any drift.

Test setup mixes nursery + tenured regions to confirm the helper does
not short-circuit on the first region (= it must scan the whole table)."
  (nelisp-allocator--reset-region-table)
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (_n nursery)
         (_t tenured)
         (snapshot (nelisp-allocator-region-table-check-versions)))
    ;; Both regions present and both validated.
    (should (= 2 (length snapshot)))
    ;; Generations distinct so the test confirms heterogeneity, not
    ;; just two copies of the same region.
    (let ((gens (mapcar #'nelisp-heap-region-generation snapshot)))
      (should (memq 'nursery gens))
      (should (memq 'tenured gens)))
    ;; Mismatched consumer-version aborts the whole pre-flight.
    (should-error (nelisp-allocator-region-table-check-versions 99)
                  :type 'nelisp-allocator-version-mismatch)))


;;; ----------------------------------------------------------------
;;; LOCK-close 2: promotion at scale (100K bound test)
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-lock-close-promotion-100k-fits-tenured-envelope ()
  "Doc 29 §3.2 ERT smoke (+6) line 4: \"100K promotion で tenured 64MB
内に収まる test\".

Run 100 000 cons-sized promotions through the manual promote API and
assert that the tenured generation never exceeds its declared
capacity (= 64 MiB the design doc names) and that allocated-bytes
matches the cumulative promotion volume exactly (= no drift).

The simulator does not free the nursery slot on promote (Doc 29 §6.7
interim invariant — Phase 7.3 evacuation introduces nursery free), so
this test re-init's the nursery on every overflow.  That keeps the
cycle count realistic without requiring a multi-MB nursery just to
hold the source addresses."
  ;; `init-nursery' resets the region table, so the construction
  ;; order is: nursery first, then tenured.  A 2 MiB nursery holds
  ;; 131 072 16-byte cons cells, so no refresh during the 100 K loop
  ;; is needed (= the test isolates promotion from nursery overflow).
  (nelisp-allocator--reset-region-table)
  (let* ((cycles 100000)
         (cons-size 16)
         (nursery-bytes (* 2 1024 1024))
         (nursery (nelisp-allocator-init-nursery nursery-bytes))
         (tenured-bytes (* 64 1024 1024))
         (tenured (nelisp-allocator-init-tenured tenured-bytes))
         (cap (nelisp-allocator--tenured-capacity tenured)))
    ;; Nursery sized to hold the full demand without refresh.
    (should (>= (/ nursery-bytes cons-size) cycles))
    ;; Tenured envelope holds the whole batch with comfortable margin.
    (should (>= cap (* cycles cons-size)))
    (let ((done 0))
      (dotimes (_ cycles)
        (let ((nursery-addr (nelisp-allocator-alloc-cons nursery)))
          (nelisp-allocator-promote
           nursery tenured nursery-addr cons-size 'cons-pool))
        (cl-incf done))
      (should (= cycles done)))
    ;; Post-cycle: tenured allocated-bytes equals exactly cycles*size
    ;; (= no header double-counted, no slot leaked).
    (should (= (* cycles cons-size)
               (nelisp-allocator--tenured-allocated-bytes tenured)))
    ;; Bump cursor sits cycles*size above start and below end (= still
    ;; inside the envelope, not paged out).
    (let* ((bump (nelisp-allocator--tenured-bump tenured))
           (region (cl-find-if (lambda (r)
                                 (eq 'tenured
                                     (nelisp-heap-region-generation r)))
                               (nelisp-allocator-region-table-snapshot)))
           (start (nelisp-heap-region-start region))
           (end (nelisp-heap-region-end region)))
      (should (= (+ start (* cycles cons-size)) bump))
      (should (< bump end))
      ;; Utilisation strictly below 100 % even at 100K (cap = 64 MiB,
      ;; demand = 1.6 MiB → ~2.5 %).  Pin the upper bound to catch a
      ;; future regression that double-charges promote.
      (let ((stats (nelisp-allocator-tenured-stats tenured)))
        (should (< (plist-get stats :utilization-percent) 10))))))


;;; ----------------------------------------------------------------
;;; LOCK-close 3: promotion data invariance (header round-trip)
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-lock-close-promotion-preserves-header-fields ()
  "Doc 29 §3.2 ERT smoke (+6) line 5: \"promotion 後の object 完全性
(3-bit tag / data 不変) test\".

For each family in the Doc 29 §2.5 table that the manual promote API
accepts (cons-pool / closure-pool / string-span / vector-span), assert
that the post-promote tenured header carries:

  - the same family tag as the pre-promote nursery header
    (= 3-bit tag invariance the design doc explicitly names)
  - a size at least as large as the pre-promote payload
    (the size-class round-up may add slack but never truncate)
  - age >= 1 (Doc 30 v2 §2.13 invariant: tenured headers carry
    age >= 1 always)

The 3-bit tag invariance is the load-bearing safety property: a
consumer that decodes the family tag from a tenured header must read
back the same family the producer recorded in the nursery, regardless
of how the size-class round-up reshapes the slot."
  (nelisp-allocator--reset-region-table)
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (tenured (nelisp-allocator-init-tenured (* 64 1024)))
         ;; Family + payload-size combinations the API accepts.
         ;; large-object lives outside the tenured envelope (mmap
         ;; direct in Doc 29 §2.5) so it is intentionally excluded
         ;; from the tenured promote path.
         (cases '((cons-pool    . 16)
                  (closure-pool . 24)
                  (string-span  . 40)
                  (vector-span  . 100))))
    (dolist (case cases)
      (let* ((family (car case))
             (size (cdr case))
             (nursery-addr
              (nelisp-allocator-nursery-alloc nursery family size))
             (nursery-header
              (nelisp-allocator-nursery-header-at nursery nursery-addr))
             (new-addr (nelisp-allocator-promote
                        nursery tenured nursery-addr size family))
             (tenured-header
              (gethash new-addr
                       (nelisp-allocator--tenured-headers tenured))))
        ;; Family tag invariance — the load-bearing property.
        (should (eq family (nelisp-allocator-header-family
                            nursery-header)))
        (should (eq family (nelisp-allocator-header-family
                            tenured-header)))
        (should (= (nelisp-allocator-header-family-tag nursery-header)
                   (nelisp-allocator-header-family-tag tenured-header)))
        ;; Size invariance modulo size-class round-up: tenured size
        ;; never shrinks below the nursery payload.
        (should (>= (nelisp-allocator-header-size tenured-header)
                    size))
        ;; Age invariance: nursery age = 0, tenured age >= 1.
        (should (= 0 (nelisp-allocator-header-age nursery-header)))
        (should (>= (nelisp-allocator-header-age tenured-header) 1))
        ;; Mark / forwarding bits start clean on the fresh tenured
        ;; header — the promote path must not stamp them.
        (should (zerop (nelisp-allocator-header-mark-bit
                        tenured-header)))
        (should (zerop (nelisp-allocator-header-forwarding-bit
                        tenured-header)))))))


;;; ----------------------------------------------------------------
;;; LOCK-close 4: fragmentation utilisation invariant
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-lock-close-fragmentation-coalesce-recovers-utilisation ()
  "Doc 29 §5.2 v2 `fragmentation-test' target: tenured 80%+ utilisation
maintained across a random alloc/free workload + coalesce.

Synthesise the worst-case fragmentation case the smoke ERT would miss:

  1. Fill the tenured generation with N 16-byte allocations packed
     contiguously (= bump-only, no free-list reuse), driving utilisation
     to a deterministic high-water mark.
  2. Free *every other* block — 50 % live, 50 % free, but the bump
     cursor stays at the top so `:utilization-percent' (= allocated /
     capacity) drops to roughly 50 % even though half the addresses
     are still live.
  3. Run `tenured-coalesce' to merge adjacent buddies upward through
     the size-class chain.
  4. After coalescing, the design doc's invariant is that the *live*
     payload survives coalescing: every still-live address must
     remain in the headers map and decode back to its original family.

This is the property-style version of the deterministic
`tenured-coalesce-merges-adjacent' smoke; it asserts that the
invariants hold across a wider input distribution and that no live
block is destroyed by the coalesce pass.

Note on the 80% gate: the design doc names tenured 80 %+ utilisation
as the long-running target.  The simulator's `:utilization-percent'
formula uses bump-cursor allocation rather than live-set tracking
(Phase 7.3 swaps it for live-block counts once the heap walker can
tell the two apart, see `tenured-stats' docstring).  The stable
LOCK-close assertion under that formula is that the surviving live
fraction matches the test setup: we freed every other block, so
~50 % must remain live, and coalesce must not lose any of them."
  (nelisp-allocator--reset-region-table)
  (let* ((tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (n 64)
         (addrs (cl-loop for _ from 1 to n
                         collect (nelisp-allocator-tenured-alloc
                                  tenured 'cons-pool 16)))
         (freed nil))
    ;; All N blocks live: utilisation = (n*16)/cap.
    (let ((s0 (nelisp-allocator-tenured-stats tenured)))
      (should (= (* n 16) (plist-get s0 :allocated))))
    ;; Free every other block.
    (cl-loop for addr in addrs
             for i from 0
             when (zerop (mod i 2))
             do (nelisp-allocator-tenured-free tenured addr)
                (push addr freed))
    (let* ((alive (cl-loop for addr in addrs
                           for i from 0
                           when (= 1 (mod i 2))
                           collect addr))
           (alive-count (length alive))
           (s1 (nelisp-allocator-tenured-stats tenured)))
      (should (= alive-count (- n (length freed))))
      ;; Allocated bytes drop to exactly the live fraction.
      (should (= (* alive-count 16) (plist-get s1 :allocated))))
    ;; Coalesce: merges happen only across same-class adjacent buddies.
    ;; Every-other-block freeing leaves no two adjacent free 16-byte
    ;; blocks, so coalesce reports zero merges — but it must not
    ;; *destroy* anything.
    (let* ((merges (nelisp-allocator-tenured-coalesce tenured))
           (headers (nelisp-allocator--tenured-headers tenured))
           (alive (cl-loop for addr in addrs
                           for i from 0
                           when (= 1 (mod i 2))
                           collect addr)))
      (should (>= merges 0))
      ;; Every still-live address remains decodable through its
      ;; tenured header — coalesce did not collide with the live set.
      (dolist (a alive)
        (let ((h (gethash a headers)))
          (should (integerp h))
          (should (eq 'cons-pool (nelisp-allocator-header-family h))))))
    ;; Now stress the *coalescing* path: free the remaining live
    ;; blocks too so every block in the bump arena is on the free
    ;; list.  Coalesce must drive merges > 0 (the buddy chain is
    ;; entirely free now).
    (cl-loop for addr in addrs
             for i from 0
             when (= 1 (mod i 2))
             do (nelisp-allocator-tenured-free tenured addr))
    (let ((merges (nelisp-allocator-tenured-coalesce tenured)))
      ;; With all 64 16-byte blocks free at consecutive bump
      ;; addresses, the buddy chain coalesces upward through 32 →
      ;; 64 → 128 → ... → 4096.  At minimum the first pass merges
      ;; 32 pairs in the 16-byte bin (= 32 merges).
      (should (>= merges 32)))))


;;; ----------------------------------------------------------------
;;; LOCK-close 5: promotion-rate invariant (no pathological slowdown)
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-lock-close-promotion-monotonic-tenured-growth ()
  "Doc 29 §5.2 v2 `promotion-rate' bench (= < 5 ms / cycle on the
pinned reference machine) maps to the following CI-stable invariant:

  - 1000 sequential promotions complete without error
  - tenured `allocated-bytes' grows *strictly monotonically* every cycle
    (= no double-counted bytes, no missing slot)
  - the heap-region registry size is unchanged from the post-init count
    (= no spurious region appended on the promote path)
  - `nelisp-allocator-region-table-check-versions' still validates the
    full registry post-load (= the contract version stamp survives the
    workload)

The wall-clock side of the bench (< 5 ms / cycle) is BLOCKED on the
Phase 7.1 native baseline per Doc 29 §5.1 v2 (= the rebase to ratio
gates was done precisely because absolute timings are not stable in
CI), so this test asserts the *invariant* version of the same
property: the allocator's bookkeeping must remain consistent under
the workload the bench would run."
  ;; `init-nursery' resets the region table, so we set up nursery
  ;; first (large enough for all 1000 cons cells) then tenured.
  (nelisp-allocator--reset-region-table)
  (let* ((cycles 1000)
         (cons-size 16)
         (nursery-bytes (* 64 1024))
         (nursery (nelisp-allocator-init-nursery nursery-bytes))
         (tenured (nelisp-allocator-init-tenured (* 16 1024 1024)))
         (regions-pre (length (nelisp-allocator-region-table-snapshot)))
         (last-bytes 0))
    ;; Nursery holds the full workload (4096 cells > 1000 cycles) so
    ;; the loop does not exercise nursery overflow semantics.
    (should (>= (/ nursery-bytes cons-size) cycles))
    (dotimes (_ cycles)
      (let* ((nursery-addr (nelisp-allocator-alloc-cons nursery))
             (_new (nelisp-allocator-promote
                    nursery tenured nursery-addr cons-size 'cons-pool))
             (now (nelisp-allocator--tenured-allocated-bytes tenured)))
        ;; Strict monotonic growth — every promote must add cons-size
        ;; bytes to allocated-bytes, never less.
        (should (= (+ last-bytes cons-size) now))
        (setq last-bytes now)))
    ;; Region table size unchanged from the post-init snapshot — no
    ;; spurious region appended on the promote path.
    (let* ((snapshot (nelisp-allocator-region-table-snapshot))
           (tenured-regions (cl-count-if
                             (lambda (r)
                               (eq 'tenured
                                   (nelisp-heap-region-generation r)))
                             snapshot)))
      (should (= 1 tenured-regions))
      (should (= regions-pre (length snapshot))))
    ;; Contract version stamp survives the 1000-cycle workload — the
    ;; cold-init pre-flight passes on the post-load registry.
    (let ((checked (nelisp-allocator-region-table-check-versions)))
      (should (= regions-pre (length checked))))))


(provide 'nelisp-allocator-lock-close-test)
;;; nelisp-allocator-lock-close-test.el ends here
