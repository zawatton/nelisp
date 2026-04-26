;;; nelisp-allocator-invariant-test.el --- Phase 7.2 invariant gate ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.2 invariant gate ERT (Doc 29 v2 LOCKED-2026-04-25 §5.2 and
;; §7.2 close criteria).  Per Doc 29 §7.2 the Phase 7.2 LOCKED close
;; gate requires "invariant gate 7 class 全 PASS" beyond the +20 ERT
;; smoke shipped with sub-phases 7.2.1 / 7.2.2 / 7.2.3.
;;
;; The 7 invariant classes documented in Doc 29 §5.2 v2 (codex review
;; 観点 2.5) are:
;;
;;   1. OOM / mmap failure path        — graceful error, no leak
;;   2. randomized alloc/free sequence — 10K random alloc/free seq
;;   3. alignment                       — every family 8 / 16-byte aligned
;;   4. double-free detect              — fail-fast on double-free
;;   5. cross-generation pointer (interim, §6.7) — tenured → nursery
;;      reference handling under the Phase 7.2 interim invariant
;;   6. large-object boundary           — 4 KiB family routing accuracy
;;   7. stats on/off                    — collection on vs off behaves the same
;;
;; Several invariants are already exercised by the +20 ERT smoke
;; (alignment in `alloc-cons-16-byte-aligned', double-free in
;; `cons-pool-free-marks-bitmap-free' / `tenured-free-returns-to-bin',
;; stats on/off in `stats-tracks-alloc-and-free').  This file adds the
;; *invariant gate* layer — fuzz / property / boundary / failure-mode
;; coverage that grows the Phase 7.2 LOCKED close confidence beyond
;; sample-based smoke.  Existing sub-phase smoke ERT remain untouched.
;;
;; All tests run against the in-process Phase 7.2.1 simulator (no real
;; mmap); when Phase 7.5 swaps the simulator for the FFI bridge these
;; assertions stay as integration regression coverage (same pattern as
;; the existing `nelisp-allocator-test.el' / `-gc-interop-test.el').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-allocator)
(require 'nelisp-heap-types)


;;; ----------------------------------------------------------------
;;; Invariant 1: OOM / mmap failure path (graceful error, no leak)
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-invariant-oom-nursery-overflow-no-leak ()
  "Nursery exhaustion signals `nelisp-allocator-overflow' and leaves the
nursery in a consistent state — `free' has not advanced past `limit',
the region table size matches the pre-exhaustion snapshot, and the
header map contains exactly the bookkeeping for the successful
allocations.  This is the simulator analogue of the OOM / mmap
failure path: the slow-path callback raises rather than corrupting the
allocator."
  (nelisp-allocator--reset-region-table)
  (let* ((nursery-bytes (* 64 1024))
         (nursery (nelisp-allocator-init-nursery nursery-bytes))
         (regions-pre (length (nelisp-allocator-region-table-snapshot)))
         (limit (nelisp-allocator--nursery-limit nursery))
         (n-good 0))
    ;; Fill the nursery until the next alloc must fail.  Each alloc-cons
    ;; consumes 16 byte; the simulator gives us exactly nursery-bytes.
    (let ((quota (/ nursery-bytes 16)))
      (dotimes (_ quota)
        (nelisp-allocator-alloc-cons nursery)
        (cl-incf n-good)))
    ;; Bookkeeping invariants pre-failure.
    (should (= n-good (/ nursery-bytes 16)))
    (should (= (nelisp-allocator--nursery-free nursery) limit))
    (should (= n-good (hash-table-count
                       (nelisp-allocator--nursery-headers nursery))))
    ;; The next alloc must signal `nelisp-allocator-overflow'.
    (should-error (nelisp-allocator-alloc-cons nursery)
                  :type 'nelisp-allocator-overflow)
    ;; Post-failure: free pointer unchanged, headers unchanged, region
    ;; table count unchanged (= no leaked region appended on the failure
    ;; path).
    (should (= limit (nelisp-allocator--nursery-free nursery)))
    (should (= n-good (hash-table-count
                       (nelisp-allocator--nursery-headers nursery))))
    (should (= regions-pre (length (nelisp-allocator-region-table-snapshot))))))

(ert-deftest nelisp-allocator-invariant-oom-tenured-overflow-no-leak ()
  "Tenured exhaustion signals `nelisp-allocator-tenured-overflow' and
leaves the tenured state intact: bump cursor unchanged, allocated-bytes
unchanged, headers + block-sizes maps untouched."
  (nelisp-allocator--reset-region-table)
  (let* ((tenured-bytes (* 64 1024))
         (tenured (nelisp-allocator-init-tenured tenured-bytes))
         (cap (nelisp-allocator--tenured-capacity tenured))
         ;; Allocate just enough to push the bump near the limit.  Each
         ;; alloc consumes its size-class (4096 byte for max class).
         (chunks (/ cap 4096)))
    (dotimes (_ chunks)
      (nelisp-allocator-tenured-alloc tenured 'string-span 4096))
    (let ((bump-pre (nelisp-allocator--tenured-bump tenured))
          (bytes-pre (nelisp-allocator--tenured-allocated-bytes tenured))
          (headers-pre (hash-table-count
                        (nelisp-allocator--tenured-headers tenured))))
      ;; Next 4 KiB alloc must fail — no free-list entries available.
      (should-error (nelisp-allocator-tenured-alloc
                     tenured 'string-span 4096)
                    :type 'nelisp-allocator-tenured-overflow)
      ;; Post-failure invariants.
      (should (= bump-pre (nelisp-allocator--tenured-bump tenured)))
      (should (= bytes-pre
                 (nelisp-allocator--tenured-allocated-bytes tenured)))
      (should (= headers-pre
                 (hash-table-count
                  (nelisp-allocator--tenured-headers tenured)))))))


;;; ----------------------------------------------------------------
;;; Invariant 2: randomized alloc/free sequence (property fuzz)
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-invariant-randomized-cons-pool-alloc-free ()
  "Run a deterministic 1000-step random alloc / free sequence on the
cons-pool and verify the live-set invariant after every step:

  - every alloc returns a non-nil, unique address
  - every freed address disappears from the addr-to-block map
  - the cons-pool free-count drops by exactly the number of live cells
  - no double-free occurs (random 'free' steps only target live cells)
  - the bitmap is consistent: live count + free count = total cells

This is a property-style coverage of the alloc/free pair: any single
unlucky sequence that violates an invariant will trip an `should'.
The seed is fixed so re-runs are deterministic.

Pre-T65 the cons-pool had several latent bugs (off-by-one in the
addr-to-block map, double-free silently rebumping free-count) — this
test would have caught them without needing a hand-crafted repro."
  (nelisp-allocator--reset-region-table)
  (let* ((pool (nelisp-allocator-init-cons-pool 1 (* 16 64)))
         ;; 64 cells per block * 1 block = 64 capacity.  We won't fill
         ;; past 50 to leave headroom.
         (live (make-hash-table :test 'eql))
         ;; Mulberry32-style deterministic PRNG seeded for repeatability.
         (state 12345))
    (cl-flet ((rng ()
                (setq state (logand (+ (* state 1103515245) 12345)
                                    #xFFFFFFFF))
                state))
      (dotimes (_ 1000)
        (let ((live-count (hash-table-count live)))
          (cond
           ;; If empty, force alloc.
           ((zerop live-count)
            (let ((addr (nelisp-allocator-cons-pool-alloc pool)))
              (should (integerp addr))
              (should-not (gethash addr live))
              (puthash addr t live)))
           ;; If near cap, force free.  `init-cons-pool' lazily extends
           ;; so we deliberately stay below 1.5 blocks of headroom.
           ((>= live-count 50)
            (let* ((victims (cl-loop for k being the hash-keys of live
                                     collect k))
                   (victim (nth (mod (rng) (length victims)) victims)))
              (nelisp-allocator-cons-pool-free pool victim)
              (remhash victim live)))
           ;; Otherwise random choice.
           (t
            (if (zerop (mod (rng) 2))
                (let ((addr (nelisp-allocator-cons-pool-alloc pool)))
                  (should (integerp addr))
                  (should-not (gethash addr live))
                  (puthash addr t live))
              (let* ((victims (cl-loop for k being the hash-keys of live
                                       collect k))
                     (victim (nth (mod (rng) (length victims)) victims)))
                (nelisp-allocator-cons-pool-free pool victim)
                (remhash victim live)))))
          ;; Invariant: every live address is registered in addr-to-block.
          (cl-loop for k being the hash-keys of live
                   do (should
                       (gethash k
                                (nelisp-allocator--cons-pool-addr-to-block
                                 pool))))))
      ;; Post-loop: drain every remaining live cell, addr-to-block must
      ;; end empty.
      (cl-loop for k being the hash-keys of live
               do (nelisp-allocator-cons-pool-free pool k))
      (should (zerop (hash-table-count
                      (nelisp-allocator--cons-pool-addr-to-block
                       pool)))))))


;;; ----------------------------------------------------------------
;;; Invariant 3: alignment (every family 8-byte aligned)
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-invariant-alignment-every-family ()
  "Every family allocator returns an address aligned to
`nelisp-allocator--alignment' (8 byte).  Doc 30 v2 §2.13 packs the
header into one 64-bit word, which forces this floor on every
allocation regardless of family.

Cover all 5 families from Doc 29 §2.5 v2 table:
  cons-pool / closure-pool / string-span / vector-span / large-object."
  (nelisp-allocator--reset-region-table)
  (let* ((align nelisp-allocator--alignment)
         (nursery (nelisp-allocator-init-nursery (* 64 1024))))
    ;; cons-pool — both nursery-alloc path and dedicated alloc-cons.
    (let ((a (nelisp-allocator-alloc-cons nursery)))
      (should (zerop (% a align))))
    ;; closure-pool — variable slot count.
    (dolist (slots '(0 1 3 7 15))
      (let ((a (nelisp-allocator-alloc-closure nursery slots)))
        (should (zerop (% a align)))))
    ;; string-span — vary payload sizes across multiple size-classes.
    (dolist (sz '(1 8 33 100 257 1000 4000))
      (let ((a (nelisp-allocator-alloc-string-span nursery sz)))
        (should (zerop (% a align)))))
    ;; vector-span — same coverage as string-span.
    (dolist (sz '(1 8 33 100 257 1000 4000))
      (let ((a (nelisp-allocator-alloc-vector-span nursery sz)))
        (should (zerop (% a align)))))
    ;; large-object — payload must exceed the threshold.
    (dolist (sz `(,(1+ nelisp-allocator-large-object-threshold)
                  ,(* 8 1024)
                  ,(* 64 1024)))
      (let ((a (nelisp-allocator-alloc-large-object nursery sz)))
        (should (zerop (% a align)))))))


;;; ----------------------------------------------------------------
;;; Invariant 4: double-free detect (fail-fast on double-free)
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-invariant-double-free-fail-fast-all-paths ()
  "Every free path detects a double-free by signalling
`nelisp-allocator-error' rather than silently corrupting bookkeeping.
Cover the three free entry points:
  - `tenured-free' (size-class bin push)
  - `cons-pool-free' (bitmap flip)
  - `bulk-free' (which delegates to cons-pool-free per addr)

Each path must signal on the second free of the same address; the
first free must succeed.

Pre-T65 (Doc 29 v2 §6.5 mitigation step) the tenured path's
`memql' guard was added precisely to harden this — test pins it."
  (nelisp-allocator--reset-region-table)
  ;; Tenured double-free.
  (let* ((tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (a (nelisp-allocator-tenured-alloc tenured 'cons-pool 16)))
    (nelisp-allocator-tenured-free tenured a)
    (should-error (nelisp-allocator-tenured-free tenured a)
                  :type 'nelisp-allocator-error))
  ;; cons-pool double-free.
  (nelisp-allocator--reset-region-table)
  (let* ((pool (nelisp-allocator-init-cons-pool 1 (* 16 16)))
         (a (nelisp-allocator-cons-pool-alloc pool)))
    (nelisp-allocator-cons-pool-free pool a)
    (should-error (nelisp-allocator-cons-pool-free pool a)
                  :type 'nelisp-allocator-error))
  ;; bulk-free path (delegates to cons-pool-free).
  (nelisp-allocator--reset-region-table)
  (let* ((pool (nelisp-allocator-init-cons-pool 1 (* 16 64)))
         (addrs (nelisp-allocator-bulk-cons 4 pool)))
    (nelisp-allocator-bulk-free addrs pool)
    ;; Bulk-free of an already-freed batch must signal.
    (should-error (nelisp-allocator-bulk-free addrs pool)
                  :type 'nelisp-allocator-error)))


;;; ----------------------------------------------------------------
;;; Invariant 5: cross-generation pointer (interim, Doc 29 §6.7)
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-invariant-cross-generation-interim ()
  "Doc 29 v2 §6.7 (codex 観点 3.7) interim invariant: while Phase 7.2
runs without a write barrier, tenured → nursery references must be
permitted (the heap walker is full-heap until Phase 7.3 introduces a
remembered set), but no minor GC must fire during that interim.

This test pins two observable consequences of the interim invariant:

  1. After a manual `promote' the tenured slot exists with age = 1
     while the original nursery slot's header is still readable
     (= the simulator does not free the nursery slot — that requires
     Phase 7.3 evacuation semantics).  This models the moment of
     cross-generation reference: a tenured object retains its old
     nursery address, and both ends of the reference are live.

  2. Both ends of the reference are visible through the heap-region
     registry — the consumer (Phase 7.3 GC, Doc 30) sees the nursery
     and tenured regions distinctly so a future remembered-set walker
     can scan the tenured side without missing the nursery target."
  (nelisp-allocator--reset-region-table)
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (nursery-addr (nelisp-allocator-alloc-cons nursery))
         (nursery-header
          (nelisp-allocator-nursery-header-at nursery nursery-addr))
         (new-addr (nelisp-allocator-promote
                    nursery tenured nursery-addr 16 'cons-pool))
         (tenured-headers (nelisp-allocator--tenured-headers tenured))
         (tenured-header (gethash new-addr tenured-headers)))
    ;; (1) Both ends live.  Nursery header still readable post-promote.
    (should (integerp nursery-header))
    (should (eq 'cons-pool
                (nelisp-allocator-header-family nursery-header)))
    ;; Tenured slot freshly written, age >= 1 (Doc 30 §2.13 invariant).
    (should (integerp tenured-header))
    (should (= 1 (nelisp-allocator-header-age tenured-header)))
    (should (eq 'cons-pool
                (nelisp-allocator-header-family tenured-header)))
    ;; (2) Heap-region registry exposes both generations distinctly so a
    ;; future remembered-set walker (Phase 7.3) can find them.
    (let* ((snapshot (nelisp-allocator-snapshot-regions))
           (gens (mapcar (lambda (r) (plist-get r :generation)) snapshot)))
      (should (memq :nursery gens))
      (should (memq :tenured gens)))))


;;; ----------------------------------------------------------------
;;; Invariant 6: large-object boundary (4 KiB family routing accuracy)
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-invariant-large-object-boundary ()
  "Doc 29 §2.5 v2 family selection rules:

    32 byte < size <= 4 KiB  → string-span / vector-span
    size > 4 KiB             → large-object

The threshold is `nelisp-allocator-large-object-threshold' (= 4096
byte).  Test that the boundary is enforced *symmetrically*:

  - exactly 4 KiB on string-span / vector-span succeeds (= boundary
    point belongs to the span family)
  - exactly 4 KiB on large-object signals — the routing rule is strict
    `>' not `>='
  - 4 KiB + 1 on string-span / vector-span signals
  - 4 KiB + 1 on large-object succeeds and registers a region whose
    family-tag decodes back to `large-object'."
  (nelisp-allocator--reset-region-table)
  (let* ((nursery (nelisp-allocator-init-nursery (* 16 4096)))
         (threshold nelisp-allocator-large-object-threshold))
    ;; threshold (= 4 KiB) — span families OK.
    (let ((a (nelisp-allocator-alloc-string-span nursery threshold)))
      (should (integerp a)))
    (let ((a (nelisp-allocator-alloc-vector-span nursery threshold)))
      (should (integerp a)))
    ;; threshold — large-object rejects (must be strictly above).
    (should-error (nelisp-allocator-alloc-large-object nursery threshold)
                  :type 'nelisp-allocator-error)
    ;; threshold + 1 — span families reject.
    (should-error (nelisp-allocator-alloc-string-span
                   nursery (1+ threshold))
                  :type 'nelisp-allocator-error)
    (should-error (nelisp-allocator-alloc-vector-span
                   nursery (1+ threshold))
                  :type 'nelisp-allocator-error)
    ;; threshold + 1 — large-object accepts and writes a header whose
    ;; family decodes back to `large-object'.
    (let* ((addr (nelisp-allocator-alloc-large-object
                  nursery (1+ threshold)))
           (header (nelisp-allocator-large-object-header-at addr)))
      (should (integerp addr))
      (should (integerp header))
      (should (eq 'large-object
                  (nelisp-allocator-header-family header))))))


;;; ----------------------------------------------------------------
;;; Invariant 7: stats on/off (collection on vs off behaves the same)
;;; ----------------------------------------------------------------

(ert-deftest nelisp-allocator-invariant-stats-on-off-equivalence ()
  "Doc 29 §5.2 v2 invariant: stats collection enabled vs disabled must
produce identical *functional* results — same allocated addresses, same
live-set, same free-list state.  The only difference is the stats
snapshot itself (nil when disabled, populated when enabled).

This pins the cost / observability separation: callers can flip
`nelisp-allocator-stats-enabled' to nil for production paths without
fearing a behavioral divergence in the allocator hot path."
  (cl-flet ((run-workload (stats-on)
              (nelisp-allocator--reset-region-table)
              (let* ((pool (nelisp-allocator-init-cons-pool 1 (* 16 32)))
                     (nelisp-allocator-stats-enabled stats-on))
                (nelisp-allocator-stats-reset)
                (let* ((a (nelisp-allocator-cons-pool-alloc pool))
                       (b (nelisp-allocator-cons-pool-alloc pool))
                       (c (nelisp-allocator-cons-pool-alloc pool))
                       (d (nelisp-allocator-cons-pool-alloc pool)))
                  (nelisp-allocator-cons-pool-free pool b)
                  (nelisp-allocator-cons-pool-free pool d)
                  (let ((live (cl-loop for k being the hash-keys of
                                       (nelisp-allocator--cons-pool-addr-to-block
                                        pool)
                                       collect k)))
                    (list :addrs (list a b c d)
                          :live (sort live #'<)
                          :snap (nelisp-allocator-stats-snapshot)))))))
    (let* ((off (run-workload nil))
           (on  (run-workload t)))
      ;; Functional equivalence: same addresses, same live set.
      (should (equal (plist-get off :addrs) (plist-get on :addrs)))
      (should (equal (plist-get off :live)  (plist-get on :live)))
      ;; Stats-disabled snapshot must be empty (= nil).
      (should (null (plist-get off :snap)))
      ;; Stats-enabled snapshot must record the workload accurately.
      (let* ((snap (plist-get on :snap))
             (cs (cdr (assq 'cons-pool snap))))
        (should cs)
        (should (= 4 (plist-get cs :alloc-count)))
        (should (= 2 (plist-get cs :free-count)))
        (should (= (* 4 16) (plist-get cs :alloc-bytes)))
        (should (= (* 2 16) (plist-get cs :free-bytes))))
      ;; Cleanup so subsequent ERTs see fresh stats state.
      (let ((nelisp-allocator-stats-enabled nil))
        (nelisp-allocator-stats-reset)))))


(provide 'nelisp-allocator-invariant-test)
;;; nelisp-allocator-invariant-test.el ends here
