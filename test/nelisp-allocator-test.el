;;; nelisp-allocator-test.el --- ERT for Phase 7.2.1 bump allocator + heap-region registry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.2.1 ERT smoke (+8) for `nelisp-allocator'.  Doc 29 §3.1
;; LOCKED 2026-04-25 v2 commits +8 ERT; this file ships exactly that:
;;
;;   1. init-nursery registers a nursery region in the table.
;;   2. nursery-alloc bumps the free pointer monotonically.
;;   3. alloc-cons returns 8-byte aligned addresses.
;;   4. nursery overflow invokes the configured callback.
;;   5. region-table-snapshot returns every registered region.
;;   6. object header family-tag round-trips via pack / decode.
;;   7. alloc-large-object bypasses the nursery (registers its own
;;      large-object region).
;;   8. NELISP_HEAP_REGION_VERSION = 1 (Doc 29 §1.4 contract LOCK).
;;
;; Simulator path only — no real `mmap'.  Phase 7.5 will swap the
;; simulator for the FFI bridge while these assertions stay as
;; integration regression coverage (same pattern as the Phase 7.1.4
;; runtime simulator tests).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-allocator)

;;; (1) init-nursery registers a region ----------------------------

(ert-deftest nelisp-allocator-init-nursery-creates-region ()
  "`init-nursery' registers exactly one region (generation = nursery)."
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (snapshot (nelisp-allocator-region-table-snapshot)))
    (should (nelisp-allocator--nursery-p nursery))
    (should (= 1 (length snapshot)))
    (let ((region (car snapshot)))
      (should (nelisp-heap-region-p region))
      (should (eq 'nursery (nelisp-heap-region-generation region)))
      ;; Init-nursery uses cons-pool as the dominant nursery family.
      (should (eq 'cons-pool (nelisp-heap-region-family region)))
      (should (= 1 (nelisp-heap-region-version region)))
      (should (< (nelisp-heap-region-start region)
                 (nelisp-heap-region-end region))))))

;;; (2) bump-pointer monotonic increase ----------------------------

(ert-deftest nelisp-allocator-nursery-alloc-bumps-pointer ()
  "Two successive `nursery-alloc' calls return strictly ascending
addresses, and `free' advances by the requested (8-byte aligned) amount."
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (start (nelisp-allocator--nursery-free nursery))
         (a1 (nelisp-allocator-nursery-alloc nursery 'cons-pool 16))
         (free1 (nelisp-allocator--nursery-free nursery))
         (a2 (nelisp-allocator-nursery-alloc nursery 'cons-pool 16))
         (free2 (nelisp-allocator--nursery-free nursery)))
    (should (= a1 start))
    (should (= free1 (+ start 16)))
    (should (= a2 (+ start 16)))
    (should (= free2 (+ start 32)))
    (should (< a1 a2))))

;;; (3) cons cell alignment ----------------------------------------

(ert-deftest nelisp-allocator-alloc-cons-16-byte-aligned ()
  "`alloc-cons' returns 8-byte aligned addresses (Doc 30 §2.13
header demands 8-byte minimum).  Header-less cons cells inherit
this floor through the bump pointer's alignment, so subsequent
allocations stay aligned even after odd-sized requests upstream."
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (a1 (nelisp-allocator-alloc-cons nursery))
         (a2 (nelisp-allocator-alloc-cons nursery))
         (a3 (nelisp-allocator-alloc-cons nursery)))
    (should (zerop (% a1 nelisp-allocator--alignment)))
    (should (zerop (% a2 nelisp-allocator--alignment)))
    (should (zerop (% a3 nelisp-allocator--alignment)))
    ;; Each cons consumes exactly 16 bytes (no padding because
    ;; 16 is already an 8-byte multiple).
    (should (= 16 (- a2 a1)))
    (should (= 16 (- a3 a2)))))

;;; (4) overflow triggers callback ---------------------------------

(ert-deftest nelisp-allocator-overflow-triggers-callback ()
  "Allocating past the nursery's `limit' invokes the configured
`overflow-callback' with (NURSERY SIZE FAMILY).  When the callback
returns nil the allocator signals `nelisp-allocator-overflow'."
  (let* ((calls nil)
         (cb (lambda (_n s f) (push (list :size s :family f) calls) nil))
         ;; Use the smallest legal nursery (one page) so a single
         ;; oversized request blows past `limit' on the first alloc.
         (nursery (nelisp-allocator-init-nursery
                   nelisp-allocator-page-size cb)))
    ;; Fill the nursery exactly.
    (nelisp-allocator-nursery-alloc nursery 'cons-pool
                                    nelisp-allocator-page-size)
    ;; Next alloc must overflow.
    (should-error
     (nelisp-allocator-nursery-alloc nursery 'cons-pool 16)
     :type 'nelisp-allocator-overflow)
    (should (= 1 (length calls)))
    (let ((rec (car calls)))
      (should (= 16 (plist-get rec :size)))
      (should (eq 'cons-pool (plist-get rec :family))))))

;;; (5) region-table-snapshot ---------------------------------------

(ert-deftest nelisp-allocator-region-registry-snapshot ()
  "`region-table-snapshot' returns every registered region in
ascending-id order (Doc 29 §1.4 contract).  Adding more regions —
e.g. via `alloc-large-object' — extends the snapshot."
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (_n nursery)
         (initial (nelisp-allocator-region-table-snapshot)))
    (should (= 1 (length initial)))
    ;; Allocate two large objects → two new regions registered.
    (nelisp-allocator-alloc-large-object nursery (* 8 1024))
    (nelisp-allocator-alloc-large-object nursery (* 16 1024))
    (let ((snapshot (nelisp-allocator-region-table-snapshot)))
      (should (= 3 (length snapshot)))
      ;; Ids are ascending and unique.
      (let ((ids (mapcar #'nelisp-heap-region-id snapshot)))
        (should (equal ids (sort (copy-sequence ids) #'<)))
        (should (= (length ids) (length (cl-remove-duplicates ids)))))
      ;; First region is the nursery; the others are large-object.
      (should (eq 'cons-pool   (nelisp-heap-region-family (nth 0 snapshot))))
      (should (eq 'large-object (nelisp-heap-region-family (nth 1 snapshot))))
      (should (eq 'large-object (nelisp-heap-region-family (nth 2 snapshot)))))))

;;; (6) object header family-tag encoding ---------------------------

(ert-deftest nelisp-allocator-header-family-tag-encodes-correctly ()
  "Doc 30 v2 §2.13 layout: family-tag occupies bits [55:48].
`pack-header' / decode round-trip through every family in
`nelisp-allocator--family-tag-alist' preserving family + size."
  (dolist (family '(cons-pool closure-pool string-span vector-span
                              large-object))
    (let* ((size 1234)
           (header (nelisp-allocator-pack-header family size)))
      (should (= size (nelisp-allocator-header-size header)))
      (should (eq family (nelisp-allocator-header-family header)))
      (should (= 0 (nelisp-allocator-header-mark-bit header)))
      (should (= 0 (nelisp-allocator-header-forwarding-bit header)))
      (should (= 0 (nelisp-allocator-header-age header)))
      ;; Family-tag bits land exactly at [55:48] — verify by shifting
      ;; them out and checking the cleared header has the size only.
      (let* ((tag (nelisp-allocator--family-tag family))
             (without-tag (logxor header (ash tag 48))))
        (should (= size without-tag))))))

;;; (7) large-object bypasses the nursery --------------------------

(ert-deftest nelisp-allocator-large-object-path-bypasses-nursery ()
  "An allocation > 4 KiB threshold routes to `alloc-large-object',
which registers a *separate* region rather than consuming nursery
space.  The nursery's `free' pointer must not advance; the new
region must carry family = `large-object'."
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (free-before (nelisp-allocator--nursery-free nursery))
         (addr (nelisp-allocator-alloc-large-object nursery (* 8 1024)))
         (free-after (nelisp-allocator--nursery-free nursery))
         (snapshot (nelisp-allocator-region-table-snapshot)))
    (should (integerp addr))
    (should (= free-before free-after))
    (should (= 2 (length snapshot)))
    (let ((large (cadr snapshot)))
      (should (eq 'large-object (nelisp-heap-region-family large)))
      (should (= addr (nelisp-heap-region-start large)))
      ;; The large-object region is at least as large as the request
      ;; (rounded to a page) plus the span header.
      (should (>= (- (nelisp-heap-region-end large)
                     (nelisp-heap-region-start large))
                  (* 8 1024))))
    ;; Anything below or equal to threshold must stay in nursery (the
    ;; family routing is the caller's responsibility, but the
    ;; large-object guard rejects sub-threshold sizes outright).
    (should-error
     (nelisp-allocator-alloc-large-object nursery 1024)
     :type 'nelisp-allocator-error)))

;;; (8) version constant ------------------------------------------

(ert-deftest nelisp-allocator-version-constant-equals-1 ()
  "Doc 29 §1.4 LOCKS `NELISP_HEAP_REGION_VERSION' = 1 for Phase 7.2
SHIPPED.  Phase 7.3 must refuse any region carrying a version it
does not understand; bumping this constant is an ABI break that
requires Doc 29 / Doc 30 amendment + LOCKED-bump."
  (should (= 1 nelisp-heap-region-version))
  ;; Every region produced by the allocator stamps the current
  ;; version so consumers can rely on the field rather than
  ;; assuming the constant.
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (_n nursery)
         (region (car (nelisp-allocator-region-table-snapshot))))
    (should (= 1 (nelisp-heap-region-version region)))))

;;; ----------------------------------------------------------------
;;; Phase 7.2.2 — tenured generation + free-list + promotion (+6 ERT)
;;; ----------------------------------------------------------------
;;
;; Doc 29 v2 LOCKED 2026-04-25 §3.2 ERT smoke (+6) tabulated below.
;; Every test re-resets the region table so tenured bookkeeping
;; starts from a known clean state — production code never calls
;; `--reset-region-table' but ERT fixtures must isolate.
;;
;;   1. init-tenured registers a `tenured' generation region.
;;   2. tenured-alloc rounds the request up to the matching size-class
;;      bin (request 9 → class 16 etc).
;;   3. tenured-free pushes the block back onto the size-class bin so
;;      the next alloc reuses the same address.
;;   4. promote bumps the age 6-bit field (Doc 30 v2 §2.13) by 1.
;;   5. tenured-coalesce merges adjacent buddies of the same size-class
;;      into the next-larger bin.
;;   6. tenured-stats reports utilisation that tracks alloc / free.

;;; (9) init-tenured registers a tenured region ---------------------

(ert-deftest nelisp-allocator-init-tenured-creates-region ()
  "`init-tenured' registers exactly one new region (generation =
`tenured') in addition to whatever the nursery already added."
  (nelisp-allocator--reset-region-table)
  (let* ((tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (snapshot (nelisp-allocator-region-table-snapshot)))
    (should (nelisp-allocator--tenured-p tenured))
    (should (= 1 (length snapshot)))
    (let ((region (car snapshot)))
      (should (nelisp-heap-region-p region))
      (should (eq 'tenured (nelisp-heap-region-generation region)))
      (should (= 1 (nelisp-heap-region-version region)))
      (should (= (nelisp-heap-region-start region)
                 (nelisp-allocator--tenured-bump tenured)))
      (should (= (- (nelisp-heap-region-end region)
                    (nelisp-heap-region-start region))
                 (nelisp-allocator--tenured-capacity tenured))))))

;;; (10) tenured-alloc uses the size-class bin ---------------------

(ert-deftest nelisp-allocator-tenured-alloc-uses-size-class ()
  "Allocating 9 byte rounds up to the 16-byte size-class; the
allocator records the size-class for later free-list lookup, and
the bump cursor advances by exactly 16 byte (not the requested 9)."
  (nelisp-allocator--reset-region-table)
  (let* ((tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (start (nelisp-allocator--tenured-bump tenured))
         (addr (nelisp-allocator-tenured-alloc tenured 'closure-pool 9)))
    (should (= addr start))
    (should (= 16 (gethash addr
                           (nelisp-allocator--tenured-block-sizes tenured))))
    (should (= (+ start 16) (nelisp-allocator--tenured-bump tenured)))
    (let ((header (gethash addr
                           (nelisp-allocator--tenured-headers tenured))))
      (should (integerp header))
      (should (eq 'closure-pool
                  (nelisp-allocator-header-family header)))
      (should (= 16 (nelisp-allocator-header-size header))))
    ;; Round-up holds at every bin: 5 → 8, 33 → 64, 200 → 256.
    (should (= 8  (nelisp-allocator--size-class-of 5)))
    (should (= 64 (nelisp-allocator--size-class-of 33)))
    (should (= 256 (nelisp-allocator--size-class-of 200)))))

;;; (11) tenured-free returns to the bin ---------------------------

(ert-deftest nelisp-allocator-tenured-free-returns-to-bin ()
  "`tenured-free' pushes the block back onto the size-class bin so
a subsequent same-class alloc reuses the address (LIFO order)."
  (nelisp-allocator--reset-region-table)
  (let* ((tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (a (nelisp-allocator-tenured-alloc tenured 'cons-pool 16))
         (b (nelisp-allocator-tenured-alloc tenured 'cons-pool 16)))
    ;; Both blocks are live → no free-list entries yet.
    (should (null (cdr (assq 16 (nelisp-allocator--tenured-free-lists
                                 tenured)))))
    (nelisp-allocator-tenured-free tenured a)
    (should (equal (list a)
                   (cdr (assq 16 (nelisp-allocator--tenured-free-lists
                                  tenured)))))
    ;; Re-alloc reuses the freed block (fast path) without bumping.
    (let ((bump-before (nelisp-allocator--tenured-bump tenured))
          (c (nelisp-allocator-tenured-alloc tenured 'cons-pool 16)))
      (should (= a c))
      (should (= bump-before (nelisp-allocator--tenured-bump tenured))))
    ;; Free a second time should still work for B independently.
    (nelisp-allocator-tenured-free tenured b)
    (should (equal (list b)
                   (cdr (assq 16 (nelisp-allocator--tenured-free-lists
                                  tenured)))))
    ;; Double-free guards against bookkeeping corruption.
    (should-error (nelisp-allocator-tenured-free tenured b)
                  :type 'nelisp-allocator-error)))

;;; (12) promote bumps the age field -------------------------------

(ert-deftest nelisp-allocator-promote-bumps-age-bit ()
  "`nelisp-allocator-promote' allocates a tenured slot and writes a
header whose age field is 1 (the freshly-promoted object's first
generational tick).  Doc 30 v2 §2.13 invariant: tenured headers
carry age >= 1."
  (nelisp-allocator--reset-region-table)
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (nursery-addr (nelisp-allocator-alloc-cons nursery))
         (new-addr (nelisp-allocator-promote
                    nursery tenured nursery-addr 16 'cons-pool))
         (headers (nelisp-allocator--tenured-headers tenured))
         (header (gethash new-addr headers)))
    (should (integerp new-addr))
    (should (= 1 (nelisp-allocator-header-age header)))
    (should (eq 'cons-pool (nelisp-allocator-header-family header)))
    ;; Promote a second time on the same slot's bytes — age saturates
    ;; only at 63 (6-bit ceiling) so a single extra bump must increment.
    (nelisp-allocator--update-age tenured new-addr 1)
    (should (= 2 (nelisp-allocator-header-age
                  (gethash new-addr headers))))
    ;; Saturation test: bump by a huge increment, age clamps at 63.
    (nelisp-allocator--update-age tenured new-addr 1000)
    (should (= nelisp-allocator--header-age-mask
               (nelisp-allocator-header-age
                (gethash new-addr headers))))))

;;; (13) coalesce merges adjacent buddies --------------------------

(ert-deftest nelisp-allocator-tenured-coalesce-merges-adjacent ()
  "`tenured-coalesce' merges adjacent same-class free blocks whose
base address is aligned to the next-larger class.

Allocate 4 consecutive 16-byte blocks at 16-byte-aligned bump-base,
free all 4 → coalesce should merge (a, b) → 32, (c, d) → 32, then
(ab, cd) → 64 if the alignment supports it."
  (nelisp-allocator--reset-region-table)
  (let* ((tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (a (nelisp-allocator-tenured-alloc tenured 'cons-pool 16))
         (b (nelisp-allocator-tenured-alloc tenured 'cons-pool 16))
         (c (nelisp-allocator-tenured-alloc tenured 'cons-pool 16))
         (d (nelisp-allocator-tenured-alloc tenured 'cons-pool 16)))
    ;; Confirm contiguous layout (the bump cursor packs them tightly).
    (should (= b (+ a 16)))
    (should (= c (+ b 16)))
    (should (= d (+ c 16)))
    (mapc (lambda (addr) (nelisp-allocator-tenured-free tenured addr))
          (list a b c d))
    ;; All 4 sit in the 16-byte bin pre-coalesce.
    (should (= 4 (length (cdr (assq 16
                                    (nelisp-allocator--tenured-free-lists
                                     tenured))))))
    (let ((merges (nelisp-allocator-tenured-coalesce tenured)))
      ;; A & B align on 32 only if A is 32-aligned.  Construct so that
      ;; a is 32-aligned by the simulator's page-aligned region.start
      ;; (=`nelisp-allocator--alloc-sim-address' returns page-aligned
      ;; addresses, so the bump pointer at start is 4096-aligned, hence
      ;; trivially 32-aligned).  After (a,b) → 32 and (c,d) → 32, the
      ;; pair (ab=a, cd=c) is 32-aligned only if a is 64-aligned, which
      ;; holds for the 4096-aligned start.  So we expect 3 merges:
      ;; (a,b)→32, (c,d)→32, then (a,c)→64.
      (should (= 3 merges)))
    ;; 16-byte bin is now empty (all blocks coalesced upward).
    (should (null (cdr (assq 16 (nelisp-allocator--tenured-free-lists
                                 tenured)))))
    ;; A single 64-byte block now lives in the 64-byte bin.
    (should (equal (list a)
                   (cdr (assq 64 (nelisp-allocator--tenured-free-lists
                                  tenured)))))))

;;; (14) tenured-stats utilisation ---------------------------------

(ert-deftest nelisp-allocator-tenured-stats-utilization ()
  "`tenured-stats' reports utilisation that tracks alloc and free.

Allocate two blocks → utilisation rises above zero.  Free both →
utilisation returns to zero (every byte back on the free-list, no
live tenured payload).  The plist format must include every key
documented at the top of the function."
  (nelisp-allocator--reset-region-table)
  (let* ((tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (cap (nelisp-allocator--tenured-capacity tenured))
         (s0 (nelisp-allocator-tenured-stats tenured)))
    (should (= cap (plist-get s0 :capacity)))
    (should (= 0  (plist-get s0 :allocated)))
    (should (= cap (plist-get s0 :free)))
    (should (= 0  (plist-get s0 :utilization-percent)))
    (should (listp (plist-get s0 :size-class-distribution)))
    ;; All bins start empty.
    (dolist (entry (plist-get s0 :size-class-distribution))
      (should (= 0 (cdr entry))))
    ;; Allocate two 256-byte blocks → 512 byte allocated.
    (let ((a (nelisp-allocator-tenured-alloc tenured 'string-span 200))
          (b (nelisp-allocator-tenured-alloc tenured 'string-span 250))
          (s1 nil))
      (setq s1 (nelisp-allocator-tenured-stats tenured))
      (should (= 512 (plist-get s1 :allocated)))
      (should (= (- cap 512) (plist-get s1 :free)))
      (should (= (/ (* 100 512) cap)
                 (plist-get s1 :utilization-percent)))
      ;; Free both → allocated returns to 0; the 256-byte bin holds 2.
      (nelisp-allocator-tenured-free tenured a)
      (nelisp-allocator-tenured-free tenured b)
      (let ((s2 (nelisp-allocator-tenured-stats tenured)))
        (should (= 0 (plist-get s2 :allocated)))
        (should (= cap (plist-get s2 :free)))
        (should (= 0 (plist-get s2 :utilization-percent)))
        (let ((dist (plist-get s2 :size-class-distribution)))
          (should (= 2 (cdr (assq 256 dist)))))))))

(provide 'nelisp-allocator-test)
;;; nelisp-allocator-test.el ends here
