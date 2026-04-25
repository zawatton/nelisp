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

(provide 'nelisp-allocator-test)
;;; nelisp-allocator-test.el ends here
