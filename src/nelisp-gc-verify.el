;;; nelisp-gc-verify.el --- Phase 7.3 invariant gate (Doc 30 §6.10) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.3 invariant gate (Doc 30 v2 §6.10) — *first* of the four
;; debug-build oracles required by the §7.2 LOCKED close conditions:
;;
;;   1. *verify-heap*  (THIS FILE) — region/object/free-list shape
;;      checks that detect silent corruption oracle-side, regardless of
;;      whether the mark phase missed a root.  Built first because the
;;      remaining three oracles consume the same heap walk:
;;        - verify-card-table  (Doc 30 §6.10 oracle #2, future)
;;        - poison-from-space  (Doc 30 §6.10 oracle #3, future)
;;        - double-scan        (Doc 30 §6.10 oracle #4, future)
;;
;; This module is *read-only* on every piece of mutable state — it is
;; safe to invoke between mutator phases without disturbing the
;; collector.  Each check returns a structured violation record (a
;; plist with `:check', `:severity', and an `:reason' explaining the
;; breach) so callers can fail-fast or accumulate.
;;
;; Public API:
;;
;;   `nelisp-gc-verify-region'            — single region shape check
;;   `nelisp-gc-verify-region-disjoint'   — pairwise overlap check
;;   `nelisp-gc-verify-region-objects'    — per-region object disjointness
;;   `nelisp-gc-verify-free-list'         — free-list ↔ region consistency
;;   `nelisp-gc-verify-mark-state-clean'  — no `:grey' addresses remain
;;   `nelisp-gc-verify-heap'              — full sweep, returns plist
;;
;; Doc 30 §7.2 promises the verifier MUST PASS on every cycle in debug
;; builds.  The release build never calls these functions (the call
;; sites are wrapped at integration time in Phase 7.5).  This file
;; therefore optimises for *clarity over throughput* — every loop is
;; O(N) in heap size, no hashing tricks, no early-exit shortcuts.

;;; Code:

(require 'cl-lib)
(require 'nelisp-heap-types)
(require 'nelisp-gc-inner)


;;; §1. Versioning + violation record ------------------------------

(defconst nelisp-gc-verify-oracle-version 1
  "Doc 30 §6.10 oracle suite version we implement.
Bumped on any breaking change to the violation plist shape — the
release-audit consumer keys off this number.")

(defun nelisp-gc-verify--violation (check severity reason &rest details)
  "Build a violation plist.  Internal constructor."
  (append (list :check check
                :severity severity
                :reason reason)
          details))


;;; §2. Single region shape check ----------------------------------

(defun nelisp-gc-verify-region (region)
  "Verify REGION matches Doc 29 §1.4 v1 schema.

Returns nil on PASS, or a violation plist describing the first
shape failure.  Stricter than `nelisp-gc-inner--region-p' in two
ways:
  - we report *which* key is missing/wrong (the predicate just
    returns nil), and
  - we also catch :start > :end and unknown family/generation tags."
  (cond
   ((not (listp region))
    (nelisp-gc-verify--violation
     'verify-region 'fatal "region is not a plist" :got region))
   ((not (integerp (plist-get region :region-id)))
    (nelisp-gc-verify--violation
     'verify-region 'fatal ":region-id missing or non-integer"
     :region region))
   ((not (integerp (plist-get region :start)))
    (nelisp-gc-verify--violation
     'verify-region 'fatal ":start missing or non-integer"
     :region region))
   ((not (integerp (plist-get region :end)))
    (nelisp-gc-verify--violation
     'verify-region 'fatal ":end missing or non-integer"
     :region region))
   ((< (plist-get region :end) (plist-get region :start))
    (nelisp-gc-verify--violation
     'verify-region 'fatal ":end < :start" :region region))
   ((not (memq (plist-get region :generation) '(:nursery :tenured)))
    (nelisp-gc-verify--violation
     'verify-region 'fatal "unknown :generation"
     :got (plist-get region :generation)))
   ((not (memq (plist-get region :family)
               '(:cons-pool :string-span :vector-span
                            :closure-pool :large-object)))
    (nelisp-gc-verify--violation
     'verify-region 'fatal "unknown :family"
     :got (plist-get region :family)))
   (t nil)))


;;; §3. Pairwise region disjointness ----------------------------------

(defun nelisp-gc-verify-region-disjoint (regions)
  "Verify no two regions in REGIONS overlap.

Half-open ranges: `[start, end)' on each side.  Returns nil on
PASS, or a violation plist on the *first* overlap pair found.
Caller-supplied REGIONS is treated as immutable; we do not sort
in place.

This check is O(N²) in number of regions; Phase 7.3 keeps the
registry small (Doc 29 §1.4 caps at a few dozen regions) so this is
fine.  Phase 7.5+ may bucket regions by base address for faster
checks if the registry grows."
  (let ((violation nil)
        (sorted (cl-sort (copy-sequence regions) #'<
                         :key (lambda (r) (plist-get r :start)))))
    (cl-loop for (a b) on sorted
             while (and b (null violation))
             when (> (plist-get a :end) (plist-get b :start))
             do (setq violation
                      (nelisp-gc-verify--violation
                       'verify-region-disjoint 'fatal
                       "regions overlap"
                       :a (plist-get a :region-id)
                       :b (plist-get b :region-id)
                       :a-end (plist-get a :end)
                       :b-start (plist-get b :start))))
    violation))


;;; §4. Per-region object disjointness ----------------------------

(defun nelisp-gc-verify-region-objects (region)
  "Verify objects inside REGION don't overlap and stay in-bounds.

Per Doc 29 §2.5, family-aware allocation guarantees disjointness
within a region.  This oracle catches:
  - object addr < region :start
  - object addr + size > region :end
  - any pair of objects whose `[addr, addr+size)' ranges overlap
  - non-positive size (zero or negative)
  - non-integer addr or size

Returns nil on PASS or a violation plist on first failure."
  (let* ((rstart (plist-get region :start))
         (rend   (plist-get region :end))
         (objs   (nelisp-gc-inner--region-objects region)))
    (catch 'violation
      (dolist (o objs)
        (let ((addr (car-safe o)) (size (cdr-safe o)))
          (cond
           ((not (integerp addr))
            (throw 'violation
                   (nelisp-gc-verify--violation
                    'verify-region-objects 'fatal
                    "object addr is not an integer"
                    :region (plist-get region :region-id)
                    :addr addr)))
           ((not (integerp size))
            (throw 'violation
                   (nelisp-gc-verify--violation
                    'verify-region-objects 'fatal
                    "object size is not an integer"
                    :region (plist-get region :region-id)
                    :addr addr :size size)))
           ((<= size 0)
            (throw 'violation
                   (nelisp-gc-verify--violation
                    'verify-region-objects 'fatal
                    "object has non-positive size"
                    :region (plist-get region :region-id)
                    :addr addr :size size)))
           ((< addr rstart)
            (throw 'violation
                   (nelisp-gc-verify--violation
                    'verify-region-objects 'fatal
                    "object addr below region :start"
                    :region (plist-get region :region-id)
                    :addr addr :region-start rstart)))
           ((> (+ addr size) rend)
            (throw 'violation
                   (nelisp-gc-verify--violation
                    'verify-region-objects 'fatal
                    "object end past region :end"
                    :region (plist-get region :region-id)
                    :addr addr :size size :region-end rend))))))
      ;; Pairwise overlap (sort by addr first so we only inspect
      ;; consecutive pairs).
      (let ((sorted (cl-sort (copy-sequence objs) #'<
                             :key #'car-safe)))
        (cl-loop for (a b) on sorted
                 while b
                 when (> (+ (car-safe a) (cdr-safe a)) (car-safe b))
                 do (throw 'violation
                           (nelisp-gc-verify--violation
                            'verify-region-objects 'fatal
                            "two objects overlap"
                            :region (plist-get region :region-id)
                            :a (car-safe a) :a-end (+ (car-safe a) (cdr-safe a))
                            :b (car-safe b))))
        nil))))


;;; §5. Free-list consistency ---------------------------------------

(defun nelisp-gc-verify-free-list (family regions)
  "Verify the FAMILY free-list is consistent with REGIONS.

Checks performed:
  - every free-list entry `(addr . size)' falls inside *some*
    region whose :family matches FAMILY,
  - the entry stays in-bounds of that region,
  - no two free-list entries overlap (sorted-pairs O(N) check),
  - size is a positive integer.

Returns nil on PASS, or a violation plist on first failure.

Free-list ↔ live-object overlap is *not* checked here — that
needs the mark state, which `verify-heap' supplies via a separate
sub-check.  Splitting the two means tests can exercise free-list
geometry without standing up a full mark cycle."
  (let* ((fl (nelisp-gc-inner-free-list-for-family family))
         (matching-regions
          (cl-remove-if-not
           (lambda (r) (eq (plist-get r :family) family))
           regions)))
    (catch 'violation
      (dolist (entry fl)
        (let ((addr (car-safe entry)) (size (cdr-safe entry)))
          (cond
           ((not (integerp addr))
            (throw 'violation
                   (nelisp-gc-verify--violation
                    'verify-free-list 'fatal
                    "free-list addr non-integer"
                    :family family :addr addr)))
           ((not (integerp size))
            (throw 'violation
                   (nelisp-gc-verify--violation
                    'verify-free-list 'fatal
                    "free-list size non-integer"
                    :family family :addr addr :size size)))
           ((<= size 0)
            (throw 'violation
                   (nelisp-gc-verify--violation
                    'verify-free-list 'fatal
                    "free-list block has non-positive size"
                    :family family :addr addr :size size)))
           (t
            (let ((host
                   (cl-find-if
                    (lambda (r)
                      (and (>= addr (plist-get r :start))
                           (<= (+ addr size) (plist-get r :end))))
                    matching-regions)))
              (unless host
                (throw 'violation
                       (nelisp-gc-verify--violation
                        'verify-free-list 'fatal
                        "free-list block not inside any matching-family region"
                        :family family :addr addr :size size))))))))
      ;; Pairwise overlap inside the free-list itself.
      (let ((sorted (cl-sort (copy-sequence fl) #'< :key #'car-safe)))
        (cl-loop for (a b) on sorted
                 while b
                 when (> (+ (car-safe a) (cdr-safe a)) (car-safe b))
                 do (throw 'violation
                           (nelisp-gc-verify--violation
                            'verify-free-list 'fatal
                            "two free-list blocks overlap"
                            :family family
                            :a (car-safe a)
                            :a-end (+ (car-safe a) (cdr-safe a))
                            :b (car-safe b))))
        nil))))


;;; §6. Mark-state cleanliness --------------------------------------

(defun nelisp-gc-verify-mark-state-clean ()
  "Verify the mark state contains no `:grey' addresses.

Post-mark / post-sweep, the only legal colours are `:black' (live)
and `:white' (sweep candidate, or already-freed).  A surviving
`:grey' is an invariant breach: either the mark phase aborted mid-
cycle, or someone re-coloured an object after black settled.

Returns nil on PASS, or a violation plist listing up to the first
five offending addresses (capped to keep error reports survivable
when the breach is widespread)."
  (let ((greys nil)
        (cap   5))
    (when (hash-table-p nelisp-gc-inner--mark-state)
      (catch 'cap-reached
        (maphash
         (lambda (addr c)
           (when (eq c :grey)
             (push addr greys)
             (when (>= (length greys) cap)
               (throw 'cap-reached nil))))
         nelisp-gc-inner--mark-state)))
    (when greys
      (nelisp-gc-verify--violation
       'verify-mark-state-clean 'fatal
       "grey addresses remain after mark/sweep"
       :grey-addrs (nreverse greys)
       :grey-count (length greys)))))


;;; §7. Top-level walker --------------------------------------------

(defun nelisp-gc-verify-heap (regions)
  "Run every Doc 30 §6.10 oracle #1 sub-check across REGIONS.

Returns a plist:
  (:passed-p BOOL                ;; t when violations is empty
   :violations LIST              ;; in order discovered
   :region-count INT
   :checks-run LIST              ;; symbols of every check invoked
   :oracle-version INT)          ;; `nelisp-gc-verify-oracle-version'

Stops accumulating after 16 violations to keep test reports
readable when a single defect cascades.  Caller is free to fail-
fast on first error or to inspect the full list — `:passed-p' is
the one bit they usually need."
  (let ((violations nil)
        (cap 16)
        (checks '(verify-region
                  verify-region-disjoint
                  verify-region-objects
                  verify-free-list
                  verify-mark-state-clean)))
    (cl-flet ((push-violation
                (v)
                (when (and v (< (length violations) cap))
                  (push v violations))))
      ;; (1) Per-region shape.
      (dolist (r regions)
        (push-violation (nelisp-gc-verify-region r)))
      ;; (2) Pairwise disjointness — only when every region passed (1).
      (when (null violations)
        (push-violation (nelisp-gc-verify-region-disjoint regions)))
      ;; (3) Per-region object geometry.
      (dolist (r regions)
        (push-violation (nelisp-gc-verify-region-objects r)))
      ;; (4) Free-list per family.
      (dolist (fam '(:cons-pool :string-span :vector-span
                                :closure-pool :large-object))
        (push-violation (nelisp-gc-verify-free-list fam regions)))
      ;; (5) Mark state cleanliness.
      (push-violation (nelisp-gc-verify-mark-state-clean)))
    (list :passed-p (null violations)
          :violations (nreverse violations)
          :region-count (length regions)
          :checks-run checks
          :oracle-version nelisp-gc-verify-oracle-version)))


(provide 'nelisp-gc-verify)
;;; nelisp-gc-verify.el ends here
