;;; nelisp-gc-verify.el --- Phase 7.3 invariant gate (Doc 30 §6.10) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.3 invariant gate (Doc 30 v2 §6.10) — all four debug-build
;; oracles required by the §7.2 LOCKED close conditions:
;;
;;   1. *verify-heap*               — region/object/free-list shape
;;   2. *verify-card-table*         — card dirty bit ↔ live cross-gen ref
;;   3. *verify-poison-from-space*  — Cheney from-space stays dead post-flip
;;   4. *verify-double-scan*        — mark phase deterministic across runs
;;
;; Oracles #2/#3/#4 reuse the violation plist shape and structural walk
;; pattern from oracle #1.  Oracle #3 is the lone non-read-only check —
;; it stamps a side-table marker on the dead semispace half so any
;; subsequent live-object lookup is observable.
;;
;; Each check returns a structured violation record (a plist with
;; `:check', `:severity', and a `:reason' explaining the breach) so
;; callers can fail-fast or accumulate.
;;
;; Public API:
;;
;;   `nelisp-gc-verify-region'              — single region shape check
;;   `nelisp-gc-verify-region-disjoint'     — pairwise overlap check
;;   `nelisp-gc-verify-region-objects'      — per-region object disjointness
;;   `nelisp-gc-verify-free-list'           — free-list ↔ region consistency
;;   `nelisp-gc-verify-mark-state-clean'    — no `:grey' addresses remain
;;   `nelisp-gc-verify-heap'                — full sweep, returns plist
;;   `nelisp-gc-verify-card-table'          — oracle #2 (cross-gen edge)
;;   `nelisp-gc-verify-poison-mark'         — oracle #3 setup (post-flip)
;;   `nelisp-gc-verify-poison-touch'        — oracle #3 access witness
;;   `nelisp-gc-verify-poison-from-space'   — oracle #3 verdict
;;   `nelisp-gc-verify-poison-reset'        — oracle #3 cycle teardown
;;   `nelisp-gc-verify-double-scan'         — oracle #4 (mark determinism)
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


;;; §8. Oracle #2 — verify-card-table -----------------------------
;;
;; *Contract* (Doc 30 v2 §6.10):
;;   For every live tenured object, every outgoing pointer that lands
;;   inside the nursery region must lie on a *dirty* card in the
;;   tenured-side card table.  A nursery-pointer on a clean card means
;;   the write barrier was bypassed or the card-table flip cleared a
;;   bit prematurely — in either case minor GC will miss this root and
;;   silently corrupt the heap once the to-space copy completes.
;;
;; *False positives are OK*: a dirty card with no live nursery pointer
;; just costs Phase 7.3.5 a wasted scan; only false-negatives (nursery
;; pointer on a clean card) violate the invariant.
;;
;; *Object enumeration*: we reuse the `:objects-of' / `:objects' walker
;; pattern from `nelisp-gc-inner--region-objects' but accept *plist*
;; objects of shape `(ADDR . PLIST-WITH-:fields)' — the same shape the
;; Phase 7.3.4 remembered-set scan consumes.  TENURED-REGION's
;; `:objects-with-fields' key (or `:objects-with-fields-of' callback)
;; supplies them; if neither is present the oracle returns nil
;; (vacuously — there's nothing to check).

(defun nelisp-gc-verify--objects-with-fields (region)
  "Return REGION's `(ADDR . PLIST-WITH-:fields)' object list, or nil.
Accepts either the static `:objects-with-fields' key or a
`:objects-with-fields-of' callback (parallel to `:objects-of')."
  (let ((static (plist-get region :objects-with-fields)))
    (cond
     ((listp static) static)
     (t
      (let ((walker (plist-get region :objects-with-fields-of)))
        (when (functionp walker)
          (funcall walker region)))))))

(defun nelisp-gc-verify-card-table (card-table tenured-region nursery-region)
  "Verify CARD-TABLE's dirty bits cover every live cross-gen pointer.

Walks every object listed under TENURED-REGION's
`:objects-with-fields' (or `:objects-with-fields-of') key.  For each
object's `:fields' addresses that fall inside NURSERY-REGION's
`[:start :end)' window, computes the tenured-side card index and
asserts CARD-TABLE's dirty bit at that index is t.

Returns nil on PASS, or a violation plist on the *first* missing
dirty bit — the typical failure mode is a single overlooked write
barrier so reporting one is informative without flooding logs.

Skipped silently when:
  - CARD-TABLE is not a `nelisp-gc-inner--card-table'.
  - TENURED-REGION carries no object walker (vacuously empty heap).
This keeps the oracle composable with partially-initialised cycles
(boot, post-major-sweep) without falsely reporting empty heaps."
  (cond
   ((not (nelisp-gc-inner--card-table-p card-table))
    (nelisp-gc-verify--violation
     'verify-card-table 'fatal "card-table arg is not a card-table struct"
     :got card-table))
   ((not (listp tenured-region))
    (nelisp-gc-verify--violation
     'verify-card-table 'fatal "tenured-region is not a plist"
     :got tenured-region))
   ((not (listp nursery-region))
    (nelisp-gc-verify--violation
     'verify-card-table 'fatal "nursery-region is not a plist"
     :got nursery-region))
   (t
    (let ((objs (nelisp-gc-verify--objects-with-fields tenured-region))
          (dirty-bv (nelisp-gc-inner--card-table-dirty-bits card-table)))
      (catch 'violation
        (dolist (entry objs)
          (let* ((addr   (car-safe entry))
                 (plist  (cdr-safe entry))
                 (fields (and (listp plist) (plist-get plist :fields))))
            (dolist (field fields)
              (when (nelisp-gc-inner--addr-in-region-p
                     nursery-region field)
                ;; Live cross-generation pointer.  Demand a dirty card.
                (let ((card (nelisp-gc-inner--card-of card-table addr)))
                  (cond
                   ((null card)
                    ;; Source object falls outside the card table's
                    ;; window — either the table covers the wrong
                    ;; region or the object lives in a peer region
                    ;; that should have its own table.  Either way it
                    ;; is a contract breach we surface.
                    (throw 'violation
                           (nelisp-gc-verify--violation
                            'verify-card-table 'fatal
                            "object addr outside card-table region"
                            :addr addr :field field)))
                   ((not (aref dirty-bv card))
                    (throw 'violation
                           (nelisp-gc-verify--violation
                            'verify-card-table 'fatal
                            "live cross-gen pointer on clean card"
                            :addr addr
                            :field field
                            :card-index card)))))))))
        nil)))))


;;; §9. Oracle #3 — poison-from-space -----------------------------
;;
;; *Contract* (Doc 30 v2 §6.10):
;;   After a Cheney semispace flip, the formerly-live half is dead and
;;   nothing should ever read from it again.  Production fills it with
;;   `0xDEADBEEF' and relies on the MMU + signal handler to catch
;;   accidental reads (= SIGSEGV on a guard page).  In the simulator
;;   we model this with a side-table: `verify-poison-mark' stamps the
;;   from-space half as poisoned, `verify-poison-touch' is the explicit
;;   read witness mutator code calls when it would have dereferenced
;;   the address (in production it would just deref and SIGSEGV;
;;   simulator wires the witness in instead), and `verify-poison-from-
;;   space' returns the verdict.
;;
;; The oracle's job is *witnessing*, not preventing — the test suite
;; exercises code paths that *might* touch the poisoned region and the
;; oracle reports any actual touch.  This is exactly the same
;; signal/noise tradeoff as the production SIGSEGV handler.

(defvar nelisp-gc-verify--poison-regions nil
  "Alist `(REGION-ID . PLIST)' of currently poisoned regions.
PLIST shape: `(:start S :end E :stamp INT :touches LIST)'.
`:touches' accumulates `(ADDR . CONTEXT)' witness records every
time `verify-poison-touch' fires.  Reset by `verify-poison-reset'.")

(defconst nelisp-gc-verify-poison-stamp #xDEADBEEF
  "Magic stamp the simulator records in lieu of an in-page byte fill.
Production replaces the dead half's bytes with this value (Doc 30
§6.10 v2); the simulator just records the stamp on the side table.")

(defun nelisp-gc-verify-poison-reset ()
  "Drop every poisoned-region marker.
Called between mark cycles (poisoning a half lasts one cycle: the
next minor GC's flip un-poisons it by re-using as to-space).
Returns the number of regions cleared."
  (let ((n (length nelisp-gc-verify--poison-regions)))
    (setq nelisp-gc-verify--poison-regions nil)
    n))

(defun nelisp-gc-verify-poison-mark (region)
  "Stamp REGION as poisoned (Cheney from-space post-flip).
REGION is a Doc 29 §1.4 nursery descriptor (the half that just
finished serving as `from-space' for a minor GC).  Returns the new
poison entry plist for caller-side bookkeeping; signals
`wrong-type-argument' on a malformed region."
  (unless (and (listp region)
               (integerp (plist-get region :region-id))
               (integerp (plist-get region :start))
               (integerp (plist-get region :end)))
    (signal 'wrong-type-argument (list 'verify-poison-region region)))
  (let* ((rid   (plist-get region :region-id))
         (entry (list :start   (plist-get region :start)
                      :end     (plist-get region :end)
                      :stamp   nelisp-gc-verify-poison-stamp
                      :touches nil)))
    ;; Idempotent on REGION-ID — a re-mark just resets touches.
    (setq nelisp-gc-verify--poison-regions
          (cons (cons rid entry)
                (assq-delete-all rid nelisp-gc-verify--poison-regions)))
    entry))

(defun nelisp-gc-verify-poison-touch (addr &optional context)
  "Record that ADDR was read post-poisoning, if it falls in any region.
Returns t when ADDR was inside a poisoned region (and a touch was
recorded), nil otherwise.  CONTEXT is an opaque marker the caller
attaches for diagnostic backtracking (e.g. the calling function's
symbol)."
  (let ((hit nil))
    (dolist (cell nelisp-gc-verify--poison-regions)
      (let* ((entry (cdr cell))
             (start (plist-get entry :start))
             (end   (plist-get entry :end)))
        (when (and (integerp addr) (>= addr start) (< addr end))
          (setq hit t)
          ;; Push touch record onto the entry's :touches list.
          (let ((touches (plist-get entry :touches)))
            (plist-put entry :touches
                       (cons (cons addr context) touches))))))
    hit))

(defun nelisp-gc-verify-poison-from-space ()
  "Return the verdict for every currently poisoned region.

Returns nil on PASS (every poisoned region's `:touches' is empty)
or a violation plist on the first region whose `:touches' is non-
empty.  Reports up to the first five offending touch records to
keep error logs survivable when a bad path keeps reading the same
page.

Doc 30 §6.10 oracle #3 — false-positive impossible (a clean from-
space cannot witness a touch if no caller invoked
`verify-poison-touch'), false-negative possible only when callers
forget to wire the touch witness."
  (catch 'violation
    (dolist (cell nelisp-gc-verify--poison-regions)
      (let* ((rid     (car cell))
             (entry   (cdr cell))
             (touches (plist-get entry :touches)))
        (when touches
          (let ((capped (cl-subseq touches 0 (min 5 (length touches)))))
            (throw 'violation
                   (nelisp-gc-verify--violation
                    'verify-poison-from-space 'fatal
                    "from-space read after poison"
                    :region-id   rid
                    :touch-count (length touches)
                    :touches     capped
                    :stamp       nelisp-gc-verify-poison-stamp))))))
    nil))


;;; §10. Oracle #4 — double-scan ----------------------------------
;;
;; *Contract* (Doc 30 v2 §6.10):
;;   The mark phase is a pure function of (root-set, heap-regions).
;;   Running it twice consecutively must yield byte-identical
;;   `nelisp-gc-inner--mark-state' contents.  A divergence indicates
;;   either a non-determinism in the worklist drain order that *also*
;;   leaks into the final colour assignment (= a logic bug), or a
;;   data race against some shared scan state (= a freshly-introduced
;;   thread-safety bug).
;;
;; In a single-threaded simulator the first failure mode is the only
;; viable one — the oracle still catches it and is cheap (~2x
;; mark-throughput in debug builds).

(defun nelisp-gc-verify--snapshot-mark-state ()
  "Return a fresh hash-table copy of `nelisp-gc-inner--mark-state'.
Snapshot is detached from the live state so a subsequent mark
overwrite cannot retroactively change the snapshot's contents."
  (let ((copy (make-hash-table :test 'eql)))
    (maphash (lambda (k v) (puthash k v copy))
             nelisp-gc-inner--mark-state)
    copy))

(defun nelisp-gc-verify--mark-state-equal (a b)
  "Return t when hash-tables A and B carry identical key/value pairs.
Asymmetric: caller checks both directions or uses this within an
already-size-matched comparison."
  (and (hash-table-p a) (hash-table-p b)
       (= (hash-table-count a) (hash-table-count b))
       (catch 'mismatch
         (maphash (lambda (k v)
                    (unless (eq (gethash k b 'absent) v)
                      (throw 'mismatch nil)))
                  a)
         t)))

(defun nelisp-gc-verify-double-scan (root-set heap-regions)
  "Run mark phase twice; verify the two mark states are identical.

Re-runs `nelisp-gc-inner-run-mark-phase' twice with the same
ROOT-SET and HEAP-REGIONS, snapshotting the resulting mark state
each time.  Returns nil on PASS (the two states are equal) or a
violation plist describing the divergence on FAIL.

The violation plist includes:
  :first-marked-count  — `:marked-count' from the first run
  :second-marked-count — `:marked-count' from the second run
  :first-grey-count    — should be 0 (mark phase invariant)
  :second-grey-count   — should be 0 (mark phase invariant)
  :diff-keys           — up to 5 addresses whose colour differs
  :reason              — human-readable cause

Side effect: the final mark state on disk reflects the *second*
run.  Callers who need to preserve the original cycle's mark state
must snapshot it before calling this oracle.  The two-run pattern
is deliberate — Doc 30 §6.10 specifies the second run as the
oracle's authoritative state (the first is the subject under
test)."
  (let* ((first-result  (nelisp-gc-inner-run-mark-phase
                         root-set heap-regions))
         (first-snap    (nelisp-gc-verify--snapshot-mark-state))
         (second-result (nelisp-gc-inner-run-mark-phase
                         root-set heap-regions))
         (second-snap   (nelisp-gc-verify--snapshot-mark-state)))
    (cond
     ((not (= (plist-get first-result  :marked-count)
              (plist-get second-result :marked-count)))
      (nelisp-gc-verify--violation
       'verify-double-scan 'fatal
       "marked-count diverged between runs"
       :first-marked-count  (plist-get first-result  :marked-count)
       :second-marked-count (plist-get second-result :marked-count)))
     ((not (nelisp-gc-verify--mark-state-equal first-snap second-snap))
      (let ((diff-keys nil)
            (cap 5))
        (catch 'cap-reached
          (maphash
           (lambda (k v1)
             (let ((v2 (gethash k second-snap 'absent)))
               (unless (eq v1 v2)
                 (push (list :addr k :first v1 :second v2) diff-keys)
                 (when (>= (length diff-keys) cap)
                   (throw 'cap-reached nil)))))
           first-snap))
        (nelisp-gc-verify--violation
         'verify-double-scan 'fatal
         "mark-state diverged between runs"
         :first-marked-count  (plist-get first-result  :marked-count)
         :second-marked-count (plist-get second-result :marked-count)
         :first-grey-count    (plist-get first-result  :grey-count)
         :second-grey-count   (plist-get second-result :grey-count)
         :diff-keys           (nreverse diff-keys))))
     (t nil))))


(provide 'nelisp-gc-verify)
;;; nelisp-gc-verify.el ends here
