;;; nelisp-allocator-gc-interop-test.el --- ERT for T65 allocator+gc-inner CRITICAL fix bundle  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T65 = T51 codex critical 6 fix bundle.  ERT 18 covering each
;; remediation:
;;
;;   #1 provider-consumer schema mismatch
;;     - 1a `nelisp-heap-types-shared-defconsts'        — shared enum
;;     - 1b `allocator-snapshot-regions-plist-schema'   — bridge fn shape
;;     - 1c `gc-inner-consumes-allocator-snapshot'      — interop walk
;;
;;   #2 family-tag enum mismatch
;;     - 2a `family-tag-allocator-and-gc-inner-agree'   — enum unified
;;     - 2b `family-tag-decode-roundtrips'              — pack/decode RT
;;     - 2c `family-tag-unknown-hard-fails'             — no nil fallback
;;
;;   #3 nursery object header missing
;;     - 3a `nursery-alloc-writes-header'               — bump path
;;     - 3b `cons-pool-alloc-still-works'               — pool path
;;     - 3c `large-object-alloc-writes-header'          — large path
;;
;;   #4 semispace overflow
;;     - 4a `init-semispace-rejects-size-mismatch'      — symmetry check
;;     - 4b `minor-copy-rejects-overflow'               — bump bound check
;;
;;   #5 synthetic root removal
;;     - 5a `collect-roots-no-synthetic-region-start'   — region.start dropped
;;     - 5b `collect-roots-still-emits-stack-roots'     — bitmap path intact
;;
;;   #6 scheduler connection
;;     - 6a `scheduler-record-alloc-bumps-counter'      — basic counter
;;     - 6b `scheduler-record-alloc-fires-trigger'      — threshold reached
;;     - 6c `nursery-alloc-feeds-scheduler'             — alloc → scheduler
;;     - 6d `cons-pool-alloc-feeds-scheduler'           — pool → scheduler
;;     - 6e `tenured-alloc-feeds-scheduler'             — tenured → scheduler
;;
;; Total: 18 ERT.  Spec asks for ≥ 15.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-heap-types)
(require 'nelisp-allocator)
(require 'nelisp-gc-inner)

;;; Fixture helper ---------------------------------------------------

(defun nelisp-allocator-gc-interop-test--reset ()
  "Reset both subsystem global state for a clean per-test fixture."
  (nelisp-allocator--reset-region-table)
  (when (boundp 'nelisp-allocator--current-nursery)
    (setq nelisp-allocator--current-nursery nil))
  (when (boundp 'nelisp-allocator--current-tenured)
    (setq nelisp-allocator--current-tenured nil))
  (when (boundp 'nelisp-allocator--current-cons-pool)
    (setq nelisp-allocator--current-cons-pool nil))
  (when (boundp 'nelisp-allocator--current-closure-pool)
    (setq nelisp-allocator--current-closure-pool nil))
  (nelisp-gc-inner-scheduler-reset)
  (clrhash nelisp-gc-inner--mark-state))

;;; #1 schema bridge -------------------------------------------------

(ert-deftest nelisp-allocator-gc-interop-test-shared-defconsts ()
  "T65 #1a: family-tag constants are sourced from `nelisp-heap-types'.
The allocator's `--family-tag-alist' must be `eq' to the shared
`nelisp-heap-family-tag-bare-alist'; gc-inner's keyword alist is
derived from the same data."
  (should (eq nelisp-allocator--family-tag-alist
              nelisp-heap-family-tag-bare-alist))
  (should (= 5 nelisp-heap-family-tag-count))
  ;; Both sides resolve cons-pool to the same numeric tag.
  (should (= (cdr (assq 'cons-pool nelisp-heap-family-tag-bare-alist))
             (cdr (assq :cons-pool nelisp-heap-family-tag-keyword-alist))))
  (should (= (cdr (assq 'large-object nelisp-heap-family-tag-bare-alist))
             (cdr (assq :large-object
                        nelisp-heap-family-tag-keyword-alist)))))

(ert-deftest nelisp-allocator-gc-interop-test-snapshot-regions-shape ()
  "T65 #1b: `nelisp-allocator-snapshot-regions' returns plist schema
matching `nelisp-gc-inner--region-p'."
  (nelisp-allocator-gc-interop-test--reset)
  (let ((nursery (nelisp-allocator-init-nursery (* 64 1024))))
    (ignore nursery)
    (let ((snapshot (nelisp-allocator-snapshot-regions)))
      (should (= 1 (length snapshot)))
      (let ((region (car snapshot)))
        (should (nelisp-gc-inner--region-p region))
        (should (integerp (plist-get region :region-id)))
        (should (integerp (plist-get region :start)))
        (should (integerp (plist-get region :end)))
        (should (eq :nursery (plist-get region :generation)))
        (should (eq :cons-pool (plist-get region :family)))
        (should (= 1 (plist-get region :version)))))))

(ert-deftest nelisp-allocator-gc-interop-test-gc-inner-consumes-snapshot ()
  "T65 #1c: gc-inner consumes an allocator-produced region snapshot
without a separate translation layer.  `--region-of' resolves an
allocated address to the right region descriptor."
  (nelisp-allocator-gc-interop-test--reset)
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (addr (nelisp-allocator-alloc-cons nursery))
         (snapshot (nelisp-allocator-snapshot-regions))
         (region (nelisp-gc-inner--region-of snapshot addr)))
    (should region)
    (should (eq :cons-pool (plist-get region :family)))
    (should (eq :nursery (plist-get region :generation)))
    (should (>= addr (plist-get region :start)))
    (should (< addr (plist-get region :end)))))

;;; #2 family-tag enum ----------------------------------------------

(ert-deftest nelisp-allocator-gc-interop-test-family-tag-agreement ()
  "T65 #2a: every family symbol packs to the same numeric tag on both
sides of the bridge.  Pre-T65 cons-pool was 1 on the allocator and 0
on gc-inner; post-T65 both sides resolve to 0."
  (dolist (cell '((cons-pool    . :cons-pool)
                  (string-span  . :string-span)
                  (vector-span  . :vector-span)
                  (closure-pool . :closure-pool)
                  (large-object . :large-object)))
    (let* ((bare (car cell))
           (kw   (cdr cell))
           (alloc-tag (cdr (assq bare nelisp-allocator--family-tag-alist)))
           (gc-tag    (cdr (assq kw  nelisp-heap-family-tag-keyword-alist))))
      (should (integerp alloc-tag))
      (should (integerp gc-tag))
      (should (= alloc-tag gc-tag))
      ;; And the inverse decode lands on the matching bare/keyword pair.
      (should (eq bare (nelisp-heap-family-bare-of-tag alloc-tag)))
      (should (eq kw   (nelisp-heap-family-keyword-of-tag gc-tag))))))

(ert-deftest nelisp-allocator-gc-interop-test-family-tag-roundtrips ()
  "T65 #2b: pack-header → decode round-trips through every family.
Validates the *unified* enum end-to-end (allocator pack, gc-inner
header decode)."
  (dolist (family '(cons-pool string-span vector-span
                              closure-pool large-object))
    (let* ((header (nelisp-allocator-pack-header family 1234))
           (alloc-decoded (nelisp-allocator-header-family header))
           (gc-tag (nelisp-allocator-header-family-tag header))
           (gc-decoded (nelisp-gc-inner--family-tag-to-keyword gc-tag)))
      (should (eq family alloc-decoded))
      (should (eq (nelisp-heap-family-bare-to-keyword family) gc-decoded)))))

(ert-deftest nelisp-allocator-gc-interop-test-family-tag-unknown-hard-fails ()
  "T65 #2c: unknown family symbol / tag signal hard errors instead of
silent fallback.  Pre-T65 the silent fallback masked schema bugs."
  (should-error (nelisp-heap-family-tag-of-bare 'no-such-family)
                :type 'nelisp-heap-unknown-family)
  (should-error (nelisp-heap-family-tag-of-keyword :no-such-family)
                :type 'nelisp-heap-unknown-family)
  (should-error (nelisp-heap-family-bare-of-tag 99)
                :type 'nelisp-heap-unknown-family)
  (should-error (nelisp-heap-family-keyword-of-tag 99)
                :type 'nelisp-heap-unknown-family)
  ;; gc-inner-side tag→keyword also hard-fails.
  (should-error (nelisp-gc-inner--family-tag-to-keyword 99)
                :type 'nelisp-heap-unknown-family))

;;; #3 nursery header writes ----------------------------------------

(ert-deftest nelisp-allocator-gc-interop-test-nursery-alloc-writes-header ()
  "T65 #3a: `nursery-alloc' writes a Doc 30 v2 §2.13 packed header
to the nursery's headers map.  Pre-T65 the nursery skipped header
writes entirely (only tenured had them)."
  (nelisp-allocator-gc-interop-test--reset)
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (addr (nelisp-allocator-alloc-cons nursery))
         (header (nelisp-allocator-nursery-header-at nursery addr)))
    (should (integerp header))
    (should (eq 'cons-pool (nelisp-allocator-header-family header)))
    (should (= nelisp-allocator--cons-size
               (nelisp-allocator-header-size header)))
    (should (= 0 (nelisp-allocator-header-mark-bit header)))
    (should (= 0 (nelisp-allocator-header-forwarding-bit header)))
    (should (= 0 (nelisp-allocator-header-age header)))))

(ert-deftest nelisp-allocator-gc-interop-test-nursery-alloc-string-span-header ()
  "T65 #3a (string-span variant): header carries the string-span tag,
not cons-pool.  Confirms the family argument flows correctly through
the `nursery-alloc' → `--nursery-record-header' edge."
  (nelisp-allocator-gc-interop-test--reset)
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (addr (nelisp-allocator-alloc-string-span nursery 64))
         (header (nelisp-allocator-nursery-header-at nursery addr)))
    (should (integerp header))
    (should (eq 'string-span (nelisp-allocator-header-family header)))))

(ert-deftest nelisp-allocator-gc-interop-test-large-object-writes-header ()
  "T65 #3c: `alloc-large-object' records a header in the
large-object headers map."
  (nelisp-allocator-gc-interop-test--reset)
  (let* ((nursery (nelisp-allocator-init-nursery (* 64 1024)))
         (addr (nelisp-allocator-alloc-large-object nursery (* 8 1024)))
         (header (nelisp-allocator-large-object-header-at addr)))
    (should (integerp header))
    (should (eq 'large-object (nelisp-allocator-header-family header)))
    (should (>= (nelisp-allocator-header-size header) (* 8 1024)))))

;;; #4 semispace overflow -------------------------------------------

(ert-deftest nelisp-allocator-gc-interop-test-init-semispace-size-mismatch ()
  "T65 #4a: `init-semispace' hard-fails on asymmetric from/to halves."
  (let ((from (list :region-id 0 :start 0 :end 1000
                    :generation :nursery :family :cons-pool))
        (to   (list :region-id 1 :start 10000 :end 10500
                    :generation :nursery :family :cons-pool)))
    (should-error (nelisp-gc-inner-init-semispace from to)
                  :type 'error)))

(ert-deftest nelisp-allocator-gc-interop-test-init-semispace-symmetric-ok ()
  "T65 #4a (positive): `init-semispace' accepts symmetric halves
(regression test that the new check doesn't break the happy path)."
  (let* ((from (list :region-id 0 :start 0 :end 1000
                     :generation :nursery :family :cons-pool))
         (to   (list :region-id 1 :start 10000 :end 11000
                     :generation :nursery :family :cons-pool))
         (semi (nelisp-gc-inner-init-semispace from to)))
    (should (nelisp-gc-inner--semispace-p semi))
    (should (= 10000 (nelisp-gc-inner--semispace-scan-pointer semi)))
    (should (= 10000 (nelisp-gc-inner--semispace-free-pointer semi)))))

(ert-deftest nelisp-allocator-gc-interop-test-minor-copy-overflow ()
  "T65 #4b: `--minor-copy-one' hard-fails when bump cursor would step
past `to-end'.  We provoke this by handing the fn an over-sized cell."
  (let* ((from (list :region-id 0 :start 0 :end 1000
                     :generation :nursery :family :cons-pool))
         (to   (list :region-id 1 :start 10000 :end 11000
                     :generation :nursery :family :cons-pool))
         (semi (nelisp-gc-inner-init-semispace from to))
         (objs (list (cons 100 (list :addr 100 :header 0 :fields nil
                                     :size 9999))))
         (objs-fn (lambda (a) (cdr (assq a objs)))))
    (should-error
     (nelisp-gc-inner--minor-copy-one
      semi 100 objs-fn (make-hash-table :test 'eql) (list nil))
     :type 'error)))

;;; #5 synthetic root removal ---------------------------------------

(ert-deftest nelisp-allocator-gc-interop-test-collect-roots-no-synthetic ()
  "T65 #5a: `collect-roots' no longer injects region.start as a
synthetic live root.  The pre-T65 path forced the head object of
every region permanently black, breaking GC correctness."
  (let* ((bv (let ((v (make-bool-vector 1 nil))) (aset v 0 t) v))
         (meta (list :gc-metadata-version 1
                     :function-name 'fa
                     :safe-points
                     (list (list :id 0 :kind 'entry :pc-offset 0
                                 :live-roots bv :frame-size 8
                                 :frame-layout (list :base 700 :slots 1)))))
         (region (list :region-id 0 :start 500 :end 600
                       :generation :nursery :family :cons-pool))
         (roots (nelisp-gc-inner-collect-roots (list meta) (list region))))
    ;; Stack root included.
    (should (memq 700 roots))
    ;; Region :start NOT injected.
    (should (null (memq 500 roots)))))

(ert-deftest nelisp-allocator-gc-interop-test-collect-roots-stack-only ()
  "T65 #5b: stack-derived roots from multiple safe-points still flow
through after the synthetic-root code path was removed.  Regression
test covering the surviving code path."
  (let* ((bv0 (let ((v (make-bool-vector 1 nil))) (aset v 0 t) v))
         (bv1 (let ((v (make-bool-vector 2 nil))) (aset v 1 t) v))
         (meta (list :gc-metadata-version 1
                     :function-name 'f
                     :safe-points
                     (list (list :id 0 :kind 'entry :pc-offset 0
                                 :live-roots bv0 :frame-size 8
                                 :frame-layout (list :base 100 :slots 1))
                           (list :id 1 :kind 'back-edge :pc-offset 4
                                 :live-roots bv1 :frame-size 16
                                 :frame-layout (list :base 200 :slots 2))))))
    (let ((roots (nelisp-gc-inner-collect-roots (list meta) nil)))
      (should (memq 100 roots))
      (should (memq 208 roots))
      (should (= 2 (length roots))))))

;;; #6 scheduler connection -----------------------------------------

(ert-deftest nelisp-allocator-gc-interop-test-scheduler-record-alloc ()
  "T65 #6a: `scheduler-record-alloc' bumps `bytes-since-minor'
without yet hitting the trigger threshold."
  (nelisp-allocator-gc-interop-test--reset)
  (let ((nelisp-gc-inner-nursery-trigger-bytes (* 1 1024 1024)))
    (should (eq 'none (nelisp-gc-inner-scheduler-record-alloc 100)))
    (should (= 100 nelisp-gc-inner--bytes-since-minor))
    (should (eq 'none (nelisp-gc-inner-scheduler-record-alloc 200)))
    (should (= 300 nelisp-gc-inner--bytes-since-minor))
    (should (= 0 nelisp-gc-inner--minor-trigger-count))))

(ert-deftest nelisp-allocator-gc-interop-test-scheduler-trigger-fires ()
  "T65 #6b: crossing `nursery-trigger-bytes' returns `'minor' and
bumps the trigger counter."
  (nelisp-allocator-gc-interop-test--reset)
  (let ((nelisp-gc-inner-nursery-trigger-bytes 1024))
    (should (eq 'none (nelisp-gc-inner-scheduler-record-alloc 500)))
    (should (eq 'minor (nelisp-gc-inner-scheduler-record-alloc 600)))
    (should (= 1 nelisp-gc-inner--minor-trigger-count))))

(ert-deftest nelisp-allocator-gc-interop-test-nursery-feeds-scheduler ()
  "T65 #6c: `nursery-alloc' reports byte deltas to the scheduler so a
pure-allocation workload eventually trips the trigger."
  (nelisp-allocator-gc-interop-test--reset)
  (let ((nelisp-gc-inner-nursery-trigger-bytes (* 64 1024)))
    (let ((nursery (nelisp-allocator-init-nursery (* 1024 1024))))
      (should (= 0 nelisp-gc-inner--bytes-since-minor))
      (nelisp-allocator-alloc-cons nursery)
      (should (= nelisp-allocator--cons-size
                 nelisp-gc-inner--bytes-since-minor))
      (dotimes (_ 16)
        (nelisp-allocator-alloc-cons nursery))
      (should (= (* 17 nelisp-allocator--cons-size)
                 nelisp-gc-inner--bytes-since-minor)))))

(ert-deftest nelisp-allocator-gc-interop-test-cons-pool-feeds-scheduler ()
  "T65 #6d: `cons-pool-alloc' fast path also reports to the scheduler."
  (nelisp-allocator-gc-interop-test--reset)
  ;; init-cons-pool registers a region, which requires either a fresh
  ;; sim-address pool or a prior init-nursery (which clears the table).
  (nelisp-allocator-init-nursery (* 64 1024))
  (let ((before nelisp-gc-inner--bytes-since-minor)
        (pool (nelisp-allocator-init-cons-pool 1)))
    (nelisp-allocator-cons-pool-alloc pool)
    (should (> nelisp-gc-inner--bytes-since-minor before))
    (should (= (- nelisp-gc-inner--bytes-since-minor before)
               nelisp-allocator--cons-size))))

(ert-deftest nelisp-allocator-gc-interop-test-tenured-feeds-scheduler ()
  "T65 #6e: `tenured-alloc' reports its size-class delta to the
scheduler (so a tenured-only workload still triggers periodic GC)."
  (nelisp-allocator-gc-interop-test--reset)
  (nelisp-allocator-init-nursery (* 64 1024))
  (let* ((tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (before nelisp-gc-inner--bytes-since-minor))
    (nelisp-allocator-tenured-alloc tenured 'cons-pool 16)
    (should (> nelisp-gc-inner--bytes-since-minor before))))

(provide 'nelisp-allocator-gc-interop-test)
;;; nelisp-allocator-gc-interop-test.el ends here
