;;; nelisp-gc-inner-test.el --- Phase 7.3.1 mark phase ERTs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.3.1 MVP subset — wire decoder round-trip, tri-color invariant
;; on simulated heaps, root extraction from Doc 28 §2.9 metadata, and
;; finalizer queue enqueue/drain with the `:once' resurrection policy.
;; See docs/design/30-phase7.3-gc-inner.org §3.1.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-gc-inner)

;;; Helpers ----------------------------------------------------------

(defun nelisp-gc-inner-test--mk-region (id start end children-of)
  "Build a Doc 29 §1.4 region descriptor for tests."
  (list :region-id   id
        :start       start
        :end         end
        :generation  :nursery
        :family      :cons-pool
        :children-of children-of))

(defun nelisp-gc-inner-test--reset ()
  "Reset every piece of mutable state between ERTs."
  (nelisp-gc-inner-init-mark-state)
  (nelisp-gc-inner-finalizer-queue-reset))


;;; 1. Wire decoder round-trip --------------------------------------

(ert-deftest nelisp-gc-inner-decode-wire-roundtrip ()
  "encode-wire-for-test → decode-wire is identity on a canonical fixture."
  (nelisp-gc-inner-test--reset)
  (let* ((bv (make-bool-vector 4 nil))
         (_  (progn (aset bv 0 t) (aset bv 2 t)))
         (internal
          (list :gc-metadata-version 1
                :function-name 'fn-roundtrip
                :safe-points
                (list (list :id 0 :kind 'entry :pc-offset 0
                            :live-roots bv :frame-size 32
                            :frame-layout (list :size 32 :slots 4
                                                :base 0 :slot-table nil)))))
         (wire   (nelisp-gc-inner-encode-wire-for-test internal))
         (decoded (nelisp-gc-inner-decode-wire wire))
         (sp0     (car (plist-get decoded :safe-points))))
    (should (eq 1 (plist-get decoded :gc-metadata-version)))
    (should (eq 'fn-roundtrip (plist-get decoded :function-name)))
    (should (eq 0 (plist-get sp0 :id)))
    (should (eq 'entry (plist-get sp0 :kind)))
    (should (equal bv (plist-get sp0 :live-roots)))
    (should (eq 32 (plist-get sp0 :frame-size)))))

(ert-deftest nelisp-gc-inner-decode-wire-rejects-version-skew ()
  "Decoder signals on unknown metadata version (forward-compat guard)."
  (let ((wire (list :gc-metadata-version 999
                    :function-name 'x
                    :safe-points nil)))
    (should-error (nelisp-gc-inner-decode-wire wire))))


;;; 2. Mark phase on empty heap -------------------------------------

(ert-deftest nelisp-gc-inner-mark-phase-on-empty-heap-marks-zero ()
  "Empty root set + empty registry → 0 marked, 0 grey, invariant holds."
  (nelisp-gc-inner-test--reset)
  (let ((stats (nelisp-gc-inner-run-mark-phase nil nil)))
    (should (eq 0 (plist-get stats :marked-count)))
    (should (eq 0 (plist-get stats :grey-count)))
    (should (integerp (plist-get stats :elapsed-ms)))
    (should (>= (plist-get stats :elapsed-ms) 0))))


;;; 3. Tri-color invariant on a non-trivial graph -------------------

(ert-deftest nelisp-gc-inner-mark-phase-tri-color-invariant ()
  "After mark phase, no address remains :grey (work-list fully drained).
We build a tiny address graph 100→200→300 with one cycle (300→200) and
mark from root {100}; every reachable address must end up :black, every
unreachable must stay :white, and `:grey-count' must be 0."
  (nelisp-gc-inner-test--reset)
  (let* ((graph (let ((h (make-hash-table :test 'eql)))
                  (puthash 100 (list 200) h)
                  (puthash 200 (list 300) h)
                  (puthash 300 (list 200) h)  ; cycle back to 200
                  (puthash 400 nil           h)  ; unreachable leaf
                  h))
         (region (nelisp-gc-inner-test--mk-region
                  0 100 500
                  (lambda (a) (gethash a graph))))
         (stats  (nelisp-gc-inner-run-mark-phase '(100) (list region))))
    (should (eq 0 (plist-get stats :grey-count)))
    (should (eq :black (nelisp-gc-inner-mark-color 100)))
    (should (eq :black (nelisp-gc-inner-mark-color 200)))
    (should (eq :black (nelisp-gc-inner-mark-color 300)))
    ;; 400 was never enqueued by the walker, so it's implicit :white.
    (should (eq :white (nelisp-gc-inner-mark-color 400)))
    (should (eq 3 (plist-get stats :marked-count)))))


;;; 4. Root scan via Doc 28 §2.9 GC metadata ------------------------

(ert-deftest nelisp-gc-inner-collect-roots-from-gc-metadata ()
  "Collect-roots resolves live-roots bitmap through frame layout.
Two safe-points (one entry, one back-edge) on different frames each
contribute one address; the heap-region :start gets contributed too."
  (nelisp-gc-inner-test--reset)
  (let* ((bv0 (let ((v (make-bool-vector 2 nil))) (aset v 0 t) v))
         (bv1 (let ((v (make-bool-vector 2 nil))) (aset v 1 t) v))
         (meta-a
          (list :gc-metadata-version 1
                :function-name 'fa
                :safe-points
                (list (list :id 0 :kind 'entry :pc-offset 0
                            :live-roots bv0 :frame-size 16
                            :frame-layout (list :base 1000 :slots 2)))))
         (meta-b
          (list :gc-metadata-version 1
                :function-name 'fb
                :safe-points
                (list (list :id 0 :kind 'back-edge :pc-offset 12
                            :live-roots bv1 :frame-size 16
                            :frame-layout (list :base 2000 :slots 2)))))
         (region (nelisp-gc-inner-test--mk-region 0 500 600 #'ignore))
         (roots  (nelisp-gc-inner-collect-roots
                  (list meta-a meta-b) (list region))))
    ;; vid 0 at base 1000 → 1000; vid 1 at base 2000 → 2008; region :start 500.
    (should (memq 1000 roots))
    (should (memq 2008 roots))
    (should (memq 500  roots))
    ;; Dedup invariant: re-running must keep length identical.
    (should (= (length roots)
               (length (cl-remove-duplicates roots))))))


;;; 5. Finalizer queue enqueue + drain ------------------------------

(ert-deftest nelisp-gc-inner-finalizer-queue-enqueue-and-drain ()
  "Enqueue two finalizers, drain runs both with their ADDR argument,
counters update, and the `:once' policy blocks re-fire of the same
ADDR after drain."
  (nelisp-gc-inner-test--reset)
  (let* ((seen-addrs nil)
         (fn (lambda (a) (push a seen-addrs))))
    (should (= 1 (nelisp-gc-inner-enqueue-finalizer fn 111)))
    (should (= 2 (nelisp-gc-inner-enqueue-finalizer fn 222)))
    (should (= 2 (nelisp-gc-inner-finalizer-queue-length)))
    (let ((stats (nelisp-gc-inner-drain-finalizer-queue)))
      (should (eq 2 (plist-get stats :fired)))
      (should (eq 0 (plist-get stats :errors)))
      (should (eq 0 (plist-get stats :remaining))))
    ;; Both ADDRs appeared exactly once (order = head-to-tail).
    (should (equal (sort (copy-sequence seen-addrs) #'<)
                   '(111 222)))
    ;; :once policy — re-enqueue 111 is dropped.
    (should (= 0 (nelisp-gc-inner-enqueue-finalizer fn 111)))
    (should (= 0 (nelisp-gc-inner-finalizer-queue-length)))))


(provide 'nelisp-gc-inner-test)
;;; nelisp-gc-inner-test.el ends here
