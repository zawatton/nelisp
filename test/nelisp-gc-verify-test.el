;;; nelisp-gc-verify-test.el --- ERTs for Phase 7.3 §6.10 oracle #1 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT smoke tests for `nelisp-gc-verify' (Doc 30 v2 §6.10 oracle #1
;; = `verify-heap').  Each oracle sub-check has a positive (PASS) and
;; negative (catches the breach) test; the full walker
;; (`verify-heap') gets a couple of integration tests proving it
;; stops after the cap and keeps its accounting sane.
;;
;; See docs/design/30-phase7.3-gc-inner.org §6.10 for the contract.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-gc-inner)
(require 'nelisp-gc-verify)


;;; Helpers ---------------------------------------------------------

(defun nelisp-gc-verify-test--mk-region (id start end family
                                            &optional generation objects)
  "Build a Doc 29 §1.4 region descriptor with optional :objects."
  (list :region-id id
        :start start
        :end   end
        :generation (or generation :tenured)
        :family family
        :objects (or objects nil)))

(defun nelisp-gc-verify-test--reset ()
  "Reset every piece of state the verifier reads."
  (nelisp-gc-inner-init-mark-state)
  (nelisp-gc-inner--reset-free-lists))


;;; 1. verify-region (single-region shape) --------------------------

(ert-deftest nelisp-gc-verify-region-passes-on-clean-descriptor ()
  "A correctly-shaped Doc 29 §1.4 region passes verify-region."
  (let ((r (nelisp-gc-verify-test--mk-region 1 0 1024 :cons-pool)))
    (should-not (nelisp-gc-verify-region r))))

(ert-deftest nelisp-gc-verify-region-rejects-end-below-start ()
  "verify-region flags :end < :start as fatal."
  (let* ((r (list :region-id 1 :start 1024 :end 512
                  :generation :tenured :family :cons-pool))
         (v (nelisp-gc-verify-region r)))
    (should v)
    (should (eq (plist-get v :severity) 'fatal))
    (should (string-match-p ":end < :start" (plist-get v :reason)))))

(ert-deftest nelisp-gc-verify-region-rejects-unknown-family ()
  "Family tags outside the Doc 29 §2.5 v2 enum are rejected."
  (let* ((r (list :region-id 1 :start 0 :end 1024
                  :generation :tenured :family :bogus-family))
         (v (nelisp-gc-verify-region r)))
    (should v)
    (should (eq (plist-get v :got) :bogus-family))))

(ert-deftest nelisp-gc-verify-region-rejects-unknown-generation ()
  "Generation tags outside (:nursery :tenured) are rejected."
  (let* ((r (list :region-id 1 :start 0 :end 1024
                  :generation :weird :family :cons-pool))
         (v (nelisp-gc-verify-region r)))
    (should v)
    (should (eq (plist-get v :got) :weird))))


;;; 2. verify-region-disjoint (pairwise overlap) --------------------

(ert-deftest nelisp-gc-verify-region-disjoint-passes-on-non-overlapping ()
  "Two adjacent-but-disjoint regions pass."
  (let ((rs (list (nelisp-gc-verify-test--mk-region 1 0 1024 :cons-pool)
                  (nelisp-gc-verify-test--mk-region 2 1024 2048 :cons-pool))))
    (should-not (nelisp-gc-verify-region-disjoint rs))))

(ert-deftest nelisp-gc-verify-region-disjoint-flags-overlap ()
  "Overlapping regions produce a violation naming both region-ids."
  (let* ((rs (list (nelisp-gc-verify-test--mk-region 1 0    1024 :cons-pool)
                   (nelisp-gc-verify-test--mk-region 2 512  2048 :cons-pool)))
         (v (nelisp-gc-verify-region-disjoint rs)))
    (should v)
    (should (eq (plist-get v :severity) 'fatal))
    (should (and (memq (plist-get v :a) '(1 2))
                 (memq (plist-get v :b) '(1 2))))))


;;; 3. verify-region-objects (in-region geometry) -------------------

(ert-deftest nelisp-gc-verify-region-objects-passes-on-clean ()
  "Three non-overlapping in-bounds objects pass."
  (let ((r (nelisp-gc-verify-test--mk-region
            1 0 4096 :cons-pool :tenured
            '((100 . 16) (200 . 32) (1024 . 64)))))
    (should-not (nelisp-gc-verify-region-objects r))))

(ert-deftest nelisp-gc-verify-region-objects-flags-overlap ()
  "Overlapping objects within a region are caught."
  (let* ((r (nelisp-gc-verify-test--mk-region
             1 0 4096 :cons-pool :tenured
             '((100 . 64) (140 . 32))))
         (v (nelisp-gc-verify-region-objects r)))
    (should v)
    (should (eq (plist-get v :severity) 'fatal))
    (should (string-match-p "overlap" (plist-get v :reason)))))

(ert-deftest nelisp-gc-verify-region-objects-flags-out-of-bounds ()
  "Object whose end exceeds region :end is flagged."
  (let* ((r (nelisp-gc-verify-test--mk-region
             1 0 1024 :cons-pool :tenured
             '((900 . 256))))
         (v (nelisp-gc-verify-region-objects r)))
    (should v)
    (should (string-match-p "past region :end" (plist-get v :reason)))))

(ert-deftest nelisp-gc-verify-region-objects-flags-non-positive-size ()
  "Zero-size objects are rejected (non-positive size guard)."
  (let* ((r (nelisp-gc-verify-test--mk-region
             1 0 1024 :cons-pool :tenured
             '((100 . 0))))
         (v (nelisp-gc-verify-region-objects r)))
    (should v)
    (should (string-match-p "non-positive size" (plist-get v :reason)))))


;;; 4. verify-free-list (free-list ↔ region) -----------------------

(ert-deftest nelisp-gc-verify-free-list-passes-on-empty ()
  "An empty free-list passes (nothing to violate)."
  (nelisp-gc-verify-test--reset)
  (let ((rs (list (nelisp-gc-verify-test--mk-region
                   1 0 1024 :cons-pool))))
    (should-not (nelisp-gc-verify-free-list :cons-pool rs))))

(ert-deftest nelisp-gc-verify-free-list-passes-on-in-bounds-blocks ()
  "Free-list entries inside a matching-family region pass."
  (nelisp-gc-verify-test--reset)
  (let ((rs (list (nelisp-gc-verify-test--mk-region
                   1 0 4096 :cons-pool))))
    (puthash :cons-pool '((100 . 16) (200 . 32))
             nelisp-gc-inner--free-lists)
    (should-not (nelisp-gc-verify-free-list :cons-pool rs))))

(ert-deftest nelisp-gc-verify-free-list-flags-out-of-region ()
  "A free-list block whose addr falls outside every region is flagged."
  (nelisp-gc-verify-test--reset)
  (let ((rs (list (nelisp-gc-verify-test--mk-region
                   1 0 1024 :cons-pool))))
    (puthash :cons-pool '((9999 . 16))
             nelisp-gc-inner--free-lists)
    (let ((v (nelisp-gc-verify-free-list :cons-pool rs)))
      (should v)
      (should (string-match-p "not inside" (plist-get v :reason))))))

(ert-deftest nelisp-gc-verify-free-list-flags-overlapping-blocks ()
  "Two free-list blocks whose ranges overlap produce a violation."
  (nelisp-gc-verify-test--reset)
  (let ((rs (list (nelisp-gc-verify-test--mk-region
                   1 0 4096 :cons-pool))))
    (puthash :cons-pool '((100 . 64) (140 . 32))
             nelisp-gc-inner--free-lists)
    (let ((v (nelisp-gc-verify-free-list :cons-pool rs)))
      (should v)
      (should (string-match-p "overlap" (plist-get v :reason))))))

(ert-deftest nelisp-gc-verify-free-list-flags-wrong-family ()
  "A free-list block lands in a region of *different* family → fail."
  (nelisp-gc-verify-test--reset)
  (let ((rs (list (nelisp-gc-verify-test--mk-region
                   1 0 4096 :string-span))))
    (puthash :cons-pool '((100 . 16))
             nelisp-gc-inner--free-lists)
    (let ((v (nelisp-gc-verify-free-list :cons-pool rs)))
      (should v))))


;;; 5. verify-mark-state-clean --------------------------------------

(ert-deftest nelisp-gc-verify-mark-state-clean-passes-on-fresh ()
  "Empty mark state passes the cleanliness oracle."
  (nelisp-gc-verify-test--reset)
  (should-not (nelisp-gc-verify-mark-state-clean)))

(ert-deftest nelisp-gc-verify-mark-state-clean-passes-on-black-only ()
  "All-black mark state passes (black is the post-mark legal colour)."
  (nelisp-gc-verify-test--reset)
  (nelisp-gc-inner-set-color 100 :black)
  (nelisp-gc-inner-set-color 200 :black)
  (should-not (nelisp-gc-verify-mark-state-clean)))

(ert-deftest nelisp-gc-verify-mark-state-clean-flags-grey-leftovers ()
  "A surviving :grey address is reported as a fatal breach."
  (nelisp-gc-verify-test--reset)
  (nelisp-gc-inner-set-color 100 :grey)
  (nelisp-gc-inner-set-color 200 :black)
  (let ((v (nelisp-gc-verify-mark-state-clean)))
    (should v)
    (should (eq (plist-get v :severity) 'fatal))
    (should (memq 100 (plist-get v :grey-addrs)))))


;;; 6. verify-heap (top-level walker) -------------------------------

(ert-deftest nelisp-gc-verify-heap-passes-on-clean-fixture ()
  "Three regions, no objects, no free-list, fresh mark → :passed-p t."
  (nelisp-gc-verify-test--reset)
  (let* ((rs (list (nelisp-gc-verify-test--mk-region 1 0    1024 :cons-pool)
                   (nelisp-gc-verify-test--mk-region 2 1024 2048 :string-span)
                   (nelisp-gc-verify-test--mk-region 3 2048 4096 :vector-span)))
         (result (nelisp-gc-verify-heap rs)))
    (should (eq t (plist-get result :passed-p)))
    (should (null (plist-get result :violations)))
    (should (eq 3 (plist-get result :region-count)))
    (should (eq 1 (plist-get result :oracle-version)))))

(ert-deftest nelisp-gc-verify-heap-detects-region-overlap ()
  "Top-level walker surfaces a region-disjoint violation."
  (nelisp-gc-verify-test--reset)
  (let* ((rs (list (nelisp-gc-verify-test--mk-region 1 0    1024 :cons-pool)
                   (nelisp-gc-verify-test--mk-region 2 512  2048 :cons-pool)))
         (result (nelisp-gc-verify-heap rs)))
    (should-not (plist-get result :passed-p))
    (should (cl-some (lambda (v)
                       (eq (plist-get v :check) 'verify-region-disjoint))
                     (plist-get result :violations)))))

(ert-deftest nelisp-gc-verify-heap-detects-grey-leak ()
  "Top-level walker catches grey leftovers via mark-state-clean check."
  (nelisp-gc-verify-test--reset)
  (nelisp-gc-inner-set-color 100 :grey)
  (let* ((rs (list (nelisp-gc-verify-test--mk-region 1 0 1024 :cons-pool)))
         (result (nelisp-gc-verify-heap rs)))
    (should-not (plist-get result :passed-p))
    (should (cl-some (lambda (v)
                       (eq (plist-get v :check) 'verify-mark-state-clean))
                     (plist-get result :violations)))))

(ert-deftest nelisp-gc-verify-heap-stops-after-cap ()
  "Walker caps violations at 16 even when many regions are malformed."
  (nelisp-gc-verify-test--reset)
  ;; Build 20 regions all with :end < :start — every one violates.
  (let* ((rs (cl-loop for i from 0 below 20
                      collect (list :region-id i
                                    :start (+ 1024 i)
                                    :end   i
                                    :generation :tenured
                                    :family :cons-pool)))
         (result (nelisp-gc-verify-heap rs)))
    (should-not (plist-get result :passed-p))
    (should (<= (length (plist-get result :violations)) 16))))


(provide 'nelisp-gc-verify-test)
;;; nelisp-gc-verify-test.el ends here
