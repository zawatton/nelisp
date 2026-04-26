;;; nelisp-gc-verify-test.el --- ERTs for Phase 7.3 §6.10 oracles -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT smoke tests for `nelisp-gc-verify' (Doc 30 v2 §6.10 oracles
;; #1-#4):
;;
;;   #1 verify-heap              — region/object/free-list shape
;;   #2 verify-card-table        — cross-gen pointer ↔ dirty bit
;;   #3 verify-poison-from-space — Cheney from-space stays untouched
;;   #4 verify-double-scan       — mark-phase determinism across runs
;;
;; Each oracle sub-check has a positive (PASS) and negative (catches
;; the breach) test; the full walker (`verify-heap') gets integration
;; tests proving it stops after the cap and keeps its accounting sane.
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


;;; 7. verify-card-table (oracle #2) -------------------------------

(defun nelisp-gc-verify-test--mk-tenured-with-fields
    (id start end objs-with-fields)
  "Build a tenured region carrying :objects-with-fields."
  (list :region-id id
        :start start
        :end   end
        :generation :tenured
        :family :cons-pool
        :objects-with-fields objs-with-fields))

(defun nelisp-gc-verify-test--mk-nursery-region (id start end)
  "Build a nursery region descriptor."
  (list :region-id id
        :start start
        :end   end
        :generation :nursery
        :family :cons-pool))

(ert-deftest nelisp-gc-verify-card-table-passes-on-empty-tenured ()
  "A tenured region with no walker (vacuously empty) → PASS."
  (let* ((tenured (list :region-id 10 :start 0 :end 8192
                        :generation :tenured :family :cons-pool))
         (nursery (nelisp-gc-verify-test--mk-nursery-region 11 8192 16384))
         (ct      (nelisp-gc-inner-init-card-table tenured)))
    (should-not (nelisp-gc-verify-card-table ct tenured nursery))))

(ert-deftest nelisp-gc-verify-card-table-passes-on-no-cross-gen ()
  "Tenured object whose fields all stay in tenured → PASS."
  (let* ((tenured (nelisp-gc-verify-test--mk-tenured-with-fields
                   10 0 8192
                   '((100 :fields (200 300))   ; both intra-tenured
                     (400 :fields (500)))))
         (nursery (nelisp-gc-verify-test--mk-nursery-region 11 8192 16384))
         (ct      (nelisp-gc-inner-init-card-table tenured)))
    (should-not (nelisp-gc-verify-card-table ct tenured nursery))))

(ert-deftest nelisp-gc-verify-card-table-passes-when-write-barrier-fired ()
  "Cross-gen pointer with a dirty card → PASS."
  (let* ((tenured (nelisp-gc-verify-test--mk-tenured-with-fields
                   10 0 8192
                   '((100 :fields (8200))))) ; points into nursery
         (nursery (nelisp-gc-verify-test--mk-nursery-region 11 8192 16384))
         (ct      (nelisp-gc-inner-init-card-table tenured)))
    ;; Simulate the write barrier having fired for the write to addr 100.
    (nelisp-gc-inner-write-barrier ct 100 8200 nursery)
    (should-not (nelisp-gc-verify-card-table ct tenured nursery))))

(ert-deftest nelisp-gc-verify-card-table-flags-clean-card-with-cross-gen ()
  "Cross-gen pointer on a *clean* card → fatal violation."
  (let* ((tenured (nelisp-gc-verify-test--mk-tenured-with-fields
                   10 0 8192
                   '((100 :fields (8200)))))
         (nursery (nelisp-gc-verify-test--mk-nursery-region 11 8192 16384))
         (ct      (nelisp-gc-inner-init-card-table tenured)))
    ;; Deliberately *do not* fire the write barrier — silent corruption.
    (let ((v (nelisp-gc-verify-card-table ct tenured nursery)))
      (should v)
      (should (eq (plist-get v :severity) 'fatal))
      (should (string-match-p "clean card" (plist-get v :reason)))
      (should (eq (plist-get v :addr) 100))
      (should (eq (plist-get v :field) 8200)))))

(ert-deftest nelisp-gc-verify-card-table-rejects-non-card-table ()
  "Non-card-table arg → fatal violation, no crash."
  (let* ((tenured (nelisp-gc-verify-test--mk-tenured-with-fields
                   10 0 8192 nil))
         (nursery (nelisp-gc-verify-test--mk-nursery-region 11 8192 16384)))
    (let ((v (nelisp-gc-verify-card-table :not-a-table tenured nursery)))
      (should v)
      (should (eq (plist-get v :severity) 'fatal)))))

(ert-deftest nelisp-gc-verify-card-table-honours-objects-with-fields-of ()
  "Callback walker (`:objects-with-fields-of') is consulted when static absent."
  (let* ((nursery (nelisp-gc-verify-test--mk-nursery-region 11 8192 16384))
         (tenured (list :region-id 10 :start 0 :end 8192
                        :generation :tenured :family :cons-pool
                        :objects-with-fields-of
                        (lambda (_r) '((100 :fields (8200))))))
         (ct      (nelisp-gc-inner-init-card-table tenured)))
    ;; Fire barrier so the walker-supplied edge is covered → PASS.
    (nelisp-gc-inner-write-barrier ct 100 8200 nursery)
    (should-not (nelisp-gc-verify-card-table ct tenured nursery))))


;;; 8. verify-poison-from-space (oracle #3) ------------------------

(defun nelisp-gc-verify-test--reset-poison ()
  "Reset poison-from-space side-table for clean test isolation."
  (nelisp-gc-verify-poison-reset))

(ert-deftest nelisp-gc-verify-poison-passes-when-no-touch ()
  "Mark a region as poisoned, never touch it → PASS."
  (nelisp-gc-verify-test--reset-poison)
  (let ((r (nelisp-gc-verify-test--mk-nursery-region 20 0 4096)))
    (nelisp-gc-verify-poison-mark r)
    (should-not (nelisp-gc-verify-poison-from-space))))

(ert-deftest nelisp-gc-verify-poison-passes-on-out-of-range-touch ()
  "Touching an addr OUTSIDE every poisoned region → no-op, still PASS."
  (nelisp-gc-verify-test--reset-poison)
  (let ((r (nelisp-gc-verify-test--mk-nursery-region 20 0 4096)))
    (nelisp-gc-verify-poison-mark r)
    (should-not (nelisp-gc-verify-poison-touch 9999 'unrelated))
    (should-not (nelisp-gc-verify-poison-from-space))))

(ert-deftest nelisp-gc-verify-poison-flags-touch-after-mark ()
  "Touching a poisoned addr → fatal verdict naming the addr."
  (nelisp-gc-verify-test--reset-poison)
  (let ((r (nelisp-gc-verify-test--mk-nursery-region 20 0 4096)))
    (nelisp-gc-verify-poison-mark r)
    (should (nelisp-gc-verify-poison-touch 100 'minor-gc-bug))
    (let ((v (nelisp-gc-verify-poison-from-space)))
      (should v)
      (should (eq (plist-get v :severity) 'fatal))
      (should (eq (plist-get v :region-id) 20))
      (should (= 1 (plist-get v :touch-count)))
      (should (eq nelisp-gc-verify-poison-stamp
                  (plist-get v :stamp))))))

(ert-deftest nelisp-gc-verify-poison-caps-touch-list-at-five ()
  "Many touches → :touches reports up to five entries."
  (nelisp-gc-verify-test--reset-poison)
  (let ((r (nelisp-gc-verify-test--mk-nursery-region 21 0 4096)))
    (nelisp-gc-verify-poison-mark r)
    (dotimes (i 12) (nelisp-gc-verify-poison-touch (* 16 i) 'soak))
    (let ((v (nelisp-gc-verify-poison-from-space)))
      (should v)
      (should (= 12 (plist-get v :touch-count)))
      (should (<= (length (plist-get v :touches)) 5)))))

(ert-deftest nelisp-gc-verify-poison-mark-rejects-malformed-region ()
  "Region without :region-id / :start / :end → wrong-type-argument."
  (nelisp-gc-verify-test--reset-poison)
  (should-error (nelisp-gc-verify-poison-mark '(:start 0))
                :type 'wrong-type-argument))

(ert-deftest nelisp-gc-verify-poison-reset-clears-state ()
  "Reset drops every poisoned-region marker."
  (nelisp-gc-verify-test--reset-poison)
  (nelisp-gc-verify-poison-mark
   (nelisp-gc-verify-test--mk-nursery-region 22 0 4096))
  (nelisp-gc-verify-poison-mark
   (nelisp-gc-verify-test--mk-nursery-region 23 4096 8192))
  (let ((cleared (nelisp-gc-verify-poison-reset)))
    (should (= 2 cleared))
    (should (null nelisp-gc-verify--poison-regions))
    ;; A subsequent touch on the previously-poisoned addr is silent.
    (should-not (nelisp-gc-verify-poison-touch 100 'post-reset))
    (should-not (nelisp-gc-verify-poison-from-space))))


;;; 9. verify-double-scan (oracle #4) ------------------------------

(defun nelisp-gc-verify-test--mk-region-with-children
    (id start end children-alist)
  "Build a region with a `:children-of' lookup table."
  (list :region-id id
        :start start
        :end   end
        :generation :tenured
        :family :cons-pool
        :children-of
        (lambda (addr) (cdr (assq addr children-alist)))))

(ert-deftest nelisp-gc-verify-double-scan-passes-on-empty-roots ()
  "Empty root set → both runs mark zero, identical → PASS."
  (nelisp-gc-verify-test--reset)
  (let ((rs (list (nelisp-gc-verify-test--mk-region 1 0 1024 :cons-pool))))
    (should-not (nelisp-gc-verify-double-scan nil rs))))

(ert-deftest nelisp-gc-verify-double-scan-passes-on-linear-chain ()
  "A → B → C: both runs settle :black on all three addrs identically."
  (nelisp-gc-verify-test--reset)
  (let ((rs (list (nelisp-gc-verify-test--mk-region-with-children
                   1 0 1024 '((100 200) (200 300) (300))))))
    (should-not (nelisp-gc-verify-double-scan '(100) rs))))

(ert-deftest nelisp-gc-verify-double-scan-passes-on-cyclic-graph ()
  "Cycles don't break determinism — visited set guards re-enqueue."
  (nelisp-gc-verify-test--reset)
  (let ((rs (list (nelisp-gc-verify-test--mk-region-with-children
                   1 0 1024
                   '((100 200) (200 300) (300 100))))))
    (should-not (nelisp-gc-verify-double-scan '(100) rs))))

(ert-deftest nelisp-gc-verify-double-scan-passes-on-multi-root ()
  "Two roots, two separate sub-graphs — second pass identical to first."
  (nelisp-gc-verify-test--reset)
  (let ((rs (list (nelisp-gc-verify-test--mk-region-with-children
                   1 0 2048
                   '((100 200) (200) (1000 1100) (1100))))))
    (should-not (nelisp-gc-verify-double-scan '(100 1000) rs))))

(ert-deftest nelisp-gc-verify-double-scan-flags-non-deterministic-walker ()
  "Walker that returns *different* children on each call → divergence."
  (nelisp-gc-verify-test--reset)
  (let* ((toggle (cons 0 nil))      ; mutable counter inside cons cell
         (rs (list (list :region-id 1 :start 0 :end 1024
                         :generation :tenured :family :cons-pool
                         :children-of
                         (lambda (_addr)
                           (cl-incf (car toggle))
                           ;; Run 1: returns (200 300); run 2: (200 400)
                           (if (eq (car toggle) 1) '(200 300) '(200 400))))))
         (v (nelisp-gc-verify-double-scan '(100) rs)))
    (should v)
    (should (eq (plist-get v :severity) 'fatal))))

(ert-deftest nelisp-gc-verify-double-scan-reports-marked-counts ()
  "On divergence the violation includes both runs' marked-counts."
  (nelisp-gc-verify-test--reset)
  ;; Walker that on run 1 yields no child, on run 2 yields one.
  (let* ((calls (cons 0 nil))
         (rs (list (list :region-id 1 :start 0 :end 1024
                         :generation :tenured :family :cons-pool
                         :children-of
                         (lambda (_addr)
                           (cl-incf (car calls))
                           (if (= (car calls) 1) nil '(200))))))
         (v (nelisp-gc-verify-double-scan '(100) rs)))
    (should v)
    (should (integerp (plist-get v :first-marked-count)))
    (should (integerp (plist-get v :second-marked-count)))))


(provide 'nelisp-gc-verify-test)
;;; nelisp-gc-verify-test.el ends here
