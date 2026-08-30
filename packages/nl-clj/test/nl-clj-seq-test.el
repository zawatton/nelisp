;;; nl-clj-seq-test.el --- Tests for nl-clj-seq.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The generic dispatch layer over vector/map/set/list.  Doc 195's own
;; DoD-shaped verification: no host-Emacs control (plain Emacs has
;; none of these types), checked against Clojure's own documented seq/
;; collection contract by hand.

;;; Code:

(require 'ert)
(require 'nl-clj-vector)
(require 'nl-clj-hash)
(require 'nl-clj-seq)

;;;; seq / first / rest / next / cons, across all seqable shapes ----------

(ert-deftest nl-clj-seq-test-seq-nil-is-nil ()
  (should (null (nl-clj-seq nil))))

(ert-deftest nl-clj-seq-test-seq-over-vector ()
  (should (equal (nl-clj-seq (nl-clj-vector 1 2 3)) '(1 2 3))))

(ert-deftest nl-clj-seq-test-seq-over-empty-vector ()
  (should (null (nl-clj-seq (nl-clj-vector)))))

(ert-deftest nl-clj-seq-test-seq-over-map-is-entry-conses ()
  (let ((entries (nl-clj-seq (nl-clj-hash-map :a 1))))
    (should (equal entries (list (cons :a 1))))))

(ert-deftest nl-clj-seq-test-seq-over-set ()
  (should (equal (sort (nl-clj-seq (nl-clj-hash-set 1 2 3)) #'<) '(1 2 3))))

(ert-deftest nl-clj-seq-test-seq-over-plain-list ()
  (should (equal (nl-clj-seq '(1 2 3)) '(1 2 3))))

(ert-deftest nl-clj-seq-test-seq-over-plain-vector-bonus ()
  (should (equal (nl-clj-seq [1 2 3]) '(1 2 3))))

(ert-deftest nl-clj-seq-test-seq-signals-on-atom ()
  (should-error (nl-clj-seq (nl-clj-atom 1)) :type 'nl-clj-type-error))

(ert-deftest nl-clj-seq-test-first-rest-next ()
  (let ((v (nl-clj-vector 1 2 3)))
    (should (= (nl-clj-first v) 1))
    (should (equal (nl-clj-rest v) '(2 3)))
    (should (equal (nl-clj-next v) '(2 3)))))

(ert-deftest nl-clj-seq-test-first-rest-of-empty ()
  (should (null (nl-clj-first (nl-clj-vector))))
  (should (null (nl-clj-rest (nl-clj-vector))))
  (should (null (nl-clj-next (nl-clj-vector)))))

(ert-deftest nl-clj-seq-test-first-rest-of-single-element ()
  (should (= (nl-clj-first (nl-clj-vector 1)) 1))
  (should (null (nl-clj-rest (nl-clj-vector 1))))
  (should (null (nl-clj-next (nl-clj-vector 1)))))

(ert-deftest nl-clj-seq-test-cons-prepends ()
  (should (equal (nl-clj-cons 0 (nl-clj-vector 1 2 3)) '(0 1 2 3)))
  (should (equal (nl-clj-cons 1 nil) '(1))))

;;;; count ------------------------------------------------------------------

(ert-deftest nl-clj-seq-test-count-across-types ()
  (should (= (nl-clj-count nil) 0))
  (should (= (nl-clj-count (nl-clj-vector 1 2 3)) 3))
  (should (= (nl-clj-count (nl-clj-hash-map :a 1 :b 2)) 2))
  (should (= (nl-clj-count (nl-clj-hash-set 1 2 3)) 3))
  (should (= (nl-clj-count '(1 2 3 4)) 4))
  (should (= (nl-clj-count [1 2]) 2)))

(ert-deftest nl-clj-seq-test-count-signals-on-non-collection ()
  (should-error (nl-clj-count 42) :type 'nl-clj-type-error))

;;;; get / contains? / nth ---------------------------------------------

(ert-deftest nl-clj-seq-test-get-vector-in-range-and-out-of-range ()
  (let ((v (nl-clj-vector 10 20 30)))
    (should (= (nl-clj-get v 1) 20))
    (should (null (nl-clj-get v 5)))
    (should (eq (nl-clj-get v 5 :default) :default))))

(ert-deftest nl-clj-seq-test-get-map ()
  (let ((m (nl-clj-hash-map :a 1)))
    (should (= (nl-clj-get m :a) 1))
    (should (null (nl-clj-get m :missing)))
    (should (eq (nl-clj-get m :missing :d) :d))))

(ert-deftest nl-clj-seq-test-get-set-returns-element-itself ()
  (let ((s (nl-clj-hash-set 1 2 3)))
    (should (= (nl-clj-get s 2) 2))
    (should (null (nl-clj-get s 99)))))

(ert-deftest nl-clj-seq-test-get-nil-collection ()
  (should (null (nl-clj-get nil :a)))
  (should (eq (nl-clj-get nil :a :d) :d)))

(ert-deftest nl-clj-seq-test-contains-distinguishes-nil-value-from-absent ()
  (let ((m (nl-clj-hash-map :a nil)))
    (should (nl-clj-contains? m :a))
    (should-not (nl-clj-contains? m :b))
    ;; get can't tell these apart without a default, contains? can
    (should (null (nl-clj-get m :a)))
    (should (null (nl-clj-get m :b)))))

(ert-deftest nl-clj-seq-test-contains-vector-is-index-range ()
  (let ((v (nl-clj-vector 1 2 3)))
    (should (nl-clj-contains? v 0))
    (should (nl-clj-contains? v 2))
    (should-not (nl-clj-contains? v 3))
    (should-not (nl-clj-contains? v -1))))

(ert-deftest nl-clj-seq-test-nth-in-range ()
  (should (= (nl-clj-nth (nl-clj-vector 10 20 30) 2) 30))
  (should (= (nl-clj-nth '(1 2 3) 1) 2)))

(ert-deftest nl-clj-seq-test-nth-out-of-range-signals ()
  (should-error (nl-clj-nth (nl-clj-vector 1 2) 5) :type 'nl-clj-index-error)
  (should-error (nl-clj-nth '(1 2) 5) :type 'nl-clj-index-error))

(ert-deftest nl-clj-seq-test-nth-out-of-range-with-default ()
  (should (eq (nl-clj-nth (nl-clj-vector 1 2) 5 :d) :d))
  (should (eq (nl-clj-nth '(1 2) 5 :d) :d)))

;;;; conj polymorphism --------------------------------------------------

(ert-deftest nl-clj-seq-test-conj-vector-appends ()
  (should (equal (nl-clj-seq (nl-clj-conj (nl-clj-vector 1 2) 3)) '(1 2 3))))

(ert-deftest nl-clj-seq-test-conj-list-prepends ()
  (should (equal (nl-clj-conj '(2 3) 1) '(1 2 3)))
  (should (equal (nl-clj-conj nil 1) '(1))))

(ert-deftest nl-clj-seq-test-conj-map-entry-cons ()
  (let ((m (nl-clj-conj (nl-clj-hash-map :a 1) (cons :b 2))))
    (should (= (nl-clj-get m :b) 2))))

(ert-deftest nl-clj-seq-test-conj-map-entry-vector ()
  (let ((m (nl-clj-conj (nl-clj-hash-map :a 1) (vector :b 2))))
    (should (= (nl-clj-get m :b) 2))))

(ert-deftest nl-clj-seq-test-conj-map-entry-bad-shape-signals ()
  (should-error (nl-clj-conj (nl-clj-hash-map) 42) :type 'nl-clj-error))

(ert-deftest nl-clj-seq-test-conj-set-adds ()
  (let ((s (nl-clj-conj (nl-clj-hash-set 1 2) 3)))
    (should (nl-clj-contains? s 3))))

(ert-deftest nl-clj-seq-test-conj-variadic ()
  (should (equal (nl-clj-seq (nl-clj-conj (nl-clj-vector) 1 2 3)) '(1 2 3))))

;;;; assoc / dissoc / disj ------------------------------------------------

(ert-deftest nl-clj-seq-test-assoc-vector ()
  (let ((v2 (nl-clj-assoc (nl-clj-vector 1 2 3) 1 :x)))
    (should (eq (nl-clj-get v2 1) :x))))

(ert-deftest nl-clj-seq-test-assoc-map ()
  (let ((m2 (nl-clj-assoc (nl-clj-hash-map) :a 1)))
    (should (= (nl-clj-get m2 :a) 1))))

(ert-deftest nl-clj-seq-test-assoc-variadic-pairs ()
  (let ((m (nl-clj-assoc (nl-clj-hash-map) :a 1 :b 2 :c 3)))
    (should (= (nl-clj-get m :a) 1))
    (should (= (nl-clj-get m :b) 2))
    (should (= (nl-clj-get m :c) 3))))

(ert-deftest nl-clj-seq-test-assoc-signals-on-list ()
  (should-error (nl-clj-assoc '(1 2 3) 0 :x) :type 'nl-clj-type-error))

(ert-deftest nl-clj-seq-test-dissoc-basic-and-variadic ()
  (let ((m (nl-clj-hash-map :a 1 :b 2 :c 3)))
    (let ((m2 (nl-clj-dissoc m :a :b)))
      (should-not (nl-clj-contains? m2 :a))
      (should-not (nl-clj-contains? m2 :b))
      (should (nl-clj-contains? m2 :c)))))

(ert-deftest nl-clj-seq-test-dissoc-absent-key-eq-identical ()
  (let ((m (nl-clj-hash-map :a 1)))
    (should (eq (nl-clj-dissoc m :nonexistent) m))))

(ert-deftest nl-clj-seq-test-dissoc-signals-on-non-map ()
  (should-error (nl-clj-dissoc (nl-clj-vector 1 2) 0) :type 'nl-clj-type-error))

(ert-deftest nl-clj-seq-test-disj-basic-and-variadic ()
  (let* ((s (nl-clj-hash-set 1 2 3))
         (s2 (nl-clj-disj s 1 2)))
    (should-not (nl-clj-contains? s2 1))
    (should-not (nl-clj-contains? s2 2))
    (should (nl-clj-contains? s2 3))))

(ert-deftest nl-clj-seq-test-disj-absent-eq-identical ()
  (let ((s (nl-clj-hash-set 1 2 3)))
    (should (eq (nl-clj-disj s 999) s))))

(ert-deftest nl-clj-seq-test-disj-signals-on-non-set ()
  (should-error (nl-clj-disj (nl-clj-vector 1) 1) :type 'nl-clj-type-error))

;;;; peek / pop (polymorphic: vector=last, list=first) ---------------------

(ert-deftest nl-clj-seq-test-peek-vector-is-last ()
  (should (= (nl-clj-peek (nl-clj-vector 1 2 3)) 3))
  (should (null (nl-clj-peek (nl-clj-vector)))))

(ert-deftest nl-clj-seq-test-peek-list-is-first ()
  (should (= (nl-clj-peek '(1 2 3)) 1))
  (should (null (nl-clj-peek nil))))

(ert-deftest nl-clj-seq-test-pop-vector-removes-last ()
  (let ((v2 (nl-clj-pop (nl-clj-vector 1 2 3))))
    (should (equal (nl-clj-seq v2) '(1 2)))))

(ert-deftest nl-clj-seq-test-pop-list-removes-first ()
  (should (equal (nl-clj-pop '(1 2 3)) '(2 3))))

(ert-deftest nl-clj-seq-test-pop-nil-signals ()
  (should-error (nl-clj-pop nil) :type 'nl-clj-index-error))

;;;; into / vec / subvec ----------------------------------------------------

(ert-deftest nl-clj-seq-test-into-vector-from-list ()
  (should (equal (nl-clj-seq (nl-clj-into (nl-clj-vector) '(1 2 3))) '(1 2 3))))

(ert-deftest nl-clj-seq-test-into-map-from-alist ()
  (let ((m (nl-clj-into (nl-clj-hash-map) (list (cons :a 1) (cons :b 2)))))
    (should (= (nl-clj-get m :a) 1))
    (should (= (nl-clj-get m :b) 2))))

(ert-deftest nl-clj-seq-test-into-set-from-vector ()
  (let ((s (nl-clj-into (nl-clj-hash-set) (nl-clj-vector 1 2 2 3))))
    (should (= (nl-clj-count s) 3))))

(ert-deftest nl-clj-seq-test-vec-from-list ()
  (let ((v (nl-clj-vec '(1 2 3 4 5))))
    (should (nl-clj-vector-p v))
    (should (= (nl-clj-count v) 5))))

(ert-deftest nl-clj-seq-test-vec-idempotent-on-vector ()
  (let ((v (nl-clj-vector 1 2 3)))
    (should (eq (nl-clj-vec v) v))))

(ert-deftest nl-clj-seq-test-subvec ()
  (let ((sv (nl-clj-subvec (nl-clj-vec (number-sequence 0 9)) 2 5)))
    (should (equal (nl-clj-seq sv) '(2 3 4)))))

(ert-deftest nl-clj-seq-test-subvec-default-end ()
  (let ((sv (nl-clj-subvec (nl-clj-vec (number-sequence 0 4)) 2)))
    (should (equal (nl-clj-seq sv) '(2 3 4)))))

(ert-deftest nl-clj-seq-test-subvec-out-of-range-signals ()
  (should-error (nl-clj-subvec (nl-clj-vector 1 2 3) 1 5) :type 'nl-clj-index-error))

(ert-deftest nl-clj-seq-test-subvec-signals-on-non-vector ()
  (should-error (nl-clj-subvec '(1 2 3) 0 1) :type 'nl-clj-type-error))

;;;; map / filter / reduce (eager) ------------------------------------------

(ert-deftest nl-clj-seq-test-map-function ()
  (should (equal (nl-clj-map #'1+ (nl-clj-vector 1 2 3)) '(2 3 4))))

(ert-deftest nl-clj-seq-test-filter-function ()
  (should (equal (nl-clj-filter (lambda (x) (= 0 (mod x 2))) (nl-clj-vector 1 2 3 4 5 6))
                 '(2 4 6))))

(ert-deftest nl-clj-seq-test-reduce-function ()
  (should (= (nl-clj-reduce #'+ 0 (nl-clj-vector 1 2 3 4 5)) 15)))

(ert-deftest nl-clj-seq-test-reduce-over-empty ()
  (should (= (nl-clj-reduce #'+ 0 (nl-clj-vector)) 0)))

(ert-deftest nl-clj-seq-test-reduce-large-seq-does-not-overflow-stack ()
  ;; against-the-bug: this MUST use a plain loop, not recursion -- try
  ;; the naive recursive version first and it stack-overflows well
  ;; under this size (this repo's own "red on the defect, then green"
  ;; discipline, Doc 195 §4.7).
  (let ((v (nl-clj-vector)))
    (dotimes (i 5000) (setq v (nl-clj-vector--conj v i)))
    (should (= (nl-clj-reduce #'+ 0 v) (/ (* 4999 5000) 2)))))

;;;; equal / hash -------------------------------------------------------

(ert-deftest nl-clj-seq-test-equal-vectors-different-build-order ()
  (let ((a (nl-clj-vec (number-sequence 0 999)))
        (b (let ((vv (nl-clj-vector)))
             (dolist (i (number-sequence 0 999)) (setq vv (nl-clj-conj vv i)))
             vv)))
    (should (nl-clj-equal a b))
    ;; and yet a real `equal' need not agree on their raw representation
    ;; identity-wise this is not asserted (implementation detail); the
    ;; point is nl-clj-equal must be TRUE regardless.
    ))

(ert-deftest nl-clj-seq-test-equal-vectors-different-content ()
  (should-not (nl-clj-equal (nl-clj-vector 1 2 3) (nl-clj-vector 1 2 4))))

(ert-deftest nl-clj-seq-test-equal-map-order-independent ()
  (should (nl-clj-equal (nl-clj-hash-map :a 1 :b 2) (nl-clj-hash-map :b 2 :a 1))))

(ert-deftest nl-clj-seq-test-equal-map-different-values ()
  (should-not (nl-clj-equal (nl-clj-hash-map :a 1) (nl-clj-hash-map :a 2))))

(ert-deftest nl-clj-seq-test-equal-set-order-independent ()
  (should (nl-clj-equal (nl-clj-hash-set 1 2 3) (nl-clj-hash-set 3 2 1))))

(ert-deftest nl-clj-seq-test-equal-different-kinds-not-equal ()
  (should-not (nl-clj-equal (nl-clj-vector 1 2 3) (nl-clj-hash-set 1 2 3))))

(ert-deftest nl-clj-seq-test-equal-plain-values ()
  (should (nl-clj-equal 1 1))
  (should (nl-clj-equal "a" "a"))
  (should-not (nl-clj-equal 1 2)))

(ert-deftest nl-clj-seq-test-hash-consistent-with-equal ()
  (let ((a (nl-clj-vec (number-sequence 0 99)))
        (b (let ((vv (nl-clj-vector)))
             (dolist (i (number-sequence 0 99)) (setq vv (nl-clj-conj vv i)))
             vv)))
    (should (nl-clj-equal a b))
    (should (= (nl-clj-hash a) (nl-clj-hash b)))))

(ert-deftest nl-clj-seq-test-hash-map-order-independent ()
  (should (= (nl-clj-hash (nl-clj-hash-map :a 1 :b 2))
             (nl-clj-hash (nl-clj-hash-map :b 2 :a 1)))))

;;; nl-clj-seq-test.el ends here
