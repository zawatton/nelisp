;;; nl-clj-atom-test.el --- Tests for nl-clj-atom.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 §4.5's own DoD-shaped verification: no host-Emacs control
;; exists (Emacs has no `atom' constructor), so every assertion below
;; is checked against Clojure's own documented reference contract by
;; hand, not by differential comparison.

;;; Code:

(require 'ert)
(require 'nl-clj-atom)

(ert-deftest nl-clj-atom-test-construct-and-deref ()
  (let ((a (nl-clj-atom 42)))
    (should (nl-clj-atom-p a))
    (should (= (nl-clj-deref a) 42))))

(ert-deftest nl-clj-atom-test-predicate-rejects-non-atoms ()
  (should-not (nl-clj-atom-p 42))
  (should-not (nl-clj-atom-p [1 2 3]))
  (should-not (nl-clj-atom-p nil)))

(ert-deftest nl-clj-atom-test-deref-signals-on-non-atom ()
  (should-error (nl-clj-deref 42) :type 'nl-clj-type-error))

(ert-deftest nl-clj-atom-test-swap-applies-function ()
  (let ((a (nl-clj-atom 10)))
    (should (= (nl-clj-swap! a #'1+) 11))
    (should (= (nl-clj-deref a) 11))))

(ert-deftest nl-clj-atom-test-swap-with-extra-args ()
  (let ((a (nl-clj-atom 10)))
    (nl-clj-swap! a #'+ 5 5)
    (should (= (nl-clj-deref a) 20))))

(ert-deftest nl-clj-atom-test-swap-calls-f-exactly-once ()
  ;; NeLisp's single-thread execution model (Doc 195 §2.7/§4.5) means
  ;; `swap!' never retries -- F runs exactly once, a strictly STRONGER
  ;; guarantee than Clojure's own "F may run more than once" contract.
  ;; A future caller relying on this must not be silently broken by a
  ;; future concurrency-safe rewrite that reintroduces retries.
  (let ((calls 0)
        (a (nl-clj-atom 0)))
    (nl-clj-swap! a (lambda (v) (setq calls (1+ calls)) (1+ v)))
    (should (= calls 1))))

(ert-deftest nl-clj-atom-test-reset-discards-previous-value ()
  (let ((a (nl-clj-atom 10)))
    (should (= (nl-clj-reset! a 999) 999))
    (should (= (nl-clj-deref a) 999))))

(ert-deftest nl-clj-atom-test-compare-and-set-succeeds-on-match ()
  (let ((a (nl-clj-atom 10)))
    (should (nl-clj-compare-and-set! a 10 20))
    (should (= (nl-clj-deref a) 20))))

(ert-deftest nl-clj-atom-test-compare-and-set-fails-on-mismatch ()
  (let ((a (nl-clj-atom 10)))
    (should-not (nl-clj-compare-and-set! a 999 20))
    ;; value must be UNCHANGED on a failed compare-and-set
    (should (= (nl-clj-deref a) 10))))

(ert-deftest nl-clj-atom-test-compare-and-set-uses-eq-not-equal ()
  ;; Two `equal' but not `eq' values must NOT satisfy the compare.
  (let ((a (nl-clj-atom (list 1 2 3))))
    (should-not (nl-clj-compare-and-set! a (list 1 2 3) :new))
    (should (equal (nl-clj-deref a) (list 1 2 3)))))

(ert-deftest nl-clj-atom-test-independent-atoms-do-not-alias ()
  (let ((a (nl-clj-atom 1)) (b (nl-clj-atom 1)))
    (nl-clj-swap! a #'1+)
    (should (= (nl-clj-deref a) 2))
    (should (= (nl-clj-deref b) 1))))

(ert-deftest nl-clj-atom-test-swap-signals-on-non-atom ()
  (should-error (nl-clj-swap! 42 #'1+) :type 'nl-clj-type-error))

(ert-deftest nl-clj-atom-test-reset-signals-on-non-atom ()
  (should-error (nl-clj-reset! 42 1) :type 'nl-clj-type-error))

(ert-deftest nl-clj-atom-test-compare-and-set-signals-on-non-atom ()
  (should-error (nl-clj-compare-and-set! 42 1 2) :type 'nl-clj-type-error))

;;; nl-clj-atom-test.el ends here
