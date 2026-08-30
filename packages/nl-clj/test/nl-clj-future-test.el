;;; nl-clj-future-test.el --- ERT for nl-clj-future -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 199 Tier 1.  Correctness of the cooperative future/deref/pcalls/pmap,
;; including the against-the-bug controls: pmap collects in COLL order, and
;; every thunk actually runs (none dropped).  Zero speedup is not tested --
;; it is the honest, documented ceiling, not a defect.

;;; Code:

(require 'ert)
(require 'nl-clj-core)
(require 'nl-clj-atom)
(require 'nl-clj-vector)
(require 'nl-clj-future)

(ert-deftest nl-clj-future-basic-deref ()
  "A future's thunk value is returned by `nl-clj-future-await'."
  (let ((f (nl-clj-future (lambda () (+ 40 2)))))
    (should (nl-clj-future-p f))
    (should (equal (nl-clj-future-await f) 42))))

(ert-deftest nl-clj-future-done-p-transitions ()
  "done-p is nil before realisation and t after."
  (let ((f (nl-clj-future (lambda () 'ready))))
    (should-not (nl-clj-future-done-p f))
    (should (eq (nl-clj-future-await f) 'ready))
    (should (nl-clj-future-done-p f))))

(ert-deftest nl-clj-future-await-is-idempotent ()
  "Awaiting an already-realised future returns the same value again."
  (let ((f (nl-clj-future (lambda () (list 1 2 3)))))
    (should (equal (nl-clj-future-await f) '(1 2 3)))
    (should (equal (nl-clj-future-await f) '(1 2 3)))))

(ert-deftest nl-clj-pcalls-argument-order ()
  "`nl-clj-pcalls' returns values in argument order."
  (should (equal (nl-clj-pcalls (lambda () 'a) (lambda () 'b) (lambda () 'c))
                 '(a b c))))

(ert-deftest nl-clj-pcalls-empty ()
  "`nl-clj-pcalls' with no thunks returns the empty list."
  (should (equal (nl-clj-pcalls) '())))

(ert-deftest nl-clj-pmap-maps-in-order ()
  "`nl-clj-pmap' applies F elementwise and returns results in COLL order."
  (should (equal (nl-clj-pmap #'1+ '(1 2 3 4 5)) '(2 3 4 5 6))))

(ert-deftest nl-clj-pmap-order-against-the-bug ()
  "Distinct inputs map to distinct outputs positionally: a collector that
lost future ordering would misalign these."
  (should (equal (nl-clj-pmap (lambda (x) (* x x)) '(1 2 3 4 5 6 7 8 9 10))
                 '(1 4 9 16 25 36 49 64 81 100))))

(ert-deftest nl-clj-pmap-runs-every-thunk-against-the-bug ()
  "Every element's thunk runs exactly once (none dropped, none doubled).
Uses an nl-clj atom + swap! -- the §7-disciplined shared-state path."
  (let* ((n 500)
         (acc (nl-clj-atom 0))
         (coll (number-sequence 1 n)))
    (nl-clj-pmap (lambda (_x) (nl-clj-swap! acc #'1+)) coll)
    (should (equal (nl-clj-deref acc) n))))

(ert-deftest nl-clj-pmap-empty ()
  "`nl-clj-pmap' over an empty collection returns the empty list."
  (should (equal (nl-clj-pmap #'1+ '()) '())))

(ert-deftest nl-clj-pmap-over-nl-clj-vector ()
  "`nl-clj-pmap' accepts an nl-clj immutable vector as COLL (§7 discipline)."
  (should (equal (nl-clj-pmap #'1+ (nl-clj-vector 10 20 30))
                 '(11 21 31))))

(ert-deftest nl-clj-deref-polymorphic-on-future ()
  "`nl-clj-deref' realises a future (Clojure `@')."
  (should (equal (nl-clj-deref (nl-clj-future (lambda () 99))) 99)))

(ert-deftest nl-clj-deref-still-works-on-atom ()
  "Regression: `nl-clj-deref' on an atom returns its value, unchanged."
  (should (equal (nl-clj-deref (nl-clj-atom 'held)) 'held)))

(ert-deftest nl-clj-deref-rejects-non-derefable ()
  "Regression: `nl-clj-deref' on a plain object still signals a type error."
  (should-error (nl-clj-deref 42) :type 'nl-clj-type-error))

(ert-deftest nl-clj-future-p-discriminates ()
  "`nl-clj-future-p' is true only for future handles."
  (should (nl-clj-future-p (nl-clj-future (lambda () 1))))
  (should-not (nl-clj-future-p (nl-clj-atom 1)))
  (should-not (nl-clj-future-p 'symbol))
  (should-not (nl-clj-future-p [1 2 3])))

(ert-deftest nl-clj-future-nested ()
  "A future thunk may itself await another future (cooperative nesting)."
  (let* ((inner (nl-clj-future (lambda () 7)))
         (outer (nl-clj-future (lambda () (* 6 (nl-clj-future-await inner))))))
    (should (equal (nl-clj-future-await outer) 42))))

(provide 'nl-clj-future-test)

;;; nl-clj-future-test.el ends here
