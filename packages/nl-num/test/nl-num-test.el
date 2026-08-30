;;; nl-num-test.el --- Reference-contract tests for nl-num -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Emacs has no rational or complex type, so host Emacs is not the semantic
;; oracle for these values.  The exact-division, normalization, contagion,
;; complex-canonicalization, and comparison assertions below encode the
;; documented Common Lisp (CLHS 12.1.5.2-3), Scheme numeric-tower, and
;; Clojure ratio contracts named by Doc 196.  The same bodies run against
;; target/nelisp through nl-num-standalone-smoke.el.

;;; Code:

(require 'ert)

;; Host ERT reaches this file before nl-num is required.  The standalone
;; runner binds these before loading nl-num, so both paths prove opt-in load
;; leaves the stock arithmetic function objects untouched.
(defvar nl-num-test--stock-plus (symbol-function '+))
(defvar nl-num-test--stock-minus (symbol-function '-))
(defvar nl-num-test--stock-times (symbol-function '*))
(defvar nl-num-test--stock-divide (symbol-function '/))
(defvar nl-num-test--stock-less (symbol-function '<))
(defvar nl-num-test--stock-equal (symbol-function '=))

(require 'nl-num)

(defconst nl-num-test--big 1267650600228229401496703205376
  "2^100, guaranteed to be a tag-13 bignum on standalone NeLisp.")

(defun nl-num-test--check-divmod (numerator denominator)
  "Assert the Euclidean contract for NUMERATOR and DENOMINATOR."
  (let* ((result (nl-num--divmod numerator denominator))
         (quotient (car result))
         (remainder (cdr result))
         (positive-denominator
          (if (< denominator 0) (- 0 denominator) denominator)))
    (should (= (+ (* quotient denominator) remainder) numerator))
    (should (>= remainder 0))
    (should (< remainder positive-denominator))))

;;;; Opt-in boundary ----------------------------------------------------

(ert-deftest nl-num-test-opt-in-does-not-redefine-stock-arithmetic ()
  (should (eq nl-num-test--stock-plus (symbol-function '+)))
  (should (eq nl-num-test--stock-minus (symbol-function '-)))
  (should (eq nl-num-test--stock-times (symbol-function '*)))
  (should (eq nl-num-test--stock-divide (symbol-function '/)))
  (should (eq nl-num-test--stock-less (symbol-function '<)))
  (should (eq nl-num-test--stock-equal (symbol-function '=))))

;;;; Phase 0: doubling divmod and gcd ----------------------------------

(ert-deftest nl-num-test-divmod-fixnum-cases ()
  (should (equal (nl-num--divmod 17 5) '(3 . 2)))
  (should (equal (nl-num--divmod 4 9) '(0 . 4)))
  (should (equal (nl-num--divmod 20 5) '(4 . 0))))

(ert-deftest nl-num-test-divmod-negative-cases-are-euclidean ()
  (should (equal (nl-num--divmod -17 5) '(-4 . 3)))
  (should (equal (nl-num--divmod 17 -5) '(-3 . 2)))
  (should (equal (nl-num--divmod -17 -5) '(4 . 3)))
  (nl-num-test--check-divmod -17 5)
  (nl-num-test--check-divmod 17 -5)
  (nl-num-test--check-divmod -17 -5))

(ert-deftest nl-num-test-divmod-bignum-cases ()
  (nl-num-test--check-divmod (+ nl-num-test--big 12345) 97)
  (nl-num-test--check-divmod (* nl-num-test--big 7)
                                (+ nl-num-test--big 19))
  (nl-num-test--check-divmod (- 0 (+ nl-num-test--big 1)) 65537))

(ert-deftest nl-num-test-divmod-zero-denominator-signals ()
  (should-error (nl-num--divmod 1 0)
                :type 'nl-num-division-by-zero-error))

(ert-deftest nl-num-test-divmod-rejects-non-integers ()
  (should-error (nl-num--divmod 1.0 2) :type 'nl-num-error)
  (should-error (nl-num--divmod 1 2.0) :type 'nl-num-error))

(ert-deftest nl-num-test-gcd-fixnum-and-signs ()
  (should (= (nl-num--gcd 54 24) 6))
  (should (= (nl-num--gcd -54 24) 6))
  (should (= (nl-num--gcd 0 24) 24))
  (should (= (nl-num--gcd 0 0) 0)))

(ert-deftest nl-num-test-gcd-bignum ()
  (should (= (nl-num--gcd (* nl-num-test--big 18)
                          (* nl-num-test--big 24))
             (* nl-num-test--big 6))))

(ert-deftest nl-num-test-bignum-to-float-is-correctly-rounded ()
  ;; Against the sign-word bug: native standalone (+ BIG 0.0) used to
  ;; produce 0.0 for this positive bignum, and decimal digit accumulation
  ;; alone lands one ULP low.  The bridge must match IEEE binary64 rounding.
  (should (= (nl-num--integer-to-float nl-num-test--big)
             1.2676506002282294e+30))
  (should (= (nl-num--integer-to-float (- 0 nl-num-test--big))
             -1.2676506002282294e+30))
  ;; Binary64 boundary and round-to-nearest/ties-to-even cases.
  (should (= (nl-num--integer-to-float 9007199254740991)
             9007199254740991.0))
  (should (= (nl-num--integer-to-float 9007199254740992)
             9007199254740992.0))
  (should (= (nl-num--integer-to-float 9007199254740993)
             9007199254740992.0))
  (should (= (nl-num--integer-to-float 9007199254740995)
             9007199254740996.0)))

;;;; Phase 1: rational canonical form ----------------------------------

(ert-deftest nl-num-test-rational-reduces-and-keeps-positive-denominator ()
  (let ((ratio (nl-rational 6 -8)))
    (should (nl-rationalp ratio))
    (should (nl-realp ratio))
    (should (nl-realp 7))
    (should (nl-realp 7.0))
    (should-not (nl-realp (nl-complex 1 1)))
    (should (= (nl-numerator ratio) -3))
    (should (= (nl-denominator ratio) 4))
    (should (= (nl-num--gcd (nl-num--integer-abs (nl-numerator ratio))
                            (nl-denominator ratio))
               1))))

(ert-deftest nl-num-test-rational-bignum-normalization ()
  (let ((ratio (nl-rational (* nl-num-test--big 6)
                            (* nl-num-test--big 8))))
    (should (equal ratio [nl--rational 3 4]))))

(ert-deftest nl-num-test-rational-denominator-one-demotes ()
  (let ((answer (nl-rational 12 6)))
    (should (= answer 2))
    (should (integerp answer))
    (should-not (nl-rationalp answer))))

(ert-deftest nl-num-test-rational-zero-demotes ()
  (should (= (nl-rational 0 nl-num-test--big) 0)))

(ert-deftest nl-num-test-rational-accessors-accept-integers ()
  (should (= (nl-numerator -7) -7))
  (should (= (nl-denominator -7) 1)))

(ert-deftest nl-num-test-rational-round-trip-through-accessors ()
  (let ((ratio (nl-rational -42 55)))
    (should (equal (nl-rational (nl-numerator ratio)
                                (nl-denominator ratio))
                   ratio))))

(ert-deftest nl-num-test-rational-zero-denominator-signals ()
  (should-error (nl-rational 1 0)
                :type 'nl-num-division-by-zero-error))

(ert-deftest nl-num-test-rational-rejects-non-integer-parts ()
  (should-error (nl-rational 1.0 2) :type 'nl-num-error)
  (should-error (nl-rational 1 2.0) :type 'nl-num-error))

(ert-deftest nl-num-test-rational-cosmetic-printing ()
  (should (equal (nl-num-pr-str 3) "3"))
  (should (equal (nl-num-pr-str 0.5) "0.5"))
  (should (equal (nl-num-pr-str (nl-rational 6 8)) "3/4"))
  (should (equal (nl-num-pr-str (nl-rational nl-num-test--big 3))
                 "1267650600228229401496703205376/3")))

;;;; Phase 2: arithmetic and contagion ---------------------------------

(ert-deftest nl-num-test-arithmetic-identities-and-unary-forms ()
  (should (= (nl-+) 0))
  (should (= (nl-*) 1))
  (should (= (nl-- 7) -7))
  (should (equal (nl-/ 3) [nl--rational 1 3])))

(ert-deftest nl-num-test-exact-division-is-rational ()
  ;; CL, Scheme, and Clojure all keep 1/3 exact.
  (let ((answer (nl-/ 1 3)))
    (should (equal answer [nl--rational 1 3]))
    (should (equal (nl-num-pr-str answer) "1/3"))))

(ert-deftest nl-num-test-exact-even-division-demotes ()
  (let ((answer (nl-/ 6 3)))
    (should (= answer 2))
    (should (integerp answer))))

(ert-deftest nl-num-test-rational-addition-reference-case ()
  (should (equal (nl-+ (nl-/ 1 3) (nl-/ 1 6))
                 [nl--rational 1 2])))

(ert-deftest nl-num-test-rational-arithmetic-round-trips ()
  (let ((a (nl-/ 17 23))
        (b (nl-/ -5 11)))
    (should (nl-num-= (nl-- (nl-+ a b) b) a))
    (should (nl-num-= (nl-/ (nl-* a b) b) a))))

(ert-deftest nl-num-test-rational-subtract-and-multiply ()
  (should (equal (nl-- (nl-/ 3 4) (nl-/ 5 6))
                 [nl--rational -1 12]))
  (should (equal (nl-* (nl-/ 6 35) (nl-/ 14 9))
                 [nl--rational 4 15])))

(ert-deftest nl-num-test-left-to-right-nary-division ()
  (should (= (nl-/ 8 2 2) 2)))

(ert-deftest nl-num-test-exact-division-by-zero-signals ()
  (should-error (nl-/ 1 0) :type 'nl-num-division-by-zero-error)
  (should-error (nl-/ (nl-/ 1 2) 0)
                :type 'nl-num-division-by-zero-error))

(ert-deftest nl-num-test-rational-float-contagion ()
  (let ((answer (nl-+ (nl-/ 1 4) 0.5)))
    (should (floatp answer))
    (should (= answer 0.75))))

(ert-deftest nl-num-test-bignum-float-add-avoids-sign-word-hazard ()
  (should (= (nl-+ nl-num-test--big 1.0)
             1.2676506002282294e+30)))

(ert-deftest nl-num-test-bignum-float-other-operators-avoid-hazard ()
  (should (= (nl-- 1.0 nl-num-test--big)
             -1.2676506002282294e+30))
  (should (= (nl-* nl-num-test--big 2.0)
             2.535301200456459e+30))
  (should (= (nl-/ nl-num-test--big 2.0)
             6.338253001141147e+29)))

(ert-deftest nl-num-test-arithmetic-rejects-non-numbers ()
  (should-error (nl-+ 1 'x) :type 'nl-num-error)
  (should-error (nl-* [not-a-number] 2) :type 'nl-num-error))

;;;; Phase 3: complex --------------------------------------------------

(ert-deftest nl-num-test-complex-tagged-vector-representation ()
  (let ((number (nl-complex 1 2)))
    (should (nl-complexp number))
    (should (equal number [nl--complex 1 2]))))

(ert-deftest nl-num-test-complex-exact-zero-imaginary-demotes ()
  ;; CLHS 12.1.5.3: exact zero collapses the complex.
  (let ((number (nl-complex (nl-/ 3 2) 0)))
    (should-not (nl-complexp number))
    (should (equal number [nl--rational 3 2]))))

(ert-deftest nl-num-test-complex-float-zero-imaginary-does-not-demote ()
  ;; CLHS 12.1.5.3's easy-to-lose exact/inexact distinction.
  (let ((number (nl-complex 1.0 0.0)))
    (should (nl-complexp number))
    (should (equal number [nl--complex 1.0 0.0]))))

(ert-deftest nl-num-test-complex-bilateral-float-contagion ()
  (let ((number (nl-complex 1 2.0)))
    (should (floatp (nl-realpart number)))
    (should (floatp (nl-imagpart number)))
    (should (= (nl-realpart number) 1.0))
    (should (= (nl-imagpart number) 2.0))))

(ert-deftest nl-num-test-realpart-and-imagpart-on-reals ()
  (should (= (nl-realpart 7) 7))
  (should (= (nl-imagpart 7) 0))
  (should (= (nl-imagpart 7.0) 0.0))
  (should (floatp (nl-imagpart 7.0))))

(ert-deftest nl-num-test-complex-addition-with-real-contagion ()
  (should (equal (nl-+ (nl-complex 1 2) 3)
                 [nl--complex 4 2])))

(ert-deftest nl-num-test-complex-multiplication-reference-formula ()
  ;; (1+2i)(3+4i) = -5+10i.
  (should (equal (nl-* (nl-complex 1 2) (nl-complex 3 4))
                 [nl--complex -5 10])))

(ert-deftest nl-num-test-complex-division-by-conjugate ()
  ;; (1+i)/(1-i) = i.
  (should (equal (nl-/ (nl-complex 1 1) (nl-complex 1 -1))
                 [nl--complex 0 1])))

(ert-deftest nl-num-test-complex-rational-arithmetic ()
  (let ((answer (nl-* (nl-complex (nl-/ 1 2) (nl-/ 1 3)) 6)))
    (should (equal answer [nl--complex 3 2]))))

(ert-deftest nl-num-test-complex-division-by-exact-zero-signals ()
  (should-error (nl-/ (nl-complex 1 1) 0)
                :type 'nl-num-division-by-zero-error))

(ert-deftest nl-num-test-complex-magnitude-matches-clhs-formula ()
  (should (= (nl-magnitude (nl-complex 3 4)) 5.0))
  (should (= (nl-magnitude (nl-complex (nl-/ 3 2) 2)) 2.5)))

(ert-deftest nl-num-test-sqrt-negative-produces-complex ()
  (let ((answer (nl-num-sqrt -4)))
    (should (nl-complexp answer))
    (should (= (nl-realpart answer) 0.0))
    (should (= (nl-imagpart answer) 2.0))))

(ert-deftest nl-num-test-complex-cosmetic-printing ()
  (should (equal (nl-num-pr-str (nl-complex (nl-/ 1 2) 3))
                 "#C(1/2 3)")))

;;;; Phase 4: comparisons ----------------------------------------------

(ert-deftest nl-num-test-value-equality-across-real-representations ()
  (should (nl-num-= (nl-/ 1 2) 0.5))
  (should (nl-num-= nl-num-test--big 1.2676506002282294e+30))
  ;; CL-style mixed exact/inexact comparison first promotes the rational.
  (should (nl-num-= (nl-/ 1 3) 0.3333333333333333))
  (should-not (nl-num-= (nl-/ 1 3) 0.3333333333333334)))

(ert-deftest nl-num-test-value-equality-across-complex-and-real ()
  (should (nl-num-= 2 (nl-complex 2 0)))
  (should (nl-num-= 2.0 (nl-complex 2.0 0.0)))
  (should-not (nl-num-= 2 (nl-complex 2 1))))

(ert-deftest nl-num-test-rational-ordering-cross-multiplies ()
  (should (nl-num-< (nl-/ -2 3) (nl-/ -1 2)))
  (should (nl-num-< (nl-/ 1 3) (nl-/ 2 5)))
  (should-not (nl-num-< (nl-/ 3 4) (nl-/ 2 3))))

(ert-deftest nl-num-test-ordering-spans-bignum-and-float-safely ()
  (should (nl-num-< 1.0 nl-num-test--big))
  (should (nl-num-> nl-num-test--big 1.0))
  (should (nl-num-<= nl-num-test--big 1.2676506002282294e+30))
  (should (nl-num->= nl-num-test--big 1.2676506002282294e+30)))

(ert-deftest nl-num-test-comparison-chains ()
  (should (nl-num-< -1 (nl-/ 1 3) 0.5 2))
  (should (nl-num-<= 1 1.0 (nl-/ 2 1)))
  (should (nl-num-> 3 2.0 (nl-/ 3 2)))
  (should (nl-num->= 3 3.0 (nl-/ 5 2))))

(ert-deftest nl-num-test-comparison-full-five-by-five-type-matrix ()
  "Exercise every ordered pair of integer/bignum/rational/float/complex."
  ;; Entries are (KIND VALUE REAL-RANK).  Every value is distinct, so value
  ;; equality is true exactly on the diagonal.  REAL-RANK gives the numeric
  ;; order for the four real entries; complex ordering must signal.
  (let ((lefts (list (list 'integer -5 0)
                     (list 'bignum nl-num-test--big 3)
                     (list 'rational (nl-/ 3 2) 1)
                     (list 'float 2.0 2)
                     (list 'complex (nl-complex 1 1) nil))))
    (while lefts
      (let ((left (car lefts))
            (rights (list (list 'integer -5 0)
                          (list 'bignum nl-num-test--big 3)
                          (list 'rational (nl-/ 3 2) 1)
                          (list 'float 2.0 2)
                          (list 'complex (nl-complex 1 1) nil))))
        (while rights
          (let* ((right (car rights))
                 (left-value (car (cdr left)))
                 (right-value (car (cdr right)))
                 (left-rank (car (cdr (cdr left))))
                 (right-rank (car (cdr (cdr right)))))
            (should (eq (nl-num-= left-value right-value)
                        (eq (car left) (car right))))
            (if (or (null left-rank) (null right-rank))
                (progn
                  (should-error (nl-num-< left-value right-value)
                                :type 'nl-num-error)
                  (should-error (nl-num-<= left-value right-value)
                                :type 'nl-num-error)
                  (should-error (nl-num-> left-value right-value)
                                :type 'nl-num-error)
                  (should-error (nl-num->= left-value right-value)
                                :type 'nl-num-error))
              (should (eq (nl-num-< left-value right-value)
                          (< left-rank right-rank)))
              (should (eq (nl-num-<= left-value right-value)
                          (<= left-rank right-rank)))
              (should (eq (nl-num-> left-value right-value)
                          (> left-rank right-rank)))
              (should (eq (nl-num->= left-value right-value)
                          (>= left-rank right-rank)))))
          (setq rights (cdr rights))))
      (setq lefts (cdr lefts)))))

(ert-deftest nl-num-test-ordering-complex-signals ()
  (let ((complex (nl-complex 1 1)))
    (should-error (nl-num-< complex 2) :type 'nl-num-error)
    (should-error (nl-num-<= 2 complex) :type 'nl-num-error)
    (should-error (nl-num-> complex 2) :type 'nl-num-error)
    (should-error (nl-num->= 2 complex) :type 'nl-num-error)))

(ert-deftest nl-num-test-comparison-rejects-non-numbers ()
  (should-error (nl-num-= 1 'one) :type 'nl-num-error)
  (should-error (nl-num-< 1 'one) :type 'nl-num-error))

(provide 'nl-num-test)

;;; nl-num-test.el ends here
