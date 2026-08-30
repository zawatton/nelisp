;;; nl-num-rational.el --- Normalized rationals and real arithmetic -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 196 Phases 1-2.  A non-integral rational is the tagged plain vector
;; [nl--rational NUM DEN], always reduced and with DEN > 0.  DEN = 1 is
;; canonicalized to the integer NUM.  Arithmetic follows the contagion
;; lattice integer < rational < float; exact integer division produces a
;; rational instead of truncating or becoming a float.

;;; Code:

(require 'nl-num-core)

;;;; Public predicates and construction --------------------------------

(defun nl-rationalp (object)
  "Return non-nil when OBJECT is a non-integral nl-num rational vector."
  (nl-num--rational-vector-p object))

(defun nl-realp (object)
  "Return non-nil when OBJECT is an nl-num real value."
  (nl-num--real-p object))

(defun nl-rational (numerator denominator)
  "Return the normalized exact ratio NUMERATOR/DENOMINATOR.
NUMERATOR and DENOMINATOR must be integers.  The sign is kept in the
numerator, the denominator is positive, common factors are removed, and a
denominator of one demotes the result to an integer."
  (nl-num--ensure-integer numerator 'nl-rational)
  (nl-num--ensure-integer denominator 'nl-rational)
  (when (= denominator 0)
    (signal 'nl-num-division-by-zero-error
            (list 'nl-rational numerator denominator)))
  (when (< denominator 0)
    (setq numerator (- 0 numerator))
    (setq denominator (- 0 denominator)))
  (let* ((divisor (nl-num--gcd numerator denominator))
         (reduced-num (nl-num--exact-quotient numerator divisor))
         (reduced-den (nl-num--exact-quotient denominator divisor)))
    (if (= reduced-den 1)
        reduced-num
      (vector nl-num--rational-tag reduced-num reduced-den))))

(defun nl-numerator (rational)
  "Return RATIONAL's numerator; an integer is its own numerator."
  (cond
   ((integerp rational) rational)
   ((nl-num--rational-vector-p rational) (aref rational 1))
   (t (signal 'nl-num-error
              (list 'nl-numerator "expected exact rational" rational)))))

(defun nl-denominator (rational)
  "Return RATIONAL's positive denominator; an integer has denominator 1."
  (cond
   ((integerp rational) 1)
   ((nl-num--rational-vector-p rational) (aref rational 2))
   (t (signal 'nl-num-error
              (list 'nl-denominator "expected exact rational" rational)))))

(defun nl-num-pr-str (number)
  "Return NUMBER in nl-num's cosmetic numeric notation.
Rationals use N/D.  Complex values use #C(RE IM) for display only; this
function does not extend the Elisp reader."
  (cond
   ((integerp number) (number-to-string number))
   ((floatp number) (number-to-string number))
   ((nl-num--rational-vector-p number)
    (format "%s/%s"
            (number-to-string (aref number 1))
            (number-to-string (aref number 2))))
   ((nl-num--complex-vector-p number)
    (format "#C(%s %s)"
            (nl-num-pr-str (aref number 1))
            (nl-num-pr-str (aref number 2))))
   (t (signal 'nl-num-error
              (list 'nl-num-pr-str "expected number" number)))))

;;;; Exact rational helpers --------------------------------------------

(defun nl-num--exact-parts (number)
  "Return exact real NUMBER as a (NUMERATOR . DENOMINATOR) pair."
  (cond
   ((integerp number) (cons number 1))
   ((nl-num--rational-vector-p number)
    (cons (aref number 1) (aref number 2)))
   (t (signal 'nl-num-error
              (list 'nl-num--exact-parts "expected exact real" number)))))

(defun nl-num--exact-add (a b)
  "Return exact A+B for integer/rational operands."
  (let* ((ap (nl-num--exact-parts a))
         (bp (nl-num--exact-parts b))
         (an (car ap)) (ad (cdr ap))
         (bn (car bp)) (bd (cdr bp)))
    (nl-rational (+ (* an bd) (* bn ad)) (* ad bd))))

(defun nl-num--exact-sub (a b)
  "Return exact A-B for integer/rational operands."
  (let* ((ap (nl-num--exact-parts a))
         (bp (nl-num--exact-parts b))
         (an (car ap)) (ad (cdr ap))
         (bn (car bp)) (bd (cdr bp)))
    (nl-rational (- (* an bd) (* bn ad)) (* ad bd))))

(defun nl-num--exact-mul (a b)
  "Return exact A*B for integer/rational operands."
  (let* ((ap (nl-num--exact-parts a))
         (bp (nl-num--exact-parts b)))
    (nl-rational (* (car ap) (car bp)) (* (cdr ap) (cdr bp)))))

(defun nl-num--exact-div (a b)
  "Return exact A/B for integer/rational operands."
  (let* ((ap (nl-num--exact-parts a))
         (bp (nl-num--exact-parts b))
         (bn (car bp)))
    (when (= bn 0)
      (signal 'nl-num-division-by-zero-error
              (list 'nl-/ a b)))
    (nl-rational (* (car ap) (cdr bp)) (* (cdr ap) bn))))

(defun nl-num--exact-negate (number)
  "Return exact -NUMBER for an integer or rational."
  (if (integerp number)
      (- 0 number)
    (nl-rational (- 0 (aref number 1)) (aref number 2))))

(defun nl-num--exact-zero-p (number)
  "Return non-nil when NUMBER is an exact integer/rational zero."
  (cond
   ((integerp number) (= number 0))
   ((nl-num--rational-vector-p number) (= (aref number 1) 0))
   (t nil)))

;;;; Float-safe real contagion -----------------------------------------

(defun nl-num--real-to-float (number)
  "Promote real NUMBER to a float without a native bignum/float fold."
  (cond
   ((floatp number) number)
   ((integerp number) (nl-num--integer-to-float number))
   ((nl-num--rational-vector-p number)
    (/ (nl-num--integer-to-float (aref number 1))
       (nl-num--integer-to-float (aref number 2))))
   (t (signal 'nl-num-error
              (list 'nl-num--real-to-float "expected real number" number)))))

(defun nl-num--real-add (a b)
  "Return real A+B under integer < rational < float contagion."
  (nl-num--ensure-real a 'nl-+)
  (nl-num--ensure-real b 'nl-+)
  (cond
   ((or (floatp a) (floatp b))
    (+ (nl-num--real-to-float a) (nl-num--real-to-float b)))
   ((or (nl-num--rational-vector-p a) (nl-num--rational-vector-p b))
    (nl-num--exact-add a b))
   (t (+ a b))))

(defun nl-num--real-sub (a b)
  "Return real A-B under integer < rational < float contagion."
  (nl-num--ensure-real a 'nl--)
  (nl-num--ensure-real b 'nl--)
  (cond
   ((or (floatp a) (floatp b))
    (- (nl-num--real-to-float a) (nl-num--real-to-float b)))
   ((or (nl-num--rational-vector-p a) (nl-num--rational-vector-p b))
    (nl-num--exact-sub a b))
   (t (- a b))))

(defun nl-num--real-mul (a b)
  "Return real A*B under integer < rational < float contagion."
  (nl-num--ensure-real a 'nl-*)
  (nl-num--ensure-real b 'nl-*)
  (cond
   ((or (floatp a) (floatp b))
    (* (nl-num--real-to-float a) (nl-num--real-to-float b)))
   ((or (nl-num--rational-vector-p a) (nl-num--rational-vector-p b))
    (nl-num--exact-mul a b))
   (t (* a b))))

(defun nl-num--real-div (a b)
  "Return real A/B under integer < rational < float contagion.
When both operands are exact, the result is exact and normalized."
  (nl-num--ensure-real a 'nl-/)
  (nl-num--ensure-real b 'nl-/)
  (if (or (floatp a) (floatp b))
      (/ (nl-num--real-to-float a) (nl-num--real-to-float b))
    (nl-num--exact-div a b)))

(defun nl-num--real-negate (number)
  "Return real -NUMBER without mixing native bignum and float operands."
  (nl-num--ensure-real number 'nl--)
  (if (floatp number)
      (- number)
    (nl-num--exact-negate number)))

(defun nl-num--real-equal (a b)
  "Return non-nil when real A and B have equal numeric values."
  (nl-num--ensure-real a 'nl-num-=)
  (nl-num--ensure-real b 'nl-num-=)
  (cond
   ((or (floatp a) (floatp b))
    (= (nl-num--real-to-float a) (nl-num--real-to-float b)))
   ((or (nl-num--rational-vector-p a) (nl-num--rational-vector-p b))
    (let ((ap (nl-num--exact-parts a))
          (bp (nl-num--exact-parts b)))
      (= (* (car ap) (cdr bp)) (* (car bp) (cdr ap)))))
   (t (= a b))))

(defun nl-num--real-less (a b)
  "Return non-nil when real A is numerically less than real B."
  (nl-num--ensure-real a 'nl-num-<)
  (nl-num--ensure-real b 'nl-num-<)
  (cond
   ((or (floatp a) (floatp b))
    (< (nl-num--real-to-float a) (nl-num--real-to-float b)))
   ((or (nl-num--rational-vector-p a) (nl-num--rational-vector-p b))
    (let ((ap (nl-num--exact-parts a))
          (bp (nl-num--exact-parts b)))
      (< (* (car ap) (cdr bp)) (* (car bp) (cdr ap)))))
   (t (< a b))))

(defun nl-num--real-abs (number)
  "Return the absolute value of real NUMBER."
  (nl-num--ensure-real number 'nl-magnitude)
  (cond
   ((floatp number) (if (< number 0.0) (- number) number))
   ((nl-num--real-less number 0) (nl-num--real-negate number))
   (t number)))

(provide 'nl-num-rational)

;;; nl-num-rational.el ends here
