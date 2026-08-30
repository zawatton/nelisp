;;; nl-num.el --- Opt-in rational and complex numeric tower -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `(require 'nl-num)' opts a caller into Doc 196's Clojure/Common Lisp/
;; Scheme-style numeric tower.  Stock `+', `-', `*', `/', and comparisons
;; are never advised or redefined.  Use `nl-+', `nl--', `nl-*', `nl-/' and
;; the `nl-num-' comparison family explicitly.

;;; Code:

(require 'nl-num-core)
(require 'nl-num-rational)
(require 'nl-num-complex)

;;;; Arithmetic --------------------------------------------------------

(defun nl-num--add2 (a b)
  "Return A+B across the real/complex contagion boundary."
  (nl-num--ensure-number a 'nl-+)
  (nl-num--ensure-number b 'nl-+)
  (if (or (nl-num--complex-vector-p a) (nl-num--complex-vector-p b))
      (nl-num--complex-add a b)
    (nl-num--real-add a b)))

(defun nl-num--sub2 (a b)
  "Return A-B across the real/complex contagion boundary."
  (nl-num--ensure-number a 'nl--)
  (nl-num--ensure-number b 'nl--)
  (if (or (nl-num--complex-vector-p a) (nl-num--complex-vector-p b))
      (nl-num--complex-sub a b)
    (nl-num--real-sub a b)))

(defun nl-num--mul2 (a b)
  "Return A*B across the real/complex contagion boundary."
  (nl-num--ensure-number a 'nl-*)
  (nl-num--ensure-number b 'nl-*)
  (if (or (nl-num--complex-vector-p a) (nl-num--complex-vector-p b))
      (nl-num--complex-mul a b)
    (nl-num--real-mul a b)))

(defun nl-num--div2 (a b)
  "Return A/B across the real/complex contagion boundary."
  (nl-num--ensure-number a 'nl-/)
  (nl-num--ensure-number b 'nl-/)
  (if (or (nl-num--complex-vector-p a) (nl-num--complex-vector-p b))
      (nl-num--complex-div a b)
    (nl-num--real-div a b)))

(defun nl-+ (&rest numbers)
  "Return the sum of NUMBERS under nl-num contagion; zero args return 0."
  (let ((answer 0))
    (while numbers
      (setq answer (nl-num--add2 answer (car numbers)))
      (setq numbers (cdr numbers)))
    answer))

(defun nl-- (number &rest subtrahends)
  "Negate NUMBER, or subtract SUBTRAHENDS from it from left to right."
  (nl-num--ensure-number number 'nl--)
  (if (null subtrahends)
      (if (nl-num--complex-vector-p number)
          (nl-num--complex-negate number)
        (nl-num--real-negate number))
    (let ((answer number))
      (while subtrahends
        (setq answer (nl-num--sub2 answer (car subtrahends)))
        (setq subtrahends (cdr subtrahends)))
      answer)))

(defun nl-* (&rest numbers)
  "Return the product of NUMBERS under nl-num contagion; zero args return 1."
  (let ((answer 1))
    (while numbers
      (setq answer (nl-num--mul2 answer (car numbers)))
      (setq numbers (cdr numbers)))
    answer))

(defun nl-/ (number &rest divisors)
  "Return the reciprocal of NUMBER, or divide by DIVISORS left to right.
Exact integer division constructs a normalized rational."
  (nl-num--ensure-number number 'nl-/)
  (if (null divisors)
      (nl-num--div2 1 number)
    (let ((answer number))
      (while divisors
        (setq answer (nl-num--div2 answer (car divisors)))
        (setq divisors (cdr divisors)))
      answer)))

;;;; Equality and ordering ---------------------------------------------

(defun nl-num--equal2 (a b)
  "Return non-nil when A and B have equal values across the tower."
  (nl-num--ensure-number a 'nl-num-=)
  (nl-num--ensure-number b 'nl-num-=)
  (if (or (nl-num--complex-vector-p a) (nl-num--complex-vector-p b))
      (let ((ap (nl-num--complex-parts a))
            (bp (nl-num--complex-parts b)))
        (and (nl-num--real-equal (car ap) (car bp))
             (nl-num--real-equal (cdr ap) (cdr bp))))
    (nl-num--real-equal a b)))

(defun nl-num--ordered-real-pair (a b caller)
  "Validate A and B as ordered real operands for CALLER."
  (when (or (nl-num--complex-vector-p a) (nl-num--complex-vector-p b))
    (signal 'nl-num-error
            (list caller "complex numbers have no total order" a b)))
  (nl-num--ensure-real a caller)
  (nl-num--ensure-real b caller)
  t)

(defun nl-num--less2 (a b)
  "Return non-nil when real A is less than real B."
  (nl-num--ordered-real-pair a b 'nl-num-<)
  (nl-num--real-less a b))

(defun nl-num--less-equal2 (a b)
  "Return non-nil when real A is less than or equal to real B."
  (nl-num--ordered-real-pair a b 'nl-num-<=)
  (or (nl-num--real-less a b) (nl-num--real-equal a b)))

(defun nl-num--greater2 (a b)
  "Return non-nil when real A is greater than real B."
  (nl-num--ordered-real-pair a b 'nl-num->)
  (nl-num--real-less b a))

(defun nl-num--greater-equal2 (a b)
  "Return non-nil when real A is greater than or equal to real B."
  (nl-num--ordered-real-pair a b 'nl-num->=)
  (or (nl-num--real-less b a) (nl-num--real-equal a b)))

(defun nl-num--chain (predicate first rest)
  "Apply binary PREDICATE pairwise across FIRST followed by REST."
  (let ((previous first)
        (answer t))
    (while (and rest answer)
      (setq answer (funcall predicate previous (car rest)))
      (setq previous (car rest))
      (setq rest (cdr rest)))
    answer))

(defun nl-num-= (number &rest numbers)
  "Return non-nil when NUMBER and all NUMBERS are pairwise value-equal."
  (nl-num--ensure-number number 'nl-num-=)
  (nl-num--chain #'nl-num--equal2 number numbers))

(defun nl-num-< (number &rest numbers)
  "Return non-nil when NUMBER and NUMBERS are in strictly increasing order."
  (nl-num--ensure-real number 'nl-num-<)
  (nl-num--chain #'nl-num--less2 number numbers))

(defun nl-num-<= (number &rest numbers)
  "Return non-nil when NUMBER and NUMBERS are in nondecreasing order."
  (nl-num--ensure-real number 'nl-num-<=)
  (nl-num--chain #'nl-num--less-equal2 number numbers))

(defun nl-num-> (number &rest numbers)
  "Return non-nil when NUMBER and NUMBERS are in strictly decreasing order."
  (nl-num--ensure-real number 'nl-num->)
  (nl-num--chain #'nl-num--greater2 number numbers))

(defun nl-num->= (number &rest numbers)
  "Return non-nil when NUMBER and NUMBERS are in nonincreasing order."
  (nl-num--ensure-real number 'nl-num->=)
  (nl-num--chain #'nl-num--greater-equal2 number numbers))

(provide 'nl-num)

;;; nl-num.el ends here
