;;; nl-num-core.el --- Integer foundation and numeric tags for nl-num -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 196 Phase 0.  NeLisp's tag-13 bignums support native `+', `-', `*'
;; and comparisons, but not `/', `mod', `%', `ash', or the bitwise family.
;; Rational normalization therefore starts with a Euclidean divmod made only
;; from doubling, subtraction, addition, and comparison.
;;
;; Rational and complex values are tagged plain vectors, never records and
;; never native Sexp tags:
;;
;;   [nl--rational NUM DEN]
;;   [nl--complex RE IM]
;;
;; This file also owns the safe integer-to-float bridge.  In particular, no
;; native arithmetic call made here ever receives a Bignum and a Float in the
;; same argument list: the standalone float fold would read a tag-13 sign word
;; as the integer magnitude.  The bridge rounds through a <=53-bit fixnum
;; mantissa and then scales using float-only multiplication instead.

;;; Code:

(require 'nl-prelude)

(define-error 'nl-num-error "nl-num numeric tower error" 'nl-error)
(define-error 'nl-num-division-by-zero-error
              "nl-num exact division by zero"
              'nl-num-error)

(defconst nl-num--rational-tag 'nl--rational
  "Tag symbol in slot 0 of a rational vector.")

(defconst nl-num--complex-tag 'nl--complex
  "Tag symbol in slot 0 of a complex vector.")

(defun nl-num--tagged-p (object tag)
  "Return non-nil when OBJECT is a three-slot vector tagged with TAG."
  (and (vectorp object)
       (= (length object) 3)
       (eq (aref object 0) tag)))

(defun nl-num--rational-vector-p (object)
  "Return non-nil when OBJECT has the nl-num rational representation."
  (nl-num--tagged-p object nl-num--rational-tag))

(defun nl-num--complex-vector-p (object)
  "Return non-nil when OBJECT has the nl-num complex representation."
  (nl-num--tagged-p object nl-num--complex-tag))

(defun nl-num--real-p (object)
  "Return non-nil when OBJECT is an integer, float, or rational vector."
  (or (numberp object) (nl-num--rational-vector-p object)))

(defun nl-num--number-p (object)
  "Return non-nil when OBJECT belongs to the nl-num numeric tower."
  (or (nl-num--real-p object) (nl-num--complex-vector-p object)))

(defun nl-num--ensure-integer (object caller)
  "Return OBJECT when it is an integer; otherwise signal for CALLER."
  (unless (integerp object)
    (signal 'nl-num-error (list caller "expected integer" object)))
  object)

(defun nl-num--ensure-real (object caller)
  "Return OBJECT when it is a real nl-num value; otherwise signal for CALLER."
  (unless (nl-num--real-p object)
    (signal 'nl-num-error (list caller "expected real number" object)))
  object)

(defun nl-num--ensure-number (object caller)
  "Return OBJECT when it is an nl-num value; otherwise signal for CALLER."
  (unless (nl-num--number-p object)
    (signal 'nl-num-error (list caller "expected number" object)))
  object)

(defun nl-num--integer-abs (integer)
  "Return the absolute value of INTEGER using bignum-safe subtraction."
  (if (< integer 0) (- 0 integer) integer))

(defun nl-num--positive-divmod (numerator denominator)
  "Divide non-negative NUMERATOR by positive DENOMINATOR.
Return (QUOTIENT . REMAINDER), with REMAINDER in [0, DENOMINATOR).
Only bignum-safe addition, subtraction, and comparison are used."
  (let ((multiple denominator)
        (place 1)
        (stack nil)
        (quotient 0)
        (remainder numerator))
    ;; Push d, 2d, 4d, ... through the largest multiple not above n.
    (while (<= multiple numerator)
      (setq stack (cons (cons multiple place) stack))
      (setq multiple (+ multiple multiple))
      (setq place (+ place place)))
    ;; The stack is already largest-first.  Greedily subtract each place.
    (while stack
      (let ((candidate (car stack)))
        (when (<= (car candidate) remainder)
          (setq remainder (- remainder (car candidate)))
          (setq quotient (+ quotient (cdr candidate)))))
      (setq stack (cdr stack)))
    (cons quotient remainder)))

(defun nl-num--divmod (numerator denominator)
  "Return Euclidean (QUOTIENT . REMAINDER) for two integers.
The identity NUMERATOR = QUOTIENT*DENOMINATOR + REMAINDER holds and
0 <= REMAINDER < abs(DENOMINATOR).  A zero denominator signals
`nl-num-division-by-zero-error'.  The implementation never calls native
division, remainder, shifts, or bitwise operations."
  (nl-num--ensure-integer numerator 'nl-num--divmod)
  (nl-num--ensure-integer denominator 'nl-num--divmod)
  (when (= denominator 0)
    (signal 'nl-num-division-by-zero-error
            (list 'nl-num--divmod numerator denominator)))
  (let* ((den-negative (< denominator 0))
         (positive-den (if den-negative (- 0 denominator) denominator))
         (num-negative (< numerator 0))
         (positive-num (if num-negative (- 0 numerator) numerator))
         (positive-result
          (nl-num--positive-divmod positive-num positive-den))
         (positive-q (car positive-result))
         (positive-r (cdr positive-result))
         quotient remainder)
    ;; Euclidean adjustment for a negative dividend: truncating -n/d would
    ;; leave a negative remainder, so move one quotient step and complement
    ;; the non-zero remainder instead.
    (if (and num-negative (/= positive-r 0))
        (setq quotient (- 0 (+ positive-q 1))
              remainder (- positive-den positive-r))
      (setq quotient (if num-negative (- 0 positive-q) positive-q)
            remainder positive-r))
    (when den-negative
      (setq quotient (- 0 quotient)))
    (cons quotient remainder)))

(defun nl-num--exact-quotient (numerator denominator)
  "Return exact integer NUMERATOR/DENOMINATOR, or signal if not divisible."
  (let ((result (nl-num--divmod numerator denominator)))
    (unless (= (cdr result) 0)
      (signal 'nl-num-error
              (list 'nl-num--exact-quotient
                    "non-exact internal quotient"
                    numerator denominator)))
    (car result)))

(defun nl-num--gcd (a b)
  "Return the non-negative greatest common divisor of integers A and B."
  (nl-num--ensure-integer a 'nl-num--gcd)
  (nl-num--ensure-integer b 'nl-num--gcd)
  (let ((x (nl-num--integer-abs a))
        (y (nl-num--integer-abs b)))
    (while (/= y 0)
      (let ((remainder (cdr (nl-num--divmod x y))))
        ;; Keep a corrupted divmod from turning Euclid's descent into an
        ;; infinite loop.  This is redundant on the valid implementation and
        ;; deliberately loud under the doubling-bound gate mutation.
        (when (>= remainder y)
          (signal 'nl-num-error
                  (list 'nl-num--gcd "divmod remainder invariant" x y
                        remainder)))
        (setq x y)
        (setq y remainder)))
    x))

(defun nl-num--integer-to-float (integer)
  "Convert INTEGER, including tag-13 bignums, to a float safely.
The standalone's native mixed float fold mistakes a bignum sign slot for
its magnitude.  Repeated Phase-0 division by two retains a correctly rounded
53-bit mantissa, which is a plain fixnum, then float-only multiplication
restores its exponent.  No native call receives both a bignum and a float."
  (nl-num--ensure-integer integer 'nl-num--integer-to-float)
  (let ((negative (< integer 0))
        (mantissa (nl-num--integer-abs integer))
        (shift 0)
        (half-bit 0)
        (sticky nil))
    ;; IEEE binary64 carries 53 significant bits.  Bits are discarded from
    ;; least to most significant; HALF-BIT is the final discarded bit and
    ;; STICKY records whether any lower discarded bit was non-zero.
    (while (> mantissa 9007199254740991)
      (let ((step (nl-num--positive-divmod mantissa 2)))
        (when (/= half-bit 0)
          (setq sticky t))
        (setq mantissa (car step))
        (setq half-bit (cdr step))
        (setq shift (+ shift 1))))
    ;; Round to nearest, ties to even.  A carry to 2^53 is still an exactly
    ;; representable plain fixnum on this runtime.
    (when (and (= half-bit 1)
               (or sticky
                   (= (cdr (nl-num--positive-divmod mantissa 2)) 1)))
      (setq mantissa (+ mantissa 1)))
    (let ((answer (+ 0.0 mantissa)))
      (while (> shift 0)
        (setq answer (* answer 2.0))
        (setq shift (- shift 1)))
      (if negative (- answer) answer))))

(provide 'nl-num-core)

;;; nl-num-core.el ends here
