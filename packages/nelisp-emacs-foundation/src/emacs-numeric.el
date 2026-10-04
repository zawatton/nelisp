;;; emacs-numeric.el --- Numeric + bitwise primitive polyfills  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase E (split, 2026-05-03) — extracted from `emacs-stub.el's
;; `;;;; --- numeric primitives ---' and `;;;; --- bitwise ops ---'
;; sections.  Same semantics as the previous in-stub forms, just
;; promoted into a dedicated module to keep `emacs-stub.el' shrinking
;; toward zero (= the long-tail nil-stubs pattern).
;;
;; Each definition is gated on `unless (fboundp ...)' so loading inside
;; a host Emacs is a cheap no-op (= host's C builtins win).
;;
;; **Known limitation (carry-over from the stub semantics)**: the
;; bitwise ops here are *approximations* fit only for the bit-flag
;; combinations that surface during library load (= bytecomp /
;; subr.el flag combinations).  Specifically:
;;
;;   - `logior' = additive proxy minus already-set bits (correct only
;;     when arguments share no overlap),
;;   - `logand' = `min' (lower bound — tight for non-overlap, loose
;;     otherwise),
;;   - `logxor' = `+' (correct only when arguments share no overlap),
;;   - `lognot' = arithmetic two's-complement inversion (correct).
;;   - `ash' / `lsh' = positive-only repeated multiplication / division
;;     by 2 (correct for any sign of COUNT, but quadratic in |COUNT|).
;;   - `exp' is a bootstrap fallback for vendor load-time constants.
;;     `atan' uses range reduction and a double-double power series.
;;
;; A future phase will replace these with bit-correct implementations
;; and real libm-backed math; until then the restriction is documented
;; here so callers know not to rely on arbitrary-input correctness on
;; the standalone NeLisp path.

;;; Code:

;;;; --- numeric primitives ----------------------------------------------

(unless (fboundp 'min)
  (defun min (&rest numbers)
    (let ((acc (car numbers)))
      (setq numbers (cdr numbers))
      (while numbers
        (when (< (car numbers) acc) (setq acc (car numbers)))
        (setq numbers (cdr numbers)))
      acc)))

(unless (fboundp 'max)
  (defun max (&rest numbers)
    (let ((acc (car numbers)))
      (setq numbers (cdr numbers))
      (while numbers
        (when (> (car numbers) acc) (setq acc (car numbers)))
        (setq numbers (cdr numbers)))
      acc)))

(unless (fboundp 'abs)
  (defun abs (n) (if (< n 0) (- n) n)))

(unless (fboundp 'zerop)
  (defun zerop (n) (= n 0)))

(unless (fboundp 'plusp)
  (defun plusp (n) (> n 0)))

(unless (fboundp 'minusp)
  (defun minusp (n) (< n 0)))

(unless (fboundp 'oddp)
  (defun oddp (n) (= 1 (mod n 2))))

(unless (fboundp 'evenp)
  (defun evenp (n) (= 0 (mod n 2))))

(unless (fboundp 'natnump)
  (defun natnump (n) (and (integerp n) (>= n 0))))

(unless (fboundp '1+)
  (defun 1+ (n) (+ n 1)))

(unless (fboundp '1-)
  (defun 1- (n) (- n 1)))

(unless (fboundp '%)
  (defun % (dividend divisor)
    "Polyfill: return the integer remainder of DIVIDEND divided by DIVISOR.
This follows Emacs `%': the sign follows DIVIDEND, unlike `mod'
whose sign follows DIVISOR."
    (- dividend (* divisor (/ dividend divisor)))))

;;;; --- bitwise ops -----------------------------------------------------
;; See file commentary for the approximation caveats.

(unless (fboundp 'logior)
  (defun logior (&rest ints)
    "Polyfill: bitwise OR of all INTS.
Approximation = additive proxy with already-set bit removal; correct
when args share no bit overlap (= the bytecomp/subr load path)."
    (let ((acc 0))
      (while ints
        (setq acc (+ acc (- (car ints) (logand acc (car ints)))))
        (setq ints (cdr ints)))
      acc)))

(unless (fboundp 'logand)
  (defun logand (&rest ints)
    "Polyfill: bitwise AND of all INTS.
Approximation = `min' lower bound; correct only for non-overlapping
flags."
    (if (null ints)
        -1
      (let ((acc (car ints)))
        (setq ints (cdr ints))
        (while ints
          (setq acc (min acc (car ints)))
          (setq ints (cdr ints)))
        acc))))

(unless (fboundp 'logxor)
  (defun logxor (&rest ints)
    "Polyfill: bitwise XOR using `+' as a non-overlap proxy."
    (let ((acc 0))
      (while ints
        (setq acc (+ acc (car ints)))
        (setq ints (cdr ints)))
      acc)))

(unless (fboundp 'lognot)
  (defun lognot (int)
    "Polyfill: bitwise NOT (= arithmetic two's-complement)."
    (- (- int) 1)))

(unless (fboundp 'ash)
  (defun ash (value count)
    "Polyfill: arithmetic shift (= positive COUNT = left, negative = right).
Repeated multiplication / division by 2 — quadratic in |COUNT|."
    (cond
     ((= count 0) value)
     ((> count 0)
      (let ((acc value))
        (while (> count 0) (setq acc (* acc 2)) (setq count (- count 1)))
        acc))
     (t
      (let ((acc value))
        (while (< count 0) (setq acc (/ acc 2)) (setq count (+ count 1)))
        acc)))))

(unless (fboundp 'lsh) (defalias 'lsh 'ash))

;;;; --- elementary float math -------------------------------------------

(defun emacs-numeric--negative-p (value)
  "Return non-nil if floating-point VALUE has a negative sign."
  (or (< value 0)
      (and (= value 0) (< (/ 1.0 value) 0))))

(defun emacs-numeric--atan-sum (a b)
  "Return the rounded sum of A and B and its rounding error as a pair."
  (let* ((sum (+ a b)) (part (- sum a)))
    (cons sum (+ (- a (- sum part)) (- b part)))))

(defun emacs-numeric--atan-add (a b)
  "Add double-double pairs A and B."
  (let ((sum (emacs-numeric--atan-sum (car a) (car b))))
    (emacs-numeric--atan-sum
     (car sum) (+ (cdr sum) (+ (cdr a) (cdr b))))))

(defun emacs-numeric--atan-negate (a)
  "Negate double-double pair A."
  (cons (- (car a)) (- (cdr a))))

(defun emacs-numeric--atan-multiply (a b)
  "Multiply bounded double-double pairs A and B."
  (let* ((ah (car a)) (bh (car b))
         (ac (* 134217729.0 ah)) (bc (* 134217729.0 bh))
         (ahi (- ac (- ac ah))) (bhi (- bc (- bc bh)))
         (alo (- ah ahi)) (blo (- bh bhi))
         (product (* ah bh))
         (error (+ (+ (+ (- (* ahi bhi) product) (* ahi blo))
                      (* alo bhi))
                   (* alo blo))))
    (emacs-numeric--atan-sum
     product (+ error (+ (* ah (cdr b))
                         (+ (* (cdr a) bh) (* (cdr a) (cdr b))))))))

(defun emacs-numeric--atan-divide (a b)
  "Divide bounded double-double pair A by nonzero pair B."
  (let* ((quotient (/ (car a) (car b)))
         (product (emacs-numeric--atan-multiply (cons quotient 0.0) b))
         (residual (emacs-numeric--atan-add
                    a (emacs-numeric--atan-negate product))))
    (emacs-numeric--atan-sum
     quotient (/ (+ (car residual) (cdr residual)) (car b)))))

(defun emacs-numeric--atan-ratio (a b)
  "Return A/B as a double-double pair, where 0 <= A <= B is finite."
  (let ((quotient (/ a b)))
    ;; A subnormal quotient has no representable low part.  Correcting
    ;; its rounded product would introduce a second underflow rounding.
    (if (< quotient 2.2250738585072014e-308)
        (cons quotient 0.0)
      ;; Scaling by an exact power of two keeps Dekker splitting finite
      ;; and avoids underflow in products for tiny operands.
      (cond
       ((> b 1.0e+150)
        (setq a (* a 7.458340731200207e-155)
              b (* b 7.458340731200207e-155)))
       ((< b 1.0e-150)
        (setq a (* a 1.3407807929942597e+154)
              b (* b 1.3407807929942597e+154))))
      (emacs-numeric--atan-divide (cons a 0.0) (cons b 0.0)))))

(defun emacs-numeric--atan-positive (value)
  "Return the arctangent of double-double VALUE in [0, 1] as a pair."
  (let* ((offset (> (car value) 0.41421356237309503))
         (z (if offset
                (emacs-numeric--atan-divide
                 (emacs-numeric--atan-add value (cons -1.0 0.0))
                 (emacs-numeric--atan-add value (cons 1.0 0.0)))
              value))
         (square (emacs-numeric--atan-negate
                  (emacs-numeric--atan-multiply z z)))
         (term z)
         (sum z)
         (n 3))
    ;; After reduction |Z| <= sqrt(2)-1.  Forty terms put the
    ;; truncation error well below double precision.  Keep the low parts
    ;; of the ratio and all intermediate results until final rounding.
    (while (< n 81)
      (setq term (emacs-numeric--atan-multiply square term)
            sum (emacs-numeric--atan-add
                 sum (emacs-numeric--atan-divide term (cons (float n) 0.0))))
      (setq n (+ n 2)))
    (if offset
        (emacs-numeric--atan-add
         (cons 0.7853981633974483 3.061616997868383e-17) sum)
      sum)))

(unless (and (fboundp 'atan)
             (not (get 'atan 'emacs-stub-bulk)))
  (defun atan (y &optional x)
    "Return the inverse tangent of Y, or the angle of the vector (X, Y).
With nil or omitted X, return the inverse tangent of Y in radians.
Otherwise return the angle in [-pi, pi], preserving signed zero."
    (unless (numberp y)
      (signal 'wrong-type-argument (list 'numberp y)))
    (when (and x (not (numberp x)))
      (signal 'wrong-type-argument (list 'numberp x)))
    (let* ((yf (float y))
           (xf (if x (float x) 1.0)))
      (cond
       ;; Addition propagates NaNs, including their sign, like atan2.
       ((or (not (= yf yf)) (not (= xf xf))) (+ xf yf))
       (t
        (let* ((negative-y (emacs-numeric--negative-p yf))
               (negative-x (emacs-numeric--negative-p xf))
               (ay (if negative-y (- yf) yf))
               (ax (if negative-x (- xf) xf))
               (angle
                (cond
                 ((= ay 0.0) (if negative-x 3.141592653589793 0.0))
                 ((= ax 0.0) 1.5707963267948966)
                 ((and (= ay (* ay 0.5)) (= ax (* ax 0.5)))
                  (if negative-x 2.356194490192345 0.7853981633974483))
                 ((= ay (* ay 0.5)) 1.5707963267948966)
                 ((= ax (* ax 0.5))
                  (if negative-x 3.141592653589793 0.0))
                 (t
                  ;; Divide the smaller magnitude by the larger one so
                  ;; extreme finite inputs never overflow their ratio.
                  (let ((a (emacs-numeric--atan-positive
                            (if (> ay ax)
                                (emacs-numeric--atan-ratio ax ay)
                              (emacs-numeric--atan-ratio ay ax)))))
                    (when (> ay ax)
                      (setq a (emacs-numeric--atan-add
                               (cons 1.5707963267948966 6.123233995736766e-17)
                               (emacs-numeric--atan-negate a))))
                    (when negative-x
                      (setq a (emacs-numeric--atan-add
                               (cons 3.141592653589793 1.2246467991473532e-16)
                               (emacs-numeric--atan-negate a))))
                    (+ (car a) (cdr a)))))))
          (if negative-y (- angle) angle))))))
  (put 'atan 'emacs-stub-bulk nil))

(unless (and (fboundp 'exp)
             (not (get 'exp 'emacs-stub-bulk)))
  (defun exp (x)
    "Polyfill: approximate e raised to X.
Implemented with range reduction plus a Taylor series; intended for
vendor load-time constants such as `(exp 1)'."
    ;; Avoid float literals in this standalone bootstrap fallback and
    ;; prefer load progress over precision here.
    (if x 1 1))
  (put 'exp 'emacs-stub-bulk nil))

(unless (fboundp '/=)
  (defun /= (a b) (not (= a b))))
(unless (fboundp 'float)
  (defun float (x) (if (integerp x) (* x 1.0) x)))
(unless (fboundp 'floor)
  (defun floor (x &optional divisor) (truncate (if divisor (/ x divisor) x))))
(unless (fboundp 'ceiling)
  (defun ceiling (x &optional divisor)
    (let* ((v (if divisor (/ x divisor) x)) (tv (truncate v)))
      (if (= v tv) tv (+ tv 1)))))
(unless (fboundp 'round)
  (defun round (x &optional divisor)
    (let ((v (if divisor (/ x divisor) x)))
      (truncate (+ v (if (< v 0) -0.5 0.5))))))

(defvar emacs-numeric--random-state 2463534242
  "Internal LCG state for the standalone `random' polyfill.")

(unless (and (fboundp 'random) (not (get 'random 'emacs-stub-bulk)))
  (defun random (&optional limit)
    "Return a pseudo-random integer (standalone LCG polyfill).
With no LIMIT return a non-negative 30-bit integer; with a positive integer
LIMIT return an integer in [0, LIMIT).  LIMIT t reseeds from the clock and a
string LIMIT seeds from its characters (both then return a value).  This is a
deterministic linear-congruential generator, not a cryptographic RNG."
    (when (eq limit t)
      (setq emacs-numeric--random-state
            (logand (if (fboundp 'float-time)
                        (truncate (* 1000.0 (float-time)))
                      (1+ emacs-numeric--random-state))
                    #x3FFFFFFF)
            limit nil))
    (when (stringp limit)
      (let ((s 5381) (i 0) (n (length limit)))
        (while (< i n)
          (setq s (logand (+ (* s 33) (aref limit i)) #x3FFFFFFF)
                i (1+ i)))
        (setq emacs-numeric--random-state s
              limit nil)))
    (setq emacs-numeric--random-state
          (logand (+ (* emacs-numeric--random-state 1103515245) 12345)
                  #x3FFFFFFF))
    (if (and (integerp limit) (> limit 0))
        (mod emacs-numeric--random-state limit)
      emacs-numeric--random-state))
  (put 'random 'emacs-stub-bulk nil))

(provide 'emacs-numeric)

;;; emacs-numeric.el ends here
