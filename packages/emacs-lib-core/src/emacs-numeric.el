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
;;   - `exp' uses range reduction and a Taylor series; `atan' uses range
;;     reduction and a double-double power series.
;;
;; A future phase will replace these with bit-correct implementations
;; and real libm-backed math; until then the restriction is documented
;; here so callers know not to rely on arbitrary-input correctness on
;; the standalone NeLisp path.

;;; Code:

;;;; --- numeric primitives ----------------------------------------------

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

(unless (fboundp '%)
  (defun % (dividend divisor)
    "Polyfill: return the integer remainder of DIVIDEND divided by DIVISOR.
This follows Emacs `%': the sign follows DIVIDEND, unlike `mod'
whose sign follows DIVISOR."
    (- dividend (* divisor (/ dividend divisor)))))

;;;; --- bitwise ops -----------------------------------------------------
;; See file commentary for the approximation caveats.

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
         (w (car square))
         (term z)
         (sum z)
         (p 0.0)
         (n 79))
    ;; After reduction |Z| <= sqrt(2)-1.  Forty terms put the
    ;; truncation error well below double precision.  Keep the low parts
    ;; of the ratio and all intermediate results until final rounding.
    ;; gnu562: terms through z^13/13 in double-double; the rest,
    ;; z^15 (1/15 - w/17 + ... + w^32/79) with -w = z^2 <= 0.1716, is
    ;; below 2e-6 of the sum, so a double carries it to about 1e-21.
    ;; Bit-identical to the forty double-double terms over 200000
    ;; arguments, at about a sixth of their cost.
    (while (>= n 15)
      (setq p (+ (/ 1.0 n) (* w p)))
      (setq n (- n 2)))
    (setq n 3)
    (while (<= n 13)
      (setq term (emacs-numeric--atan-multiply square term)
            sum (emacs-numeric--atan-add
                 sum (emacs-numeric--atan-divide term (cons (float n) 0.0))))
      (setq n (+ n 2)))
    (setq sum (emacs-numeric--atan-add sum (cons (* (* (car term) w) p) 0.0)))
    (if offset
        (emacs-numeric--atan-add
         (cons 0.7853981633974483 3.061616997868383e-17) sum)
      sum)))

;; `atan' may be fbound to a standalone primitive stub.  When the runtime
;; exposes its FFI entry point, probe both public arities before trusting it;
;; retain any implementation that can execute them, including GNU Emacs's.
(unless (and (fboundp 'atan)
             (not (get 'atan 'emacs-stub-bulk))
             (or (not (fboundp 'nl-ffi-call))
                 (condition-case nil
                          (and (numberp (atan 1.0))
                               (numberp (atan 1.0 1.0)))
                        (nelisp-unsupported-primitive nil))))
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

(defun emacs-numeric--exp-pow2 (n)
  "Return the float 2^N for an integer N whose result does not overflow."
  (let ((result 1.0)
        (base (if (< n 0) 0.5 2.0))
        (count (abs n)))
    (while (> count 0)
      (when (= (% count 2) 1)
        (setq result (* result base)))
      (setq count (/ count 2))
      (when (> count 0)
        (setq base (* base base))))
    result))

(unless (and (fboundp 'exp)
             (not (get 'exp 'emacs-stub-bulk)))
  (defun exp (x)
    "Return e raised to the power X, as a float.
Pure-Elisp fallback: reduce X to R + K*ln(2) with |R| <= ln(2)/2,
sum the Taylor series of e^R, then scale by 2^K."
    (unless (numberp x)
      (signal 'wrong-type-argument (list 'numberp x)))
    (let ((xf (float x)))
      (cond
       ;; NaN propagates (with its sign, as in GNU).
       ((not (= xf xf)) xf)
       ((> xf 709.782712893384) (/ 1.0 0.0))
       ((< xf -745.1332191019412) 0.0)
       (t
        (let* ((q (/ xf 0.6931471805599453))
               (k (truncate (if (< q 0) (- q 0.5) (+ q 0.5))))
               ;; ln(2) split so that K*LN2-HI is exact for |K| < 2^11.
               (r (- (- xf (* k 6.93147180369123816490e-01))
                     (* k 1.90821492927058770002e-10)))
               (term 1.0)
               (sum 1.0)
               (i 1))
          (while (<= i 22)
            (setq term (/ (* term r) i))
            (setq sum (+ sum term))
            (setq i (1+ i)))
          ;; Split the scale so 2^K never overflows before the product.
          (let* ((k1 (/ k 2))
                 (k2 (- k k1)))
            (* (* sum (emacs-numeric--exp-pow2 k1))
               (emacs-numeric--exp-pow2 k2))))))))
  (put 'exp 'emacs-stub-bulk nil))

;;;; --- log / sin / cos (pure-Elisp fallbacks) ---
;; The standalone reader defines `sin' and `cos' (and so `tan') as
;; `fboundp' stubs that signal `nelisp-unsupported-primitive', so a plain
;; `fboundp' guard would never install the fallback.  Probe with a call.

(defun emacs-numeric--primitive-works-p (fn arg)
  "Return non-nil if calling FN on ARG yields a number instead of an error."
  (condition-case nil
      (numberp (funcall fn arg))
    (error nil)))

(defun emacs-numeric--log (arg &optional base)
    "Return the natural logarithm of ARG, or its logarithm to BASE.
Pure-Elisp fallback: write ARG as M*2^K with M in [sqrt(1/2), sqrt(2)),
then sum the series 2*atanh((M-1)/(M+1))."
    (unless (numberp arg)
      (signal 'wrong-type-argument (list 'numberp arg)))
    (when base
      (unless (numberp base)
        (signal 'wrong-type-argument (list 'numberp base))))
    (let ((x (float arg)))
      (cond
       (base
        (/ (emacs-numeric--log x) (emacs-numeric--log (float base))))
       ((not (= x x)) x)
       ((< x 0.0) (/ 0.0 0.0))
       ((= x 0.0) (/ -1.0 0.0))
       ((= x (* x 0.5)) x)
       (t
        (let ((k 0))
          (while (>= x 1.4142135623730951)
            (setq x (/ x 2.0) k (1+ k)))
          (while (< x 0.7071067811865476)
            (setq x (* x 2.0) k (1- k)))
          (let* ((s (/ (- x 1.0) (+ x 1.0)))
                 (s2 (* s s))
                 (term s)
                 (sum s)
                 (n 3))
            (while (<= n 41)
              (setq term (* term s2))
              (setq sum (+ sum (/ term n)))
              (setq n (+ n 2)))
            (+ (* k 0.6931471805599453)
               (* 2.0 sum))))))))

(unless (emacs-numeric--primitive-works-p 'log 2.0)
  (defun log (arg &optional base)
    "Return the natural logarithm of ARG, or its logarithm to BASE."
    (emacs-numeric--log arg base)))

(defun emacs-numeric--trig-reduce (x)
  "Return (N . R) with X = N*pi/2 + R, N an integer and |R| <= pi/4.
Uses a Cody-Waite reduction with pi/2 split into 33-bit pieces; N is split
into 15-bit-shifted halves so every partial product is exact.  Accurate to
about 1e-16 relative for |X| below roughly 1e10."
  (let* ((q (/ x 1.5707963267948966))
         (n (truncate (if (< q 0) (- q 0.5) (+ q 0.5))))
         (nl (float (% n 32768)))
         (nh (float (- n (% n 32768))))
         (r x))
    (dolist (c '(1.57079632673412561417e+00 6.07710050630396597660e-11
                 2.02226624871116645580e-21 8.47842766036889956997e-32))
      (setq r (- (- r (* nh c)) (* nl c))))
    (cons n r)))

(defun emacs-numeric--sin-kernel (r)
  "Taylor series of sin(R) for |R| <= pi/4, its 13 terms unrolled.
Each term is the previous one times R^2 over a negative constant: the
same value and rounding as negating it first, without a negation call."
  (let* ((r2 (* r r)) (term r) (sum r))
    (setq term (/ (* term r2) -6.0) sum (+ sum term))
    (setq term (/ (* term r2) -20.0) sum (+ sum term))
    (setq term (/ (* term r2) -42.0) sum (+ sum term))
    (setq term (/ (* term r2) -72.0) sum (+ sum term))
    (setq term (/ (* term r2) -110.0) sum (+ sum term))
    (setq term (/ (* term r2) -156.0) sum (+ sum term))
    (setq term (/ (* term r2) -210.0) sum (+ sum term))
    (setq term (/ (* term r2) -272.0) sum (+ sum term))
    (setq term (/ (* term r2) -342.0) sum (+ sum term))
    (setq term (/ (* term r2) -420.0) sum (+ sum term))
    (setq term (/ (* term r2) -506.0) sum (+ sum term))
    (setq term (/ (* term r2) -600.0) sum (+ sum term))
    (setq term (/ (* term r2) -702.0) sum (+ sum term))
    sum))

(defun emacs-numeric--cos-kernel (r)
  "Taylor series of cos(R) for |R| <= pi/4, its 13 terms unrolled.
Each term is the previous one times R^2 over a negative constant: the
same value and rounding as negating it first, without a negation call."
  (let* ((r2 (* r r)) (term 1.0) (sum 1.0))
    (setq term (/ (* term r2) -2.0) sum (+ sum term))
    (setq term (/ (* term r2) -12.0) sum (+ sum term))
    (setq term (/ (* term r2) -30.0) sum (+ sum term))
    (setq term (/ (* term r2) -56.0) sum (+ sum term))
    (setq term (/ (* term r2) -90.0) sum (+ sum term))
    (setq term (/ (* term r2) -132.0) sum (+ sum term))
    (setq term (/ (* term r2) -182.0) sum (+ sum term))
    (setq term (/ (* term r2) -240.0) sum (+ sum term))
    (setq term (/ (* term r2) -306.0) sum (+ sum term))
    (setq term (/ (* term r2) -380.0) sum (+ sum term))
    (setq term (/ (* term r2) -462.0) sum (+ sum term))
    (setq term (/ (* term r2) -552.0) sum (+ sum term))
    (setq term (/ (* term r2) -650.0) sum (+ sum term))
    sum))

(defun emacs-numeric--trig (x cos-p)
  "Return sin(X), or cos(X) when COS-P, as a float."
  (unless (numberp x)
    (signal 'wrong-type-argument (list 'numberp x)))
  (let ((xf (float x)))
    (if (or (not (= xf xf)) (= xf (* xf 0.5)) (> (abs xf) 1e18))
        (if (and (= xf xf) (= xf (* xf 0.5)) (/= xf 0.0)) (/ 0.0 0.0)
          (if (and cos-p (= xf 0.0)) 1.0 xf))
      (let* ((red (emacs-numeric--trig-reduce xf))
             (m (% (car red) 4))
             (m (if (< m 0) (+ m 4) m))
             (m (if cos-p (% (1+ m) 4) m))
             (r (cdr red)))
        (cond
         ((= m 0) (emacs-numeric--sin-kernel r))
         ((= m 1) (emacs-numeric--cos-kernel r))
         ((= m 2) (- (emacs-numeric--sin-kernel r)))
         (t (- (emacs-numeric--cos-kernel r))))))))

(unless (emacs-numeric--primitive-works-p 'sin 1.0)
  (defun sin (arg)
    "Return the sine of ARG (radians).  Pure-Elisp fallback."
    (emacs-numeric--trig arg nil)))

(unless (emacs-numeric--primitive-works-p 'cos 1.0)
  (defun cos (arg)
    "Return the cosine of ARG (radians).  Pure-Elisp fallback."
    (emacs-numeric--trig arg t)))

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
