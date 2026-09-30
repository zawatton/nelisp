;;; emacs-cc-floatfns-1.el --- C float functions -*- lexical-binding: t; -*-

(unless (fboundp 'acos)
  (defun acos (arg)
    "Return the inverse cosine of ARG.\n\n(fn ARG)"
    (unless (numberp arg) (signal 'wrong-type-argument (list 'numberp arg)))
    (let ((a (float arg)))
      (* 2.0 (asin (sqrt (/ (- 1.0 a) 2.0)))))))

(unless (fboundp 'asin)
  (defun asin (arg)
    "Return the inverse sine of ARG.\n\n(fn ARG)"
    (unless (numberp arg) (signal 'wrong-type-argument (list 'numberp arg)))
    (let ((a (float arg)))
      (if (= (abs a) 1.0) (* a 1.5707963267948966)
        (let ((v a) (i 0))
          (while (< i 12)
            (setq v (- v (/ (- (sin v) a) (cos v)))
                  i (1+ i)))
          v)))))

(unless (fboundp 'copysign)
  (defun copysign (x1 x2)
    "Copy sign of X2 to value of X1, and return the result.\nCause an error if X1 or X2 is not a float.\n\n(fn X1 X2)"
    (unless (floatp x1) (signal 'wrong-type-argument (list 'floatp x1)))
    (unless (floatp x2) (signal 'wrong-type-argument (list 'floatp x2)))
    (if (< x2 0.0) (- (abs x1)) (abs x1))))

(unless (fboundp 'frexp)
  (defun frexp (x)
    "Get significand and exponent of a floating point number.\n\n(fn X)"
    (unless (numberp x) (signal 'wrong-type-argument (list 'numberp x)))
    (if (= x 0) (cons 0.0 0)
      (let ((m (float x)) (e 0))
        (while (>= (abs m) 1.0) (setq m (/ m 2.0) e (1+ e)))
        (while (< (abs m) 0.5) (setq m (* m 2.0) e (1- e)))
        (cons m e)))))

(unless (fboundp 'ldexp)
  (defun ldexp (sgnfcand exponent)
    "Return SGNFCAND * 2**EXPONENT, as a floating point number.\n\n(fn SGNFCAND EXPONENT)"
    (unless (numberp sgnfcand) (signal 'wrong-type-argument (list 'numberp sgnfcand)))
    (unless (integerp exponent) (signal 'wrong-type-argument (list 'fixnump exponent)))
    (* (float sgnfcand) (expt 2.0 exponent))))

(unless (fboundp 'tan)
  (defun tan (arg)
    "Return the tangent of ARG.\n\n(fn ARG)"
    (unless (numberp arg) (signal 'wrong-type-argument (list 'numberp arg)))
    (/ (sin arg) (cos arg))))

(provide 'emacs-cc-floatfns-1)
