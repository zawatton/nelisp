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

(defvar emacs-cc-floatfns--libm-path nil
  "The path `nl-ffi-loader--resolve-soname' found for libm.so.6, once found.")

(defun emacs-cc-floatfns--libm-path (&optional again)
  "Return libm's path, resolved on first use or when AGAIN is non-nil.
Resolving searches the library directories, about 120 us a call."
  (or (and (not again) emacs-cc-floatfns--libm-path)
      (setq emacs-cc-floatfns--libm-path
            (nl-ffi-loader--resolve-soname "libm.so.6" nil))))

(defun emacs-cc-floatfns--typed-libm (operation arg)
  "Call OPERATION in libm with numeric ARG through the typed FFI."
  (unless (numberp arg)
    (signal 'wrong-type-argument (list 'numberp arg)))
  (unless (memq operation '(acos asin exp log log10))
    (error "Unsupported libm operation: %S" operation))
  (unless (and (fboundp 'nl-ffi-call)
               (fboundp 'nl-ffi-loader--resolve-soname)
               (fboundp 'ffi:library)
               (fboundp 'nl-ffi--invoke)
               (boundp 'nl-ffi--library-order))
    (signal 'nelisp-unsupported-primitive (list operation)))
  (let ((path (emacs-cc-floatfns--libm-path)))
    (unless path
      (signal 'nelisp-unsupported-primitive (list operation)))
    ;; A remembered path that no longer opens (an image restored on another
    ;; system) is resolved again once.
    (condition-case nil
        (ffi:library path)
      (error
       (setq path (emacs-cc-floatfns--libm-path t))
       (unless path
         (signal 'nelisp-unsupported-primitive (list operation)))
       (ffi:library path)))
    (let ((nl-ffi--library-order (list path)))
      (nl-ffi--invoke operation (symbol-name operation)
                      '(:double) :double (list arg)))))

;; Replace only native stubs that reject a valid numeric call.  Working
;; host and dynamic-library implementations keep their original function cell.
(when (fboundp 'nl-ffi-call)
  (unless (condition-case nil
              (progn (acos 0.5) t)
            (nelisp-unsupported-primitive nil))
    (defun acos (arg)
      "Return the inverse cosine of ARG.\n\n(fn ARG)"
      (emacs-cc-floatfns--typed-libm 'acos arg)))
  (unless (condition-case nil
              (progn (asin 0.5) t)
            (nelisp-unsupported-primitive nil))
    (defun asin (arg)
      "Return the inverse sine of ARG.\n\n(fn ARG)"
      (emacs-cc-floatfns--typed-libm 'asin arg))))

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

;; The standalone reader's `tan' is an `fboundp' stub that signals
;; `nelisp-unsupported-primitive'; probe with a call instead of `fboundp'.
(when (fboundp 'nl-ffi-call)
  (unless (condition-case nil (numberp (tan 1.0)) (error nil))
    (defun tan (arg)
      "Return the tangent of ARG.\n\n(fn ARG)"
      (unless (numberp arg) (signal 'wrong-type-argument (list 'numberp arg)))
      (/ (sin arg) (cos arg)))))

;; gnu562: GNU's floatfns.c computes exp and log with libm.  On a reader whose
;; own libm arms are not linked (the static Linux build), the typed FFI still
;; reaches libm's plain exp, log and log10 symbols (sin, cos, tan, atan and
;; log2 are IFUNC symbols, which the FFI loader cannot resolve).  Calling them
;; gives GNU's results to the last bit at about 14 us a call, against the
;; double-double series' 56-118 us.  An operation whose FFI call fails is
;; turned off for the session and takes the definition installed before
;; (`emacs-cc-floatfns--lisp-exp' / `--lisp-log').  Type errors are signalled
;; first, as GNU does; like GNU's extract_float, a bignum goes in as the
;; nearest double.  Log to base 2 keeps the Lisp definition (GNU uses log2).  Where `nl-ffi-call' itself
;; reaches libm (the reader's native arms are live), nothing is wrapped.
(defvar emacs-cc-floatfns--libm-off nil
  "libm operations whose FFI call failed in this session.")

(defun emacs-cc-floatfns--libm-arg-p (x)
  "Non-nil when X goes to libm: a number, converted as GNU's extract_float."
  (numberp x))

(defun emacs-cc-floatfns--libm (operation arg)
  "OPERATION of the number ARG in libm, or nil when that call is unavailable."
  (unless (memq operation emacs-cc-floatfns--libm-off)
    (let ((value (condition-case nil
                     (emacs-cc-floatfns--typed-libm operation (float arg))
                   (error nil))))
      (if (floatp value) value
        (push operation emacs-cc-floatfns--libm-off)
        nil))))

(defun emacs-cc-floatfns--exp (arg)
  "Return the exponential base e of ARG, through libm when available.\n\n(fn ARG)"
  (unless (numberp arg) (signal 'wrong-type-argument (list 'numberp arg)))
  (or (and (emacs-cc-floatfns--libm-arg-p arg)
           (emacs-cc-floatfns--libm 'exp arg))
      (emacs-cc-floatfns--lisp-exp arg)))

(defun emacs-cc-floatfns--log (arg &optional base)
  "Return the natural logarithm of ARG, or its logarithm to BASE.
Through libm when available: log10 for base 10, else log (ARG) / log (BASE),
as GNU's Flog does.\n\n(fn ARG &optional BASE)"
  (unless (numberp arg) (signal 'wrong-type-argument (list 'numberp arg)))
  (when base
    (unless (numberp base) (signal 'wrong-type-argument (list 'numberp base))))
  (or (and (emacs-cc-floatfns--libm-arg-p arg)
           (if (null base)
               (emacs-cc-floatfns--libm 'log arg)
             (and (emacs-cc-floatfns--libm-arg-p base)
                  (let ((b (float base)))
                    (cond
                     ((= b 10.0) (emacs-cc-floatfns--libm 'log10 arg))
                     ((= b 2.0) nil)
                     (t (let* ((num (emacs-cc-floatfns--libm 'log arg))
                               (den (and num (emacs-cc-floatfns--libm 'log b))))
                          (and den (/ num den)))))))))
      (emacs-cc-floatfns--lisp-log arg base)))

(when (and (fboundp 'nl-ffi--invoke)
           (fboundp 'nl-ffi-call)
           (not (condition-case nil (numberp (nl-ffi-call "exp" 0.0)) (error nil))))
  (unless (fboundp 'emacs-cc-floatfns--lisp-exp)
    (defalias 'emacs-cc-floatfns--lisp-exp (symbol-function 'exp)))
  (defalias 'exp (symbol-function 'emacs-cc-floatfns--exp))
  (unless (fboundp 'emacs-cc-floatfns--lisp-log)
    (defalias 'emacs-cc-floatfns--lisp-log (symbol-function 'log)))
  (defalias 'log (symbol-function 'emacs-cc-floatfns--log)))

(provide 'emacs-cc-floatfns-1)
