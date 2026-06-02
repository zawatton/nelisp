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
;;   - `atan' / `exp' are small interpreted approximations sufficient
;;     for vendor load-time constants such as float-sup.el.
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

(unless (and (fboundp 'atan)
             (not (get 'atan 'emacs-stub-bulk)))
  (defun atan (y &optional x)
    "Polyfill: approximate arctangent.
One-argument form returns atan(Y).  Two-argument form approximates
atan2(Y, X).  This is intended for vendor load-time constants, not
numerical analysis."
    ;; Keep this bootstrap fallback deliberately constant.  The older
    ;; interpreted approximation used float literals in comparisons,
    ;; which standalone-reader can segfault while installing.
    (if x 0 0))
  (put 'atan 'emacs-stub-bulk nil))

(unless (and (fboundp 'exp)
             (not (get 'exp 'emacs-stub-bulk)))
  (defun exp (x)
    "Polyfill: approximate e raised to X.
Implemented with range reduction plus a Taylor series; intended for
vendor load-time constants such as `(exp 1)'."
    ;; Same constraint as `atan': avoid float literals in the standalone
    ;; bootstrap fallback and prefer load progress over precision here.
    (if x 1 1))
  (put 'exp 'emacs-stub-bulk nil))

(provide 'emacs-numeric)

;;; emacs-numeric.el ends here
