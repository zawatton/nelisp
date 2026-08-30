;;; nl-num-complex.el --- Complex values over nl-num reals -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 196 Phase 3.  Complex values are [nl--complex RE IM] tagged plain
;; vectors.  CLHS 12.1.5.2-3 supplies the canonicalization rules: float
;; contagion is bilateral between the two components, an exact zero
;; imaginary component demotes to the real component, and a float zero does
;; not demote.

;;; Code:

(require 'nl-num-rational)

(defun nl-complexp (object)
  "Return non-nil when OBJECT is an nl-num complex vector."
  (nl-num--complex-vector-p object))

(defun nl-complex (real imaginary)
  "Construct a canonical complex number from REAL and IMAGINARY.
Both components must be real nl-num values.  If either is a float, both are
promoted to float.  An exact zero imaginary part demotes to the real value;
a float zero imaginary part remains complex."
  (nl-num--ensure-real real 'nl-complex)
  (nl-num--ensure-real imaginary 'nl-complex)
  (when (or (floatp real) (floatp imaginary))
    (setq real (nl-num--real-to-float real))
    (setq imaginary (nl-num--real-to-float imaginary)))
  (if (nl-num--exact-zero-p imaginary)
      real
    (vector nl-num--complex-tag real imaginary)))

(defun nl-realpart (number)
  "Return NUMBER's real component; a real number is its own realpart."
  (nl-num--ensure-number number 'nl-realpart)
  (if (nl-num--complex-vector-p number) (aref number 1) number))

(defun nl-imagpart (number)
  "Return NUMBER's imaginary component.
A real float has imaginary part 0.0; an exact real has imaginary part 0."
  (nl-num--ensure-number number 'nl-imagpart)
  (cond
   ((nl-num--complex-vector-p number) (aref number 2))
   ((floatp number) 0.0)
   (t 0)))

(defun nl-num--complex-parts (number)
  "Return NUMBER as a (REAL . IMAGINARY) pair."
  (nl-num--ensure-number number 'nl-num--complex-parts)
  (if (nl-num--complex-vector-p number)
      (cons (aref number 1) (aref number 2))
    (cons number (if (floatp number) 0.0 0))))

(defun nl-num--complex-add (a b)
  "Return A+B when at least one operand is complex."
  (let ((ap (nl-num--complex-parts a))
        (bp (nl-num--complex-parts b)))
    (nl-complex (nl-num--real-add (car ap) (car bp))
                (nl-num--real-add (cdr ap) (cdr bp)))))

(defun nl-num--complex-sub (a b)
  "Return A-B when at least one operand is complex."
  (let ((ap (nl-num--complex-parts a))
        (bp (nl-num--complex-parts b)))
    (nl-complex (nl-num--real-sub (car ap) (car bp))
                (nl-num--real-sub (cdr ap) (cdr bp)))))

(defun nl-num--complex-mul (a b)
  "Return A*B when at least one operand is complex."
  (let* ((ap (nl-num--complex-parts a))
         (bp (nl-num--complex-parts b))
         (ar (car ap)) (ai (cdr ap))
         (br (car bp)) (bi (cdr bp)))
    (nl-complex
     (nl-num--real-sub (nl-num--real-mul ar br)
                       (nl-num--real-mul ai bi))
     (nl-num--real-add (nl-num--real-mul ar bi)
                       (nl-num--real-mul ai br)))))

(defun nl-num--complex-div (a b)
  "Return A/B when at least one operand is complex, via the conjugate."
  (let* ((ap (nl-num--complex-parts a))
         (bp (nl-num--complex-parts b))
         (ar (car ap)) (ai (cdr ap))
         (br (car bp)) (bi (cdr bp))
         (denominator
          (nl-num--real-add (nl-num--real-mul br br)
                            (nl-num--real-mul bi bi))))
    (nl-complex
     (nl-num--real-div
      (nl-num--real-add (nl-num--real-mul ar br)
                        (nl-num--real-mul ai bi))
      denominator)
     (nl-num--real-div
      (nl-num--real-sub (nl-num--real-mul ai br)
                        (nl-num--real-mul ar bi))
      denominator))))

(defun nl-num--complex-negate (number)
  "Return -NUMBER for a complex NUMBER."
  (nl-complex (nl-num--real-negate (aref number 1))
              (nl-num--real-negate (aref number 2))))

(defun nl-num-sqrt (number)
  "Return the square root of real NUMBER in the nl-num tower.
A negative real produces a float-component complex value instead of NaN.
Non-negative exact inputs are promoted to float for the square-root step."
  (nl-num--ensure-real number 'nl-num-sqrt)
  (if (nl-num--real-less number 0)
      (nl-complex 0.0
                  (sqrt (nl-num--real-to-float
                         (nl-num--real-negate number))))
    (sqrt (nl-num--real-to-float number))))

(defun nl-magnitude (number)
  "Return NUMBER's magnitude.
Real values use absolute value.  Complex magnitudes are generally floats,
including when both components are exact rationals."
  (nl-num--ensure-number number 'nl-magnitude)
  (if (nl-num--complex-vector-p number)
      (nl-num-sqrt
       (nl-num--real-add
        (nl-num--real-mul (aref number 1) (aref number 1))
        (nl-num--real-mul (aref number 2) (aref number 2))))
    (nl-num--real-abs number)))

(provide 'nl-num-complex)

;;; nl-num-complex.el ends here
