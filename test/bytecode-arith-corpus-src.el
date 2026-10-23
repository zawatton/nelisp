;;; bytecode-arith-corpus-src.el --- Bnegate, Bmax, Bmin, Bquo and Brem against GNU's byte-code VM  -*- lexical-binding: t -*-

;; Source of test/bytecode-arith-corpus.eld (host GNU Emacs, from the repository root):
;;   BWC_SRC=test/bytecode-arith-corpus-src.el BWC_OUT=test/bytecode-arith-corpus.eld \
;;     emacs --batch -Q -l test/bytecode-word-corpus-gen.el
;; Each function compiles to the opcode it is named after (bytecode.c: Bnegate, Bmax, Bmin, Bquo, Brem).  GNU runs the fixnum case
;; in the loop, and any other operand, a zero divisor and the one overflowing quotient (MOST_NEGATIVE_FIXNUM / -1) through the
;; function; the cases cross those lines on both sides, with floats, NaNs, bignums, markers and wrong types.

(defun bac-neg (a) (- a))
(defun bac-max (a b) (max a b))
(defun bac-min (a b) (min a b))
(defun bac-quo (a b) (/ a b))
(defun bac-rem (a b) (% a b))
(defun bac-loop (n)
  (let ((i 0) (s 0))
    (while (< i n)
      (setq s (+ s (max i 7) (min i 7) (/ (+ i 100) 7) (% (+ i 100) 7) (- i)))
      (setq i (1+ i)))
    s))
(defun bac-loop-mixed (n)
  (let ((i 0) (s 0.0))
    (while (< i n)
      (setq s (+ s (max i 2.5) (min i 2.5) (/ (+ i 100) 7.0) (- (float i))))
      (setq i (1+ i)))
    s))
(defun bac-markers ()
  (with-temp-buffer
    (insert "abcdef")
    (let ((m (copy-marker 3)))
      (list (bac-max m 1) (bac-max 1 m) (bac-min m 10) (bac-min 10 m) (bac-quo m 2) (bac-quo 7 m) (bac-rem m 2) (bac-rem 7 m)
            (bac-neg m) (bac-max m 2.5) (bac-min m 2.5)))))
(defun bac-detached-marker ()
  (with-temp-buffer
    (let ((m (make-marker))) (list (condition-case e (bac-max m 1) (error (car e))) (condition-case e (bac-neg m) (error (car e)))))))
(defun bac-chain (a b c)
  (list (bac-max (bac-max a b) c) (bac-min (bac-min a b) c) (bac-neg (bac-neg a)) (bac-quo (bac-quo a b) c) (bac-rem (bac-rem a b) c)))

(defconst bwc-cases
  '((bac-neg 0) (bac-neg 1) (bac-neg -1) (bac-neg 5) (bac-neg -5) (bac-neg 2305843009213693951) (bac-neg -2305843009213693951)
    (bac-neg -2305843009213693952) (bac-neg 2305843009213693952) (bac-neg 2.5) (bac-neg -2.5) (bac-neg 0.0) (bac-neg -0.0)
    (bac-neg 1.0e+INF) (bac-neg 0.0e+NaN) (bac-neg 18446744073709551616) (bac-neg -18446744073709551616)
    (bac-neg a) (bac-neg nil) (bac-neg "s") (bac-neg (1))
    (bac-max 1 2) (bac-max 2 1) (bac-max 3 3) (bac-max -1 -2) (bac-max -2 -1) (bac-max 0 -1) (bac-max 0 0)
    (bac-max 2305843009213693951 -2305843009213693952) (bac-max -2305843009213693952 2305843009213693951)
    (bac-max 2305843009213693951 2305843009213693951) (bac-max -2305843009213693952 -2305843009213693952)
    (bac-max 1 2.0) (bac-max 2.0 1) (bac-max 3 2.0) (bac-max 2.0 3) (bac-max 1 1.0) (bac-max 1.0 1) (bac-max 0.0 -0.0) (bac-max -0.0 0.0)
    (bac-max 1 0.0e+NaN) (bac-max 0.0e+NaN 1) (bac-max 0.0e+NaN -0.0e+NaN) (bac-max -0.0e+NaN 0.0e+NaN) (bac-max 1.0e+INF 5)
    (bac-max 2305843009213693951 2.305843009213694e+18) (bac-max 2.305843009213694e+18 2305843009213693951)
    (bac-max 9007199254740993 9007199254740992.0) (bac-max 18446744073709551616 1) (bac-max 1 18446744073709551616)
    (bac-max 18446744073709551616 18446744073709551617) (bac-max 18446744073709551616 1.0e30) (bac-max 18446744073709551616 0.0e+NaN)
    (bac-max 1 a) (bac-max a 1) (bac-max nil 1) (bac-max 1 "s")
    (bac-min 1 2) (bac-min 2 1) (bac-min 3 3) (bac-min -1 -2) (bac-min -2 -1) (bac-min 0 -1) (bac-min 0 0)
    (bac-min 2305843009213693951 -2305843009213693952) (bac-min -2305843009213693952 2305843009213693951)
    (bac-min 1 2.0) (bac-min 2.0 1) (bac-min 3 2.0) (bac-min 2.0 3) (bac-min 1 1.0) (bac-min 1.0 1) (bac-min 0.0 -0.0) (bac-min -0.0 0.0)
    (bac-min 1 0.0e+NaN) (bac-min 0.0e+NaN 1) (bac-min 0.0e+NaN -0.0e+NaN) (bac-min -0.0e+NaN 0.0e+NaN) (bac-min -1.0e+INF 5)
    (bac-min 2305843009213693951 2.305843009213694e+18) (bac-min 9007199254740993 9007199254740992.0)
    (bac-min 18446744073709551616 1) (bac-min -18446744073709551616 1) (bac-min 18446744073709551616 18446744073709551617)
    (bac-min 18446744073709551616 1.0e30) (bac-min 1 a) (bac-min a 1) (bac-min "s" 1)
    (bac-quo 7 2) (bac-quo -7 2) (bac-quo 7 -2) (bac-quo -7 -2) (bac-quo 0 5) (bac-quo 0 -5) (bac-quo 5 0) (bac-quo 0 0) (bac-quo -5 0)
    (bac-quo 6 3) (bac-quo -6 3) (bac-quo 1 2) (bac-quo -1 2) (bac-quo 1 -2) (bac-quo 5 5) (bac-quo 5 -5)
    (bac-quo 2305843009213693951 1) (bac-quo 2305843009213693951 -1) (bac-quo -2305843009213693951 -1) (bac-quo -2305843009213693952 1)
    (bac-quo -2305843009213693952 -1) (bac-quo -2305843009213693952 2) (bac-quo -2305843009213693952 -2) (bac-quo -2305843009213693952 2305843009213693951)
    (bac-quo 2305843009213693951 2305843009213693951) (bac-quo 2305843009213693951 2) (bac-quo -2305843009213693952 3)
    (bac-quo 7.0 2) (bac-quo 7 2.0) (bac-quo 7 0.0) (bac-quo -7 0.0) (bac-quo 0 0.0) (bac-quo 7.5 2.5) (bac-quo 0.0e+NaN 2) (bac-quo 5 0.0e+NaN)
    (bac-quo 18446744073709551616 2) (bac-quo 18446744073709551616 1.5) (bac-quo a 2) (bac-quo 2 a) (bac-quo nil 1) (bac-quo 1 "s")
    (bac-rem 7 3) (bac-rem -7 3) (bac-rem 7 -3) (bac-rem -7 -3) (bac-rem 0 3) (bac-rem 3 0) (bac-rem 0 0) (bac-rem -3 0)
    (bac-rem 6 3) (bac-rem -6 3) (bac-rem 1 2) (bac-rem -1 2) (bac-rem 1 -2) (bac-rem 5 5) (bac-rem 5 -5) (bac-rem 5 1) (bac-rem 5 -1)
    (bac-rem 2305843009213693951 2) (bac-rem -2305843009213693952 2) (bac-rem -2305843009213693952 -1) (bac-rem -2305843009213693952 1)
    (bac-rem 2305843009213693951 -1) (bac-rem 2305843009213693951 2305843009213693951) (bac-rem -2305843009213693952 2305843009213693951)
    (bac-rem 2305843009213693951 -2305843009213693952) (bac-rem -2305843009213693952 -2305843009213693952) (bac-rem 7 8)
    (bac-rem 7.0 2) (bac-rem 7 2.0) (bac-rem 7.5 2) (bac-rem 0.0e+NaN 2) (bac-rem 18446744073709551616 7) (bac-rem a 2) (bac-rem 2 a) (bac-rem nil 1)
    (bac-loop 0) (bac-loop 1) (bac-loop 10) (bac-loop 500) (bac-loop-mixed 0) (bac-loop-mixed 10) (bac-loop-mixed 200)
    (bac-markers) (bac-detached-marker)
    (bac-chain 100 7 3) (bac-chain -100 7 3) (bac-chain 100 -7 3) (bac-chain 5 0 3) (bac-chain 5 3 0) (bac-chain 2305843009213693951 -1 2)
    (bac-chain 5.5 2 3) (bac-chain 5 2 3.5)))

;;; bytecode-arith-corpus-src.el ends here
