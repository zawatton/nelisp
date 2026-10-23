;;; integ-wh-maxmin.el --- max and min as GNU's Fmax and Fmin (data.c minmax_driver)  -*- lexical-binding: t; -*-

;; Run with `-l' on NeLisp and on GNU Emacs 31.1; both must print the same bytes.  The driver checks and coerces the first
;; argument, then each next one in turn; a value that orders after the accumulator in the wanted direction replaces it, and a
;; NaN (which never orders) is returned at once, the arguments after it unread; the answer is the winning argument itself, so
;; there is no float contagion ((max 3 2.0) is 3) and a marker answers its position.  Cases: mixed integers and floats and
;; ties, signed zeros, NaNs of both signs in every position (the last NaN after the first argument wins, a first-argument
;; NaN stays only while no later one is a NaN, and an argument after a NaN is never coerced), exact comparison of an integer
;; and a float beyond 2^53 and at the fixnum limits, bignums, markers (live and detached), errors naming the first bad
;; argument, arity errors through a direct call, apply and funcall, and the identity of the answer.

(defmacro iwh-mm (form)
  `(princ (format "%-58s => %S\n" ',form (condition-case e ,form (error (list 'ERR (car e) (cdr e)))))))

;; integers and floats, ties, contagion
(iwh-mm (max 3 2.0)) (iwh-mm (max 2.0 3)) (iwh-mm (max 1 2.0)) (iwh-mm (max 2.0 1))
(iwh-mm (min 3 2.0)) (iwh-mm (min 2.0 3)) (iwh-mm (min 1 2.0)) (iwh-mm (min 1.0 1))
(iwh-mm (max 1 1.0)) (iwh-mm (max 1.0 1)) (iwh-mm (min 1 1.0)) (iwh-mm (min 1.0 1))
(iwh-mm (max 5 5.0)) (iwh-mm (max 5.0 5)) (iwh-mm (min 5 5.0)) (iwh-mm (min 5.0 5))
(iwh-mm (max 1)) (iwh-mm (max 1.5)) (iwh-mm (min -3)) (iwh-mm (min 0.0e+NaN))
(iwh-mm (max 1 2 3 4)) (iwh-mm (min 4 3 2 1)) (iwh-mm (max 1 5.5 3 7 2)) (iwh-mm (min 8 5.5 6 9.0 5.5))
(iwh-mm (max 1.5 2)) (iwh-mm (min 2.5 2)) (iwh-mm (max -7 -2 -9)) (iwh-mm (min -7 -2 -9))
;; signed zeros
(iwh-mm (max 0.0 -0.0)) (iwh-mm (max -0.0 0.0)) (iwh-mm (min 0.0 -0.0)) (iwh-mm (min -0.0 0.0)) (iwh-mm (max 0 -0.0)) (iwh-mm (min -0.0 0))
;; infinities
(iwh-mm (max 1.0e+INF 5)) (iwh-mm (min -1.0e+INF 5)) (iwh-mm (max -1.0e+INF 5)) (iwh-mm (min 1.0e+INF 5))
;; NaNs: position and sign decide which one is answered
(iwh-mm (max 0.0e+NaN 1)) (iwh-mm (max 1 0.0e+NaN)) (iwh-mm (min 0.0e+NaN 1)) (iwh-mm (min 1 0.0e+NaN))
(iwh-mm (max 1 0.0e+NaN 2)) (iwh-mm (min 1 0.0e+NaN 2)) (iwh-mm (max 0.0e+NaN 1 -0.0e+NaN)) (iwh-mm (min 0.0e+NaN 1 -0.0e+NaN))
(iwh-mm (max -0.0e+NaN 0.0e+NaN)) (iwh-mm (max 0.0e+NaN -0.0e+NaN)) (iwh-mm (min -0.0e+NaN 0.0e+NaN)) (iwh-mm (min 0.0e+NaN -0.0e+NaN))
(iwh-mm (max 1 2 0.0e+NaN -0.0e+NaN)) (iwh-mm (min 1 2 -0.0e+NaN 0.0e+NaN)) (iwh-mm (max 0.0e+NaN 0.0e+NaN 0.0e+NaN))
(iwh-mm (max 3 0.0e+NaN 1.0e+INF)) (iwh-mm (min 3 0.0e+NaN -1.0e+INF)) (iwh-mm (max 1.0e+INF 0.0e+NaN))
;; the arguments after a NaN are not read: no error for a bad one
(iwh-mm (max 1 0.0e+NaN 'a)) (iwh-mm (min 1 -0.0e+NaN "s" 'b)) (iwh-mm (max 0.0e+NaN 'a)) (iwh-mm (max 'a 0.0e+NaN))
(iwh-mm (max 1 'a 0.0e+NaN)) (iwh-mm (max 0.0e+NaN 1 'a))
;; exact comparison of an integer and a float
(iwh-mm (max 9007199254740993 9007199254740992.0)) (iwh-mm (max 9007199254740992.0 9007199254740993))
(iwh-mm (min 9007199254740993 9007199254740992.0)) (iwh-mm (min 9007199254740992.0 9007199254740993))
(iwh-mm (max most-positive-fixnum (float most-positive-fixnum))) (iwh-mm (min most-positive-fixnum (float most-positive-fixnum)))
(iwh-mm (max most-negative-fixnum (float most-negative-fixnum))) (iwh-mm (min most-negative-fixnum (float most-negative-fixnum)))
(iwh-mm (max most-positive-fixnum 1.0)) (iwh-mm (min most-negative-fixnum -1.0e30))
;; bignums (literals: the reader makes them)
(iwh-mm (max 18446744073709551616 1)) (iwh-mm (max 1 18446744073709551616)) (iwh-mm (min -18446744073709551616 1))
(iwh-mm (max 18446744073709551616 18446744073709551617)) (iwh-mm (min 18446744073709551616 18446744073709551617))
(iwh-mm (max 18446744073709551616 1.0e30)) (iwh-mm (min 18446744073709551616 1.0e30)) (iwh-mm (max 18446744073709551616 1.0e+INF))
(iwh-mm (max 18446744073709551616 0.0e+NaN)) (iwh-mm (max 0.0e+NaN 18446744073709551616)) (iwh-mm (min -18446744073709551616 -1.0e30))
(iwh-mm (max 18446744073709551616 most-positive-fixnum 2.5)) (iwh-mm (min 18446744073709551616 most-negative-fixnum 2.5))
;; markers answer their position
(iwh-mm (with-temp-buffer (insert "abcdef") (let ((m (copy-marker 3))) (list (max m 1) (max 1 m) (min m 10) (min m 1) (max m 2.5) (max 9 m 4) (min m m)))))
(iwh-mm (with-temp-buffer (insert "abc") (let ((m (copy-marker 2))) (list (eq (max m 1) 2) (max m m) (max 1 m 0.5) (min 0.5 m)))))
(iwh-mm (with-temp-buffer (let ((m (make-marker))) (max m 1))))
(iwh-mm (with-temp-buffer (let ((m (make-marker))) (max 1 m))))
(iwh-mm (with-temp-buffer (let ((m (make-marker))) (min 1 0.0e+NaN m))))
(iwh-mm (with-temp-buffer (insert "ab") (let ((m (make-marker))) (max (copy-marker 2) m))))
;; errors name the first bad argument
(iwh-mm (max 1 'a)) (iwh-mm (max 'a 1)) (iwh-mm (min "x" 1)) (iwh-mm (max 1 2 'c 'd)) (iwh-mm (max nil)) (iwh-mm (max t 1))
(iwh-mm (min 1 '(1 2) '(1))) (iwh-mm (max [1] 2)) (iwh-mm (min 1.5 "z"))
;; arity: a direct call, apply and funcall name different things
(iwh-mm (max)) (iwh-mm (min)) (iwh-mm (apply #'max '())) (iwh-mm (apply #'min '())) (iwh-mm (funcall #'max)) (iwh-mm (funcall 'min))
(iwh-mm (apply #'max '(3 9 4))) (iwh-mm (funcall #'min 4 2 8)) (iwh-mm (mapcar (lambda (x) (max x 5)) '(1 6 5.0 7.5)))
(iwh-mm (mapcar (lambda (x) (min x 5)) '(1 6 5.0 7.5))) (iwh-mm (apply #'max 1 2 '(3 0.0e+NaN)))
(iwh-mm (func-arity 'max)) (iwh-mm (func-arity 'min)) (iwh-mm (subrp (symbol-function 'max)))
;; the answer is the winning argument itself
(iwh-mm (let ((a 3.5) (b 2.5)) (list (eq (max a b) a) (eq (min a b) b))))
(iwh-mm (let ((a 2.5) (b 3.5)) (list (eq (max a b) b) (eq (min a b) a))))
(iwh-mm (let ((a 3) (b 3.0)) (list (max a b) (max b a) (min a b) (min b a))))
(iwh-mm (let ((a 0.0e+NaN) (b -0.0e+NaN)) (list (eq (max 1 a b) b) (eq (max a 1) a) (eq (max a b) b))))
(princ "INTEG-WH-MAXMIN DONE\n")
