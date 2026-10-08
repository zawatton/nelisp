;;; gnu-bytecode-quo.el --- GNU byte-code quotient fixtures -*- lexical-binding: t; -*-

(defun nl-quo-fixture-divide (a b)
  (/ a b))

(defun nl-quo-fixture-negate (x)
  (- x))

(defconst nl-quo-fixture-positive (nl-quo-fixture-divide 7 2))
(defconst nl-quo-fixture-negative (nl-quo-fixture-divide -7 2))
(defconst nl-quo-fixture-float (nl-quo-fixture-divide 7 2.0))
(defconst nl-quo-fixture-negated-int (nl-quo-fixture-negate 7))
(defconst nl-quo-fixture-negated-float (nl-quo-fixture-negate 2.5))

(provide 'gnu-bytecode-quo)
;;; gnu-bytecode-quo.el ends here
