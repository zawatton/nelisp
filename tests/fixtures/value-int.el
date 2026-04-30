;;; value-int.el --- integer image fixture  -*- lexical-binding: t -*-

(defun vi-add3 (a b c)
  (+ a b c))

(defun vi-triangular (n)
  (if (= n 0)
      0
    (+ n (vi-triangular (- n 1)))))

(defun vi-bump-twice (n)
  (1+ (1+ n)))

(vi-add3 10 24 (vi-bump-twice (vi-triangular 3)))
