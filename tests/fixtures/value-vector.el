;;; value-vector.el --- vector image fixture  -*- lexical-binding: t -*-

(defun vv-next (n)
  (1+ n))

(defun vv-first-three (n)
  (vector n
          (vv-next n)
          (vv-next (vv-next n))))

(defun vv-identity (xs)
  xs)

(vv-identity (vv-first-three 10))
