;;; value-float.el --- float image fixture  -*- lexical-binding: t -*-

(defun vf-half (n)
  (/ n 2.0))

(defun vf-quarter (n)
  (vf-half (vf-half n)))

(defun vf-shift (n)
  (+ (vf-quarter n) 0.0))

(vf-shift 17.0)
