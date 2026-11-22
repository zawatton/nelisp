;;; -*- lexical-binding: t; -*-
(defun bad-a (x)
  (let ((y 1))
    (+ x y)
(defun bad-b (x)
  (* x 2)))
