;;; value-t.el --- truthy image fixture  -*- lexical-binding: t -*-

(defun vt-greater-p (a b)
  (< b a))

(defun vt-even-p (n)
  (= 0 (mod n 2)))

(defun vt-same-p (a b)
  (eq a b))

(and (vt-greater-p 5 3)
     (vt-even-p 4)
     (vt-same-p 'ok 'ok))
