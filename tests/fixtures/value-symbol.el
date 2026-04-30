;;; value-symbol.el --- symbol image fixture  -*- lexical-binding: t -*-

(defun vy-yes ()
  'yes)

(defun vy-no ()
  'no-no-no)

(defun vy-pick (a b)
  (if (eq a b)
      (vy-yes)
    (vy-no)))

(vy-pick 1 1)
