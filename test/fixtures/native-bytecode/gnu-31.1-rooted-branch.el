;;; -*- lexical-binding: t; -*-

(defun gnu-rooted-branch (condition car-value cdr-value)
  (if condition (car car-value) (cdr cdr-value)))
