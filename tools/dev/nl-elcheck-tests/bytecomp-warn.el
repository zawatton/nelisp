;;; -*- lexical-binding: t; -*-
(defun warn-a (x)
  (unused-undefined-function x)
  (let ((y 1)) x))
