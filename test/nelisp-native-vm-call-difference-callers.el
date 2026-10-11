;;; Genuine ordinary CALL subtraction fixtures. -*- lexical-binding: t; -*-
;; The host fixture generator temporarily clears only `-'s byte-compile
;; property.  These calls must remain ordinary CALL, rather than opcode 90.
(defun p35-vm-call-difference (a b) (- a b))
(defun p35-vm-call-zero (function) (funcall function))
(defun p35-vm-call-one (function a) (funcall function a))
(defun p35-vm-call-two (function a b) (funcall function a b))
(defun p35-vm-call-three (function a b c) (funcall function a b c))
