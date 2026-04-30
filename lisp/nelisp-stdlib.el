;;; nelisp-stdlib.el --- Sweep 9 S0 Elisp stdlib (Rust→Elisp migration)  -*- lexical-binding: t; -*-

(defun identity (x) x)
(defun null (x) (eq x nil))
(defun not (x) (eq x nil))
(defun 1+ (x) (+ x 1))
(defun 1- (x) (- x 1))

;; nelisp-stdlib.el ends here
