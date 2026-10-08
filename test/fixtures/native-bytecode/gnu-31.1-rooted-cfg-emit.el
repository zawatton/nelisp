;;; gnu-31.1-rooted-cfg-emit.el --- genuine GNU31 rooted CFG fixtures -*- lexical-binding: t; -*-

(defun gnu-rooted-cfg-two-diamonds
    (condition-a left-a right-a condition-b left-b right-b)
  (cons (car (if condition-a left-a right-a))
        (cdr (if condition-b left-b right-b))))

(defun gnu-rooted-cfg-carried-phi (a x y b z)
  (cons (if a x y) (if b z (if a x y))))

(provide 'gnu-31.1-rooted-cfg-emit)
