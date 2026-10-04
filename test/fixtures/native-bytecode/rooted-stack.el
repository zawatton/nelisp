;;; -*- lexical-binding: t; -*-

(defun nelisp-rooted-stack-car-cdr-cons (a b)
  (cons (car a) (cdr b)))

(defun nelisp-rooted-stack-constant-chain (a)
  (cons nil (car (cdr a))))

(defun nelisp-rooted-stack-nested-cons (a b)
  (cons a (cons b (cdr b))))

(defun nelisp-rooted-stack-duplicate-arg (a)
  (cons (car a) (car a)))

(defun nelisp-rooted-stack-documented-car (x)
  "Return X's car for the source-free native compiler fixture."
  (car x))
