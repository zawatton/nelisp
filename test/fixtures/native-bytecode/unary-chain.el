;;; -*- lexical-binding: t; -*-
(defun nelisp-chain-even (value) (car (cdr value)))
(defun nelisp-chain-odd (value) (cdr (car (cdr value))))
(defun nelisp-chain-error-stop (value) (car (cdr value)))
(defun nelisp-chain-intermediate-error (value) (car (cdr (cdr value))))
(provide 'nelisp-unary-chain-fixture)
