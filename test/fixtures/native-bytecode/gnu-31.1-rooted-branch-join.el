;;; -*- lexical-binding: t; -*-

(defun gnu-rooted-branch-join-car (condition left right)
  (car (if condition left right)))

(defun gnu-rooted-branch-join-cdr (condition left right)
  (cdr (if condition left right)))
