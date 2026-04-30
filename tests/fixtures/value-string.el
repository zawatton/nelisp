;;; value-string.el --- string image fixture  -*- lexical-binding: t -*-

(defun vs-prefix ()
  (concat "hi" "-"))

(defun vs-greet (who)
  (concat (vs-prefix) who))

(defun vs-identity (s)
  s)

(vs-identity (vs-greet "world"))
