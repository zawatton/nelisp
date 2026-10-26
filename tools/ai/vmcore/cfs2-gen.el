;; -*- lexical-binding: t -*-
(defvar cfs-plainv 5)
(defvar-local cfs-bl 7)
(defun cfs-plain (n) (let ((s 0) (i 0)) (while (< i n) (if cfs-plainv (setq s (1+ s))) (setq i (1+ i))) s))
(defun cfs-bufloc (n) (let ((s 0) (i 0)) (while (< i n) (if cfs-bl (setq s (1+ s))) (setq i (1+ i))) s))
(setq print-escape-newlines t print-escape-control-characters t print-escape-nonascii t print-escape-multibyte t)
(dolist (f '(cfs-plain cfs-bufloc)) (byte-compile f) (prin1 `(fset ',f ,(symbol-function f))) (terpri))
