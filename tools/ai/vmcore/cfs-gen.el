;; -*- lexical-binding: t -*-
(defun cfs-read (n) (let ((s 0) (i 0)) (while (< i n) (if case-fold-search (setq s (1+ s))) (setq i (1+ i))) s))
(defun cfs-base (n) (let ((s 0) (i 0)) (while (< i n) (if i (setq s (1+ s))) (setq i (1+ i))) s))
(setq print-escape-newlines t print-escape-control-characters t print-escape-nonascii t print-escape-multibyte t)
(dolist (f '(cfs-read cfs-base)) (byte-compile f) (prin1 `(fset ',f ,(symbol-function f))) (terpri))
