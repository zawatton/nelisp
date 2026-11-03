;;; kernels.el --- verbatim kernel defuns from source-package.el  -*- lexical-binding: t; -*-
(defun bench-vector-read (n input) (let ((i 0) (s 0)) (while (< i n) (setq s (logand (+ s (aref input (logand i 1023))) 16777215) i (1+ i))) (cons i s)))
(defun bench-vector-write (n input) (let ((i 0) (v input)) (while (< i n) (aset v (logand i 1023) (logand i 65535)) (setq i (1+ i))) (cons i v)))
(defun bench-vector-allocate64 (n input) (let ((i 0) (ring input) v) (while (< i n) (setq v (make-vector 64 0)) (aset v 0 (logand i 65535)) (aset ring (logand i 63) v) (setq i (1+ i))) (cons i ring)))
(defun bench-string-concat16 (n input) (let ((i 0) (s 0) (a (aref input 1)) (b (aref input 2)) text) (while (< i n) (setq text (concat (aref a (logand i 15)) (aref b (logand (+ i 5) 15))) s (logand (+ s (aref text (logand i 15))) 16777215) i (1+ i))) (cons i s)))
(defun bench-string-slice16 (n input) (let ((i 0) (s 0) (table (aref input 0)) text) (while (< i n) (setq text (substring (aref table (logand i 15)) 8 24) s (logand (+ s (aref text (logand i 15))) 16777215) i (1+ i))) (cons i s)))
(defun bench-hash-get1024 (n input) (let ((i 0) (s 0) (table (aref input 0)) (lookups (aref input 1))) (while (< i n) (setq s (logand (+ s (gethash (aref lookups (logand i 1023)) table)) 16777215) i (1+ i))) (cons i s)))
