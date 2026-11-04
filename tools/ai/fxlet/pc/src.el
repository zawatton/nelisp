;;; -*- lexical-binding: t; -*-
;; fxlet per-op drivers (one loop per mode), loaded as source (fx flavour).  Per-op Ir = (Ir(hi)-Ir(lo))/(hi-lo)
;; minus mode `empty'.
(defvar fl-s1 1)
(defvar fl-s2 2)
(defvar fl-s3 3)
(defvar fl-bl 7)
(make-variable-buffer-local 'fl-bl)
(defun fl-r-empty (n) (let ((a nil) (i 0)) (while (< i n) (setq a i) (setq i (1+ i))) a))
(defun fl-r-let1 (n) (let ((a nil) (i 0)) (while (< i n) (setq a (let ((fl-s1 i)) fl-s1)) (setq i (1+ i))) a))
(defun fl-r-let3 (n) (let ((a nil) (i 0)) (while (< i n) (setq a (let ((fl-s1 i) (fl-s2 i) (fl-s3 i)) fl-s3)) (setq i (1+ i))) a))
(defun fl-r-letstar3 (n) (let ((a nil) (i 0)) (while (< i n) (setq a (let* ((fl-s1 i) (fl-s2 fl-s1) (fl-s3 fl-s2)) fl-s3)) (setq i (1+ i))) a))
(defun fl-r-letlocal (n) (let ((a nil) (i 0)) (while (< i n) (setq a (let ((fl-bl i)) fl-bl)) (setq i (1+ i))) a))
(defun fl-r-setspecial (n) (let ((a nil) (i 0)) (while (< i n) (setq fl-s1 i) (setq a i) (setq i (1+ i))) a))
