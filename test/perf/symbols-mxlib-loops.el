;;; symbols-mxlib-loops.el --- Doc 226 S2.2 compiled driver loops -*- lexical-binding: t; -*-

;; Appended to notes-65's mxlib.el before GNU 31.1 compiles both, so the
;; timed loop is bytecode in every engine (symbols-check.py speed).
(defun symx-loop-a (n xs) (let ((i 0)) (while (< i n) (mx-a xs) (setq i (1+ i)))))
(defun symx-loop-b (n xs) (let ((i 0)) (while (< i n) (mx-b xs) (setq i (1+ i)))))
