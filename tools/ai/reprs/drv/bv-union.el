;;; -*- lexical-binding: t; -*-
;; reprs S4: bool-vector-union of two 1000-bit vectors per trip (word-wise driver in GNU data.c).
(defvar bvu-a (make-bool-vector 1000 t))
(defvar bvu-b (make-bool-vector 1000 nil))
(defun bvu-run (n) (let ((c 0)) (dotimes (_ n) (setq c (+ c (length (bool-vector-union bvu-a bvu-b))))) c))
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (bvu-run 2)
  (let* ((start (float-time)) (r (bvu-run n)) (seconds (- (float-time) start)))
    (princ (format "BENCH-CHECK n=%d result=%S elapsed=%.9f\n" n r seconds))))
(kill-emacs 0)
