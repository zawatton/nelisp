;;; -*- lexical-binding: t; -*-
;; length-first row on a concat result: BENCH_N trips of (length (concat u u u)) with u a 30000-character multibyte string.
(defvar b2-u (make-string 30000 ?é))
(defun b2-run (n) (let ((a 0)) (dotimes (i n) (setq a (+ a (length (concat b2-u b2-u b2-u))))) a))
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (b2-run 2)
  (let* ((start (float-time)) (r (b2-run n)) (seconds (- (float-time) start)))
    (princ (format "BENCH-CHECK n=%d result=%S elapsed=%.9f\n" n r seconds))))
(kill-emacs 0)
