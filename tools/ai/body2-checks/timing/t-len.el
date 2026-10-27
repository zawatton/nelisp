;;; -*- lexical-binding: t; -*-
;; length-first row: BENCH_N trips of a fresh 90000-character multibyte string and its first `length'.
(defun b2-run (n) (let ((a 0)) (dotimes (i n) (setq a (+ a (length (make-string 90000 ?é))))) a))
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (b2-run 2)
  (let* ((start (float-time)) (r (b2-run n)) (seconds (- (float-time) start)))
    (princ (format "BENCH-CHECK n=%d result=%S elapsed=%.9f\n" n r seconds))))
(kill-emacs 0)
