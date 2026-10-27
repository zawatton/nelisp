;;; -*- lexical-binding: t; -*-
;; expand-file-name-tilde row (survey3).
(defun b2-run (n) (let ((a 0)) (dotimes (i n) (setq a (+ a (length (expand-file-name "~/x/../y"))))) a))
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (b2-run 2)
  (let* ((start (float-time)) (r (b2-run n)) (seconds (- (float-time) start)))
    (princ (format "BENCH-CHECK n=%d result=%S elapsed=%.9f\n" n r seconds))))
(kill-emacs 0)
