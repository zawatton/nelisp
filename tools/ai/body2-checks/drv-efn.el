;;; -*- lexical-binding: t; -*-
;; expand-file-name-tilde row (survey3).
(defun b2-run (n) (let ((a 0)) (dotimes (i n) (setq a (+ a (length (expand-file-name "~/x/../y"))))) a))
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (b2-run 2)
  (princ (format "BENCH-CHECK n=%d result=%S\n" n (b2-run n))))
(kill-emacs 0)
