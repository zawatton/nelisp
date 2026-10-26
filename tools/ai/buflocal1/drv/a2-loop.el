;;; -*- lexical-binding: t; -*-
;; The survey4 A2 loop: with-temp-buffer + insert + setq-local, per iteration.
(defun bl-a2-run (n)
  (let ((a 0))
    (dotimes (k n)
      (with-temp-buffer (insert "abc") (setq-local fill-column k) (setq a (+ a fill-column))))
    a))
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (bl-a2-run 2)
  (let* ((start (float-time)) (r (bl-a2-run n)) (seconds (- (float-time) start)))
    (princ (format "BENCH-CHECK n=%d result=%S elapsed=%.9f\n" n r seconds))))
(kill-emacs 0)
