;;; -*- lexical-binding: t; -*-
;; prin1-strs-esc row: BENCH_N trips of (length (prin1-to-string 3000 strings with escapes and a non-ASCII char)).
(defvar b2-strs (let (l) (dotimes (i 3000) (push (format "line %d\n\t\"q\" \\ é" i) l)) l))
(defun b2-run (n) (let ((a 0)) (dotimes (i n) (setq a (+ a (length (prin1-to-string b2-strs))))) a))
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (b2-run 2)
  (let* ((start (float-time)) (r (b2-run n)) (seconds (- (float-time) start)))
    (princ (format "BENCH-CHECK n=%d result=%S elapsed=%.9f\n" n r seconds))))
(kill-emacs 0)
