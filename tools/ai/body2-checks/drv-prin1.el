;;; -*- lexical-binding: t; -*-
;; prin1-strs-esc row (survey3): prin1-to-string of 3000 strings holding escapes and a non-ASCII char.
(defvar b2-strs (let (l) (dotimes (i 3000) (push (format "line %d\n\t\"q\" \\ é" i) l)) l))
(defun b2-run (n) (let ((a 0)) (dotimes (i n) (setq a (+ a (length (prin1-to-string b2-strs))))) a))
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (b2-run 1)
  (princ (format "BENCH-CHECK n=%d result=%S\n" n (b2-run n))))
(kill-emacs 0)
