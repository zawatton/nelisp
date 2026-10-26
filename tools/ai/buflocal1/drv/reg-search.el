;;; -*- lexical-binding: t; -*-
;; Regression row: search in a temp buffer (reads case-fold-search, now automatically buffer-local).
(defun bl-search-run (n)
  (let ((a 0))
    (with-temp-buffer
      (insert (make-string 200 ?x) "needle" (make-string 200 ?y))
      (dotimes (_ n)
        (goto-char (point-min))
        (setq a (+ a (re-search-forward "NEEDLE" nil t)))))
    a))
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (bl-search-run 2)
  (let* ((start (float-time)) (r (bl-search-run n)) (seconds (- (float-time) start)))
    (princ (format "BENCH-CHECK n=%d result=%S elapsed=%.9f\n" n r seconds))))
(kill-emacs 0)
