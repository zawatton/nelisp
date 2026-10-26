;;; -*- lexical-binding: t; -*-
(load (expand-file-name "../cfs-bc.el" (file-name-directory load-file-name)) nil t)
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (cfs-read 100)
  (let* ((start (float-time)) (r (cfs-read n)) (seconds (- (float-time) start)))
    (princ (format "BENCH-CHECK n=%d result=%S elapsed=%.9f\n" n r seconds))))
(kill-emacs 0)
