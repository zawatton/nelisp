;;; -*- lexical-binding: t; -*-
;; First `length' of a fresh multibyte string.  BENCH_N = characters, LEN_MEASURE=1 also calls `length' once.
(let* ((n (string-to-number (getenv "BENCH_N"))) (s (make-string n ?é)))
  (if (equal (getenv "LEN_MEASURE") "1") (princ (format "LEN %d\n" (length s)))))
(kill-emacs 0)
