;; -*- lexical-binding: t; -*-
;; Per-call cost checks that are red on the v17 baseline: 17-digit float parse and
;; prin1 of a non-ASCII string.  Prints "NAME us/call LIMIT PASS|FAIL"; exit 1 on a failure.
;; The loop is inline in a function body (no closure call per iteration).
(defvar algo1-fail nil)
(defvar algo1-ml (number-sequence 1 500))
(defmacro algo1-limit (name n limit form)
  `(progn
     ,form
     (let ((t0 (float-time)))
       (dotimes (_ ,n) ,form)
       (let* ((us (/ (* 1e6 (- (float-time) t0)) ,n)) (ok (< us ,limit)))
         (unless ok (setq algo1-fail t))
         (princ (format "%s %.1f us/call limit %d %s\n" ,name us ,limit (if ok "PASS" "FAIL")))))))
(defun algo1-run ()
  (algo1-limit "string-to-number-17-digits" 1000 30 (string-to-number "369.63000000000005"))
  (algo1-limit "read-from-string-17-digits" 1000 60 (read-from-string "369.63000000000005"))
  (algo1-limit "prin1-nonascii-string" 1000 40 (prin1-to-string "line 5 é"))
  (algo1-limit "prin1-list-of-nonascii-strings" 200 100 (prin1-to-string (list "line 5 é" "line 6 é" "line 7 é" "line 8 é" "line 9 é")))
  (algo1-limit "format-S-nonascii-string" 1000 40 (format "%S" "line 5 é"))
  (algo1-limit "memql-500-element-list" 1000 100 (memql 777 algo1-ml)))
(algo1-run)
(kill-emacs (if algo1-fail 1 0))
