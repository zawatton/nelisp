;;; -*- lexical-binding: t; -*-
;; Cost row for the body defvar run-length marker (F402, review F2).  A defvar loop of 200000 must not slow
;; the later let of a fresh name: let2000 time at n=200000 / n=0 < 3 prints t.  Timing, not parity: GNU
;; prints nil (its let is linear in the marker list), NeLisp must print t after the fix and nil before it.
(defun rl-run (n)
  (funcall (eval `(lambda (n)
                    (while (> n 0) (defvar rl-g) (setq n (1- n)))
                    (let ((s 0) (i 0) (t0 (float-time)))
                      (while (< i 2000) (let ((rl-q i)) (setq s (+ s rl-q))) (setq i (1+ i)))
                      (- (float-time) t0)))
                 t) n))
(rl-run 0)
(let* ((a (rl-run 0)) (b (rl-run 200000)))
  (princ (format "%-28s %S\n" "C1 let2000 n=2e5/n=0 < 3" (< b (* 3 (max a 0.0005))))))
