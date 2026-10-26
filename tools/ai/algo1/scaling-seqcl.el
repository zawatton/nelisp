;; -*- lexical-binding: t; -*-
;; Scaling: time at N and 4N for seq-uniq, cl-union, cl-remove-duplicates (hash/linear parts).
;; Prints "NAME t1 t4 ratio PASS|FAIL"; exits nonzero when a ratio exceeds LIMIT.
;; seq-uniq (list) must be linear (ratio ~4, limit 8).  cl-union is quadratic in GNU too but its
;; inner scan is the native memq/member, so it must also stay below 8 at these sizes.
;; cl-remove-duplicates is GNU-quadratic with an interpreted inner loop: informational only.
(require 'cl-lib)
(require 'seq)
(defvar algo1-fail nil)
(defun algo1-rl (n m) (let ((s 777) l) (dotimes (_ n) (setq s (% (* s 48271) 2147483647)) (push (% s m) l)) l))
(defun algo1-time (thunk reps)
  (let ((t0 (float-time))) (dotimes (_ reps) (funcall thunk)) (/ (- (float-time) t0) reps)))
(defun algo1-ratio (name mk fn reps limit)
  (let* ((a (funcall mk 1000)) (b (funcall mk 4000))
         (t1 (algo1-time (lambda () (funcall fn a)) reps))
         (t4 (algo1-time (lambda () (funcall fn b)) reps))
         (r (/ t4 (max t1 1e-9))) (ok (< r limit)))
    (unless ok (setq algo1-fail t))
    (princ (format "%s n=1000 %.6fs n=4000 %.6fs ratio %.2f %s\n" name t1 t4 r (if ok "PASS" "FAIL")))))
(algo1-ratio "seq-uniq-list" (lambda (n) (algo1-rl n 100000)) #'seq-uniq 3 8)
(algo1-ratio "delete-dups" (lambda (n) (algo1-rl n 100000)) (lambda (l) (delete-dups (copy-sequence l))) 3 8)
;; per-comparison cost of cl-union: 500x500 random vs the same at 1000x1000 (ratio ~4 means
;; cost per comparison is constant).  Limit 8.
(algo1-ratio "cl-union" (lambda (n) (algo1-rl (/ n 2) 1000000)) (lambda (l) (cl-union l (reverse (cdr l)))) 2 8)
(algo1-ratio "cl-remove-duplicates-informational" (lambda (n) (algo1-rl (/ n 2) 1000000)) #'cl-remove-duplicates 2 1e9)
(kill-emacs (if algo1-fail 1 0))
