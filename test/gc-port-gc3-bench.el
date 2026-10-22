;;; gc-port-gc3-bench.el --- three explicit collections of the Doc 220 churn, timed  -*- lexical-binding: t; -*-

;; The churn of tools/ai/gc-port-prof.py (600 allocations x 3 rounds of a cons, a 128-byte string or a 16-slot
;; vector, each kept in a 64-slot root vector) runs once as a warm-up, then three explicit collections are timed
;; with float-time.  Prints "GC3 KIND SECONDS" per kind; tools/ai/gc-port-gc3.py runs it on GNU Emacs and on
;; the candidate in alternating rounds.
(setq gc-cons-threshold 1073741824 gc-cons-percentage 1.0)
(defvar study-roots (make-vector 64 nil))
(defvar study-checksum 0)
(defun gc3-churn (kind)
  (dotimes (r 3)
    (dotimes (i 600)
      (let ((x (cond ((eq kind 'cons) (cons i i)) ((eq kind 'string) (make-string 128 120)) (t (make-vector 16 i)))))
        (aset study-roots (% i 64) x)
        (setq study-checksum (+ study-checksum i))))))
(dolist (kind '(cons string vector))
  (gc3-churn kind)
  (garbage-collect)
  (let ((t0 (float-time)))
    (garbage-collect) (garbage-collect) (garbage-collect)
    (princ (format "GC3 %s %.6f\n" kind (- (float-time) t0)))))
nil
