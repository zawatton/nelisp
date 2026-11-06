;;; bench.el --- hash-table and cons hot-path timings  -*- lexical-binding: t; -*-
;; Prints "BENCH <name> <ms> <checksum>" per workload.
(defun ht-bench--time (name fn)
  (let* ((t0 (float-time)) (r (funcall fn)) (ms (* 1000 (- (float-time) t0))))
    (princ (format "BENCH %s %d %S\n" name (round ms) r))))
(ht-bench--time "puthash-gethash"
  (lambda () (let ((h (make-hash-table)) (s 0))
               (dotimes (i 200000) (puthash i i h))
               (dotimes (i 200000) (setq s (+ s (gethash i h))))
               s)))
(ht-bench--time "equal-strings"
  (lambda () (let ((h (make-hash-table :test 'equal)) (s 0))
               (dotimes (i 20000) (puthash (number-to-string i) i h))
               (dotimes (i 20000) (setq s (+ s (gethash (number-to-string i) h))))
               s)))
(ht-bench--time "consp-listp"
  (lambda () (let ((l (list 1 "a" 'b (cons 1 2) [v] nil)) (n 0))
               (dotimes (_ 200000)
                 (dolist (x l) (when (consp x) (setq n (1+ n))) (when (listp x) (setq n (1+ n)))))
               n)))
(ht-bench--time "car-cdr-walk"
  (lambda () (let ((l (number-sequence 1 1000)) (s 0))
               (dotimes (_ 500) (let ((p l)) (while p (setq s (+ s (car p)) p (cdr p)))))
               s)))
;;; bench.el ends here
