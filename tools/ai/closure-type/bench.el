;;; bench.el --- closure and subr call hot-path timings  -*- lexical-binding: t; -*-
;; Prints "BENCH <name> <ms> <checksum>" per workload.
(defun cl-bench--time (name fn)
  (let* ((t0 (float-time)) (r (funcall fn)) (ms (* 1000 (- (float-time) t0))))
    (princ (format "BENCH %s %d %S\n" name (round ms) r))))
(cl-bench--time "funcall-closure"
  (lambda () (let* ((k 3) (f (lambda (x) (+ x k))) (s 0))
               (dotimes (i 300000) (setq s (funcall f s)))
               s)))
(cl-bench--time "make-closure"
  (lambda () (let ((s 0))
               (dotimes (i 200000) (let ((f (lambda () i))) (setq s (+ s (funcall f)))))
               s)))
(cl-bench--time "mapcar-lambda"
  (lambda () (let ((l (number-sequence 1 1000)) (s 0))
               (dotimes (_ 200) (setq s (+ s (apply #'+ (mapcar (lambda (x) (* 2 x)) l)))))
               s)))
(cl-bench--time "funcall-subr"
  (lambda () (let ((f (symbol-function 'cons)) (s nil))
               (dotimes (i 300000) (setq s (funcall f i nil)))
               s)))
;;; bench.el ends here
