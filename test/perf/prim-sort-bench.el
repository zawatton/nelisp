;;; prim-sort-bench.el --- fixed sort workloads -*- lexical-binding: t -*-

(defvar ps-strings
  (mapcar (lambda (i) (format "symbol-name-%04d-x" (% (* i 7919) 1000)))
          (number-sequence 1 34)))
(defvar ps-integers
  (mapcar (lambda (i) (% (* i 7919) 1000)) (number-sequence 1 1000)))
(defun ps-false (_a _b) nil)
(defun ps-operation (name)
  (cond ((equal name "strings") (lambda () (sort (copy-sequence ps-strings) #'string-lessp)))
        ((equal name "integers") (lambda () (sort (copy-sequence ps-integers) #'<)))
        ((equal name "machinery") (lambda () (sort (copy-sequence ps-strings) #'ps-false)))
        ((equal name "comparison") (lambda () (string-lessp "symbol-name-0001-x" "symbol-name-0002-x")))
        ((equal name "predicate") (lambda () (funcall #'ps-false 1 2)))
        ((equal name "floor") (lambda () nil))
        (t (error "Unknown sort benchmark %s" name))))
(let* ((name (or (getenv "PS_CASE") "strings"))
       (count (string-to-number (or (getenv "PS_N") "3")))
       (function (ps-operation name)))
  (funcall function)
  (let ((start (float-time)))
    (dotimes (_ count) (funcall function))
    (princ (format "PS-TIME %s %.6f\n" name
                   (* 1000000.0 (/ (- (float-time) start) count))))))
(princ "PS-COMPLETE\n")
(kill-emacs 0)
