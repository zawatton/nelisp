;; Phase 2 Week 1-2 fixture: prove that a multi-form NeLisp source
;; file can be loaded end-to-end and produce side effects (defvars
;; carrying the result of a recursive defun applied at load time).

(defun fib (n)
  (if (< n 2)
      n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defun fact (n)
  (if (= n 0)
      1
    (* n (fact (- n 1)))))

(defvar *fib-result* (fib 15))
(defvar *fact-result* (fact 10))
