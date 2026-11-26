;;; par-cold-path-subr.el --- Live Subr decisions and packed arities -*- lexical-binding: t; -*-

(defalias 'par-cold-path-op '%)
(defun par-cold-path-dispatch (a b)
  (par-cold-path-op a b))

(defun par-cold-path-numeric (a b)
  (list (% a b) (> a b) (>= a b) (* a b) (>= 1 a b) (= a b b)))

(defun par-cold-path-report-arity (x)
  (condition-case err
      (progn (par-cold-path-op x 3) 'no-error)
    (wrong-number-of-arguments
     (let ((operator (nth 1 err)))
       (list (car err) (if (subrp operator) (subr-name operator) operator)
             (nth 2 err))))))

(dotimes (_ 8)
  (par-cold-path-numeric 7 3)
  (par-cold-path-dispatch 8 3))

(let ((held (symbol-function '%)))
  (prin1
   (list 'dispatch
         (par-cold-path-numeric 7 3)
         (par-cold-path-dispatch 8 3)
         (progn
           (fset 'par-cold-path-op (symbol-function '>))
           (par-cold-path-dispatch 8 3))
         (progn
           (fset 'par-cold-path-op (symbol-function '1+))
           (par-cold-path-report-arity 8))
         (progn
           (fset 'par-cold-path-op (lambda (a b) (list b a)))
           (par-cold-path-dispatch 8 3))
         (progn
           (fset 'par-cold-path-op held)
           (par-cold-path-dispatch 8 3))
         (funcall held 8 3)
         (condition-case err (% 8 0) (arith-error (car err)))))
  (terpri))

(defun par-cold-path-collections (item n)
  (list (vector item item) (make-vector n item) (list item)))

(let* ((item (vector 'retained))
       (result (progn
                 (dotimes (_ 8) (par-cold-path-collections item 3))
                 (par-cold-path-collections item 3))))
  (garbage-collect)
  (prin1
   (list 'constructors
         (eq (aref (nth 0 result) 1) item)
         (eq (aref (nth 1 result) 2) item)
         (eq (car (nth 2 result)) item)
         (length (nth 1 (par-cold-path-collections item 0)))
         (condition-case err
             (par-cold-path-collections item -1)
           (wrong-type-argument (car err)))))
  (terpri))
