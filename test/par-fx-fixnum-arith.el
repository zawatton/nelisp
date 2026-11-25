;;; par-fx-fixnum-arith.el --- Numeric operations in a VM loop -*- lexical-binding: t; -*-

(defun par-fx-fixnum-arith--step (a b)
  (let ((i 0) result)
    (while (< i 4)
      (setq result (list (+ a b) (- a b) (1+ a) (1- a)
                         (< a b) (= a b) (> a b))
            i (1+ i)))
    result))

(defun par-fx-fixnum-arith-run ()
  (let ((hi most-positive-fixnum) (lo most-negative-fixnum))
    (dolist (args (list '(5 -7) '(-7 -7) '(0 0) '(3.5 2)
                        (list hi 1) (list lo -1) (list hi -1) (list lo 1)
                        (list hi lo) (list lo hi) (list (1+ hi) lo)))
      (prin1 (apply #'par-fx-fixnum-arith--step args))
      (terpri))
    (prin1 (condition-case err
               (par-fx-fixnum-arith--step 'bad 1)
             (wrong-type-argument (car err))))
    (terpri)))
