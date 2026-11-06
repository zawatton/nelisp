;;; corpus-subr.el --- subrs are not conses (GNU 31.1 differential)  -*- lexical-binding: t; -*-
;; Run with tools/ai/cl-gnu/run-corpus.el; expected output is GNU 31.1's.
(let ((f (symbol-function 'car))) (list (consp f) (listp f) (atom f) (subrp f) (functionp f) (type-of f)))
(subr-arity (symbol-function 'car))
(subr-name (symbol-function 'car))
(car-safe (symbol-function 'car))
(condition-case e (car (symbol-function 'car)) (error (car e)))
(primitive-function-p (symbol-function 'car))
(cl-typep (symbol-function 'car) 'list)
(mapcar (lambda (o) (if (listp o) 'list 'atom)) (list (lambda () 1) (symbol-function 'cons) '(lambda () 1)))
