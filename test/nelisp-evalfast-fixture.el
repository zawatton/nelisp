;;; nelisp-evalfast-fixture.el --- Interpreter environment parity -*- lexical-binding: t; -*-

(defvar eft-fixture-failures 0)
(defvar eft-fixture-count 0)
(defun eft-check (name actual expected)
  (setq eft-fixture-count (1+ eft-fixture-count))
  (if (equal actual expected)
      (princ (format "EVALFAST %s PASS %S\n" name actual))
    (setq eft-fixture-failures (1+ eft-fixture-failures))
    (princ (format "EVALFAST %s FAIL actual=%S expected=%S\n" name actual expected))))

(eft-check 'shadowing
           (let ((x 7)) (list (let ((x 11)) (setq x 13) x) x)) '(13 7))
(eft-check 'outer-frame-write
           (let ((x 7)) (let ((y 9)) (setq x (+ x y))) x) 16)
(eft-check 'captured-after-pop
           (let ((f (let ((x 10)) (lambda (d) (setq x (+ x d)) x))))
             (garbage-collect) (list (funcall f 1) (funcall f 2))) '(11 13))
(eft-check 'captured-outer-write
           (let ((x 7))
             (let ((f (lambda () (setq x (1+ x)))))
               (funcall f) (funcall f) x)) 9)
(fset 'eft-recursive
      (lambda (n) (let ((x n)) (if (= n 0) x (+ x (eft-recursive (1- n)))))))
(eft-check 'recursive-frames (eft-recursive 10) 55)
(defvar eft-special 31)
(eft-check 'dynamic-special (list (let ((eft-special 41)) (setq eft-special 43) eft-special)
                                 eft-special) '(43 31))
(makunbound 'eft-later-special)
(eft-check 'special-shadowing-lexical
           (eval '(let ((eft-later-special 10))
                    (defvar eft-later-special)
                    (let ((eft-later-special 20))
                      (list eft-later-special (symbol-value 'eft-later-special)))) t)
           '(10 20))
(makunbound 'eft-undefined)
(eft-check 'undefined-setq (progn (eval '(setq eft-undefined 73) t)
                                 (list (boundp 'eft-undefined) (symbol-value 'eft-undefined)))
           '(t 73))
(eft-check 'sequential-pairs (let ((x 0) (y 0)) (setq x 3 y (+ x 4)) (list x y)) '(3 7))
(eft-check 'rhs-writes-binding (let ((x 1)) (setq x (progn (setq x 2) 3)) x) 3)
(eft-check 'rhs-gc (let ((x nil)) (setq x (progn (garbage-collect) (list 3 5))) x) '(3 5))
(eft-check 'setq-signal
           (condition-case e (let ((x 1)) (setq x (error "eft")) x) (error (car e))) 'error)
(eft-check 'odd-setq
           (let ((x 0))
             (list (condition-case e (eval '(setq x 1 x) (list (cons 'x x)))
                     (wrong-number-of-arguments (car e)))))
           '(wrong-number-of-arguments))
(eft-check 'dotted-setq
           (condition-case e (eval '(setq x 1 . 2) t) (wrong-type-argument (car e)))
           'wrong-type-argument)
(eft-check 'while-gc
           (let ((i 0) (x nil))
             (while (< i 12) (setq x (list i x)) (garbage-collect) (setq i (1+ i)))
             (list i (car x))) '(12 11))
(eft-check 'while-nil (while nil (error "unreachable")) nil)
(eft-check 'while-signal
           (condition-case e (while t (signal 'quit nil)) (quit (car e))) 'quit)
(defvar eft-local 5)
(eft-check 'buffer-local-setq
           (let ((old (current-buffer)) (b (generate-new-buffer " eft-local")))
             (set-buffer b) (make-local-variable 'eft-local) (setq eft-local 9)
             (let ((local eft-local))
               (set-buffer old) (kill-buffer b) (list local eft-local))) '(9 5))
(princ (format "EVALFAST SUMMARY checks=%d failures=%d\n" eft-fixture-count eft-fixture-failures))
(kill-emacs (if (= eft-fixture-failures 0) 0 1))
