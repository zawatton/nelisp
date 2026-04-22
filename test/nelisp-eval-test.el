;;; nelisp-eval-test.el --- ERT tests for the NeLisp evaluator  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 Week 5-8 evaluator tests.  Every test that mutates global
;; state (defun / defvar / global setq) calls `nelisp--reset' first so
;; the order of ERT selection does not matter.

;;; Code:

(require 'ert)
(require 'nelisp-eval)

;;; Self-evaluating and variable lookup -------------------------------

(ert-deftest nelisp-eval-self-evaluating ()
  (should (= (nelisp-eval 42) 42))
  (should (equal (nelisp-eval "hello") "hello"))
  (should (eq  (nelisp-eval nil) nil))
  (should (eq  (nelisp-eval t) t))
  (should (eq  (nelisp-eval :foo) :foo)))

(ert-deftest nelisp-eval-unbound-symbol ()
  (nelisp--reset)
  (should-error (nelisp-eval 'undefined-var)
                :type 'nelisp-unbound-variable))

(ert-deftest nelisp-eval-void-function ()
  (nelisp--reset)
  (should-error (nelisp-eval '(no-such-fn 1 2))
                :type 'nelisp-void-function))

;;; quote / function --------------------------------------------------

(ert-deftest nelisp-eval-quote ()
  (should (equal (nelisp-eval '(quote (1 2 3))) '(1 2 3)))
  (should (eq    (nelisp-eval '(quote foo)) 'foo)))

(ert-deftest nelisp-eval-function-symbol ()
  (nelisp--reset)
  (should (functionp (nelisp-eval '(function +)))))

(ert-deftest nelisp-eval-function-lambda ()
  (nelisp--reset)
  (let ((cl (nelisp-eval '(function (lambda (x) x)))))
    (should (eq (car cl) 'nelisp-closure))))

;;; if / progn --------------------------------------------------------

(ert-deftest nelisp-eval-if-true ()
  (should (= (nelisp-eval '(if t 1 2)) 1)))

(ert-deftest nelisp-eval-if-false ()
  (should (= (nelisp-eval '(if nil 1 2)) 2)))

(ert-deftest nelisp-eval-if-no-else ()
  (should (eq (nelisp-eval '(if nil 1)) nil)))

(ert-deftest nelisp-eval-if-multi-else ()
  "If else branch has several forms, it acts as an implicit progn."
  (should (= (nelisp-eval '(if nil :never 1 2 3)) 3)))

(ert-deftest nelisp-eval-progn ()
  (should (= (nelisp-eval '(progn 1 2 3)) 3))
  (should (eq (nelisp-eval '(progn)) nil)))

;;; let / let* --------------------------------------------------------

(ert-deftest nelisp-eval-let-basic ()
  (should (= (nelisp-eval '(let ((x 1) (y 2)) (+ x y))) 3)))

(ert-deftest nelisp-eval-let-bare-symbol-binds-nil ()
  (should (eq (nelisp-eval '(let (x) x)) nil)))

(ert-deftest nelisp-eval-let-parallel ()
  "In let, all init forms see the outer binding of x."
  (should (= (nelisp-eval '(let ((x 10))
                             (let ((x 1) (y x))
                               y)))
             10)))

(ert-deftest nelisp-eval-let*-sequential ()
  (should (= (nelisp-eval '(let* ((x 1) (y (+ x 1))) y)) 2)))

;;; lambda / closure -------------------------------------------------

(ert-deftest nelisp-eval-lambda-call ()
  (should (= (nelisp-eval '((lambda (x y) (+ x y)) 3 4)) 7)))

(ert-deftest nelisp-eval-lambda-captures-lexical ()
  (should (= (nelisp-eval '(let ((k 10))
                             ((lambda (x) (+ x k)) 5)))
             15)))

(ert-deftest nelisp-eval-lambda-rest ()
  (should (equal (nelisp-eval '((lambda (&rest xs) xs) 1 2 3))
                 '(1 2 3))))

(ert-deftest nelisp-eval-lambda-optional ()
  (should (equal (nelisp-eval '((lambda (x &optional y) (list x y)) 1))
                 '(1 nil)))
  (should (equal (nelisp-eval '((lambda (x &optional y) (list x y)) 1 2))
                 '(1 2))))

(ert-deftest nelisp-eval-lambda-too-few ()
  (should-error (nelisp-eval '((lambda (x y) x) 1))
                :type 'nelisp-eval-error))

(ert-deftest nelisp-eval-lambda-too-many ()
  (should-error (nelisp-eval '((lambda (x) x) 1 2))
                :type 'nelisp-eval-error))

;;; defun / setq / defvar / while ------------------------------------

(ert-deftest nelisp-eval-defun-and-call ()
  (nelisp--reset)
  (nelisp-eval '(defun add1 (n) (+ n 1)))
  (should (= (nelisp-eval '(add1 5)) 6)))

(ert-deftest nelisp-eval-defvar-sets-global ()
  (nelisp--reset)
  (nelisp-eval '(defvar *counter* 0))
  (should (= (nelisp-eval '*counter*) 0)))

(ert-deftest nelisp-eval-defvar-respects-existing ()
  "defvar does not overwrite an already-bound global."
  (nelisp--reset)
  (nelisp-eval '(defvar *x* 1))
  (nelisp-eval '(defvar *x* 999))
  (should (= (nelisp-eval '*x*) 1)))

(ert-deftest nelisp-eval-setq-lexical ()
  (should (= (nelisp-eval '(let ((x 0)) (setq x 42) x)) 42)))

(ert-deftest nelisp-eval-setq-global-fallback ()
  (nelisp--reset)
  (nelisp-eval '(setq g 7))
  (should (= (nelisp-eval 'g) 7)))

(ert-deftest nelisp-eval-while-counts ()
  (nelisp--reset)
  (nelisp-eval '(setq n 0))
  (nelisp-eval '(while (< n 5) (setq n (+ n 1))))
  (should (= (nelisp-eval 'n) 5)))

;;; Phase 1 anchor programs ------------------------------------------

(ert-deftest nelisp-eval-fib ()
  "Canonical Phase 1 success anchor (docs/03-architecture.org §7.2)."
  (nelisp--reset)
  (nelisp-eval '(defun fib (n)
                  (if (< n 2)
                      n
                    (+ (fib (- n 1)) (fib (- n 2))))))
  (should (= (nelisp-eval '(fib 10)) 55))
  (should (= (nelisp-eval '(fib 15)) 610)))

(ert-deftest nelisp-eval-factorial ()
  (nelisp--reset)
  (nelisp-eval '(defun fact (n)
                  (if (= n 0)
                      1
                    (* n (fact (- n 1))))))
  (should (= (nelisp-eval '(fact 5)) 120))
  (should (= (nelisp-eval '(fact 10)) 3628800)))

(ert-deftest nelisp-eval-mutual-recursion-via-defun ()
  "even? and odd? must resolve via the function table at call time,
not captured at defun time — the second defun installs AFTER the
first but the first still sees it because lookup is dynamic."
  (nelisp--reset)
  (nelisp-eval '(defun my-even? (n) (if (= n 0) t (my-odd? (- n 1)))))
  (nelisp-eval '(defun my-odd?  (n) (if (= n 0) nil (my-even? (- n 1)))))
  (should (eq (nelisp-eval '(my-even? 4)) t))
  (should (eq (nelisp-eval '(my-odd? 5)) t))
  (should (eq (nelisp-eval '(my-even? 7)) nil)))

(ert-deftest nelisp-eval-closure-counter ()
  "Closure captures lexical env; mutation through `setq' on the captured
binding is visible on each invocation.  Tightens the lexical contract."
  (nelisp--reset)
  (nelisp-eval '(defun make-counter (start)
                  (let ((n start))
                    (lambda ()
                      (setq n (+ n 1))
                      n))))
  (nelisp-eval '(defvar *c* (make-counter 10)))
  (should (= (nelisp-eval '(funcall *c*)) 11))
  (should (= (nelisp-eval '(funcall *c*)) 12))
  (should (= (nelisp-eval '(funcall *c*)) 13)))

(ert-deftest nelisp-eval-apply-splices-last ()
  (should (= (nelisp-eval '(apply (function +) 1 2 (quote (3 4))))
             10)))

;;; Eval from reader --------------------------------------------------

(ert-deftest nelisp-eval-string-roundtrip ()
  (should (= (nelisp-eval-string "(+ 1 2 3 4)") 10))
  (should (equal (nelisp-eval-string "(let ((x 5)) (list x x))")
                 '(5 5))))

(provide 'nelisp-eval-test)

;;; nelisp-eval-test.el ends here
