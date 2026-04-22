;;; nelisp-forms-test.el --- ERT tests for Week 9-10 special forms  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 Week 9-10: cond / and / or / when / unless / defconst /
;; prog1 / prog2.  Structurally similar to `nelisp-eval-test.el',
;; split off so future additions to the core evaluator stay easy to
;; locate.

;;; Code:

(require 'ert)
(require 'nelisp-eval)

;;; cond --------------------------------------------------------------

(ert-deftest nelisp-eval-cond-matches-second ()
  (should (eq (nelisp-eval '(cond (nil 1) (t 2))) 2)))

(ert-deftest nelisp-eval-cond-no-match-nil ()
  (should (eq (nelisp-eval '(cond (nil 1) (nil 2))) nil)))

(ert-deftest nelisp-eval-cond-test-value-as-result ()
  "A clause with no body returns the test value itself."
  (should (= (nelisp-eval '(cond (42))) 42)))

(ert-deftest nelisp-eval-cond-implicit-progn ()
  (should (= (nelisp-eval '(cond (t 1 2 3))) 3)))

(ert-deftest nelisp-eval-cond-short-circuits ()
  "Clauses after the first match are not evaluated."
  (nelisp--reset)
  (nelisp-eval '(setq counter 0))
  (nelisp-eval '(cond (t :ok)
                      ((progn (setq counter 99) t) :never)))
  (should (= (nelisp-eval 'counter) 0)))

(ert-deftest nelisp-eval-cond-reject-bad-clause ()
  (should-error (nelisp-eval '(cond bad-clause))
                :type 'nelisp-eval-error))

;;; and / or ----------------------------------------------------------

(ert-deftest nelisp-eval-and-empty-is-t ()
  (should (eq (nelisp-eval '(and)) t)))

(ert-deftest nelisp-eval-and-returns-last-truthy ()
  (should (= (nelisp-eval '(and 1 2 3)) 3)))

(ert-deftest nelisp-eval-and-short-circuits ()
  (nelisp--reset)
  (nelisp-eval '(setq touched nil))
  (should (eq (nelisp-eval '(and 1 nil (setq touched t))) nil))
  (should (eq (nelisp-eval 'touched) nil)))

(ert-deftest nelisp-eval-or-empty-is-nil ()
  (should (eq (nelisp-eval '(or)) nil)))

(ert-deftest nelisp-eval-or-returns-first-truthy ()
  (should (= (nelisp-eval '(or nil 1 2)) 1)))

(ert-deftest nelisp-eval-or-short-circuits ()
  (nelisp--reset)
  (nelisp-eval '(setq touched nil))
  (should (= (nelisp-eval '(or nil 42 (setq touched t))) 42))
  (should (eq (nelisp-eval 'touched) nil)))

(ert-deftest nelisp-eval-or-all-falsy ()
  (should (eq (nelisp-eval '(or nil nil nil)) nil)))

;;; when / unless -----------------------------------------------------

(ert-deftest nelisp-eval-when-true-body-as-progn ()
  (should (= (nelisp-eval '(when t 1 2 3)) 3)))

(ert-deftest nelisp-eval-when-false-nil ()
  (should (eq (nelisp-eval '(when nil :never)) nil)))

(ert-deftest nelisp-eval-unless-true-nil ()
  (should (eq (nelisp-eval '(unless t :never)) nil)))

(ert-deftest nelisp-eval-unless-false-body ()
  (should (= (nelisp-eval '(unless nil 1 2 3)) 3)))

;;; defconst / prog1 / prog2 ------------------------------------------

(ert-deftest nelisp-eval-defconst-sets ()
  (nelisp--reset)
  (nelisp-eval '(defconst *c* 3))
  (should (= (nelisp-eval '*c*) 3)))

(ert-deftest nelisp-eval-defconst-reinitializes ()
  "Unlike `defvar', `defconst' re-evaluates and re-sets each time."
  (nelisp--reset)
  (nelisp-eval '(defconst *x* 1))
  (nelisp-eval '(defconst *x* 2))
  (should (= (nelisp-eval '*x*) 2)))

(ert-deftest nelisp-eval-defconst-needs-value ()
  (nelisp--reset)
  (should-error (nelisp-eval '(defconst bare))
                :type 'nelisp-eval-error))

(ert-deftest nelisp-eval-prog1 ()
  (nelisp--reset)
  (nelisp-eval '(setq n 0))
  (should (= (nelisp-eval '(prog1 100 (setq n 1) (setq n 2))) 100))
  (should (= (nelisp-eval 'n) 2)))

(ert-deftest nelisp-eval-prog2 ()
  (should (= (nelisp-eval '(prog2 1 2 3 4)) 2)))

;;; Combined with recursion -------------------------------------------

(ert-deftest nelisp-eval-cond-drives-fib ()
  "cond-based recursion re-implements fib correctly."
  (nelisp--reset)
  (nelisp-eval '(defun fib-cond (n)
                  (cond ((= n 0) 0)
                        ((= n 1) 1)
                        (t (+ (fib-cond (- n 1))
                              (fib-cond (- n 2)))))))
  (should (= (nelisp-eval '(fib-cond 10)) 55)))

(ert-deftest nelisp-eval-when-guarded-recursion ()
  "`when' as a termination guard in a recursive list-builder."
  (nelisp--reset)
  (nelisp-eval '(defun countdown (n)
                  (when (> n 0)
                    (cons n (countdown (- n 1))))))
  (should (equal (nelisp-eval '(countdown 3)) '(3 2 1)))
  (should (eq    (nelisp-eval '(countdown 0)) nil)))

(provide 'nelisp-forms-test)

;;; nelisp-forms-test.el ends here
