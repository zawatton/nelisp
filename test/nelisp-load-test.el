;;; nelisp-load-test.el --- ERT tests for Phase 2 multi-form loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 2 Week 1-2 — multi-form parsing, `nelisp-load-string', and
;; `nelisp-load-file'.  Fixture files live in `test/fixtures/*.nl'.

;;; Code:

(require 'ert)
(require 'nelisp-load)

(defconst nelisp-load-test--fixtures-dir
  (expand-file-name
   "fixtures"
   (file-name-directory (or load-file-name buffer-file-name))))

(defun nelisp-load-test--fixture (name)
  "Expand NAME against the fixtures directory."
  (expand-file-name name nelisp-load-test--fixtures-dir))

;;; nelisp-read-all ---------------------------------------------------

(ert-deftest nelisp-read-all-empty ()
  (should (equal (nelisp-read-all "") nil))
  (should (equal (nelisp-read-all "   ") nil))
  (should (equal (nelisp-read-all "; only a comment\n") nil)))

(ert-deftest nelisp-read-all-single ()
  (should (equal (nelisp-read-all "42") '(42))))

(ert-deftest nelisp-read-all-multiple ()
  (should (equal (nelisp-read-all "1 2 3") '(1 2 3))))

(ert-deftest nelisp-read-all-mixed-whitespace ()
  (should (equal (nelisp-read-all "\n\n(a b)\n\n(c)\n")
                 '((a b) (c)))))

(ert-deftest nelisp-read-all-with-comments ()
  (should (equal
           (nelisp-read-all
            "; heading\n(defun f () 1) ; end\n(defvar x 2)\n")
           '((defun f () 1) (defvar x 2)))))

(ert-deftest nelisp-read-all-rejects-non-string ()
  (should-error (nelisp-read-all 42) :type 'wrong-type-argument))

;;; nelisp-load-string ------------------------------------------------

(ert-deftest nelisp-load-string-empty-returns-nil ()
  (nelisp--reset)
  (should (eq (nelisp-load-string "") nil))
  (should (eq (nelisp-load-string "   \n; nothing\n") nil)))

(ert-deftest nelisp-load-string-returns-last-value ()
  (nelisp--reset)
  (should (= (nelisp-load-string "1 2 3") 3)))

(ert-deftest nelisp-load-string-installs-defun ()
  (nelisp--reset)
  (nelisp-load-string "(defun double (x) (* 2 x))")
  (should (= (nelisp-eval '(double 21)) 42)))

(ert-deftest nelisp-load-string-persists-state-across-forms ()
  "A later form sees a defun / defvar from an earlier form in the
same `nelisp-load-string' call."
  (nelisp--reset)
  (nelisp-load-string
   "(defun triple (x) (* 3 x))
    (defvar *t* (triple 14))")
  (should (= (nelisp-eval '*t*) 42)))

(ert-deftest nelisp-load-string-with-dynamic-binding ()
  "Specials declared in the string behave dynamically in later forms."
  (nelisp--reset)
  (nelisp-load-string
   "(defvar *depth* 0)
    (defun bump () (setq *depth* (+ *depth* 1)))
    (bump) (bump) (bump)")
  (should (= (nelisp-eval '*depth*) 3)))

;;; nelisp-load-file --------------------------------------------------

(ert-deftest nelisp-load-file-fib-fixture ()
  "The fib / fact fixture evaluates end-to-end."
  (nelisp--reset)
  (nelisp-load-file (nelisp-load-test--fixture "fib.nl"))
  (should (= (nelisp-eval '*fib-result*) 610))
  (should (= (nelisp-eval '*fact-result*) 3628800)))

(ert-deftest nelisp-load-file-stdlib-fixture ()
  "`my-foldl' written in pure NeLisp powers sum / max / length."
  (nelisp--reset)
  (nelisp-load-file (nelisp-load-test--fixture "stdlib.nl"))
  (should (= (nelisp-eval '*sample-sum*) 31))
  (should (= (nelisp-eval '*sample-max*) 9))
  (should (= (nelisp-eval '*sample-length*) 8)))

(ert-deftest nelisp-load-file-missing-raises ()
  (should-error (nelisp-load-file "/nonexistent/nelisp/file.nl")
                :type 'file-error))

;;; Interaction with the rest of the subsystem ------------------------

(ert-deftest nelisp-load-then-macro ()
  "Load a file that installs a defun, then call it through a macro
installed after the load."
  (nelisp--reset)
  (nelisp-load-file (nelisp-load-test--fixture "fib.nl"))
  (nelisp-eval '(defmacro guard-fib (n)
                  (list (quote if) (list (quote <) n 0) 0
                        (list (quote fib) n))))
  (should (= (nelisp-eval '(guard-fib 10)) 55))
  (should (= (nelisp-eval '(guard-fib -5)) 0)))

(provide 'nelisp-load-test)

;;; nelisp-load-test.el ends here
