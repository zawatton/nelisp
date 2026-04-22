;;; nelisp-reader-ext-test.el --- ERT tests for Phase 2 reader extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 2 Week 3-6 reader additions: floats, character literals,
;; and backquote / unquote / splice with one level of nesting.  Each
;; backquote test also evaluates the expansion so the behaviour is
;; pinned end-to-end, not just at the reader layer.

;;; Code:

(require 'ert)
(require 'nelisp-read)
(require 'nelisp-eval)

;;; Floats ------------------------------------------------------------

(ert-deftest nelisp-reader-float-basic ()
  (should (equal (nelisp-read "3.14") 3.14))
  (should (equal (nelisp-read "-2.5") -2.5))
  (should (equal (nelisp-read "+1.5") 1.5)))

(ert-deftest nelisp-reader-float-edge-forms ()
  (should (equal (nelisp-read ".5")  0.5))
  (should (equal (nelisp-read "1.")  1.0))
  (should (equal (nelisp-read "-.25") -0.25)))

(ert-deftest nelisp-reader-float-scientific ()
  (should (equal (nelisp-read "1e3")    1000.0))
  (should (equal (nelisp-read "1.5e-2") 0.015))
  (should (equal (nelisp-read "-2E10")  -2e10)))

(ert-deftest nelisp-reader-integer-still-integer ()
  "A token without dot or exponent still returns an integer, not a float."
  (let ((v (nelisp-read "42")))
    (should (integerp v))
    (should (= v 42))))

(ert-deftest nelisp-reader-float-not-symbol ()
  "Shapes that look numeric must not intern as symbols."
  (should (floatp (nelisp-read "3.14")))
  (should (integerp (nelisp-read "-7"))))

;;; Character literals ------------------------------------------------

(ert-deftest nelisp-reader-char-literal-letters ()
  (should (= (nelisp-read "?a") ?a))
  (should (= (nelisp-read "?Z") ?Z))
  (should (= (nelisp-read "?0") ?0)))

(ert-deftest nelisp-reader-char-escape-common ()
  (should (= (nelisp-read "?\\n")  ?\n))
  (should (= (nelisp-read "?\\t")  ?\t))
  (should (= (nelisp-read "?\\r")  ?\r))
  (should (= (nelisp-read "?\\\\") ?\\))
  (should (= (nelisp-read "?\\\"") ?\"))
  (should (= (nelisp-read "?\\'")  ?\')))

(ert-deftest nelisp-reader-char-escape-space ()
  (should (= (nelisp-read "?\\s") ?\s)))

(ert-deftest nelisp-reader-char-literal-in-list ()
  (should (equal (nelisp-read "(?a ?b ?c)") (list ?a ?b ?c))))

(ert-deftest nelisp-reader-char-literal-at-eof ()
  (should-error (nelisp-read "?") :type 'nelisp-read-error))

;;; Backquote — reader level --------------------------------------------

(ert-deftest nelisp-reader-bq-atom-quotes-it ()
  "`atom expands to the equivalent of 'atom."
  (should (equal (nelisp-read "`foo") '(quote foo))))

(ert-deftest nelisp-reader-bq-empty-list ()
  "Backquoted empty list expands to `(quote nil)', which still
evaluates to nil."
  (nelisp--reset)
  (should (eq (nelisp-eval (nelisp-read "`()")) nil)))

(ert-deftest nelisp-reader-bq-list-no-unquote ()
  "Backquoted list with no unquotes expands to a nested cons chain.
The expansion evaluates to the same list as a plain quote."
  (nelisp--reset)
  (should (equal (nelisp-eval (nelisp-read "`(a b c)")) '(a b c))))

(ert-deftest nelisp-reader-bq-unquote-inline ()
  (nelisp--reset)
  (nelisp-eval '(defvar *x* 42))
  (should (equal (nelisp-eval (nelisp-read "`(a ,*x* c)"))
                 '(a 42 c))))

(ert-deftest nelisp-reader-bq-splice-middle ()
  (nelisp--reset)
  (nelisp-eval '(defvar *mid* (list 1 2 3)))
  (should (equal (nelisp-eval (nelisp-read "`(a ,@*mid* z)"))
                 '(a 1 2 3 z))))

(ert-deftest nelisp-reader-bq-splice-tail ()
  (nelisp--reset)
  (nelisp-eval '(defvar *tail* (list 8 9)))
  (should (equal (nelisp-eval (nelisp-read "`(a b ,@*tail*)"))
                 '(a b 8 9))))

(ert-deftest nelisp-reader-bq-splice-only ()
  "Splice as the sole element reduces to the spliced form itself."
  (nelisp--reset)
  (nelisp-eval '(defvar *xs* (list 1 2 3)))
  (should (equal (nelisp-eval (nelisp-read "`(,@*xs*)")) '(1 2 3))))

(ert-deftest nelisp-reader-bq-unquote-multiple ()
  (nelisp--reset)
  (nelisp-eval '(defvar *x* 10))
  (nelisp-eval '(defvar *y* 20))
  (should (equal (nelisp-eval (nelisp-read "`(,*x* ,*y* ,(+ *x* *y*))"))
                 '(10 20 30))))

;;; Backquote integration with macros ---------------------------------

(ert-deftest nelisp-reader-bq-macro-unless ()
  "Canonical defmacro with backquote now fits in one line."
  (nelisp--reset)
  (nelisp-eval
   (nelisp-read
    "(defmacro bq-unless (c &rest body) `(if ,c nil ,@body))"))
  (should (= (nelisp-eval (nelisp-read "(bq-unless nil 1 2 3)"))
             3))
  (should (eq (nelisp-eval (nelisp-read "(bq-unless t :never)"))
              nil)))

(ert-deftest nelisp-reader-bq-macro-when ()
  (nelisp--reset)
  (nelisp-eval
   (nelisp-read
    "(defmacro bq-when (c &rest body) `(if ,c (progn ,@body) nil))"))
  (should (= (nelisp-eval (nelisp-read "(bq-when t 1 2 3)")) 3))
  (should (eq (nelisp-eval (nelisp-read "(bq-when nil :never)")) nil)))

;;; Backquote errors ---------------------------------------------------

(ert-deftest nelisp-reader-unquote-outside-backquote ()
  (should-error (nelisp-read ",x")   :type 'nelisp-read-error)
  (should-error (nelisp-read ",@x")  :type 'nelisp-read-error))

(ert-deftest nelisp-reader-splice-outside-list ()
  "A bare `,@x' directly after a backquote (not in a list) is invalid."
  (should-error (nelisp-read "`,@x") :type 'nelisp-read-error))

(provide 'nelisp-reader-ext-test)

;;; nelisp-reader-ext-test.el ends here
