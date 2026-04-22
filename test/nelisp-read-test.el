;;; nelisp-read-test.el --- ERT tests for the NeLisp reader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 Week 3-4 reader tests.  Covers every feature listed in
;; `src/nelisp-read.el' Commentary plus the error paths.

;;; Code:

(require 'ert)
(require 'nelisp-read)

;;; Atoms --------------------------------------------------------------

(ert-deftest nelisp-read-nil ()
  (should (eq (nelisp-read "nil") nil))
  (should (eq (nelisp-read "()") nil)))

(ert-deftest nelisp-read-t ()
  (should (eq (nelisp-read "t") t)))

(ert-deftest nelisp-read-positive-int ()
  (should (equal (nelisp-read "0") 0))
  (should (equal (nelisp-read "1") 1))
  (should (equal (nelisp-read "42") 42))
  (should (equal (nelisp-read "+42") 42)))

(ert-deftest nelisp-read-negative-int ()
  (should (equal (nelisp-read "-1") -1))
  (should (equal (nelisp-read "-42") -42)))

(ert-deftest nelisp-read-symbol ()
  (should (eq (nelisp-read "foo") 'foo))
  (should (eq (nelisp-read "bar-baz") 'bar-baz))
  (should (eq (nelisp-read "+") '+))
  (should (eq (nelisp-read "-") '-))
  (should (eq (nelisp-read "a.b") 'a.b))
  (should (eq (nelisp-read "hello?") 'hello?)))

(ert-deftest nelisp-read-symbol-not-integer ()
  "A token that looks numeric but isn't must parse as a symbol."
  (should (eq (nelisp-read "1a") '1a))
  (should (eq (nelisp-read "1-2") '1-2)))

;;; Strings ------------------------------------------------------------

(ert-deftest nelisp-read-string-empty ()
  (should (equal (nelisp-read "\"\"") "")))

(ert-deftest nelisp-read-string-basic ()
  (should (equal (nelisp-read "\"hello\"") "hello")))

(ert-deftest nelisp-read-string-escapes ()
  (should (equal (nelisp-read "\"a\\\"b\"") "a\"b"))
  (should (equal (nelisp-read "\"\\n\"") "\n"))
  (should (equal (nelisp-read "\"\\t\"") "\t"))
  (should (equal (nelisp-read "\"\\r\"") "\r"))
  (should (equal (nelisp-read "\"\\\\\"") "\\")))

;;; Lists --------------------------------------------------------------

(ert-deftest nelisp-read-list-empty ()
  (should (equal (nelisp-read "()") nil)))

(ert-deftest nelisp-read-list-proper ()
  (should (equal (nelisp-read "(1 2 3)") '(1 2 3)))
  (should (equal (nelisp-read "(+ 1 2)") '(+ 1 2))))

(ert-deftest nelisp-read-list-nested ()
  (should (equal (nelisp-read "((1 2) (3 4))") '((1 2) (3 4))))
  (should (equal (nelisp-read "(a (b (c)))") '(a (b (c))))))

(ert-deftest nelisp-read-list-dotted-pair ()
  (should (equal (nelisp-read "(1 . 2)") (cons 1 2))))

(ert-deftest nelisp-read-list-improper ()
  (should (equal (nelisp-read "(1 2 . 3)") '(1 2 . 3))))

(ert-deftest nelisp-read-list-dot-is-not-symbol ()
  "`.' preceded by whitespace and followed by whitespace is a dotted-pair
marker, not the symbol `.'.  But `.foo' or `foo.' is still a symbol."
  (should (eq (nelisp-read ".foo") '.foo))
  (should (eq (nelisp-read "foo.") 'foo.))
  (should (equal (nelisp-read "(a .foo)") '(a .foo))))

;;; Quote shorthand ----------------------------------------------------

(ert-deftest nelisp-read-quote ()
  (should (equal (nelisp-read "'foo") '(quote foo)))
  (should (equal (nelisp-read "'(1 2 3)") '(quote (1 2 3))))
  (should (equal (nelisp-read "'nil") '(quote nil)))
  (should (equal (nelisp-read "''x") '(quote (quote x)))))

;;; Whitespace and comments --------------------------------------------

(ert-deftest nelisp-read-whitespace-around ()
  (should (eq (nelisp-read "  foo  ") 'foo))
  (should (equal (nelisp-read "\n\t(1 2)\n") '(1 2))))

(ert-deftest nelisp-read-line-comment ()
  (should (eq (nelisp-read "; a comment\nfoo") 'foo))
  (should (equal (nelisp-read "(1 ; inside\n 2 3)") '(1 2 3))))

;;; Errors -------------------------------------------------------------

(ert-deftest nelisp-read-error-empty ()
  (should-error (nelisp-read "") :type 'nelisp-read-error))

(ert-deftest nelisp-read-error-whitespace-only ()
  (should-error (nelisp-read "   ") :type 'nelisp-read-error)
  (should-error (nelisp-read "; comment only\n") :type 'nelisp-read-error))

(ert-deftest nelisp-read-error-unterminated-list ()
  (should-error (nelisp-read "(1 2") :type 'nelisp-read-error))

(ert-deftest nelisp-read-error-unterminated-string ()
  (should-error (nelisp-read "\"hello") :type 'nelisp-read-error))

(ert-deftest nelisp-read-error-unexpected-close ()
  (should-error (nelisp-read ")") :type 'nelisp-read-error))

(ert-deftest nelisp-read-error-trailing-input ()
  (should-error (nelisp-read "foo bar") :type 'nelisp-read-error))

(ert-deftest nelisp-read-error-unknown-escape ()
  (should-error (nelisp-read "\"\\x\"") :type 'nelisp-read-error))

(ert-deftest nelisp-read-error-dot-before-car ()
  (should-error (nelisp-read "(. 1)") :type 'nelisp-read-error))

(ert-deftest nelisp-read-error-dot-without-close ()
  (should-error (nelisp-read "(1 . 2 3)") :type 'nelisp-read-error))

(ert-deftest nelisp-read-error-non-string-input ()
  (should-error (nelisp-read 42) :type 'wrong-type-argument))

;;; Streaming API -----------------------------------------------------

(ert-deftest nelisp-read-from-string-basic ()
  (let ((res (nelisp-read-from-string "foo bar")))
    (should (eq (car res) 'foo))
    (should (= (cdr res) 3))))

(ert-deftest nelisp-read-from-string-with-start ()
  (let ((res (nelisp-read-from-string "  (1 2) tail" 2)))
    (should (equal (car res) '(1 2)))
    (should (= (cdr res) 7))))

(ert-deftest nelisp-read-from-string-skips-leading-ws ()
  (let ((res (nelisp-read-from-string "   42 rest")))
    (should (equal (car res) 42))
    (should (= (cdr res) 5))))

(provide 'nelisp-read-test)

;;; nelisp-read-test.el ends here
