;;; nelisp-reader-test.el --- ERT for nelisp-reader (Phase 7+A)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT smoke for `nelisp-reader' (Doc 40 §3.A LOCKED v2).  Covers the
;; complete public surface (`nelisp-reader-read', `-read-from-string',
;; `-read-from-string-with-position', `-read-all') and every token
;; class required by Phase 7+A.  Total: 60+ tests.
;;
;; These tests deliberately do NOT depend on `nelisp-read.el' (the
;; Phase 3a reader) so a regression in either module is isolatable.

;;; Code:

(require 'ert)
(require 'nelisp-reader)

;;; --- Atom literals --------------------------------------------------

(ert-deftest nelisp-reader-test/atom-nil ()
  (should (eq (nelisp-reader-read "nil") nil))
  (should (eq (nelisp-reader-read "()")  nil)))

(ert-deftest nelisp-reader-test/atom-t ()
  (should (eq (nelisp-reader-read "t") t)))

(ert-deftest nelisp-reader-test/atom-integer ()
  (should (= (nelisp-reader-read "0")    0))
  (should (= (nelisp-reader-read "42")   42))
  (should (= (nelisp-reader-read "+42")  42))
  (should (= (nelisp-reader-read "-7")  -7)))

(ert-deftest nelisp-reader-test/atom-float ()
  (should (= (nelisp-reader-read "3.14") 3.14))
  (should (= (nelisp-reader-read ".5")   0.5))
  (should (= (nelisp-reader-read "1.")   1.0))
  (should (= (nelisp-reader-read "-2.5") -2.5))
  (should (= (nelisp-reader-read "1e10") 1e10))
  (should (= (nelisp-reader-read "1.5e-3") 1.5e-3))
  ;; "1." must be float, not integer 1.
  (should (floatp (nelisp-reader-read "1."))))

(ert-deftest nelisp-reader-test/atom-symbol ()
  (should (eq (nelisp-reader-read "foo")     'foo))
  (should (eq (nelisp-reader-read "bar-baz") 'bar-baz))
  (should (eq (nelisp-reader-read "+")       '+))
  (should (eq (nelisp-reader-read "hello?")  'hello?)))

(ert-deftest nelisp-reader-test/atom-symbol-not-numeric ()
  ;; A token that starts numeric-looking but contains non-numeric
  ;; characters must parse as a symbol, not an integer.
  (should (eq (nelisp-reader-read "1a")  '1a))
  (should (eq (nelisp-reader-read "1-2") '1-2)))

;;; --- String literals ------------------------------------------------

(ert-deftest nelisp-reader-test/string-empty ()
  (should (equal (nelisp-reader-read "\"\"") "")))

(ert-deftest nelisp-reader-test/string-basic ()
  (should (equal (nelisp-reader-read "\"hello\"") "hello"))
  (should (equal (nelisp-reader-read "\"hello world\"") "hello world")))

(ert-deftest nelisp-reader-test/string-escape-backslash ()
  (should (equal (nelisp-reader-read "\"a\\\\b\"") "a\\b")))

(ert-deftest nelisp-reader-test/string-escape-quote ()
  (should (equal (nelisp-reader-read "\"a\\\"b\"") "a\"b")))

(ert-deftest nelisp-reader-test/string-escape-control ()
  (should (equal (nelisp-reader-read "\"\\n\"") "\n"))
  (should (equal (nelisp-reader-read "\"\\t\"") "\t"))
  (should (equal (nelisp-reader-read "\"\\r\"") "\r"))
  (should (equal (nelisp-reader-read "\"\\f\"") "\f"))
  (should (equal (nelisp-reader-read "\"\\a\"") "\a"))
  (should (equal (nelisp-reader-read "\"\\b\"") "\b")))

(ert-deftest nelisp-reader-test/string-escape-octal ()
  ;; \101 = 'A', \12 = newline, \7 = bell.
  (should (equal (nelisp-reader-read "\"\\101\"") "A"))
  (should (equal (nelisp-reader-read "\"\\12\"")  "\n"))
  (should (equal (nelisp-reader-read "\"\\7\"")   "\a")))

(ert-deftest nelisp-reader-test/string-escape-unicode ()
  ;; あ = HIRAGANA LETTER A.
  (should (equal (nelisp-reader-read "\"\\u3042\"")
                 (string ?あ)))
  (should (equal (nelisp-reader-read "\"\\u00e9\"")
                 (string ?é))))

(ert-deftest nelisp-reader-test/string-escape-unicode-U8 ()
  ;; \U0001F600 = grinning face emoji.
  (should (equal (nelisp-reader-read "\"\\U0001F600\"")
                 (string ?\U0001F600))))

(ert-deftest nelisp-reader-test/string-escape-line-continuation ()
  ;; "a\<NL>b" → "ab", the NL and backslash are dropped.
  (should (equal (nelisp-reader-read "\"a\\\nb\"") "ab")))

;;; --- List literals --------------------------------------------------

(ert-deftest nelisp-reader-test/list-empty ()
  (should (equal (nelisp-reader-read "()") nil)))

(ert-deftest nelisp-reader-test/list-singleton ()
  (should (equal (nelisp-reader-read "(a)") '(a))))

(ert-deftest nelisp-reader-test/list-flat ()
  (should (equal (nelisp-reader-read "(a b c)") '(a b c)))
  (should (equal (nelisp-reader-read "(1 2 3)") '(1 2 3))))

(ert-deftest nelisp-reader-test/list-nested ()
  (should (equal (nelisp-reader-read "(a (b c) d)") '(a (b c) d)))
  (should (equal (nelisp-reader-read "((1 2) (3 4))") '((1 2) (3 4)))))

(ert-deftest nelisp-reader-test/list-dotted-pair ()
  (should (equal (nelisp-reader-read "(a . b)") (cons 'a 'b)))
  (should (equal (nelisp-reader-read "(1 . 2)") (cons 1 2))))

(ert-deftest nelisp-reader-test/list-improper ()
  (should (equal (nelisp-reader-read "(a b . c)") '(a b . c)))
  (should (equal (nelisp-reader-read "(1 2 3 . 4)") '(1 2 3 . 4))))

(ert-deftest nelisp-reader-test/list-dot-not-symbol ()
  ;; A dotted-pair `.' is whitespace-delimited; `.foo' / `foo.' are
  ;; ordinary symbols.
  (should (eq (nelisp-reader-read ".foo") '.foo))
  (should (eq (nelisp-reader-read "foo.") 'foo.)))

;;; --- Vector literals ------------------------------------------------

(ert-deftest nelisp-reader-test/vector-empty ()
  (should (equal (nelisp-reader-read "[]") [])))

(ert-deftest nelisp-reader-test/vector-flat ()
  (should (equal (nelisp-reader-read "[1 2 3]") [1 2 3]))
  (should (equal (nelisp-reader-read "[a b c]") [a b c])))

(ert-deftest nelisp-reader-test/vector-nested ()
  (should (equal (nelisp-reader-read "[[1 2] [3 4]]")
                 [[1 2] [3 4]])))

(ert-deftest nelisp-reader-test/vector-mixed ()
  (should (equal (nelisp-reader-read "[(a b) 1 \"x\"]")
                 [(a b) 1 "x"])))

(ert-deftest nelisp-reader-test/vector-hash-alias ()
  ;; #(...) parses as a vector under the Phase 7+A alias.
  (should (equal (nelisp-reader-read "#(1 2 3)") [1 2 3])))

;;; --- Quote / function-quote -----------------------------------------

(ert-deftest nelisp-reader-test/quote-symbol ()
  (should (equal (nelisp-reader-read "'foo") '(quote foo))))

(ert-deftest nelisp-reader-test/quote-list ()
  (should (equal (nelisp-reader-read "'(1 2 3)") '(quote (1 2 3)))))

(ert-deftest nelisp-reader-test/quote-nested ()
  (should (equal (nelisp-reader-read "''x") '(quote (quote x)))))

(ert-deftest nelisp-reader-test/function-quote ()
  (should (equal (nelisp-reader-read "#'foo") '(function foo)))
  (should (equal (nelisp-reader-read "#'(lambda (x) x)")
                 '(function (lambda (x) x)))))

;;; --- Backquote / unquote / splice -----------------------------------

(ert-deftest nelisp-reader-test/backquote-pure ()
  (let ((form (nelisp-reader-read "`(a b c)")))
    (should (equal (eval form t) '(a b c)))))

(ert-deftest nelisp-reader-test/backquote-unquote ()
  (let ((x 99))
    (should (equal (eval (nelisp-reader-read "`(a ,x b)") `((x . ,x)))
                   '(a 99 b)))))

(ert-deftest nelisp-reader-test/backquote-splice ()
  (let ((xs '(1 2 3)))
    (should (equal (eval (nelisp-reader-read "`(a ,@xs b)")
                         `((xs . ,xs)))
                   '(a 1 2 3 b)))))

(ert-deftest nelisp-reader-test/backquote-comma-outside-errors ()
  (should-error (nelisp-reader-read ",foo")
                :type 'nelisp-reader-error)
  (should-error (nelisp-reader-read ",@foo")
                :type 'nelisp-reader-error))

;;; --- Character literals ---------------------------------------------

(ert-deftest nelisp-reader-test/char-ascii ()
  (should (= (nelisp-reader-read "?a") ?a))
  (should (= (nelisp-reader-read "?A") ?A))
  (should (= (nelisp-reader-read "?0") ?0)))

(ert-deftest nelisp-reader-test/char-escape ()
  (should (= (nelisp-reader-read "?\\n") ?\n))
  (should (= (nelisp-reader-read "?\\t") ?\t))
  (should (= (nelisp-reader-read "?\\r") ?\r))
  (should (= (nelisp-reader-read "?\\\\") ?\\))
  (should (= (nelisp-reader-read "?\\\"") ?\")))

(ert-deftest nelisp-reader-test/char-octal ()
  (should (= (nelisp-reader-read "?\\101") ?A))
  (should (= (nelisp-reader-read "?\\12")  ?\n)))

(ert-deftest nelisp-reader-test/char-unicode ()
  (should (= (nelisp-reader-read "?\\u3042") ?あ)))

;;; --- Comments -------------------------------------------------------

(ert-deftest nelisp-reader-test/comment-line ()
  (should (eq (nelisp-reader-read "; a comment\nfoo") 'foo))
  (should (equal (nelisp-reader-read "(1 ; inside\n 2 3)") '(1 2 3))))

(ert-deftest nelisp-reader-test/comment-block ()
  (should (eq (nelisp-reader-read "#| skip |# foo") 'foo))
  (should (equal (nelisp-reader-read "(1 #| skip |# 2)") '(1 2))))

(ert-deftest nelisp-reader-test/comment-block-nested ()
  (should (eq (nelisp-reader-read "#| outer #| inner |# tail |# foo")
              'foo)))

(ert-deftest nelisp-reader-test/comment-block-unterminated-errors ()
  (should-error (nelisp-reader-read "#| unterminated")
                :type 'nelisp-reader-error))

;;; --- Read syntax: integer prefixes ----------------------------------

(ert-deftest nelisp-reader-test/radix-binary ()
  (should (= (nelisp-reader-read "#b1010") 10))
  (should (= (nelisp-reader-read "#B1111") 15))
  (should (= (nelisp-reader-read "#b-1010") -10)))

(ert-deftest nelisp-reader-test/radix-octal ()
  (should (= (nelisp-reader-read "#o17")  15))
  (should (= (nelisp-reader-read "#O777") 511))
  (should (= (nelisp-reader-read "#o-10") -8)))

(ert-deftest nelisp-reader-test/radix-hex ()
  (should (= (nelisp-reader-read "#x1F")  31))
  (should (= (nelisp-reader-read "#xFF")  255))
  (should (= (nelisp-reader-read "#XdeadBEEF") #xdeadbeef))
  (should (= (nelisp-reader-read "#x-FF") -255)))

(ert-deftest nelisp-reader-test/radix-invalid-digit-errors ()
  (should-error (nelisp-reader-read "#b102") :type 'nelisp-reader-error)
  (should-error (nelisp-reader-read "#o89")  :type 'nelisp-reader-error)
  (should-error (nelisp-reader-read "#xGG")  :type 'nelisp-reader-error))

;;; --- Read syntax: record literal ------------------------------------

(ert-deftest nelisp-reader-test/record-bare ()
  (let ((r (nelisp-reader-read "#s(my-struct)")))
    (should (recordp r))
    (should (eq (aref r 0) 'my-struct))))

(ert-deftest nelisp-reader-test/record-with-slots ()
  (let ((r (nelisp-reader-read "#s(point 1 2)")))
    (should (recordp r))
    (should (eq  (aref r 0) 'point))
    (should (eql (aref r 1) 1))
    (should (eql (aref r 2) 2))))

(ert-deftest nelisp-reader-test/record-many-slots ()
  (let ((r (nelisp-reader-read "#s(rec 1 2 3 4 5)")))
    (should (recordp r))
    (should (eq (aref r 0) 'rec))
    (should (= (length r) 6))))

(ert-deftest nelisp-reader-test/record-nested ()
  (let ((r (nelisp-reader-read "#s(outer #s(inner 1) 2)")))
    (should (recordp r))
    (should (recordp (aref r 1)))
    (should (eq (aref (aref r 1) 0) 'inner))))

(ert-deftest nelisp-reader-test/record-string-slot ()
  (let ((r (nelisp-reader-read "#s(named \"hello\")")))
    (should (recordp r))
    (should (equal (aref r 1) "hello"))))

;;; --- Whitespace ----------------------------------------------------

(ert-deftest nelisp-reader-test/whitespace-around ()
  (should (eq (nelisp-reader-read "  foo  ") 'foo))
  (should (equal (nelisp-reader-read "\n\t(1 2)\n") '(1 2))))

;;; --- Error paths ----------------------------------------------------

(ert-deftest nelisp-reader-test/error-empty ()
  (should-error (nelisp-reader-read "") :type 'nelisp-reader-error))

(ert-deftest nelisp-reader-test/error-whitespace-only ()
  (should-error (nelisp-reader-read "   ") :type 'nelisp-reader-error))

(ert-deftest nelisp-reader-test/error-unterminated-list ()
  (should-error (nelisp-reader-read "(1 2") :type 'nelisp-reader-error))

(ert-deftest nelisp-reader-test/error-unterminated-string ()
  (should-error (nelisp-reader-read "\"hello") :type 'nelisp-reader-error))

(ert-deftest nelisp-reader-test/error-unterminated-vector ()
  (should-error (nelisp-reader-read "[1 2") :type 'nelisp-reader-error))

(ert-deftest nelisp-reader-test/error-unexpected-close ()
  (should-error (nelisp-reader-read ")") :type 'nelisp-reader-error)
  (should-error (nelisp-reader-read "]") :type 'nelisp-reader-error))

(ert-deftest nelisp-reader-test/error-trailing-input ()
  (should-error (nelisp-reader-read "foo bar") :type 'nelisp-reader-error))

(ert-deftest nelisp-reader-test/error-dot-before-car ()
  (should-error (nelisp-reader-read "(. 1)") :type 'nelisp-reader-error))

(ert-deftest nelisp-reader-test/error-dot-without-close ()
  (should-error (nelisp-reader-read "(1 . 2 3)") :type 'nelisp-reader-error))

(ert-deftest nelisp-reader-test/error-dot-in-vector ()
  (should-error (nelisp-reader-read "[1 . 2]") :type 'nelisp-reader-error))

(ert-deftest nelisp-reader-test/error-bad-hash ()
  (should-error (nelisp-reader-read "#?") :type 'nelisp-reader-error))

(ert-deftest nelisp-reader-test/error-non-string-input ()
  (should-error (nelisp-reader-read 42)  :type 'wrong-type-argument)
  (should-error (nelisp-reader-read nil) :type 'wrong-type-argument))

(ert-deftest nelisp-reader-test/error-unknown-string-escape ()
  (should-error (nelisp-reader-read "\"\\q\"")
                :type 'nelisp-reader-error))

;;; --- nelisp-reader-read-from-string --------------------------------

(ert-deftest nelisp-reader-test/from-string-basic ()
  (let ((res (nelisp-reader-read-from-string "foo bar")))
    (should (eq (car res) 'foo))
    (should (= (cdr res) 3))))

(ert-deftest nelisp-reader-test/from-string-with-start ()
  (let ((res (nelisp-reader-read-from-string "  (1 2) tail" 2)))
    (should (equal (car res) '(1 2)))
    (should (= (cdr res) 7))))

(ert-deftest nelisp-reader-test/from-string-with-end ()
  (let ((res (nelisp-reader-read-from-string "(1 2) tail" 0 5)))
    (should (equal (car res) '(1 2)))
    (should (= (cdr res) 5))))

;;; --- nelisp-reader-read-from-string-with-position -------------------

(ert-deftest nelisp-reader-test/with-position-basic ()
  (let ((res (nelisp-reader-read-from-string-with-position "foo bar" 0)))
    (should (eq (car res) 'foo))
    (should (= (cdr res) 3))))

(ert-deftest nelisp-reader-test/with-position-mid-string ()
  (let ((res (nelisp-reader-read-from-string-with-position
              "abc 42 def" 4)))
    (should (= (car res) 42))
    (should (= (cdr res) 6))))

(ert-deftest nelisp-reader-test/with-position-pump-multiple ()
  ;; Pump 3 forms out of one buffer using the explicit-position API.
  (let* ((src "(a) (b) (c)")
         (forms nil)
         (pos 0)
         (len (length src)))
    (while (< pos len)
      (let ((skipped (let ((p pos))
                       (while (and (< p len)
                                   (memq (aref src p) '(?\s ?\t ?\n)))
                         (setq p (1+ p)))
                       p)))
        (if (>= skipped len)
            (setq pos len)
          (let ((res (nelisp-reader-read-from-string-with-position
                      src skipped)))
            (push (car res) forms)
            (setq pos (cdr res))))))
    (should (equal (nreverse forms) '((a) (b) (c))))))

(ert-deftest nelisp-reader-test/with-position-rejects-bad-pos ()
  (should-error (nelisp-reader-read-from-string-with-position "foo" -1)
                :type 'wrong-type-argument)
  (should-error (nelisp-reader-read-from-string-with-position "foo" 'x)
                :type 'wrong-type-argument))

;;; --- nelisp-reader-read-all -----------------------------------------

(ert-deftest nelisp-reader-test/read-all-empty ()
  (should (equal (nelisp-reader-read-all "") nil))
  (should (equal (nelisp-reader-read-all "   ") nil)))

(ert-deftest nelisp-reader-test/read-all-multiple ()
  (should (equal (nelisp-reader-read-all "1 2 3") '(1 2 3)))
  (should (equal (nelisp-reader-read-all "(a) (b) (c)")
                 '((a) (b) (c)))))

(ert-deftest nelisp-reader-test/read-all-with-comments ()
  (should (equal (nelisp-reader-read-all
                  "; first\n1 ; mid\n#| skip |# 2 3")
                 '(1 2 3))))

;;; --- Edge cases -----------------------------------------------------

(ert-deftest nelisp-reader-test/edge-deep-nesting ()
  ;; 50 levels of nested lists.
  (let* ((depth 50)
         (open  (make-string depth ?\())
         (close (make-string depth ?\)))
         (src   (concat open "x" close))
         (form  (nelisp-reader-read src)))
    ;; Walk down the tree and confirm depth.
    (let ((cur form)
          (n 0))
      (while (consp cur)
        (setq n (1+ n))
        (setq cur (car cur)))
      (should (= n depth))
      (should (eq cur 'x)))))

(ert-deftest nelisp-reader-test/edge-large-string ()
  ;; A 1024-char string round-trips intact.
  (let* ((body (make-string 1024 ?a))
         (src  (concat "\"" body "\"")))
    (should (equal (nelisp-reader-read src) body))))

(ert-deftest nelisp-reader-test/edge-unicode-symbol ()
  ;; Unicode characters inside a symbol name (Emacs allows this).
  (should (eq (nelisp-reader-read "あいう") 'あいう)))

(ert-deftest nelisp-reader-test/edge-unicode-string-raw ()
  ;; Raw UTF-8 unicode inside a string literal.
  (should (equal (nelisp-reader-read "\"あいう\"") "あいう")))

(ert-deftest nelisp-reader-test/edge-mixed-buffer ()
  ;; Mix several legal top-level forms (line comment + block comment +
  ;; quote + radix integer + record literal) in one read-all pass.
  (let ((forms (nelisp-reader-read-all
                "; lead\n'a #| mid |# foo 3 #b101 #s(p 1)")))
    (should (equal (nth 0 forms) '(quote a)))
    (should (eq    (nth 1 forms) 'foo))
    (should (=     (nth 2 forms) 3))
    (should (=     (nth 3 forms) 5))
    (should (recordp (nth 4 forms)))
    (should (eq    (aref (nth 4 forms) 0) 'p))))

(provide 'nelisp-reader-test)

;;; nelisp-reader-test.el ends here
