;;; nelisp-phase3a-reader-test.el --- ERT tests for Phase 3a reader completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 3a reader completions:
;;
;;   - #x / #o / #b numeric prefixes (hex / octal / binary)
;;   - vector literal [...]
;;   - nested backquote (depth > 1)
;;
;; See docs/design/07-phase3a-reader-completion.org for scope.

;;; Code:

(require 'ert)
(require 'nelisp-read)
(require 'nelisp-eval)

;;; Radix numeric prefixes --------------------------------------------

(ert-deftest nelisp-phase3a-hex-basic ()
  (should (equal (nelisp-read "#x0")   #x0))
  (should (equal (nelisp-read "#x1F")  #x1F))
  (should (equal (nelisp-read "#xFF")  255))
  (should (equal (nelisp-read "#x100") 256)))

(ert-deftest nelisp-phase3a-hex-case-insensitive ()
  (should (equal (nelisp-read "#X1f")  31))
  (should (equal (nelisp-read "#xaBcD") #xABCD))
  (should (equal (nelisp-read "#XFACE") #xFACE)))

(ert-deftest nelisp-phase3a-hex-signed ()
  (should (equal (nelisp-read "#x-1F") -31))
  (should (equal (nelisp-read "#x+10")  16)))

(ert-deftest nelisp-phase3a-oct-basic ()
  (should (equal (nelisp-read "#o0")    0))
  (should (equal (nelisp-read "#o10")   8))
  (should (equal (nelisp-read "#o777") 511))
  (should (equal (nelisp-read "#O17")  15)))

(ert-deftest nelisp-phase3a-bin-basic ()
  (should (equal (nelisp-read "#b0")     0))
  (should (equal (nelisp-read "#b1010") 10))
  (should (equal (nelisp-read "#B111")   7)))

(ert-deftest nelisp-phase3a-radix-in-list ()
  (should (equal (nelisp-read "(#x10 #o10 #b10)") '(16 8 2)))
  (should (equal (nelisp-read "(1 #xA 2)") '(1 10 2))))

(ert-deftest nelisp-phase3a-radix-errors ()
  (should-error (nelisp-read "#xG")   :type 'nelisp-read-error)
  (should-error (nelisp-read "#b2")   :type 'nelisp-read-error)
  (should-error (nelisp-read "#o9")   :type 'nelisp-read-error)
  (should-error (nelisp-read "#x")    :type 'nelisp-read-error)
  (should-error (nelisp-read "#x ")   :type 'nelisp-read-error))

;;; Vector literal ----------------------------------------------------

(ert-deftest nelisp-phase3a-vector-empty ()
  (should (equal (nelisp-read "[]") [])))

(ert-deftest nelisp-phase3a-vector-basic ()
  (should (equal (nelisp-read "[1 2 3]") [1 2 3]))
  (should (equal (nelisp-read "[a b c]") [a b c]))
  (should (equal (nelisp-read "[\"x\" \"y\"]") ["x" "y"])))

(ert-deftest nelisp-phase3a-vector-mixed-types ()
  (should (equal (nelisp-read "[1 \"s\" foo ?x 3.14]")
                 (vector 1 "s" 'foo ?x 3.14))))

(ert-deftest nelisp-phase3a-vector-nested ()
  (should (equal (nelisp-read "[[1 2] [3 4]]")
                 (vector [1 2] [3 4])))
  (should (equal (nelisp-read "[(a b) [c d]]")
                 (vector '(a b) [c d]))))

(ert-deftest nelisp-phase3a-vector-in-list ()
  (should (equal (nelisp-read "(a [1 2] b)")
                 (list 'a [1 2] 'b))))

(ert-deftest nelisp-phase3a-vector-whitespace ()
  (should (equal (nelisp-read "[  1   2   3  ]") [1 2 3]))
  (should (equal (nelisp-read "[\n1\n2\n]") [1 2])))

(ert-deftest nelisp-phase3a-vector-errors ()
  (should-error (nelisp-read "[1 2")    :type 'nelisp-read-error)
  (should-error (nelisp-read "[1 . 2]") :type 'nelisp-read-error))

;;; Nested backquote --------------------------------------------------
;;
;; Reference semantics: each test's expected value is produced by the
;; host Emacs reader via `read' so the NeLisp reader's expansion is
;; validated at the eval level against canonical Elisp output.

(defun nelisp-phase3a--bq-eval (src)
  "Read SRC with `nelisp-read' and evaluate the expansion."
  (eval (nelisp-read src) t))

(defun nelisp-phase3a--host-eval (src)
  "Read SRC with host `read' and evaluate in host Emacs."
  (eval (read src) t))

(ert-deftest nelisp-phase3a-bq-depth1-baseline ()
  ;; Make sure depth=1 behaviour still matches host.
  (let ((x 42))
    (should (equal (nelisp-phase3a--bq-eval "`(a ,x b)")
                   (nelisp-phase3a--host-eval "`(a ,x b)")))))

(ert-deftest nelisp-phase3a-bq-depth2-basic ()
  (should (equal (nelisp-phase3a--bq-eval "`(a `(b c) d)")
                 (nelisp-phase3a--host-eval "`(a `(b c) d)"))))

(ert-deftest nelisp-phase3a-bq-depth2-inner-unquote ()
  ;; Inner `,x' is quoted one level from outer perspective.
  (let ((x 7))
    (should (equal (nelisp-phase3a--bq-eval "`(a `(b ,x) c)")
                   (nelisp-phase3a--host-eval "`(a `(b ,x) c)")))))

(ert-deftest nelisp-phase3a-bq-depth2-outer-unquote ()
  (let ((x 7))
    (should (equal (nelisp-phase3a--bq-eval "`(a ,x `(b c))")
                   (nelisp-phase3a--host-eval "`(a ,x `(b c))")))))

(ert-deftest nelisp-phase3a-bq-depth3-smoke ()
  (should (equal (nelisp-phase3a--bq-eval "`(a `(b `(c d)))")
                 (nelisp-phase3a--host-eval "`(a `(b `(c d)))"))))

(ert-deftest nelisp-phase3a-bq-splice-with-nested ()
  (let ((xs '(1 2)))
    (should (equal (nelisp-phase3a--bq-eval "`(a ,@xs `(b c))")
                   (nelisp-phase3a--host-eval "`(a ,@xs `(b c))")))))

(provide 'nelisp-phase3a-reader-test)
;;; nelisp-phase3a-reader-test.el ends here
