;;; emacs-string-test.el --- ERT tests for emacs-string  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for string utility polyfills used by vendored Emacs Lisp.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-string)

(ert-deftest emacs-string-test/require-loads-cleanly ()
  (should (featurep 'emacs-string))
  (dolist (sym '(string-empty-p string-blank-p string-prefix-p
                 string-suffix-p string-trim-left string-trim-right
                 string-trim string-lines string< char-equal string-width
                 int-to-string assoc-string))
    (should (fboundp sym))))

(ert-deftest emacs-string-test/string-lines-basic-shape ()
  (should (equal (string-lines "") '("")))
  (should (equal (string-lines "a") '("a")))
  (should (equal (string-lines "a\n") '("a")))
  (should (equal (string-lines "a\n\n") '("a" "")))
  (should (equal (string-lines "\na") '("" "a"))))

(ert-deftest emacs-string-test/string-lines-omit-nulls ()
  (should (equal (string-lines "" t) nil))
  (should (equal (string-lines "a\n\nb" t) '("a" "b")))
  (should (equal (string-lines "\na" t) '("a"))))

(ert-deftest emacs-string-test/string-lines-keep-newlines ()
  (should (equal (string-lines "a\n" nil t) '("a\n")))
  (should (equal (string-lines "a\n\n" nil t) '("a\n" "\n")))
  (should (equal (string-lines "\na" nil t) '("\n" "a")))
  (should (equal (string-lines "a\n\n" t t) '("a\n"))))

(ert-deftest emacs-string-test/runtime-callable-fallbacks ()
  (should (string< "a" "b"))
  (should-not (string< "b" "a"))
  (should-not (string< "a" "a"))
  (should (char-equal ?x ?x))
  (should (= 3 (string-width "abc")))
  (should (string-equal "-42" (int-to-string -42)))
  (should (equal '("A" . 1)
                 (assoc-string "a" '(("A" . 1)) t)))
  (should (equal '(A . 1)
                 (assoc-string "a" '((A . 1)) t)))
  (should-not (assoc-string 'a '((A . 1)) nil)))

(ert-deftest emacs-string-test/doc16-breadth-string-builtins ()
  "Doc 16 breadth: string-equal-ignore-case / string-clean-whitespace /
string-split were void in the standalone runtime."
  ;; string-equal-ignore-case
  (should (string-equal-ignore-case "ABc" "abC"))
  (should-not (string-equal-ignore-case "abc" "abd"))
  ;; string-clean-whitespace (collapse runs + trim ends).
  ;; On host Emacs this name is an *autoload* into subr-x, and the test
  ;; load-path (-L src) shadows subr-x with the partial NeLisp port, so
  ;; calling it live would mis-resolve.  Pin a literal copy of the
  ;; standalone polyfill body (residuals-test parity pattern) instead.
  (cl-letf (((symbol-function 'string-clean-whitespace)
             (lambda (string)
               (string-trim
                (replace-regexp-in-string "[ \t\r\n]+" " " string t t)))))
    (should (equal "a b c" (string-clean-whitespace "  a   b\tc \n")))
    (should (equal "single" (string-clean-whitespace "single")))
    (should (equal "" (string-clean-whitespace "   "))))
  ;; string-split is an alias of split-string
  (should (equal '("a" "b" "c") (string-split "a,b,c" ",")))
  (should (equal '("a" "b") (string-split "  a b  "))))

(provide 'emacs-string-test)

;;; emacs-string-test.el ends here
