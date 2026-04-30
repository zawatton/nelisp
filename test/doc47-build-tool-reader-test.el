;;; doc47-build-tool-reader-test.el --- ERT for doc47 build-tool reader -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'doc47-build-tool-reader)

(ert-deftest doc47-build-tool-reader-test/smoke-proper-list ()
  (should (equal (doc47-build-tool-reader-read "(a b c)") '(a b c))))

(ert-deftest doc47-build-tool-reader-test/smoke-dotted-pair ()
  (should (equal (doc47-build-tool-reader-read "(a . b)") '(a . b))))

(ert-deftest doc47-build-tool-reader-test/smoke-vector ()
  (should (equal (doc47-build-tool-reader-read "[1 2 3]") [1 2 3])))

(ert-deftest doc47-build-tool-reader-test/smoke-quote ()
  (should (equal (doc47-build-tool-reader-read "'x") '(quote x)))
  (should (equal (doc47-build-tool-reader-fmt '(quote x)) "'x")))

(ert-deftest doc47-build-tool-reader-test/smoke-function-quote ()
  (should (equal (doc47-build-tool-reader-read "#'foo") '(function foo)))
  (should (equal (doc47-build-tool-reader-fmt '(function foo)) "#'foo")))

(ert-deftest doc47-build-tool-reader-test/smoke-string-escapes ()
  (should (equal (doc47-build-tool-reader-read "\"hello\\n\"") "hello\n"))
  (should (equal (doc47-build-tool-reader-read "\"a\\tb\"") "a\tb"))
  (should (equal (doc47-build-tool-reader-read "\"\\\\\"") "\\"))
  (should (equal (doc47-build-tool-reader-read "\"\\\"\"") "\""))
  (should (equal (doc47-build-tool-reader-fmt "hi\n\"\\")
                 "\"hi\\n\\\"\\\\\"")))

(ert-deftest doc47-build-tool-reader-test/smoke-string-backslash-newline ()
  (should (equal (doc47-build-tool-reader-read "\"hi\\\nthere\"")
                 "hithere")))

(ert-deftest doc47-build-tool-reader-test/smoke-radix-integers ()
  (should (= (doc47-build-tool-reader-read "#x10") 16))
  (should (= (doc47-build-tool-reader-read "#o17") 15))
  (should (= (doc47-build-tool-reader-read "#b1010") 10)))

(ert-deftest doc47-build-tool-reader-test/smoke-float-exponent ()
  (should (= (doc47-build-tool-reader-read "1e3") 1000.0))
  (should (= (doc47-build-tool-reader-read "-1.5e-2") -0.015))
  (should (equal (doc47-build-tool-reader-fmt 1.0) "1.0"))
  (should (equal (doc47-build-tool-reader-fmt 3.14) "3.14")))

(ert-deftest doc47-build-tool-reader-test/smoke-symbol-punctuation ()
  (should (eq (doc47-build-tool-reader-read "foo-bar") 'foo-bar))
  (should (eq (doc47-build-tool-reader-read "ns:name") 'ns:name))
  (should (eq (doc47-build-tool-reader-read "file.ext") 'file.ext))
  (should (eq (doc47-build-tool-reader-read "a_b") 'a_b)))

(ert-deftest doc47-build-tool-reader-test/smoke-unbalanced-paren-errors ()
  (should-error (doc47-build-tool-reader-read "(a b"))
  (should-error (doc47-build-tool-reader-read "a)")))

(ert-deftest doc47-build-tool-reader-test/smoke-deep-nest ()
  (should (equal (doc47-build-tool-reader-read "(((((1)))))")
                 '(((((1))))))))

(ert-deftest doc47-build-tool-reader-test/read-all-multi-form ()
  (should (equal (doc47-build-tool-reader-read-all "1 2 3 ; tail comment\n")
                 '(1 2 3))))

(ert-deftest doc47-build-tool-reader-test/read-str-rejects-trailing ()
  (should-error (doc47-build-tool-reader-read "1 2")))

(ert-deftest doc47-build-tool-reader-test/read-all-empty-inputs ()
  (should (equal (doc47-build-tool-reader-read-all "") nil))
  (should (equal (doc47-build-tool-reader-read-all "   ; only comment\n") nil)))

(ert-deftest doc47-build-tool-reader-test/fmt-roundtrip-shape ()
  (let* ((src "(foo [1 2] '(bar . baz) 3.0)")
         (parsed (doc47-build-tool-reader-read src))
         (printed (doc47-build-tool-reader-fmt parsed)))
    (should (equal (doc47-build-tool-reader-read printed) parsed))))

(ert-deftest doc47-build-tool-reader-test/fmt-roundtrip-new-reader-forms ()
  (let* ((src "#'(lambda (x) `(,x ,@y))")
         (parsed (doc47-build-tool-reader-read src))
         (printed (doc47-build-tool-reader-fmt parsed)))
    (should (equal (doc47-build-tool-reader-read printed) parsed))))

(ert-deftest doc47-build-tool-reader-test/deferred-features-explicit ()
  (dolist (src '("?\\M-a" "#[1 2]" "#s(x)"))
    (should-error (doc47-build-tool-reader-read src)
                  :type 'error)))

(provide 'doc47-build-tool-reader-test)

;;; doc47-build-tool-reader-test.el ends here
