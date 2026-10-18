;;; gnu-tests-selftest.el --- Fixed fixture for gnu-tests.sh --self-test -*- lexical-binding: t -*-

;; Exactly one test of each kind: a pass, an expected failure, a skip, and a
;; should-error that passes.  GNU Emacs 31.1 reports the same four results.

(require 'ert)

(ert-deftest gnu-tests-selftest-pass ()
  (should (equal (list 1 2) '(1 2))))

(ert-deftest gnu-tests-selftest-expected-failure ()
  :expected-result :failed
  (should (eq 'a 'b)))

(ert-deftest gnu-tests-selftest-skip ()
  (skip-unless nil)
  (should nil))

(ert-deftest gnu-tests-selftest-should-error ()
  (should-error (car 1) :type 'wrong-type-argument))

;;; gnu-tests-selftest.el ends here
