;;; gnu-tests-selftest-fail.el --- One failing test for gnu-tests.sh --self-test -*- lexical-binding: t -*-

;; The ERT report must print the failing `should' form, and the runner's
;; JSON summary must name the test as unexpected.

(require 'ert)

(defun gnu-tests-selftest--two () 2)

(ert-deftest gnu-tests-selftest-fails ()
  (should (equal (gnu-tests-selftest--two) 3)))

;;; gnu-tests-selftest-fail.el ends here
