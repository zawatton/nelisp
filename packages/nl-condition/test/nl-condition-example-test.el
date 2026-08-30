;;; nl-condition-example-test.el --- ERT coverage for the restart-resume demo -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercises `examples/nl-condition/restart-resume.el' so the example
;; cannot silently rot.  This file's name matches `make test's
;; `packages/*/test/nl-*-test.el' glob (host Emacs, part of the
;; `ert-full' gate) and it is also loaded directly by
;; `nl-condition-standalone-smoke.el' (target/nelisp, no ert) -- one
;; copy of the assertions covers both.

;;; Code:

(require 'ert)
(require 'nl-condition)
;; Plain `load' with a relative FILE searches `load-path' in host
;; Emacs (it does not fall back to `default-directory' the way
;; `file-exists-p'/`load-file' do), so an absolute path is the one
;; form that resolves identically here and under the standalone
;; binary's own `load'.
(load (expand-file-name "examples/nl-condition/restart-resume.el"))

(ert-deftest nl-condition-example-resumes-past-bad-element ()
  "The restart resumes the sum instead of aborting on the bad element."
  (should (= (nl-demo-sum-resuming '(1 2 "x" 4)) 7)))

(ert-deftest nl-condition-example-uses-caller-fallback ()
  "The substitute value comes from the handler, not a fixed default."
  (should (= (nl-demo-sum-resuming '(1 2 "x" 4) 10) 17)))

(ert-deftest nl-condition-example-all-numbers-untouched ()
  "With no fault, every element is used as-is."
  (should (= (nl-demo-sum-resuming '(1 2 3 4)) 10)))

(ert-deftest nl-condition-example-multiple-faults-each-resume ()
  "Two faulty elements each trigger their own resume, in order."
  (should (= (nl-demo-sum-resuming '("a" 1 "b" 2)) 3)))

(provide 'nl-condition-example-test)

;;; nl-condition-example-test.el ends here
