;;; nl-safe-example-test.el --- ERT coverage for the borrow-violation demo -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercises `examples/nl-safe/borrow-violation.el' so the example
;; cannot silently rot.  This file's name matches `make test's
;; `packages/*/test/nl-*-test.el' glob (host Emacs, part of the
;; `ert-full' gate) and it is also loaded directly by
;; `nl-safe-standalone-smoke.el' (target/nelisp, no ert) -- one copy
;; of the assertions covers both.

;;; Code:

(require 'ert)
(require 'nl-safe)
;; Plain `load' with a relative FILE searches `load-path' in host
;; Emacs (it does not fall back to `default-directory' the way
;; `file-exists-p'/`load-file' do), so an absolute path is the one
;; form that resolves identically here and under the standalone
;; binary's own `load'.
(load (expand-file-name "examples/nl-safe/borrow-violation.el"))

(ert-deftest nl-safe-example-violation-caught-at-clash ()
  "The exclusive borrow is rejected at the exact call that clashes."
  (should (plist-get (nl-demo-borrow-violation) :caught)))

(ert-deftest nl-safe-example-rejected-write-never-happened ()
  "A rejected borrow leaves the cell's value untouched."
  (let ((result (nl-demo-borrow-violation)))
    (should (= (plist-get result :value-during) 10))
    (should (= (plist-get result :value-after) 10))))

(ert-deftest nl-safe-example-violation-logged-with-context ()
  "The violation record names the cell and both borrow kinds."
  (let* ((result (nl-demo-borrow-violation))
         (entry (plist-get result :log-entry)))
    (should entry)
    (should (eq (plist-get entry :kind) 'borrow))
    (should (eq (plist-get entry :cell) 'nl-demo-counter))
    (should (eq (plist-get entry :existing) 'shared))
    (should (eq (plist-get entry :requested) 'exclusive))))

(provide 'nl-safe-example-test)

;;; nl-safe-example-test.el ends here
