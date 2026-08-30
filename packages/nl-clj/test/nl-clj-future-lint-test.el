;;; nl-clj-future-lint-test.el --- Worker-discipline lint tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Against-the-bug coverage for Doc 199 section 7: disciplined workers
;; produce no nl-safe records, while an unheld borrow-cell write is visible
;; through nl-clj-future's opt-in lint report today, before real threads can
;; turn the same mistake into a race.

;;; Code:

(require 'ert)
(require 'nl-safe)
(require 'nl-clj-vector)
(require 'nl-clj-future)

(ert-deftest nl-clj-future-lint-disciplined-worker-is-clean ()
  "Reading a captured persistent vector produces no lint violations."
  (let ((shared (nl-clj-vector 10 20 30)))
    (should (equal (nl-clj-future-with-lint
                     (nl-clj-pmap (lambda (i) (nl-clj-nth shared i))
                                  '(0 1 2)))
                   '(10 20 30)))
    (should-not (nl-clj-future-lint-violations))))

(ert-deftest nl-clj-future-lint-unheld-cell-write-is-logged ()
  "An unheld shared-cell write is the against-the-bug lint control."
  (let ((shared (nl-cell 0)))
    (should-error
     (nl-clj-future-with-lint
       (nl-clj-pcalls (lambda () (nl-cell-set shared 1))))
     :type 'nl-borrow-error)
    (let ((violations (nl-clj-future-lint-violations)))
      (should (= (length violations) 1))
      (let ((record (car violations)))
        (should (eq (plist-get record :kind) 'borrow))
        (should (eq (plist-get record :cell) 'nl-cell-set))
        (should (eq (plist-get record :existing) 'free))
        (should (eq (plist-get record :requested) 'write))))))

(provide 'nl-clj-future-lint-test)

;;; nl-clj-future-lint-test.el ends here
