;;; let-alist-test.el --- Tests for the let-alist shim  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 16 breadth round 20.  On the batch host the real GNU `let-alist'
;; runs, pinning the contract the NeLisp runtime shim must reproduce.

;;; Code:

(require 'ert)
(require 'let-alist)

(ert-deftest let-alist-test/doc16-round20-basic ()
  (should (equal '(1 2) (let-alist '((a . 1) (b . 2)) (list .a .b))))
  (should (= 1 (let-alist '((a . 1)) .a)))
  (should (null (let-alist '((a . 1)) .missing))))

(ert-deftest let-alist-test/doc16-round20-nested ()
  (should (= 5 (let-alist '((a . ((b . 5)))) .a.b)))
  (should (equal '(1 5)
                 (let-alist '((a . 1) (deep . ((inner . 5))))
                   (list .a .deep.inner)))))

(ert-deftest let-alist-test/doc16-round20-only-referenced-bound ()
  ;; Unreferenced keys are not required to exist; referenced-but-missing => nil.
  (should (equal '(1 nil) (let-alist '((a . 1)) (list .a .x))))
  ;; Repeated references collapse to a single binding.
  (should (equal '(1 1) (let-alist '((a . 1)) (list .a .a)))))

(provide 'let-alist-test)

;;; let-alist-test.el ends here
