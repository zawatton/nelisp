;;; emacs-cl-macros-test.el --- Tests for emacs-cl-macros  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the CL compatibility layer split out of `emacs-stub.el'.
;; The batch host already provides the real CL library, so these tests
;; focus on the contract the module preserves under host Emacs and on
;; the internal parsing helpers that are defined in this file.

;;; Code:

(require 'ert)
(require 'emacs-cl-macros)

;;;; Load / feature contract

(ert-deftest emacs-cl-macros-test/require-loads-cleanly ()
  (should (featurep 'emacs-cl-macros))
  (should (featurep 'cl-macs))
  (should (featurep 'cl-seq))
  (should (featurep 'cl-extra))
  (should (featurep 'cl-generic))
  (should (fboundp 'cl-deftype)))

;;;; Arglist helpers

(ert-deftest emacs-cl-macros-test/split-arglist-positional-only ()
  (should (equal (emacs-cl-macros--split-arglist '(a b c))
                 '((a b c) nil nil nil))))

(ert-deftest emacs-cl-macros-test/split-arglist-optional-rest-and-key ()
  (should (equal (emacs-cl-macros--split-arglist
                  '(a &optional b &rest rest &key (k1 1) k2))
                 '((a) (b) rest ((:k1 k1 1) (:k2 k2 nil))))))

(ert-deftest emacs-cl-macros-test/key-bindings-shape-is-correct ()
  (should (equal (emacs-cl-macros--key-bindings '((:k1 k1 1) (:k2 k2 nil)) 'rest)
                 '((k1 (or (car (cdr (memq ':k1 rest))) 1))
                   (k2 (or (car (cdr (memq ':k2 rest))) nil))))))

;;;; Sequence predicates

(ert-deftest emacs-cl-macros-test/cl-some-every-find-position-contract ()
  (should (equal (cl-some (lambda (x) (and (= x 5) 'hit)) '(1 3 5 7)) 'hit))
  (should (cl-every (lambda (x) (< x 10)) '(1 2 3)))
  (should-not (cl-every (lambda (x) (< x 3)) '(1 2 3)))
  (should (equal (cl-find 3 '(1 2 3 4)) 3))
  (should (equal (cl-position 3 '(1 2 3 4)) 2))
  (should (equal (cl-position-if (lambda (x) (> x 3)) '(1 2 4 5)) 2)))

(ert-deftest emacs-cl-macros-test/cl-remove-if-and-cl-remove-if-not-filter-correctly ()
  (should (equal (cl-remove-if (lambda (x) (= 1 (% x 2))) '(1 2 3 4 5)) '(2 4)))
  (should (equal (cl-remove-if-not (lambda (x) (= 1 (% x 2))) '(1 2 3 4 5))
                 '(1 3 5))))

;;;; Loop / mutation macros

(ert-deftest emacs-cl-macros-test/cl-loop-collect-roundtrip ()
  (should (equal (cl-loop for x in '(1 2 3) collect (* 2 x))
                 '(2 4 6))))

(ert-deftest emacs-cl-macros-test/cl-loop-sum-and-count-roundtrip ()
  (should (= 6 (cl-loop for x in '(1 2 3) sum x)))
  (should (= 1 (cl-loop for x in '(1 0 2) count (> x 1)))))

(ert-deftest emacs-cl-macros-test/cl-incf-cl-decf-and-cl-pushnew-contract ()
  (let ((n 1)
        (xs '(b a)))
    (should (= 4 (cl-incf n 3)))
    (should (= 1 (cl-decf n 3)))
    (should (equal (progn (cl-pushnew 'a xs) xs) '(b a)))
    (should (equal (progn (cl-pushnew 'c xs) xs) '(c b a)))))

(ert-deftest emacs-cl-macros-test/letrec-and-cl-progv-load-time-contract ()
  (letrec ((countdown (lambda (n)
                        (if (= n 0)
                            42
                          (funcall countdown (1- n))))))
    (should (= 42 (funcall countdown 3))))
  (should (equal (cl-progv nil nil 'ok) 'ok)))

(provide 'emacs-cl-macros-test)

;;; emacs-cl-macros-test.el ends here
