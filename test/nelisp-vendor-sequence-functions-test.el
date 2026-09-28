;;; nelisp-vendor-sequence-functions-test.el --- GNU sequence helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Verify the source evaluator uses the pinned GNU Emacs 31.1 implementations.

;;; Code:

(require 'ert)
(require 'nelisp-eval)

(ert-deftest nelisp-vendor-sequence-functions-source-eval ()
  (nelisp--reset)
  (should (equal '(0 2 4 6) (nelisp-eval '(number-sequence 0 6 2))))
  (should (equal '(3) (nelisp-eval '(number-sequence 3 3 0))))
  (should (eq 'error
              (nelisp-eval
               '(condition-case err
                    (number-sequence 0 1 0)
                  (error (car err))))))
  (should (eq nil (nelisp-eval '(ensure-list nil))))
  (should (equal '(a b) (nelisp-eval '(ensure-list '(a b)))))
  (should (equal '(1 2 3 4 5 6 7)
                 (nelisp-eval '(flatten-tree '(1 (2 . 3) nil (4 5 (6)) 7)))))
  (should (equal '("leaf" b)
                 (nelisp-eval '(flatten-tree '("leaf" (b)))))))

(provide 'nelisp-vendor-sequence-functions-test)

;;; nelisp-vendor-sequence-functions-test.el ends here
