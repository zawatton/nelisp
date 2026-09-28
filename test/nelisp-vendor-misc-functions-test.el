;;; nelisp-vendor-misc-functions-test.el --- Vendor function parity -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Check that the source evaluator uses GNU Emacs 31.1's pinned function
;; forms and matches the host Emacs oracle for representative calls.

;;; Code:

(require 'ert)
(require 'nelisp-eval)

(ert-deftest nelisp-vendor-misc-functions-match-gnu-emacs ()
  (let ((nelisp-jit-enabled nil))
    (dolist (form '((funcall (apply-partially #'list 'fixed) 'tail)
                    (frame-configuration-p '(frame-configuration state))
                    (frame-configuration-p '(other state))
                    (bignump most-positive-fixnum)
                    (bignump most-negative-fixnum)
                    (bignump 2305843009213693952)
                    (bignump -2305843009213693953)))
      (nelisp--reset)
      (should (equal (nelisp-eval form) (eval form))))
    (let ((gensym-counter 0))
      (let ((expected (symbol-name (gensym "vendor-"))))
        (nelisp--reset)
        (should (equal (nelisp-eval
                        '(progn (setq gensym-counter 0)
                                (symbol-name (gensym "vendor-"))))
                       expected))))))

(provide 'nelisp-vendor-misc-functions-test)

;;; nelisp-vendor-misc-functions-test.el ends here
