;;; quit-test.el --- Quit handlers and cleanup parity -*- lexical-binding: t; -*-
(require 'ert)

(ert-deftest nelisp-eval-quit-cleanup-chain-and-handler-parity ()
  (let ((form '(let ((trace nil))
                 (condition-case data
                     (unwind-protect
                         (unwind-protect (signal 'quit '(payload))
                           (setq trace (cons 'inner trace)))
                       (setq trace (cons 'outer trace)))
                   (quit (list (car data) (cdr data) trace))))))
    (should (equal (eval form) '(quit (payload) (outer inner))))
    (should (equal (condition-case escaped (nelisp-eval form)
                     (quit (list :escaped escaped)))
                   (eval form)))))

(ert-deftest nelisp-eval-quit-unmatched-propagates-and-t-catches ()
  (let ((quit-form '(condition-case data (signal 'quit '(payload))
                     (error 'wrong-handler)))
        (all-form '(condition-case data (signal 'quit '(payload))
                    (t data))))
    (should (equal (condition-case data (eval quit-form) (quit data)) '(quit payload)))
    (should (equal (condition-case data (nelisp-eval quit-form) (quit data)) '(quit payload)))
    (should (equal (eval all-form) '(quit payload)))
    (should (equal (condition-case escaped (nelisp-eval all-form)
                     (quit (list :escaped escaped)))
                   (eval all-form)))))

(ert-deftest nelisp-eval-quit-cleanup-throw-replaces-quit ()
  (let ((form '(catch 'cleanup-tag
                 (condition-case data
                     (unwind-protect (signal 'quit nil)
                       (throw 'cleanup-tag 'cleanup-won))
                   (quit 'wrong-handler)))))
    (should (eq (eval form) 'cleanup-won))
    (should (eq (nelisp-eval form) (eval form)))))
