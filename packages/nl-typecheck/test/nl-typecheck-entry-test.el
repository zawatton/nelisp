;;; nl-typecheck-entry-test.el --- entry check tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nl-typecheck-entry)

(defconst nl-typecheck-entry-test--fixture
  (expand-file-name "fixtures/entry-typed.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun nl-typecheck-entry-test--load ()
  (nl-typecheck-entry-load nl-typecheck-entry-test--fixture))

(ert-deftest nl-typecheck-entry-natural-and-generated-predicates ()
  (let ((nelisp-typecheck-enforce t))
    (nl-typecheck-entry-test--load)
    (should (equal (condition-case e (funcall 'entry-add "x" 1) (wrong-type-argument e))
                   '(wrong-type-argument integerp "x")))
    (should (equal (condition-case e (funcall 'entry-greet 5) (wrong-type-argument e))
                   '(wrong-type-argument stringp 5)))
    (let ((e (condition-case e (funcall 'entry-label 1.5) (wrong-type-argument e))))
      (should (eq (car e) 'wrong-type-argument))
      (should (eq (car (nth 1 e)) 'lambda))
      (should (equal (nth 2 e) 1.5)))
    (should (equal (funcall 'entry-greet "a" nil) "hi a"))
    (should (equal (funcall 'entry-label 3) "<3>"))
    (should (equal (funcall 'entry-untyped "anything") '("anything")))))

(ert-deftest nl-typecheck-entry-fset-and-advice ()
  (let ((nelisp-typecheck-enforce t))
    (nl-typecheck-entry-test--load)
    (advice-add 'entry-add :around (lambda (f &rest a) (apply f a)) '((name . wrap)))
    (unwind-protect
        (should-error (funcall 'entry-add "x" 1) :type 'wrong-type-argument)
      (advice-remove 'entry-add 'wrap))
    (fset 'entry-add (lambda (a b) (list a b)))
    (should (equal (funcall 'entry-add "x" 1) '("x" 1)))))

(ert-deftest nl-typecheck-entry-untyped-file-unaffected ()
  (let ((f (make-temp-file "nl-entry-" nil ".el")) (nelisp-typecheck-enforce t))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert ";;; x.el --- x -*- lexical-binding: t -*-\n"
                    "(defun entry-plain (a) (declare (ftype (function (integer) integer))) a)\n"))
          (nl-typecheck-entry-load f)
          (should (equal (funcall 'entry-plain "s") "s")))
      (delete-file f))))

(ert-deftest nl-typecheck-entry-negative-control ()
  (nl-typecheck-entry-test--load)
  (let ((nelisp-typecheck-enforce t))
    ;; `format' accepts a float, so only the entry check can signal here.
    (should-error (funcall 'entry-label 1.5) :type 'wrong-type-argument)
    (should (equal (funcall 'entry-add 1 2) 3)))
  (let ((nelisp-typecheck-enforce nil))
    (should (equal (condition-case nil (funcall 'entry-label 1.5) (error 'signalled))
                   "<1.5>"))))

(provide 'nl-typecheck-entry-test)
;;; nl-typecheck-entry-test.el ends here
