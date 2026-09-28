;;; nelisp-documentation-stringp-test.el --- GNU docstring predicate parity -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-eval)

(ert-deftest nelisp-documentation-stringp-matches-host-native-subr ()
  (nelisp--reset)
  (dolist (case (list (list "doc" t)
                      (list "日本語" t)
                      (list 17 t)
                      (list (cons "doc" 17) t)
                      (list (cons "日本語" -3) t)
                      (list nil nil)
                      (list 1.5 nil)
                      (list (ash 1 100) nil)
                      (list (cons "doc" 1.5) nil)
                      (list (cons "doc" nil) nil)
                      (list (cons 17 3) nil)
                      (list (list "doc" 3) nil)))
    (let ((object (car case))
          (expected (documentation-stringp (car case))))
      (should (eq expected (cadr case)))
      (should (eq (nelisp-eval
                   (list 'documentation-stringp (list 'quote object)))
                  expected)))))

(provide 'nelisp-documentation-stringp-test)

;;; nelisp-documentation-stringp-test.el ends here
