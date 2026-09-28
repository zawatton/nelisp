;;; nelisp-documentation-stringp-standalone-smoke.el --- native predicate parity -*- lexical-binding: t; -*-

(load "scripts/nelisp-ert-shim.el")

(ert-deftest nelisp-documentation-stringp/gnu-accepted-shapes ()
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
    (should (eq (documentation-stringp (car case)) (cadr case)))))

(let* ((result (nelisp-ert-run-all "nelisp-documentation-stringp"))
       (pass (car result))
       (fail (cadr result)))
  (princ (format "GATE-COUNT checked=%d findings=%d\n" (+ pass fail) fail))
  (if (> fail 0)
      (error "nelisp-documentation-stringp-standalone-smoke: %d failure(s)"
             fail)
    (princ (format "nelisp-documentation-stringp-standalone-smoke: PASS (%d tests)\n"
                   pass))))

;;; nelisp-documentation-stringp-standalone-smoke.el ends here
