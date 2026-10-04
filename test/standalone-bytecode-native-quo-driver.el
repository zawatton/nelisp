;;; standalone-bytecode-native-quo-driver.el --- runtime quotient assertion -*- lexical-binding: t; -*-

(let* ((root (getenv "NELISP_REPO_ROOT"))
       (elc (getenv "NELISP_QUO_ELC")))
  (unless (and root elc)
    (error "NELISP_REPO_ROOT and NELISP_QUO_ELC are required"))
  (load (expand-file-name "lisp/nelisp-bytecode-native-package.el" root)
        nil nil t)
  (nelisp-bytecode-native-package-raw-eval-elc-forms
   (nelisp-bytecode-native-package-raw-read-elc-forms elc)))

;; Bytecode arithmetic intentionally reaches the tagged builtin primitive,
;; even if the Lisp function cell is rebound after byte compilation.
(let ((original (symbol-function '/)))
  (unwind-protect
      (progn
        (fset '/ (lambda (&rest _args) 99))
        (unless (= (nl-quo-fixture-divide 7 2) 3)
          (error "bytecode quo did not use the tagged primitive")))
    (fset '/ original)))

(unless (and (equal (list (nl-quo-fixture-divide 7 2)
                          (nl-quo-fixture-divide -7 2)
                          (nl-quo-fixture-divide 7 2.0)
                          (nl-quo-fixture-negate 7)
                          (nl-quo-fixture-negate 2.5))
                    '(3 -3 3.5 -7 -2.5))
             (eq (condition-case err (nl-quo-fixture-divide 1 0)
                   (error (car err))) 'arith-error)
             (eq (condition-case err (nl-quo-fixture-divide 'wrong 2)
                   (error (car err))) 'wrong-type-argument)
             (eq (condition-case err
                     (funcall (make-byte-code nil (unibyte-string 165 135) [] 1))
                   (error (car err))) 'error)
             (eq (condition-case err
                     (funcall (make-byte-code nil (unibyte-string 91 135) [] 1))
                   (error (car err))) 'error))
  (error "GNU quo runtime oracle mismatch"))
(princ "gnu-bytecode-runtime: (quo=3,-3,3.5 negate=-7,-2.5 errors=arith-error,wrong-type-argument malformed=error)\n")

(provide 'standalone-bytecode-native-quo-driver)
;;; standalone-bytecode-native-quo-driver.el ends here
