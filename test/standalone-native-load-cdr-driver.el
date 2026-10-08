;;; standalone-native-load-cdr-driver.el --- Checked native CDR smoke -*- lexical-binding: t; -*-

(require 'nelisp-native-load)

(defun nelisp-test-native-load-cdr-frame-clean-p (env)
  "Return non-nil when a subsequent legacy pin frame can be opened and closed."
  (let ((marker (nelisp-native-load--pin-begin env)))
    (and (integerp marker) (> marker 0)
         (progn (nelisp-native-load--pin-end env marker) t))))

(defun nelisp-test-native-load-cdr-smoke ()
  (let* ((env (nelisp--native-env))
         (nil-result (nelisp-native-load-cdr nil))
         (empty-result (nelisp-native-load-cdr '()))
         (integer-tail (nelisp-native-load-cdr '(head 42)))
         (string-tail (nelisp-native-load-cdr '(head "cdr string")))
         (nested-result (nelisp-native-load-cdr '(head (nested deeper))))
         (nested (car nested-result))
         (gc (garbage-collect))
         (nested-identity (eq nested (car nested-result)))
         (wrong-error
          (condition-case error-data
              (progn (nelisp-native-load-cdr 17) nil)
            (wrong-type-argument error-data)))
         (wrong-clean (nelisp-test-native-load-cdr-frame-clean-p env))
         (injected-error
          (condition-case error-data
              (progn (nelisp-native-load-cdr (vector 1)) nil)
            (error (and (eq (car error-data) 'error) error-data))))
         (injected-clean (nelisp-test-native-load-cdr-frame-clean-p env))
         (improper-error
          (condition-case error-data
              (progn (nelisp-native-load-cdr (cons 1 2)) nil)
            (error (and (eq (car error-data) 'error) error-data))))
         (improper-clean (nelisp-test-native-load-cdr-frame-clean-p env))
         (cyclic (cons 1 nil))
         (circular-error nil)
         (circular-clean nil)
         (status-function (symbol-function 'nelisp-native-load--cdr-v2-status))
         (status2-error nil)
         (status2-clean nil))
    (setcdr cyclic cyclic)
    (setq circular-error
          (condition-case error-data
              (progn (nelisp-native-load-cdr cyclic) nil)
            (error (and (eq (car error-data) 'error) error-data))))
    (setq circular-clean (nelisp-test-native-load-cdr-frame-clean-p env))
    (unwind-protect
        (progn
          (fset 'nelisp-native-load--cdr-v2-status
                (lambda (_env _ticket _input-index _output-index) 2))
          (setq status2-error
                (condition-case error-data
                    (progn (nelisp-native-load-cdr nil) nil)
                  (error (and (eq (car error-data) 'error) error-data))))
          (setq status2-clean
                (nelisp-test-native-load-cdr-frame-clean-p env)))
      (fset 'nelisp-native-load--cdr-v2-status status-function))
    (and (null nil-result) (null empty-result)
         (equal integer-tail '(42)) (equal string-tail '("cdr string"))
         gc nested-identity
         (and (eq (car wrong-error) 'wrong-type-argument)
              (equal (cdr wrong-error) '(listp 17)))
         wrong-clean injected-error injected-clean
         improper-error improper-clean circular-error circular-clean
         (and (eq (car status2-error) 'error)
              (equal (cadr status2-error)
                     "nelisp-native-load: native CDR rejected its v2 request"))
         status2-clean)))

(provide 'standalone-native-load-cdr-driver)

;;; standalone-native-load-cdr-driver.el ends here
