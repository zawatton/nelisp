;;; standalone-native-load-car-driver.el --- Checked native CAR wrapper smoke -*- lexical-binding: t; -*-

(require 'nelisp-native-load)

(defun nelisp-test-native-load-car-frame-clean-p (env)
  "Return non-nil when the legacy pin mode can start and end after a call."
  (let ((marker (nelisp-native-load--pin-begin env)))
    (and (integerp marker) (> marker 0)
         (progn (nelisp-native-load--pin-end env marker) t))))

(defun nelisp-test-native-load-car-smoke ()
  (let* ((env (nelisp--native-env))
         (nil-result (nelisp-native-load-car nil))
         (integer-result (nelisp-native-load-car '(42)))
         (string-result (nelisp-native-load-car '("string car")))
         (cons-result (nelisp-native-load-car '((nested deeper))) )
         (cons-tail (cdr cons-result))
         (gc (garbage-collect))
         (cons-identity (and (equal cons-result '(nested deeper))
                             (eq cons-tail (cdr cons-result))))
         (wrong-error
          (condition-case error-data
              (progn (nelisp-native-load-car 17) nil)
            (wrong-type-argument error-data)))
         (wrong-clean (nelisp-test-native-load-car-frame-clean-p env))
         (injected-error
          (condition-case error-data
              (progn (nelisp-native-load-car (vector 1)) nil)
            (error (and (eq (car error-data) 'error) error-data))))
         (injected-clean (nelisp-test-native-load-car-frame-clean-p env))
         (improper-error
          (condition-case error-data
              (progn (nelisp-native-load-car (cons 1 2)) nil)
            (error (and (eq (car error-data) 'error) error-data))))
         (improper-clean (nelisp-test-native-load-car-frame-clean-p env))
         (cyclic (cons 1 nil))
         (circular-error nil)
         (circular-clean nil)
         (status-function (symbol-function 'nelisp-native-load--car-v2-status))
         (status2-error nil)
         (status2-clean nil))
    (setcdr cyclic cyclic)
    (setq circular-error
          (condition-case error-data
              (progn (nelisp-native-load-car cyclic) nil)
            (error (and (eq (car error-data) 'error) error-data))))
    (setq circular-clean (nelisp-test-native-load-car-frame-clean-p env))
    (unwind-protect
        (progn
          (fset 'nelisp-native-load--car-v2-status
                (lambda (_env _ticket _input-index _output-index) 2))
          (setq status2-error
                (condition-case error-data
                    (progn (nelisp-native-load-car nil) nil)
                  (error (and (eq (car error-data) 'error) error-data))))
          (setq status2-clean
                (nelisp-test-native-load-car-frame-clean-p env)))
      (fset 'nelisp-native-load--car-v2-status status-function))
    (and (null nil-result)
         (= integer-result 42)
         (equal string-result "string car")
         gc cons-identity
         (and (eq (car wrong-error) 'wrong-type-argument)
              (equal (cdr wrong-error) '(listp 17)))
         wrong-clean
         injected-error injected-clean
         improper-error improper-clean
         circular-error circular-clean
         (and (eq (car status2-error) 'error)
              (equal (cadr status2-error)
                     "nelisp-native-load: native CAR rejected its v2 request"))
         status2-clean)))

(provide 'standalone-native-load-car-driver)

;;; standalone-native-load-car-driver.el ends here
