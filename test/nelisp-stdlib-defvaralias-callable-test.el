;;; nelisp-stdlib-defvaralias-callable-test.el --- callable frontend tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-stdlib-defvaralias-callable)

(ert-deftest nelisp-defvaralias-callable-installs-compiled-bcl-signature ()
  (let* ((nelisp--functions (make-hash-table :test 'eq))
         (nelisp-gnu-bytecode-vm--callable-signatures
          (make-hash-table :test 'eq))
         (host-functions
          (mapcar (lambda (symbol) (cons symbol (symbol-function symbol)))
                  '(defvaralias boundp symbol-value makunbound set))))
    (should (eq (nelisp-stdlib-defvaralias-install-callable) 'defvaralias))
    (dolist (entry '((defvaralias (2 3)) (boundp (1))
                     (symbol-value (1)) (makunbound (1)) (set (2))))
      (should (nelisp-bcl-p (gethash (car entry) nelisp--functions)))
      (should (equal (gethash (car entry)
                              (nelisp-gnu-bytecode-vm--live-call-signatures))
                     (cadr entry))))
    (dolist (entry host-functions)
      (should (eq (symbol-function (car entry)) (cdr entry))))))

(ert-deftest nelisp-defvaralias-public-call-uses-current-logical-bcl-cell ()
  (let* ((nelisp--functions (make-hash-table :test 'eq))
         (nelisp-gnu-bytecode-vm--callable-signatures
          (make-hash-table :test 'eq))
         (nelisp--globals (make-hash-table :test 'eq))
         (nelisp--variable-aliases (make-hash-table :test 'eq))
         (nelisp--specials (make-hash-table :test 'eq)))
    (nelisp-stdlib-defvaralias-install-callable)
    (let ((caller (nelisp-bc-run
                   (nelisp-bc-compile '(lambda (symbol) (makunbound symbol))))))
      (should (nelisp-bcl-p caller))
      (nelisp-variable-put 'nelisp-va-public-target 'stored-value)
      (nelisp-variable-install-alias 'nelisp-va-public-alias
                                     'nelisp-va-public-target)
      (should (eq (nelisp-bc-run caller '(nelisp-va-public-alias))
                  'nelisp-va-public-alias))
      (should-not (nelisp-variable-boundp 'nelisp-va-public-alias))
      (should (eq (nelisp-variable-get 'nelisp-va-public-target)
                  'stored-value))
      (let ((replacement (nelisp-bc-run
                          (nelisp-bc-compile '(lambda (symbol) symbol)))))
        (puthash 'makunbound replacement nelisp--functions)
        (should (eq (nelisp-bc-run caller '(nelisp-va-public-target))
                    'nelisp-va-public-target))
        (should (nelisp-variable-boundp 'nelisp-va-public-target))))))

(ert-deftest nelisp-defvaralias-callable-forwards-target-and-docstring ()
  (let (received)
    (cl-letf (((symbol-function 'nelisp-variable-install-alias)
               (lambda (alias target)
                 (setq received (list alias target))
                 target)))
      (should (eq (nelisp-stdlib-defvaralias-host-call
                   'nelisp-va-test-alias 'nelisp-va-test-target "alias doc")
                  'nelisp-va-test-target))
      (should (equal received '(nelisp-va-test-alias nelisp-va-test-target)))
      (should (equal (get 'nelisp-va-test-alias 'variable-documentation)
                     "alias doc")))))

(ert-deftest nelisp-defvaralias-callable-delegates-symbol-constant-policy ()
  (let (received)
    (cl-letf (((symbol-function 'nelisp-variable-install-alias)
               (lambda (alias target)
                 (push (list alias target) received)
                 target)))
      (should (eq (nelisp-stdlib-defvaralias-host-call nil :target) :target))
      (should (eq (nelisp-stdlib-defvaralias-host-call 'ordinary t) t))
      (should (eq (nelisp-stdlib-defvaralias-host-call :keyword 'ordinary) 'ordinary))
      (should (equal (nreverse received)
                     '((nil :target) (ordinary t) (:keyword ordinary)))))))

(ert-deftest nelisp-defvaralias-callable-fails-closed-without-installer ()
  (unless (fboundp 'nelisp-variable-install-alias)
    (should-error
     (nelisp-stdlib-defvaralias-host-call 'nelisp-va-test-alias 'nelisp-va-test-target)
     :type 'nelisp-variable-alias-error)))

(ert-deftest nelisp-defvaralias-callable-validates-before-installer-effects ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'nelisp-variable-install-alias)
               (lambda (&rest _args) (setq calls (1+ calls)) 'target)))
      (should-error (nelisp-stdlib-defvaralias-host-call 17 'nelisp-va-test-target)
                    :type 'wrong-type-argument)
      (should-error (nelisp-stdlib-defvaralias-host-call 'nelisp-va-test-alias 17)
                    :type 'wrong-type-argument)
      (should-error
       (nelisp-stdlib-defvaralias-host-call 'nelisp-va-test-alias 'nelisp-va-test-target 17)
       :type 'wrong-type-argument)
      (should (= calls 0)))))

(ert-deftest nelisp-defvaralias-callable-keeps-doc-property-on-installer-refusal ()
  (put 'nelisp-va-test-alias 'variable-documentation 'previous-doc)
  (unwind-protect
      (dolist (status '(1 2))
        (cl-letf (((symbol-function 'nelisp-variable-install-alias)
                   (lambda (&rest _args)
                     (signal 'nelisp-variable-alias-error (list status)))))
          (should-error
           (nelisp-stdlib-defvaralias-host-call
            'nelisp-va-test-alias 'nelisp-va-test-target "new doc")
           :type 'nelisp-variable-alias-error)
          (should (eq (get 'nelisp-va-test-alias 'variable-documentation)
                      'previous-doc))))
    (put 'nelisp-va-test-alias 'variable-documentation nil)))

(ert-deftest nelisp-defvaralias-vm-variable-adapters-use-core-store-api ()
  (let (calls)
    (cl-letf (((symbol-function 'nelisp-variable-boundp)
               (lambda (symbol) (push (list 'boundp symbol) calls) t))
              ((symbol-function 'nelisp-variable-get)
               (lambda (symbol &optional _default)
                 (push (list 'get symbol) calls) 'vm-value))
              ((symbol-function 'nelisp-variable-makunbound)
               (lambda (symbol) (push (list 'makunbound symbol) calls) symbol)))
      (should (nelisp-stdlib-defvaralias-host-vm-boundp 'nelisp-va-test-alias))
      (should (eq (nelisp-stdlib-defvaralias-host-vm-symbol-value
                   'nelisp-va-test-alias)
                  'vm-value))
      (should (eq (nelisp-stdlib-defvaralias-host-vm-makunbound
                   'nelisp-va-test-alias)
                  'nelisp-va-test-alias))
      (should (equal (nreverse calls)
                     '((boundp nelisp-va-test-alias)
                       (get nelisp-va-test-alias)
                       (makunbound nelisp-va-test-alias)))))))

(provide 'nelisp-stdlib-defvaralias-callable-test)
;;; nelisp-stdlib-defvaralias-callable-test.el ends here
