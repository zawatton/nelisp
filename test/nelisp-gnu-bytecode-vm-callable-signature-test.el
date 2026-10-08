;;; nelisp-gnu-bytecode-vm-callable-signature-test.el --- CALL admission -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'cl-lib)
(require 'nelisp-gnu-bytecode-vm)

(ert-deftest nelisp-gnu-bytecode-vm-callable-signatures-bind-live-bcl-identity ()
  (let* ((nelisp--functions (make-hash-table :test 'eq))
         (nelisp--unbound (make-symbol "unbound"))
         (nelisp-gnu-bytecode-vm--callable-signatures (make-hash-table :test 'eq))
         (callable (nelisp-bc-make nil '(alias target &optional docstring)
                                   [] [0] 1 0))
         signatures)
    (puthash 'defvaralias callable nelisp--functions)
    (nelisp-gnu-bytecode-vm-register-callable-signatures
     'defvaralias callable '(2 3))
    (setq signatures (nelisp-gnu-bytecode-vm--live-call-signatures))
    (should (equal (gethash 'defvaralias signatures) '(2 3)))
    (should (eq (nelisp-gnu-bytecode-vm--known-call-target
                 'defvaralias 2 4 signatures (make-hash-table :test 'eq))
                'defvaralias))
    (should (eq (nelisp-gnu-bytecode-vm--known-call-target
                 'defvaralias 3 8 signatures (make-hash-table :test 'eq))
                'defvaralias))
    (should-error
     (nelisp-gnu-bytecode-vm--known-call-target
      'defvaralias 1 4 signatures (make-hash-table :test 'eq))
     :type 'nelisp-gnu-bytecode-vm-error)
    (puthash 'defvaralias (nelisp-bc-make nil '(other) [] [0] 1 0)
             nelisp--functions)
    (setq signatures (nelisp-gnu-bytecode-vm--live-call-signatures))
    (should (= (gethash 'defvaralias signatures) 1))
    (should-error
     (nelisp-gnu-bytecode-vm--known-call-target
      'defvaralias 2 4 signatures (make-hash-table :test 'eq))
     :type 'nelisp-gnu-bytecode-vm-error)))

(ert-deftest nelisp-gnu-bytecode-vm-callable-signature-registration-is-strict ()
  (let* ((nelisp--functions (make-hash-table :test 'eq))
         (nelisp--unbound (make-symbol "unbound"))
         (nelisp-gnu-bytecode-vm--callable-signatures (make-hash-table :test 'eq))
         (callable (nelisp-bc-make nil '(a b &optional c) [] [0] 1 0)))
    (should-error
     (nelisp-gnu-bytecode-vm-register-callable-signatures
      'defvaralias callable '(2 3))
     :type 'nelisp-gnu-bytecode-vm-error)
    (puthash 'defvaralias callable nelisp--functions)
    (dolist (bad-arities '(nil (2 2) (2 -1) (2 16) (2 . 3)))
      (should-error
       (nelisp-gnu-bytecode-vm-register-callable-signatures
        'defvaralias callable bad-arities)
       :type 'nelisp-gnu-bytecode-vm-error))
    (dolist (bad-arities '((0 15) (1)))
      (should-error
       (nelisp-gnu-bytecode-vm-register-callable-signatures
        'defvaralias callable bad-arities)
       :type 'nelisp-gnu-bytecode-vm-error))
    (dolist (bad-params '((a &key b) (a &allow-other-keys) (a &aux b)
                          (a &whole b) (a &body b) (a &environment b)
                          (a &rest &key) (a &optional &unknown)))
      (let ((bad-callable (nelisp-bc-make nil bad-params [] [0] 1 0)))
        (puthash 'defvaralias bad-callable nelisp--functions)
        (should-error
         (nelisp-gnu-bytecode-vm-register-callable-signatures
          'defvaralias bad-callable '(1))
         :type 'nelisp-gnu-bytecode-vm-error)))))

(ert-deftest nelisp-gnu-bytecode-vm-callable-arity-lowers-genuine-gnu-call2-and-call3 ()
  (let* ((nelisp--functions (make-hash-table :test 'eq))
         (nelisp--unbound (make-symbol "unbound"))
         (nelisp-gnu-bytecode-vm--callable-signatures (make-hash-table :test 'eq))
         (callable (nelisp-bc-make nil '(alias target &optional docstring)
                                   [] [0] 1 0)))
    (puthash 'defvaralias callable nelisp--functions)
    (nelisp-gnu-bytecode-vm-register-callable-signatures
     'defvaralias callable '(2 3))
    (let ((signatures (nelisp-gnu-bytecode-vm--live-call-signatures)))
      (dolist (case '(((lambda (alias target) (defvaralias alias target)) 2)
                      ((lambda (alias target doc) (defvaralias alias target doc)) 3)))
        (let* ((function (byte-compile (car case)))
               (arity (cadr case))
               (lowered
                (nelisp-gnu-bytecode-vm--lower-code
                 (aref function 1) (aref function 2) arity (aref function 3)
                 signatures (make-hash-table :test 'eq))))
          (should (memq 30 (append lowered nil))))))))

(ert-deftest nelisp-gnu-bytecode-vm-callable-bundle-rolls-back-both-stores ()
  (let* ((first (make-symbol "callable-bundle-first"))
         (second (make-symbol "callable-bundle-second"))
         (absent (make-symbol "callable-bundle-absent"))
         (signature-absent (make-symbol "callable-signature-absent"))
         (old (nelisp-bc-make nil '(old-arg) [] [0] 1 0))
         (new-first (nelisp-bc-make nil '(first-arg) [] [0] 1 0))
         (new-second (nelisp-bc-make nil '(second-arg) [] [0] 1 0))
         (saved (nelisp-eval-function-cell-snapshot (list first second)))
         (original-register
          (symbol-function 'nelisp-gnu-bytecode-vm-register-callable-signatures))
         (old-signature (gethash first nelisp-gnu-bytecode-vm--callable-signatures
                                 signature-absent))
         (second-signature (gethash second nelisp-gnu-bytecode-vm--callable-signatures
                                    signature-absent))
         (calls 0))
    (unwind-protect
        (progn
          (nelisp-eval-function-cell-put first old)
          (nelisp-gnu-bytecode-vm-register-callable-signatures first old '(1))
          (setq old-signature
                (gethash first nelisp-gnu-bytecode-vm--callable-signatures
                         signature-absent))
          (cl-letf (((symbol-function 'nelisp-gnu-bytecode-vm-register-callable-signatures)
                     (lambda (symbol callable arities)
                       (setq calls (1+ calls))
                       (funcall original-register symbol callable arities)
                       (when (= calls 2) (error "injected signature failure")))))
            (should-error
             (nelisp-gnu-bytecode-vm-install-callable-bundle
              (list (list first new-first '(1))
                    (list second new-second '(1))))))
          (should (= calls 2))
          (should (eq old (nelisp-eval-function-cell-ref first absent)))
          (should (eq absent (nelisp-eval-function-cell-ref second absent)))
          (should (eq old-signature
                      (gethash first nelisp-gnu-bytecode-vm--callable-signatures
                               signature-absent)))
          (should (eq second-signature
                      (gethash second nelisp-gnu-bytecode-vm--callable-signatures
                               signature-absent))))
      (nelisp-eval-function-cell-restore saved)
      (if (eq old-signature signature-absent)
          (remhash first nelisp-gnu-bytecode-vm--callable-signatures)
        (puthash first old-signature nelisp-gnu-bytecode-vm--callable-signatures))
      (if (eq second-signature signature-absent)
          (remhash second nelisp-gnu-bytecode-vm--callable-signatures)
        (puthash second second-signature nelisp-gnu-bytecode-vm--callable-signatures)))))

(provide 'nelisp-gnu-bytecode-vm-callable-signature-test)
;;; nelisp-gnu-bytecode-vm-callable-signature-test.el ends here
