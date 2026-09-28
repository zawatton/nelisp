;;; nelisp-eln-registration-vectors-test.el --- flat GNU vector views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'nelisp-eln-registration-vectors)

(defun nelisp-eln-registration-vectors-test--must-fail (thunk)
  (unless (condition-case nil (progn (funcall thunk) nil)
            (error t))
    (error "Expected registration vector failure")))

(let* ((objects (nelisp-eln-objects-create))
       (vectors (nelisp-eln-registration-vectors-create-unit objects))
       (source (vector nil 23 (copy-sequence "metadata")))
       (word (nelisp-eln-registration-vectors-register vectors source))
       (base (- word 5))
       (activation (nelisp-eln-registration-vectors-begin-activation vectors)))
  (unless (and (= (logand word 7) 5)
               (= (nelisp-eln-abi-read-word base 0) 3)
               (= (nelisp-eln-abi-read-word base 8)
                  (nelisp-eln-abi-encode-nil))
               (= (nelisp-eln-abi-read-word base 16)
                  (nelisp-eln-abi-encode-fixnum 23))
               (= (nelisp-eln-registration-vectors-register vectors source) word)
               (eq (nelisp-eln-registration-vectors-activation-decode
                    activation word) source))
    (error "GNU vector header, slot words, or identity was incorrect"))
  (let ((child-word (nelisp-eln-abi-read-word base 24)))
    (unless (equal (nelisp-eln-objects-activation-decode
                    (aref activation 4) child-word)
                   "metadata")
      (error "Activation did not retain vector child codec views")))
  ;; A codec owner may close while the vector activation holds child leases.
  (nelisp-eln-objects-release objects)
  (unless (eq (nelisp-eln-registration-vectors-activation-decode
               activation word) source)
    (error "Vector activation lost its canonical source root"))
  (nelisp-eln-registration-vectors-end-activation activation)
  (nelisp-eln-registration-vectors-release-unit vectors))

(let* ((objects (nelisp-eln-objects-create))
       (vectors (nelisp-eln-registration-vectors-create-unit objects))
       (source (vector 1))
       (word (nelisp-eln-registration-vectors-register vectors source))
       (activation (nelisp-eln-registration-vectors-begin-activation vectors)))
  (aset source 0 2)
  (nelisp-eln-registration-vectors-test--must-fail
   (lambda () (nelisp-eln-registration-vectors-activation-decode activation word)))
  (aset source 0 1)
  (nelisp-eln-abi-write-word (- word 5) 8 (nelisp-eln-abi-encode-fixnum 9))
  (nelisp-eln-registration-vectors-test--must-fail
   (lambda () (nelisp-eln-registration-vectors-activation-decode activation word)))
  (nelisp-eln-registration-vectors-end-activation activation)
  (nelisp-eln-objects-release objects)
  (nelisp-eln-registration-vectors-release-unit vectors))

(let* ((objects (nelisp-eln-objects-create))
       (vectors (nelisp-eln-registration-vectors-create-unit objects))
       (source (vector 5))
       (_word (nelisp-eln-registration-vectors-register vectors source))
       (activation (nelisp-eln-registration-vectors-begin-activation vectors))
       (release-activation
        (symbol-function 'nelisp-eln-objects-activation-release))
       (fail-child t))
  (unwind-protect
      (progn
        (fset 'nelisp-eln-objects-activation-release
              (lambda (token)
                (if fail-child
                    (progn (setq fail-child nil)
                           (signal 'nelisp-eln-objects-error '(injected)))
                  (funcall release-activation token))))
        (nelisp-eln-registration-vectors-test--must-fail
         (lambda () (nelisp-eln-registration-vectors-end-activation activation)))
        (unless (eq (aref activation 2) 'closing)
          (error "Failed activation cleanup did not remain retryable"))
        (nelisp-eln-registration-vectors-end-activation activation))
    (fset 'nelisp-eln-objects-activation-release release-activation))
  (nelisp-eln-objects-release objects)
  (let ((release-memory (symbol-function 'nl-ffi-memory-release))
        (fail-memory t))
    (unwind-protect
        (progn
          (fset 'nl-ffi-memory-release
                (lambda (owner)
                  (if fail-memory
                      (progn (setq fail-memory nil)
                             (signal 'error '(injected-release)))
                    (funcall release-memory owner))))
          (nelisp-eln-registration-vectors-test--must-fail
           (lambda () (nelisp-eln-registration-vectors-release-unit vectors)))
          (unless (and (eq (aref vectors 2) 'closing)
                       (aref (car (aref vectors 3)) 2))
            (error "Failed allocation cleanup lost its retry handle"))
          (nelisp-eln-registration-vectors-release-unit vectors))
      (fset 'nl-ffi-memory-release release-memory))))

(let* ((objects (nelisp-eln-objects-create))
       (vectors (nelisp-eln-registration-vectors-create-unit objects))
       (allocated 0)
       (original (symbol-function 'nl-ffi-memory-allocate)))
  (unwind-protect
      (progn
        (fset 'nl-ffi-memory-allocate
              (lambda (&rest args) (setq allocated (1+ allocated))
                (apply original args)))
        (dolist (bad (list (vector [nested]) (vector (make-hash-table))))
          (let ((before-allocation allocated))
          (nelisp-eln-registration-vectors-test--must-fail
             (lambda () (nelisp-eln-registration-vectors-register vectors bad)))
            (unless (= allocated before-allocation)
              (error "Unsupported graph allocated native storage"))))
        (let* ((other-objects (nelisp-eln-objects-create))
               (other-vectors
                (nelisp-eln-registration-vectors-create-unit other-objects))
               (shared (vector 99))
               (before-allocation nil))
          (nelisp-eln-registration-vectors-register vectors shared)
          (setq before-allocation allocated)
          (nelisp-eln-registration-vectors-test--must-fail
           (lambda ()
             (nelisp-eln-registration-vectors-register other-vectors shared)))
          (unless (= allocated before-allocation)
            (error "Cross-unit identity rejection allocated native storage"))
          (nelisp-eln-objects-release other-objects)
          (nelisp-eln-registration-vectors-release-unit other-vectors))
        t)
    (fset 'nl-ffi-memory-allocate original))
  (nelisp-eln-objects-release objects)
  (nelisp-eln-registration-vectors-release-unit vectors))

t

;;; nelisp-eln-registration-vectors-test.el ends here
