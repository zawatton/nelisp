;;; nelisp-bytecode-jit-gnu-eln-test.el --- GNU ELN JIT adapter tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-jit)
(require 'nelisp-eln-emitter)
(require 'nelisp-eln-system-loader)

(ert-deftest nelisp-bytecode-jit-gnu-eln-admits-only-supported-leaf-ir ()
  (let* ((function (make-byte-code 0 (unibyte-string 192 135) [17] 1))
         (ir (nelisp-bytecode-jit--decode-ir function)))
    (should (equal ir '(:arity 0 :expression 17 :max-depth 1
                        :instructions ([0 192 1 17] [1 135 2 nil]))))
    (should (nelisp-bytecode-jit--gnu-eln-literal-p ir))
    (should-not
     (nelisp-bytecode-jit--gnu-eln-literal-p
      (list :arity 1 :expression 17)))
    (should-not
     (nelisp-bytecode-jit--gnu-eln-literal-p
      (list :arity 0 :expression -1)))
    (should-not
     (nelisp-bytecode-jit--gnu-eln-literal-p
      (list :arity 0 :expression #x20000000)))))

(ert-deftest nelisp-bytecode-jit-gnu-eln-validates-backend-metadata ()
  (let* ((ir '(:arity 0 :expression 17))
         (identity (list "payload" (nelisp-bytecode-jit--runtime-identity)))
         (handle (list :backend 'gnu-eln-native-subr :arity 0
                       :callable (symbol-function 'current-time) :module 'module
                       :producer-profile nelisp-eln-emitter-gnu31-profile
                       :cache-identity identity)))
    (should (nelisp-bytecode-jit--validated-handle-p ir identity handle))
    (should-not
     (nelisp-bytecode-jit--validated-handle-p
      ir identity (plist-put (copy-sequence handle) :arity 1)))
    (should-not
     (nelisp-bytecode-jit--validated-handle-p
      ir identity
      (plist-put (copy-sequence handle) :producer-profile
                 '(:producer-version "31.1" :target "x86_64-linux"
                   :abi-hash "wrong" :register-subr-slot 1030))))))

(ert-deftest nelisp-bytecode-jit-gnu-eln-executes-managed-callable-only ()
  (let ((nelisp-bytecode-jit--native-entry-state (list nil)))
    (should (= (nelisp-bytecode-jit--execute-ir
                '(:arity 0 :expression 17)
                '(:backend gnu-eln-native-subr :callable (lambda () 17)) nil)
               17))
    (should (car nelisp-bytecode-jit--native-entry-state))))

(ert-deftest nelisp-bytecode-jit-gnu-eln-preserves-emission-error-for-retryable-cleanup ()
  (let ((nelisp-bytecode-jit--pending-eln-closes nil)
        (original-delete-directory (symbol-function 'delete-directory))
        (cleanup-fails t)
        caught)
    (cl-letf (((symbol-function 'nelisp-eln-emitter-write-ir)
               (lambda (_ir _artifact _profile)
                 (error "primary emission failure")))
              ((symbol-function 'delete-directory)
               (lambda (directory &optional recursive)
                 (if cleanup-fails
                     (progn (setq cleanup-fails nil)
                            (error "injected cleanup failure"))
                   (funcall original-delete-directory directory recursive)))))
      (setq caught
            (condition-case error-data
                (nelisp-bytecode-jit--compile-eln-ir
                 nil '(:arity 0 :expression 17) '("id" nil))
              (error error-data)))
      (should (equal (error-message-string caught)
                     "primary emission failure"))
      (should (= (length nelisp-bytecode-jit--pending-eln-closes) 1))
      (let ((entry (car nelisp-bytecode-jit--pending-eln-closes)))
        (should-not (plist-get entry :module))
        (should (plist-get entry :closed))
        (should (file-directory-p (plist-get entry :directory)))
        (nelisp-bytecode-jit--retry-eln-closes)
        (should (= (length nelisp-bytecode-jit--pending-eln-closes) 1))
        (should (file-directory-p (plist-get entry :directory)))
        (nelisp-bytecode-jit--retry-eln-closes)
        (should-not nelisp-bytecode-jit--pending-eln-closes)
        (should-not (file-directory-p (plist-get entry :directory)))))))

(ert-deftest nelisp-bytecode-jit-gnu-eln-close-retries-after-invalidation ()
  (let* ((module 'module)
         (dir (make-temp-file "nelisp-jit-eln-close-test-" t))
         (artifact (expand-file-name "unit.eln" dir))
         (function (make-byte-code 0 (unibyte-string 192 135) [17] 1))
         (ir (nelisp-bytecode-jit--decode-ir function))
         (identity (nelisp-bytecode-jit--cache-identity function))
         (handle (list :backend 'gnu-eln-native-subr :module module
                       :artifact artifact :artifact-directory dir))
         (nelisp-bytecode-jit--handles (make-hash-table :test 'eq))
         (nelisp-bytecode-jit--pending-eln-closes nil)
         (original-close (symbol-function 'nelisp-eln-system-loader-close))
         (fail-close t))
    (unwind-protect
        (progn
          (with-temp-file artifact (insert "mapped module"))
          (puthash function (list :state 'ready :identity identity :ir ir
                                  :handle handle)
                   nelisp-bytecode-jit--handles)
          (fset 'nelisp-eln-system-loader-close
                (lambda (candidate)
                  (should (eq candidate module))
                  (if fail-close
                      (progn (setq fail-close nil)
                             (error "injected live callable lease"))
                    t)))
          (nelisp-bytecode-jit-invalidate function)
          (should-not (gethash function nelisp-bytecode-jit--handles))
          (should (= (length nelisp-bytecode-jit--pending-eln-closes) 1))
          (should (file-exists-p artifact))
          (nelisp-bytecode-jit--retry-eln-closes)
          (should-not nelisp-bytecode-jit--pending-eln-closes)
          (should-not (file-exists-p artifact))
          (should-not (file-directory-p dir)))
      (fset 'nelisp-eln-system-loader-close original-close)
      (when (file-directory-p dir) (delete-directory dir t)))))

(provide 'nelisp-bytecode-jit-gnu-eln-test)

;;; nelisp-bytecode-jit-gnu-eln-test.el ends here
