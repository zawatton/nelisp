;;; standalone-bytecode-native-call1-driver.el --- Fixed CALL1 token smoke -*- lexical-binding: t; -*-

(require 'nelisp-runtime-reload-abi)
(require 'nelisp-bytecode-native-package)
(require 'nelisp-bytecode-native-compiler)

(defun nelisp-call1-driver-expects-error (thunk)
  (condition-case nil (progn (funcall thunk) nil) (error t)))

(let* ((elc (getenv "CALL1_ELC"))
       (artifact (getenv "CALL1_ARTIFACT"))
       (expected-sha (getenv "CALL1_RUNTIME_SHA"))
       (function (nelisp-bytecode-native-package-read-elc-function
                  elc 'call1-probe-function))
       (input (nelisp-bytecode-compiler-input-build function))
       (built (nelisp-bytecode-native-compiler-build
               function artifact "nl_native_bytecode_call1_exit"))
       (token (plist-get built :token))
       (target 'call1-probe-callee)
       (payload (list 'rooted (list 'before)))
       (target-calls 0) (cleanup-count 0)
       (original-end (symbol-function 'nelisp-native-load-call-exit-frame-end))
       (constants (aref function 2)) (original-callee (aref (aref function 2) 0))
       (normal nil) (signal-ok nil) (throw-ok nil))
  (unless (and (eq (plist-get input :status) 'complete)
               (plist-get input :call1-symbol-template-p)
               (= (aref function 0) 257)
               (equal (aref function 1) (unibyte-string 192 1 33 135))
               (eq (plist-get built :status) 'complete)
               (not (eq token (intern-soft (symbol-name token))))
               (eq original-callee target))
    (error "CALL1 admission mismatch: %S" built))
  (cl-letf (((symbol-function 'nelisp-native-load-call-exit-frame-end)
             (lambda (frame)
               (prog1 (funcall original-end frame)
                 (setq cleanup-count (1+ cleanup-count)))))
            ((symbol-function target)
             (lambda (x) (setq target-calls (1+ target-calls)) (garbage-collect) x)))
    (unless (nelisp-call1-driver-expects-error
             (lambda () (nelisp-bytecode-native-compiler-call1
                         (make-symbol "forged-call1-token") payload)))
      (error "forged token passed validation"))
    (unless (= target-calls 0) (error "forged token entered callee"))
    (setq normal (eq (nelisp-bytecode-native-compiler-call1 token payload) payload))
    (unless (and normal (= target-calls 1) (= cleanup-count 1))
      (error "normal CALL1/GC/cleanup failed"))
    (aset constants 0 'mutated-call1-callee)
    (unless (nelisp-call1-driver-expects-error
             (lambda () (nelisp-bytecode-native-compiler-call1 token payload)))
      (error "mutated input constant passed validation"))
    (aset constants 0 original-callee)
    (unless (= target-calls 1) (error "input mutation entered callee"))
    (fset target (lambda (x) (setq target-calls (1+ target-calls))
                   (signal 'wrong-type-argument (list 'integerp x))))
    (setq signal-ok
          (eq (caddr (condition-case data
                         (nelisp-bytecode-native-compiler-call1 token payload)
                       (wrong-type-argument data))) payload))
    (fset target (lambda (x) (setq target-calls (1+ target-calls))
                   (throw 'call1-smoke-tag x)))
    (setq throw-ok (eq (catch 'call1-smoke-tag
                         (nelisp-bytecode-native-compiler-call1 token payload))
                       payload)))
  (unless (and signal-ok throw-ok (= cleanup-count 3) (= target-calls 3))
    (error "CALL1 exit/cleanup failed: %S" (list signal-ok throw-ok cleanup-count target-calls)))
  (let ((bytes (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally artifact)
                 (buffer-string))))
    (with-temp-file artifact (insert "tampered"))
    (unless (nelisp-call1-driver-expects-error
             (lambda () (nelisp-bytecode-native-compiler-call1 token payload)))
      (error "mutated artifact passed validation"))
    (with-temp-buffer
      (set-buffer-multibyte nil) (insert bytes)
      (write-region nil nil artifact nil 'silent)))
  (unless (= target-calls 3) (error "artifact mutation entered callee"))
  (nelisp-bytecode-native-compiler-call1-close token)
  (unless (nelisp-call1-driver-expects-error
           (lambda () (nelisp-bytecode-native-compiler-call1 token payload)))
    (error "closed token remained callable"))
  (unless (equal expected-sha (nelisp-native-load-running-binary-sha256))
    (error "CALL1 runtime identity changed"))
  (princ "CALL1-SOURCEFREE-TOKEN-GC-SIGNAL-THROW-MUTATION-CLEANUP-PASS\n"))

;;; standalone-bytecode-native-call1-driver.el ends here
