;;; nelisp-bytecode-native-rooted-cfg-safe-contract-admission-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bytecomp)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-cfg-plan)
(require 'nelisp-bytecode-native-rooted-cfg-emit)
(require 'nelisp-bytecode-native-rooted-cfg-safe-contract)

(defun nelisp-bytecode-native-rooted-cfg-safe-contract-admission-test--build
    (form)
  (let* ((function (byte-compile form))
         (input (nelisp-bytecode-compiler-input-build function))
         (plan (nelisp-bytecode-native-rooted-cfg-plan
                input 'safe-primitives-v3))
         (emitted (and (eq (plist-get plan :status) 'complete)
                       (nelisp-bytecode-native-rooted-cfg-emit
                        plan nelisp-bytecode-native-rooted-cfg-safe-contract-entry))))
    (nelisp-bytecode-native-rooted-cfg-safe-contract-create input plan emitted)))

(defun nelisp-bytecode-native-rooted-cfg-safe-contract-admission-test--rehash
    (contract)
  (let ((rest contract) (canonical nil))
    (while rest
      (let ((key (pop rest)) (value (pop rest)))
        (unless (eq key :digest)
          (setq canonical (append canonical (list key value))))))
    (plist-put contract :digest
               (let ((print-circle t))
                 (secure-hash 'sha256 (prin1-to-string canonical))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-contract-admission/rebuilds-cdr-mixed-branch ()
  (skip-unless (equal emacs-version "31.1"))
  (let ((contract
         (nelisp-bytecode-native-rooted-cfg-safe-contract-admission-test--build
          '(lambda (flag value)
             (if flag (cdr-safe value) (car-safe value))))))
    (should contract)
    (should (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p contract))
    (should (member "nl_native_car_v2" (plist-get contract :imports)))
    (should (member "nl_native_cdr_v2" (plist-get contract :imports)))
    (should (= (plist-get contract :argument-count) 2))
    (should (> (plist-get contract :root-count) 2))
    (should (equal (plist-get contract :entry-params) '(u64 u64 u64 u64)))
    (should (= (plist-get contract :status-base) 512))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-contract-admission/rehashed-fields-reconstruct-exactly ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((contract
          (nelisp-bytecode-native-rooted-cfg-safe-contract-admission-test--build
           '(lambda (value) (car-safe value))))
         (mutators
          (list (lambda (x) (setf (plist-get x :entry-abi) 99))
                (lambda (x) (setf (plist-get x :abi) 1))
                (lambda (x) (setf (plist-get x :root-count) 1))
                (lambda (x) (setf (plist-get x :status-base) 256))
                (lambda (x) (setf (plist-get x :dialect) 'wrong-dialect))
                (lambda (x) (setf (plist-get x :initializers) '(forged-init)))
                (lambda (x) (setf (plist-get x :input-recipe)
                                  (plist-put (plist-get x :input-recipe)
                                             :code (unibyte-string 0)))))))
    (should (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p contract))
    (dolist (mutate mutators)
      (let ((candidate (copy-tree contract)))
        (funcall mutate candidate)
        (nelisp-bytecode-native-rooted-cfg-safe-contract-admission-test--rehash
         candidate)
        (should-not
         (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p candidate))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-contract-admission/rejects-malformed-recipes-before-bytecode-construction ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((original (symbol-function 'make-byte-code))
         (cycle (list 'value))
         (cyclic-contract
          (copy-tree
           (nelisp-bytecode-native-rooted-cfg-safe-contract-admission-test--build
            '(lambda (value) (car-safe value)))))
         (large-contract
          (copy-tree
           (nelisp-bytecode-native-rooted-cfg-safe-contract-admission-test--build
            '(lambda (value) (car-safe value)))))
         (calls 0))
    (setcdr cycle cycle)
    (setf (plist-get (plist-get cyclic-contract :input-recipe) :constants)
          (vector cycle))
    (setf (plist-get (plist-get large-contract :input-recipe) :constants)
          (make-vector 5000 nil))
    (dolist (candidate (list cyclic-contract large-contract))
      (nelisp-bytecode-native-rooted-cfg-safe-contract-admission-test--rehash
       candidate))
    (dolist (candidate (list cyclic-contract large-contract))
      (let ((calls 0))
        (cl-letf (((symbol-function 'make-byte-code)
                   (lambda (&rest _args)
                     (setq calls (1+ calls))
                     (signal 'error '(unexpected-bytecode-construction)))))
          (should-not
           (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p candidate)))
        (should (= calls 0))))
    ;; Removing the guard from the same path must trip the construction counter.
    (let ((calls 0))
      (cl-letf (((symbol-function 'make-byte-code)
                 (lambda (&rest _args)
                   (setq calls (1+ calls))
                   (signal 'error '(instrumented-construction))))
                ((symbol-function
                  'nelisp-bytecode-native-rooted-cfg-safe-contract--data-p)
                 (lambda (&rest _args) t)))
        (should-not
         (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p large-contract)))
      (let ((zero-call-assertion-failed nil))
        (condition-case nil
            (should (= calls 0))
          (ert-test-failed (setq zero-call-assertion-failed t)))
        (should zero-call-assertion-failed))
      (should (= calls 1)))
    (should (functionp original))))

(ert-run-tests-batch-and-exit)

;;; nelisp-bytecode-native-rooted-cfg-safe-contract-admission-test.el ends here
