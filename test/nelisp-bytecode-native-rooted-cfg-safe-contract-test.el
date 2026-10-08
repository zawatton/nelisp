;;; nelisp-bytecode-native-rooted-cfg-safe-contract-test.el --- safe contract -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-cfg-plan)
(require 'nelisp-bytecode-native-rooted-cfg-emit)
(require 'nelisp-bytecode-native-rooted-cfg-safe-contract)

(defun nelisp-bytecode-native-rooted-cfg-safe-contract-test--contract ()
  (let* ((function (byte-compile '(lambda (value) (car-safe value))))
         (input (nelisp-bytecode-compiler-input-build function))
         (plan (nelisp-bytecode-native-rooted-cfg-plan
                input 'safe-primitives-v3))
         (emitted (and (eq (plist-get plan :status) 'complete)
                       (nelisp-bytecode-native-rooted-cfg-emit
                        plan nelisp-bytecode-native-rooted-cfg-safe-contract-entry))))
    (nelisp-bytecode-native-rooted-cfg-safe-contract-create input plan emitted)))

(defun nelisp-bytecode-native-rooted-cfg-safe-contract-test--rehash (contract)
  (let ((rest contract) (canonical nil))
    (while rest
      (let ((key (pop rest)) (value (pop rest)))
        (unless (eq key :digest)
          (setq canonical (append canonical (list key value))))))
    (plist-put contract :digest (secure-hash 'sha256 (prin1-to-string canonical)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-contract/rebuilds-genuine-contract ()
  (skip-unless (equal emacs-version "31.1"))
  (let ((contract (nelisp-bytecode-native-rooted-cfg-safe-contract-test--contract)))
    (should contract)
    (should (equal (plist-get contract :version)
                   "nelisp-native-rooted-cfg-safe-v3"))
    (should (equal (plist-get contract :entry)
                   "nl_native_rooted_cfg_safe_probe_v3"))
    (should (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p contract))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-contract/refuses-mutated-contract-data ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((contract (nelisp-bytecode-native-rooted-cfg-safe-contract-test--contract))
         (bad-version (copy-tree contract))
         (bad-entry (copy-tree contract))
         (bad-ast (copy-tree contract))
         (bad-imports (copy-tree contract))
         (bad-recipe (copy-tree contract))
         (bad-unknown (copy-tree contract)))
    (should (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p contract))
    (setf (plist-get bad-version :version) "nelisp-native-rooted-cfg-shared-v2")
    (setf (plist-get bad-entry :entry) "nl_native_rooted_cfg_probe_v1")
    (setf (plist-get bad-ast :entry-ast) '(defun forged () 0))
    (setf (plist-get bad-imports :imports) '("nl_native_car_v2"))
    (setf (plist-get (plist-get bad-recipe :input-recipe) :code)
          (unibyte-string 0))
    (setf (plist-get bad-unknown :unexpected) t)
    (dolist (mutant (list bad-version bad-entry bad-ast bad-imports bad-recipe
                          bad-unknown))
      ;; Recompute a self-checksum: reconstruction must still reject it.
      (nelisp-bytecode-native-rooted-cfg-safe-contract-test--rehash mutant)
      (should-not (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p mutant)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-contract/refuses-cyclic-data-before-rebuild ()
  (let ((cyclic (list :arbitrary nil)))
    (setcdr cyclic cyclic)
    (should-not (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p cyclic))))

(ert-run-tests-batch-and-exit)

;;; nelisp-bytecode-native-rooted-cfg-safe-contract-test.el ends here
