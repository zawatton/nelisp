;;; nelisp-bytecode-native-unary-chain-test.el --- bounded chain admission -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-native-unary-chain)
(require 'nelisp-bytecode-native-compiler)

(defun nelisp-bytecode-native-unary-chain-test--input (code &optional depth)
  (nelisp-bytecode-compiler-input-build
   (make-byte-code 257 code [] (or depth 2))))

(ert-deftest nelisp-bytecode-native-unary-chain-admits-sequences-and-refuses-nearby-code ()
  (skip-unless (equal emacs-version "31.1"))
  (should (equal (nelisp-bytecode-native-unary-chain-operations
                  (nelisp-bytecode-native-unary-chain-test--input
                   (unibyte-string 65 64 135)))
                 '(cdr car)))
  (should (equal (nelisp-bytecode-native-unary-chain-operations
                  (nelisp-bytecode-native-unary-chain-test--input
                   (unibyte-string 64 65 64 135)))
                 '(car cdr car)))
  (should (equal (nelisp-bytecode-native-unary-chain-operations
                  (nelisp-bytecode-native-unary-chain-test--input
                   (unibyte-string 137 65 64 135) 2))
                 '(cdr car)))
  (should-not (nelisp-bytecode-native-unary-chain-operations
               (nelisp-bytecode-native-unary-chain-test--input
                (unibyte-string 65 66 135))))
  (should-not (nelisp-bytecode-native-unary-chain-operations
               (nelisp-bytecode-native-unary-chain-test--input
                (unibyte-string 65 64 135) 3))))

(ert-deftest nelisp-bytecode-native-unary-chain-validator-removal-control-reaches-backend ()
  (require 'nelisp-runtime-reload-abi)
  (require 'nelisp-native-load)
  (let ((input (nelisp-bytecode-native-unary-chain-test--input
                (unibyte-string 65 66 135)))
        (backend-called nil))
    (should-not (nelisp-bytecode-native-unary-chain-operations input))
    (cl-letf (((symbol-function 'nelisp-bytecode-native-unary-chain-operations)
               (lambda (_input) '(cdr car)))
              ((symbol-function 'nelisp-native-load-running-binary-sha256)
               (lambda () "test-runtime"))
              ((symbol-function 'nelisp-runtime-reload-contract-matches-p)
               (lambda () t))
              ((symbol-function 'nelisp-native-load-raw-v2-compile-file)
               (lambda (&rest _args) (setq backend-called t) 'manifest)))
      (let ((built (nelisp-bytecode-native-unary-chain-build input "x.nelr")))
        (should (eq (plist-get built :status) 'complete))
        (should (equal (plist-get built :entry-name) "nl_native_chain_probe_v2"))
        (should (eq (plist-get built :chain-status-contract) 'v2)))
      (should backend-called))))

(ert-deftest nelisp-bytecode-native-unary-chain-result-slot-is-derived-from-length ()
  (should (= (nelisp-bytecode-native-unary-chain-result-root-index '(car)) 2))
  (should (= (nelisp-bytecode-native-unary-chain-result-root-index '(cdr car)) 1))
  (should (= (nelisp-bytecode-native-unary-chain-result-root-index '(cdr car cdr)) 2)))

(ert-deftest nelisp-bytecode-native-compiler-dispatches-chains-and-preserves-unary-route ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((chain (make-byte-code 257 (unibyte-string 137 65 64 135) [] 2))
         (single (make-byte-code 257 (unibyte-string 64 135) [] 2))
         (chain-called nil) (unary-called nil) result)
    (cl-letf (((symbol-function 'nelisp-bytecode-native-unary-chain-build)
               (lambda (_input _path) (setq chain-called t) '(:status complete)))
              ((symbol-function 'nelisp-bytecode-native-compiler--unary-build)
               (lambda (&rest _) (setq unary-called t) '(:status complete))))
      (should (equal (nelisp-bytecode-native-compiler-unary-chain-operations
                      (nelisp-bytecode-compiler-input-build chain))
                     '(cdr car)))
      (setq result (nelisp-bytecode-native-compiler-build
                    chain "chain.nelr" "nl_native_chain_probe_v2"))
      (should (eq (plist-get result :status) 'complete))
      (should chain-called)
      (should-not unary-called)
      (setq chain-called nil)
      (should (eq (plist-get (nelisp-bytecode-native-compiler-build
                    chain "chain.nelr" "wrong_entry") :status)
                  'unsupported))
      (should-not chain-called)
      (setq result (nelisp-bytecode-native-compiler-build
                    single "single.nelr" "nl_native_car_probe"))
      (should (eq (plist-get result :status) 'complete))
      (should unary-called))))

(provide 'nelisp-bytecode-native-unary-chain-test)
;;; nelisp-bytecode-native-unary-chain-test.el ends here
