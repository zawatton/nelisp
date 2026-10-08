;;; standalone-bytecode-native-unary-chain-driver.el --- source-free chain proof -*- lexical-binding: t; -*-

(require 'nelisp-bytecode-native-unary-chain)
(require 'nelisp-bytecode-native-compiler)
(require 'nelisp-native-load)
(require 'nelisp-bytecode-native-package)

(defun nelisp-test-native-unary-chain-smoke ()
  (let* ((elc (getenv "NELISP_CHAIN_ELC"))
         (source (getenv "NELISP_CHAIN_SOURCE"))
         (even-artifact (getenv "NELISP_CHAIN_EVEN"))
         (odd-artifact (getenv "NELISP_CHAIN_ODD"))
         (error-artifact (getenv "NELISP_CHAIN_ERROR"))
         (definitions (nelisp-bytecode-native-package-read-elc-functions elc))
         (even (cdr (assq 'nelisp-chain-even definitions)))
         (odd (cdr (assq 'nelisp-chain-odd definitions)))
         (error-function (cdr (assq 'nelisp-chain-intermediate-error definitions)))
         (even-input (nelisp-bytecode-compiler-input-build even))
         (odd-input (nelisp-bytecode-compiler-input-build odd))
         (even-build (nelisp-bytecode-native-compiler-build
                      even even-artifact "nl_native_chain_probe_v2"))
         (odd-build (nelisp-bytecode-native-compiler-build
                     odd odd-artifact "nl_native_chain_probe_v2"))
         (error-build (nelisp-bytecode-native-compiler-build
                       error-function error-artifact "nl_native_chain_probe_v2"))
         (hash (nelisp-native-load-running-binary-sha256))
         (even-handle nil) (odd-handle nil) (error-handle nil))
    (unless (and (stringp source) (not (file-exists-p source))
                 (eq (plist-get even-build :status) 'complete)
                 (eq (plist-get odd-build :status) 'complete)
                 (eq (plist-get error-build :status) 'complete)
                 (equal (plist-get even-input :code) (unibyte-string 137 65 64 135))
                 (equal (plist-get odd-input :code) (unibyte-string 137 65 64 65 135))
                 (= (plist-get even-input :argument-descriptor) 257)
                 (= (plist-get odd-input :argument-descriptor) 257)
                 (equal (plist-get even-input :constants) [])
                 (equal (plist-get odd-input :constants) []))
      (error "chain build or GNU 31.1 literal attestation failed"))
    (setq even-handle (nelisp-native-load-raw-v2-artifact
                      even-artifact "nl_native_chain_probe_v2" hash)
          odd-handle (nelisp-native-load-raw-v2-artifact
                     odd-artifact "nl_native_chain_probe_v2" hash)
          error-handle (nelisp-native-load-raw-v2-artifact
                        error-artifact "nl_native_chain_probe_v2" hash))
    (let* ((value (list 'outer (cons 'middle (list 'leaf))))
           (even-vm (funcall even value)) (odd-vm (funcall odd value))
           (even-native (nelisp-native-load-raw-v2-unary-chain-call
                         even-handle value (plist-get even-build :result-root-index)))
           (odd-native (nelisp-native-load-raw-v2-unary-chain-call
                        odd-handle value (plist-get odd-build :result-root-index)))
           (leaf (list 'identity)) (identity-input (cons nil (cons leaf nil))))
      (garbage-collect)
      (unless (and (equal even-vm even-native) (equal odd-vm odd-native)
                   (eq leaf (nelisp-native-load-raw-v2-unary-chain-call
                             even-handle identity-input 1)))
        (error "chain mismatch: even VM=%S native=%S odd VM=%S native=%S identity=%S"
               even-vm even-native odd-vm odd-native
               (nelisp-native-load-raw-v2-unary-chain-call even-handle identity-input 1))))
    (let* ((vm-condition (condition-case err (funcall even 9)
                           (wrong-type-argument err)))
           (native-condition
            (condition-case err
                (nelisp-native-load-raw-v2-unary-chain-call even-handle 9 1)
              (wrong-type-argument err))))
      (unless (equal vm-condition native-condition)
        (error "first VM/native condition differs: %S / %S" vm-condition native-condition))
      (let ((condition native-condition))
      (unless (equal condition '(wrong-type-argument listp 9))
        (error "first type error mismatch: %S" condition))))
    (let* ((bad-argument (cons nil 9))
           (vm-condition (condition-case err (funcall error-function bad-argument)
                           (wrong-type-argument err)))
           (native-condition
            (condition-case err
                (nelisp-native-load-raw-v2-unary-chain-call
                 error-handle bad-argument (plist-get error-build :result-root-index))
              (wrong-type-argument err))))
      (unless (equal vm-condition native-condition)
        (error "intermediate VM/native condition differs: %S / %S"
               vm-condition native-condition))
      (let ((condition native-condition))
      (unless (equal condition '(wrong-type-argument listp 9))
        (error "intermediate type error mismatch: %S" condition))))
    t))

(provide 'standalone-bytecode-native-unary-chain-driver)
;;; standalone-bytecode-native-unary-chain-driver.el ends here
