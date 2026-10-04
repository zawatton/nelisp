;;; nelisp-bytecode-native-call1-test.el --- Exact public CALL1 slice -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-native-compiler)
(require 'nelisp-bytecode-native-package)
(require 'nelisp-native-load)

(ert-deftest nelisp-bytecode-native-call1/typed-gateway-contract ()
  (let* ((entry (list :name nelisp-native-load-raw-v2-call1-import
                      :kind 'func :abi nelisp-native-load-raw-runtime-abi-v2
                      :arity 6 :params '(u64 u64 u64 u64 u64 u64) :return 'u64))
         (manifest (list :call1-contract-version
                         nelisp-native-load-raw-v2-call1-contract-version
                         :call1-contract-hash
                         (nelisp-native-load-raw-v2-call1-contract-hash)
                         :call1-producer-validation-version
                         "nelisp-call1-exact-ast-v1"
                         :call1-producer-ast-sha256 (make-string 64 ?b)
                         :call1-caller
                         '(:name "nl_native_bytecode_call1_exit"
                           :arity 2 :params (u64 u64) :return u64
                           :slots (4 5 2 0)))))
    (should (nelisp-native-load-raw-v2-call1-import-valid-p manifest entry))
    (should-not
     (nelisp-native-load-raw-v2-call1-import-valid-p
      manifest (plist-put (copy-sequence entry) :params '(u64 u64))))))

(ert-deftest nelisp-bytecode-native-call1/exact-template-and-nearby-refusals ()
  (let ((function (make-byte-code 257 (unibyte-string 192 1 33 135)
                                [call1-probe-callee] 3 "probe")))
    (should (plist-get (nelisp-bytecode-compiler-input-build function)
                       :call1-symbol-template-p)))
  (dolist (function
           (list (make-byte-code 257 (unibyte-string 192 1 32 135)
                                 [call1-probe-callee] 3)
                 (make-byte-code 257 (unibyte-string 192 1 33 135) [nil] 3)
                 (make-byte-code 257 (unibyte-string 192 1 33 135)
                                 (vector (make-symbol "callee")) 3)
                 (make-byte-code 257 (unibyte-string 192 1 33 135)
                                 [call1-probe-callee] 2)))
    (should-not (plist-get (nelisp-bytecode-compiler-input-build function)
                           :call1-symbol-template-p))))

(ert-deftest nelisp-bytecode-native-call1/package-refuses-before-publication ()
  (let* ((source (make-temp-file "nelisp-call1-package-" nil ".el"))
         (elc (concat source "c"))
         (directory (concat source ".neln"))
         (mkdirs 0) (backend 0)
         (real-mkdir (symbol-function 'make-directory)))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(defun call1-package-probe (arg)\n"
                    "  (call1-package-callee arg))\n"
                    "(provide 'call1-package-probe)\n"))
          (unless (byte-compile-file source) (error "byte compilation failed"))
          (cl-letf (((symbol-function 'make-directory)
                     (lambda (&rest args) (setq mkdirs (1+ mkdirs))
                       (apply real-mkdir args)))
                    ((symbol-function 'nelisp-bytecode-native-compiler-build)
                     (lambda (&rest _args) (setq backend (1+ backend)))))
            (should-error
             (nelisp-bytecode-native-package-compile-elc
              elc 'call1-package-probe '(call1-package-probe) directory))
            (should (= mkdirs 0))
            (should (= backend 0))
            (should-not (file-exists-p directory))))
      (dolist (path (list source elc (concat elc "~")))
        (when (file-exists-p path) (delete-file path))))))

(provide 'nelisp-bytecode-native-call1-test)
;;; nelisp-bytecode-native-call1-test.el ends here
