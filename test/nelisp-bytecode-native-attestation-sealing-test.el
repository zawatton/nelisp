;;; nelisp-bytecode-native-attestation-sealing-test.el --- attestation dependency seals -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-native-compiler)

(ert-deftest dialect-attestation-native-helper-changes-invalidate-package-seals ()
  (dolist (name (append '(nelisp-bytecode-compiler-input--standalone-runtime-p)
                       (when nelisp-bytecode-compiler-input--runtime-source-evaluator
                         '(nelisp--eval-source-string))))
    (let* ((api nelisp-bytecode-native-compiler-package-preflight-api)
           (function (byte-compile '(lambda () 'sealed-value)))
           (sealed (funcall (plist-get api :preflight) function))
           (token (plist-get sealed :token))
           (calls 0))
      (unwind-protect
          (cl-letf (((symbol-function name) (lambda (&rest _) t))
                    ((symbol-function 'nelisp-bytecode-native-compiler--build-from-input)
                     (lambda (&rest _) (setq calls (1+ calls)) 'backend-reached)))
            (should-error (funcall (plist-get api :build) token "unused.neln" "unused"))
            (should (= calls 0)))
        (funcall (plist-get api :discard) token)))))

(provide 'nelisp-bytecode-native-attestation-sealing-test)
