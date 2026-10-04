;;; nelisp-bytecode-native-rooted-cfg-native-test.el --- native CFG admission guards -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-native-rooted-cfg-native)
(require 'nelisp-bytecode-native-rooted-cfg-call)

(ert-deftest nelisp-bytecode-native-rooted-cfg-native/refuses-invalid-input-before-raw-compile ()
  (let ((compile-count 0)
        (artifact (make-temp-name
                   (expand-file-name "invalid-rooted-cfg.nelr" temporary-file-directory))))
    (cl-letf (((symbol-function 'nelisp-native-load-raw-v2-compile-file)
               (lambda (&rest _)
                 (setq compile-count (1+ compile-count))
                 (error "invalid CFG reached raw compiler"))))
      (should-error
       (nelisp-bytecode-native-rooted-cfg-native-build nil artifact))
      (should-error
       (nelisp-bytecode-native-rooted-cfg-native-build-shared-v2 nil artifact)))
    (should (= compile-count 0))
    (should-not (file-exists-p artifact))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-call/rejects-unregistered-result-before-root-context ()
  (let ((root-lookups 0))
    (cl-letf (((symbol-function 'nelisp-native-load-root-v2-addresses)
               (lambda ()
                 (setq root-lookups (1+ root-lookups))
                 (error "forged result reached root context"))))
      (should-error
       (nelisp-bytecode-native-rooted-cfg-call '(:status complete))))
    (should (= root-lookups 0))))

(provide 'nelisp-bytecode-native-rooted-cfg-native-test)
;;; nelisp-bytecode-native-rooted-cfg-native-test.el ends here
