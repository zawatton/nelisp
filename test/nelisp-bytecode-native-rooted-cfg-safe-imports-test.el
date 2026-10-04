;;; nelisp-bytecode-native-rooted-cfg-safe-imports-test.el --- bounded AST imports -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-native-rooted-cfg-safe-contract)

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-imports/accepts-shared-acyclic-nodes ()
  (let* ((vector (vector 'value))
         (call (list 'extern-call 'nl_native_car_v2 vector 2))
         (form (list call call)))
    (should (equal (nelisp-bytecode-native-rooted-cfg-safe-contract--form-imports
                    form)
                   '("nl_native_car_v2")))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-imports/rejects-active-cycles ()
  (let ((cons-cycle (list 'extern-call 'nl_native_car_v2))
        (vector-cycle (vector nil)))
    (setcdr (last cons-cycle) cons-cycle)
    (aset vector-cycle 0 vector-cycle)
    (should-not
     (nelisp-bytecode-native-rooted-cfg-safe-contract--form-imports cons-cycle))
    (should-not
     (nelisp-bytecode-native-rooted-cfg-safe-contract--form-imports vector-cycle))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-imports/rejects-depth-and-node-overflow ()
  (let ((deep '(extern-call nl_native_car_v2))
        (wide (make-vector 20000 nil)))
    (dotimes (_ 260) (setq deep (list deep)))
    (should-not
     (nelisp-bytecode-native-rooted-cfg-safe-contract--form-imports deep))
    (should-not
     (nelisp-bytecode-native-rooted-cfg-safe-contract--form-imports wide))))

(provide 'nelisp-bytecode-native-rooted-cfg-safe-imports-test)
;;; nelisp-bytecode-native-rooted-cfg-safe-imports-test.el ends here
