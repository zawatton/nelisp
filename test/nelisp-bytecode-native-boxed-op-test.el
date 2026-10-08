;;; nelisp-bytecode-native-boxed-op-test.el --- boxed op gateway contract tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-native-load)
(require 'nelisp-bytecode-native-boxed-op)
(require 'nelisp-bytecode-native-compiler)

(ert-deftest nelisp-bytecode-native-boxed-op/v2-contract-has-rooted-status-boundary ()
  (let ((contract nelisp-bytecode-native-boxed-op-gateway-contract))
    (should (= (plist-get contract :version) 2))
    (should (equal (plist-get contract :entry-symbol)
                   "nelisp_bytecode_boxed_op_v2"))
    (should (equal (plist-get contract :frame-layout)
                   '((uint32_t version)
                     (uint32_t slot_count)
                     (uint64_t generation)
                     (uint64_t runtime_cookie)
                     (NelnSexp **registered_slots))))
    (should (eq (plist-get (plist-get contract :inputs) :representation)
                'root-slot-index))
    (should (eq (plist-get (plist-get contract :output) :representation)
                'root-slot-index))
    (should (equal (cdr (assq 0 (plist-get contract :statuses))) 'success))
    (should (equal (cdr (assq 1 (plist-get contract :statuses))) 'wrong-type))
    (should (equal (cdr (assq 3 (plist-get contract :statuses))) 'condition))
    (should (eq (plist-get contract :caller-on-exit) 'vm-unwinder-required))))

(ert-deftest nelisp-bytecode-native-boxed-op/car-is-planned-but-not-emitted ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((function (make-byte-code 257 (unibyte-string 64 135) [] 2))
         (input (nelisp-bytecode-compiler-input-build function))
         (plan (nelisp-bytecode-native-boxed-op-plan input))
         (artifact (make-temp-name
                    (expand-file-name "nelisp-boxed-car-" temporary-file-directory))))
    (should-not (file-exists-p artifact))
    (should (eq (plist-get input :status) 'complete))
    (should (eq (plist-get plan :status) 'unsupported))
    (should (equal (plist-get plan :operations)
                   '((:opcode 64 :name car :arity 1 :accepted-tags (nil cons)
                      :wrong-type-status 1 :allocates nil))))
    (should (= (plist-get plan :gateway-version) 2))
    (let ((build (nelisp-bytecode-native-compiler-build
                  function artifact "nl_boxed_car_probe")))
      (should (eq (plist-get build :status) 'unsupported))
      (should-not (file-exists-p artifact)))))

(ert-deftest nelisp-bytecode-native-boxed-op/runtime-import-is-not-authenticated ()
  (should (member "nelisp_aot_builtin_call1"
                  nelisp-native-load-bridgeable-symbols))
  (should-not (member "nl_jit_cons_car"
                      nelisp-native-load-bridgeable-symbols)))

(ert-deftest nelisp-bytecode-native-boxed-op/malformed-input-is-not-unsupported ()
  (let ((plan (nelisp-bytecode-native-boxed-op-plan
               '(:status malformed :reason "bad bytecode"))))
    (should (eq (plist-get plan :status) 'malformed))
    (should (equal (plist-get plan :reason) "bad bytecode"))))

(provide 'nelisp-bytecode-native-boxed-op-test)
;;; nelisp-bytecode-native-boxed-op-test.el ends here
