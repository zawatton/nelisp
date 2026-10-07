;;; nelisp-bytecode-native-rooted-conditional-test.el --- conditional import contract -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-native-load)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-conditional)

(ert-deftest nelisp-rooted-conditional/slot-import-is-isolated-and-typed ()
  (let* ((name nelisp-native-load-raw-v2-conditional-slot-import)
         (index (nelisp-native-load--raw-v2-conditional-import-index name)))
    (should (equal name "nl_root_pin_slot_v2"))
    (should (eq (nelisp-native-load--raw-v2-conditional-import-mode name)
                'conditional-root-slot-v1))
    (should (integerp index))
    (should (equal (nth index nelisp-native-load-bridgeable-symbols) name))
    ;; The extension must not widen either pre-existing raw import path.
    (should-not (nelisp-native-load--raw-v2-import-mode name))
    (should (eq (nelisp-native-load--raw-v2-import-mode "nl_native_car_v2")
                'native-bridgeable-v1))
    (should-not (nelisp-native-load--raw-v2-conditional-import-mode
                 "nl_native_car_v2"))
    (should-not (nelisp-native-load--raw-v2-conditional-import-mode
                 "nl_root_pin_reserve_v2"))))

(ert-deftest nelisp-rooted-conditional/plans-genuine-three-argument-gnu-frame ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((code (unibyte-string 2 131 6 0 1 135 135))
         (function (make-byte-code 771 code [] 4))
         (input (nelisp-bytecode-compiler-input-build function))
         (plan (nelisp-bytecode-native-rooted-conditional-plan input))
         (body (nelisp-bytecode-native-rooted-conditional-body)))
    (should (eq (plist-get plan :status) 'complete))
    (should (= (plist-get plan :required-root-count) 4))
    (should (equal (plist-get plan :imports) '("nl_root_pin_slot_v2")))
    (should (eq (funcall function nil 'then-value 'else-value) 'else-value))
    (should (eq (funcall function t 'then-value 'else-value) 'then-value))
    (should (eq (car body) 'defun))
    (should (= (let ((text (prin1-to-string body)) (start 0) (count 0))
                 (while (string-match "nl_root_pin_slot_v2" text start)
                   (setq count (1+ count) start (match-end 0)))
                 count)
               3))
    (should (string-match-p "condition-slot" (prin1-to-string body)))
    (let ((bad (copy-sequence input)))
      (plist-put bad :code (unibyte-string 2 131 6 0 2 135 135))
      (should (eq (plist-get (nelisp-bytecode-native-rooted-conditional-plan bad)
                             :status)
                  'unsupported)))))

(ert-deftest nelisp-rooted-conditional/manifest-contract-checks-typed-pointer-abi ()
  (let* ((index (nelisp-native-load--raw-v2-conditional-import-index
                 "nl_root_pin_slot_v2"))
         (entry (list :name "nl_native_rooted_conditional_probe_v1" :type 'func
                      :abi nelisp-native-load-raw-runtime-abi-v2 :arity 4
                      :params '(u64 u64 u64 u64) :return 'u64))
         (import (list :name "nl_root_pin_slot_v2" :kind 'func
                       :abi nelisp-native-load-raw-runtime-abi-v2 :index index
                       :address-mode 'conditional-root-slot-v1 :arity 6
                       :params '(u64 u64 u64 u64 u64 u64) :return 'u64))
         (hash (nelisp-native-load--sha256
                (prin1-to-string
                 (list nelisp-native-load-raw-v2-conditional-contract-version
                       "nl_native_rooted_conditional_probe_v1"
                       '("nl_root_pin_slot_v2")
                       '(u64 u64 u64 u64 u64 u64) 'u64))))
         (manifest (list :native-rooted-conditional-contract-version
                         nelisp-native-load-raw-v2-conditional-contract-version
                         :native-rooted-conditional-entry
                         "nl_native_rooted_conditional_probe_v1"
                         :native-rooted-conditional-imports '("nl_root_pin_slot_v2")
                         :native-rooted-conditional-status-base 256
                         :native-rooted-conditional-contract-hash hash
                         :native (list :exports (list entry) :imports (list import)))))
    (should (nelisp-native-load--raw-v2-conditional-contract-valid-p manifest))
    (plist-put import :params '(u64 u64 u64 u64 u64 s64))
    (should-not (nelisp-native-load--raw-v2-conditional-contract-valid-p manifest))))

(provide 'nelisp-bytecode-native-rooted-conditional-test)
;;; nelisp-bytecode-native-rooted-conditional-test.el ends here
