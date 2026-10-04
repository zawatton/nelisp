;;; nelisp-bytecode-native-rooted-cfg-safe-primitives-test.el --- safe op plans -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-cfg-plan)
(require 'nelisp-bytecode-native-rooted-cfg-emit)

(defun nelisp-bytecode-native-rooted-cfg-safe-primitives-test--input (form)
  (nelisp-bytecode-compiler-input-build (byte-compile form)))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-primitives/default-refuses-opt-in-opcodes ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (form '((lambda (value) (car-safe value))
                  (lambda (value) (cdr-safe value))))
    (let* ((input (nelisp-bytecode-native-rooted-cfg-safe-primitives-test--input form))
           (plan (nelisp-bytecode-native-rooted-cfg-plan input)))
      (should (eq (plist-get input :status) 'unsupported))
      (should (eq (plist-get plan :status) 'unsupported)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-primitives/v3-plans-authenticated-nil-output ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (case '(((lambda (value) (car-safe value)) car-safe
                   "nl_native_car_v2")
                  ((lambda (value) (cdr-safe value)) cdr-safe
                   "nl_native_cdr_v2")))
    (let* ((input (nelisp-bytecode-native-rooted-cfg-safe-primitives-test--input
                   (car case)))
           (plan (nelisp-bytecode-native-rooted-cfg-plan
                  input 'safe-primitives-v3))
           (operation
            (car (cl-loop for block in (plist-get plan :blocks) append
                          (cl-remove-if-not
                           (lambda (op) (eq (plist-get op :opcode) (cadr case)))
                           (plist-get block :operations)))))
           (emitted (nelisp-bytecode-native-rooted-cfg-emit
                     plan "nl_native_rooted_cfg_probe_v1"))
           (printed (prin1-to-string (plist-get emitted :form))))
      (should (eq (plist-get plan :status) 'complete))
      (should (eq (plist-get plan :lowering-mode) 'safe-primitives-v3))
      (should (eq (plist-get (plist-get plan :entry-ast) :kind)
                  'rooted-cfg-safe-primitives-v3))
      (should operation)
      (should (= (length (plist-get operation :input-roots)) 1))
      (should-not (memq (plist-get operation :output-root)
                        (plist-get operation :input-roots)))
      (should (equal (plist-get plan :gateway-imports) (list (nth 2 case))))
      (should (eq (plist-get emitted :status) 'complete))
      (should (member (nth 2 case) (plist-get emitted :gateway-imports)))
      (should (member "nl_root_pin_slot_v2" (plist-get emitted :gateway-imports)))
      (should (string-match-p "ptr-write-u64" printed))
      (should (string-match-p "rooted_.*_output_slot" printed)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-primitives/v3-handles-branches-and-constant-roots ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (case '(((lambda (flag value)
                     (if flag (car-safe value) (cdr-safe value)))
                   "nl_native_car_v2" "nl_native_cdr_v2")
                  ((lambda (flag value) (car-safe (if flag value nil)))
                   "nl_native_car_v2")
                  ((lambda (left right) (cdr-safe (cons left right)))
                   "nl_native_cdr_v2" "nl_native_cons_v2")))
    (let* ((input (nelisp-bytecode-native-rooted-cfg-safe-primitives-test--input
                   (car case)))
           (plan (nelisp-bytecode-native-rooted-cfg-plan
                  input 'safe-primitives-v3))
           (emitted (and (eq (plist-get plan :status) 'complete)
                         (nelisp-bytecode-native-rooted-cfg-emit
                          plan "nl_native_rooted_cfg_probe_v1"))))
      (should (eq (plist-get input :status) 'unsupported))
      (should (eq (plist-get plan :status) 'complete))
      (should (eq (plist-get emitted :status) 'complete))
      (dolist (gateway (cdr case))
        (should (member gateway (plist-get emitted :gateway-imports))))))
  (dolist (value '(nil t 0 7 "text" "" (a . b)))
    (should (equal (car-safe value) (condition-case nil (car value) (error nil))))
    (should (equal (cdr-safe value) (condition-case nil (cdr value) (error nil)))))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-safe-primitives-test--input
                 '(lambda (value) (car-safe value))))
         (plan (nelisp-bytecode-native-rooted-cfg-plan
                input 'safe-primitives-v3))
         (mutated (copy-tree plan))
         (operation (car (plist-get (car (plist-get mutated :blocks)) :operations))))
    (setf (plist-get operation :opcode) 'car)
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-emit
                            mutated "nl_native_rooted_cfg_probe_v1")
                           :status)
                'unsupported))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-primitives/v3-rejects-mixed-unsupported-inputs ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-safe-primitives-test--input
                 '(lambda (value) (car-safe value))))
         (mixed (copy-tree input))
         (bad-arity (copy-tree input))
         (malformed (copy-tree input))
         (other-op (nelisp-bytecode-native-rooted-cfg-safe-primitives-test--input
                    '(lambda (flag) (car-safe (progn (insert flag) (if flag nil "text"))))))
         (rows (copy-sequence (plist-get (plist-get input :ir-result) :instructions))))
    (setf (plist-get (plist-get mixed :ir-result) :unsupported)
          (append (plist-get (plist-get mixed :ir-result) :unsupported)
                  '((999 . unsupported-semantics))))
    (setf (plist-get bad-arity :argument-descriptor) 258)
    (aset rows 0 [])
    (setf (plist-get (plist-get malformed :ir-result) :instructions) rows)
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                            mixed 'safe-primitives-v3)
                           :status)
                'unsupported))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                            bad-arity 'safe-primitives-v3)
                           :status)
                'unsupported))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                            malformed 'safe-primitives-v3)
                           :status)
                'unsupported))
    (should (cl-some (lambda (row) (= (aref row 1) 63))
                     (append (plist-get (plist-get other-op :ir-result) :instructions)
                             nil)))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                            other-op 'safe-primitives-v3)
                           :status)
                'unsupported))))

(ert-run-tests-batch-and-exit)

;;; nelisp-bytecode-native-rooted-cfg-safe-primitives-test.el ends here
