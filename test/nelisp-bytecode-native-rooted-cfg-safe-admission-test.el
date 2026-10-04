;;; nelisp-bytecode-native-rooted-cfg-safe-admission-test.el --- admission negatives -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-cfg-plan)

(defun nelisp-bytecode-native-rooted-cfg-safe-admission-test--build (form)
  (nelisp-bytecode-compiler-input-build (byte-compile form)))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-admission/rebuilds-genuine-pinned-input ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-safe-admission-test--build
                 '(lambda (value) (car-safe value))))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input 'safe-primitives-v3)))
    (should (eq (plist-get plan :status) 'complete))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-admission/refuses-forged-dialect-and-ir ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-safe-admission-test--build
                 '(lambda (value) (car-safe value))))
         (wrong-dialect (copy-tree input))
         (wrong-status (copy-tree input))
         (wrong-ir (copy-tree input))
         (evidence (copy-tree (plist-get input :dialect-evidence)))
         (ir (copy-tree (plist-get wrong-ir :ir-result)))
         (rows (copy-sequence (plist-get ir :instructions)))
         (frame (copy-tree (plist-get wrong-ir :frame-result)))
         (blocks (copy-sequence (plist-get frame :blocks)))
         (block (copy-tree (aref blocks 0)))
         (frame-rows (copy-sequence (plist-get block :instructions))))
    (setf (plist-get evidence :dialect) "GNU Emacs 30.1")
    (setf (plist-get wrong-dialect :dialect-evidence) evidence)
    (setf (plist-get wrong-status :status) 'complete)
    (aset rows 0 (copy-sequence (aref rows 0)))
    (aset (aref rows 0) 1 163)
    (aset (aref rows 0) 4 '(:kind cdr-safe :stack-delta 0 :lowerable nil :width 1))
    (setf (plist-get ir :instructions) rows)
    (setf (plist-get wrong-ir :ir-result) ir)
    (aset frame-rows 0 (copy-tree (aref frame-rows 0)))
    (setf (plist-get (aref frame-rows 0) :opcode) 163)
    (setf (plist-get block :instructions) frame-rows)
    (aset blocks 0 block)
    (setf (plist-get frame :blocks) blocks)
    (setf (plist-get wrong-ir :frame-result) frame)
    (should (= (aref (plist-get wrong-ir :code) 0) 162))
    (dolist (forged (list wrong-dialect wrong-status wrong-ir))
      (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                              forged 'safe-primitives-v3)
                             :status)
                  'unsupported)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-admission/refuses-captures-and-variable-arity ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((optional (nelisp-bytecode-native-rooted-cfg-safe-admission-test--build
                    '(lambda (&optional value) (car-safe value))))
         (rest (nelisp-bytecode-native-rooted-cfg-safe-admission-test--build
                '(lambda (value &rest more) (car-safe value))))
         (closure (eval '(let ((captured (make-symbol "captured")))
                           (lambda (value) (if captured (car-safe value) nil))) t))
         (captured (nelisp-bytecode-compiler-input-build closure)))
    (should (eq (plist-get optional :status) 'unsupported))
    (should (eq (plist-get rest :status) 'unsupported))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                            optional 'safe-primitives-v3)
                           :status)
                'unsupported))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                            rest 'safe-primitives-v3)
                           :status)
                'unsupported))
    (should (functionp closure))
    (should-not (byte-code-function-p closure))
    (should (eq (plist-get captured :status) 'malformed))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                            captured 'safe-primitives-v3)
                           :status)
                'unsupported))))

(ert-run-tests-batch-and-exit)

;;; nelisp-bytecode-native-rooted-cfg-safe-admission-test.el ends here
