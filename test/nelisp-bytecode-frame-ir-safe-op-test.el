;;; nelisp-bytecode-frame-ir-safe-op-test.el --- Safe primitive frame effects -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-frame-ir)

(ert-deftest nelisp-bytecode-frame-ir/classifies-real-safe-car-cdr-bytecode ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (case '((car-safe 162) (cdr-safe 163)))
    (let* ((operator (car case))
           (opcode (cadr case))
           (function (byte-compile `(lambda (value) (,operator value))))
           (code (aref function 1))
           (frame (nelisp-bytecode-frame-ir-build code (aref function 2) 1))
           (instructions
            (and (eq (plist-get frame :status) 'complete)
                 (append (plist-get (aref (plist-get frame :blocks) 0)
                                    :instructions)
                         nil)))
           (operation (cl-find opcode instructions
                               :key (lambda (item) (plist-get item :opcode)))))
      (should (byte-code-function-p function))
      (should (eq (plist-get frame :status) 'complete))
      (should operation)
      (should (eq (plist-get operation :kind) 'primitive))
      (should (= (length (plist-get operation :inputs)) 1))
      (should (= (length (plist-get operation :outputs)) 1)))))

(ert-deftest nelisp-bytecode-frame-ir/safe-primitive-underflow-is-malformed ()
  (dolist (opcode '(162 163))
    (let* ((code (unibyte-string opcode 135))
           (decoded (nelisp-bytecode-ir-decode-result code []))
           (instruction (aref (plist-get decoded :instructions) 0))
           (frame (nelisp-bytecode-frame-ir-build code [] 0)))
      ;; This assertion fails if the safe primitive is accidentally omitted
      ;; from the explicit one-input opcode table, even if RETURN also fails.
      (should (= (nelisp-bytecode-frame-ir--min-inputs instruction) 1))
      (should (= (aref instruction 0) 0))
      (should (= (aref instruction 1) opcode))
      (should (eq (plist-get frame :status) 'malformed))
      (should-not (plist-get frame :blocks)))))

(ert-run-tests-batch-and-exit)

;;; nelisp-bytecode-frame-ir-safe-op-test.el ends here
