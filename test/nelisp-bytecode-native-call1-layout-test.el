;;; nelisp-bytecode-native-call1-layout-test.el --- fixed CALL1 layout checks -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-call1-layout)

(defun nelisp-bytecode-native-call1-layout-test--input ()
  (nelisp-bytecode-compiler-input-build
   (byte-compile (lambda (f x) (funcall f x)))))

(defun nelisp-bytecode-native-call1-layout-test--mutate (input index key value)
  (let* ((copy (copy-tree input t))
         (rows (plist-get (aref (plist-get (plist-get copy :frame-result) :blocks) 0)
                          :instructions)))
    (aset rows index (plist-put (aref rows index) key value))
    copy))

(ert-deftest nelisp-bytecode-native-call1-layout/genuine-four-row-layout-is-pure ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-call1-layout-test--input))
         (before (copy-tree input t))
         (rows (plist-get (aref (plist-get (plist-get input :frame-result) :blocks) 0)
                          :instructions))
         (layout (nelisp-bytecode-native-call1-layout input)))
    (should (eq (plist-get input :status) 'complete))
    (should (= (length rows) 4))
    (should (equal (mapcar (lambda (row) (plist-get row :kind)) (append rows nil))
                   '(stack-ref stack-ref call return)))
    (should (eq (plist-get layout :status) 'complete))
    (should (equal (list (plist-get layout :function-root)
                         (plist-get layout :argument-root)
                         (plist-get layout :result-root)
                         (plist-get layout :exit-roots)
                         (plist-get layout :exit-root-base)
                         (plist-get layout :next-root)
                         (plist-get layout :required-root-count)
                         (plist-get layout :provider))
                   '(1 2 3 (4 5 6) 4 7 7 nl_native_call_v2)))
    (should (equal input before))))

(ert-deftest nelisp-bytecode-native-call1-layout/rejects-non-two-arity ()
  (let ((input (plist-put (nelisp-bytecode-native-call1-layout-test--input)
                          :argument-count 1)))
    (should (eq (plist-get (nelisp-bytecode-native-call1-layout input) :status)
                'unsupported))))

(ert-deftest nelisp-bytecode-native-call1-layout/rejects-wrong-call-operand ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-call1-layout-test--mutate
                 (nelisp-bytecode-native-call1-layout-test--input) 2 :operand 2))
         (layout (nelisp-bytecode-native-call1-layout input)))
    (should (eq (plist-get layout :status) 'unsupported))))

(ert-deftest nelisp-bytecode-native-call1-layout/rejects-extra-instruction ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (copy-tree (nelisp-bytecode-native-call1-layout-test--input) t))
         (block (aref (plist-get (plist-get input :frame-result) :blocks) 0))
         (rows (plist-get block :instructions)))
    (plist-put block :instructions (vconcat rows (vector (aref rows 3))))
    (should (eq (plist-get (nelisp-bytecode-native-call1-layout input) :status)
                'unsupported))))

(ert-deftest nelisp-bytecode-native-call1-layout/rejects-missing-call-output ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-call1-layout-test--mutate
                 (nelisp-bytecode-native-call1-layout-test--input) 2 :outputs nil)))
    (should (eq (plist-get (nelisp-bytecode-native-call1-layout input) :status)
                'unsupported))))

(ert-deftest nelisp-bytecode-native-call1-layout/rejects-unresolved-call-operands ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-call1-layout-test--mutate
                 (nelisp-bytecode-native-call1-layout-test--input) 2 :inputs
                 '((:value 999 0) (:value 1000 0)))))
    (should (eq (plist-get (nelisp-bytecode-native-call1-layout input) :status)
                'unsupported))))

(ert-deftest nelisp-bytecode-native-call1-layout/rejects-cyclic-outer-plist-safely ()
  (let ((input (list :status 'complete)))
    (setcdr (cdr input) input)
    (should (eq (plist-get (nelisp-bytecode-native-call1-layout input) :status)
                'unsupported))))

(ert-deftest nelisp-bytecode-native-call1-layout/source-mutant-fails-same-positive-oracle ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-call1-layout-test--input))
         (source (with-temp-buffer
                   (insert-file-contents "lisp/nelisp-bytecode-native-call1-layout.el")
                   (buffer-string)))
         (mutant (replace-regexp-in-string "(= (length rows) 4)"
                                           "(= (length rows) 2)" source t t))
         (names '(nelisp-bytecode-native-call1-layout
                  nelisp-bytecode-native-call1-layout--bounded-plist-p
                  nelisp-bytecode-native-call1-layout--token-p))
         (saved (mapcar (lambda (name) (cons name (symbol-function name))) names)))
    (should-not (equal source mutant))
    (should (eq (plist-get (nelisp-bytecode-native-call1-layout input) :status)
                'complete))
    (unwind-protect
        (with-temp-buffer
          (insert mutant)
          (eval-buffer)
          (should (eq (plist-get (nelisp-bytecode-native-call1-layout input) :status)
                      'unsupported))
          (should-error
           (should (eq (plist-get (nelisp-bytecode-native-call1-layout input) :status)
                       'complete))))
      (dolist (entry saved) (fset (car entry) (cdr entry))))
    (should (eq (plist-get (nelisp-bytecode-native-call1-layout input) :status)
                'complete))))

(provide 'nelisp-bytecode-native-call1-layout-test)
;;; nelisp-bytecode-native-call1-layout-test.el ends here
