;;; -*- lexical-binding: nil; -*-

(defvar nelisp-va-base-value 'initial)

(defun nelisp-va-old-value () 'old-function)
(defun nelisp-va-base-value () 'base-function)

(defun nelisp-va-call-defvaralias-2 ()
  (defvaralias 'nelisp-va-old-value 'nelisp-va-base-value))

(defun nelisp-va-call-defvaralias-3 ()
  (defvaralias 'nelisp-va-doc-value 'nelisp-va-base-value "alias doc"))

(defun nelisp-va-call-chain ()
  (defvaralias 'nelisp-va-chain-value 'nelisp-va-old-value))

(defun nelisp-va-call-symbol-value ()
  (symbol-value 'nelisp-va-base-value))

(defun nelisp-va-call-symbol-value-unbound ()
  (symbol-value 'nelisp-va-never-bound-7f31))

(defun nelisp-va-call-symbol-value-nonsymbol ()
  (symbol-value 17))

(defun nelisp-va-call-cycle ()
  (defvaralias 'nelisp-va-base-value 'nelisp-va-old-value))

(defun nelisp-va-call-dynamic-alias ()
  (let ((nelisp-va-old-value 'dynamic-value))
    (cons nelisp-va-old-value nelisp-va-base-value)))

(defun nelisp-va-call-makunbound-alias ()
  (makunbound 'nelisp-va-old-value))

(provide 'gnu-31.1-variable-alias-callable)
