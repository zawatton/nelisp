;;; shared-store.el --- GNU byte-code shared-store fixture -*- lexical-binding: t; -*-

(defvar nelisp-gnu-bytecode-vm-counter 0)
(setq nelisp-gnu-bytecode-vm-counter
      (1+ nelisp-gnu-bytecode-vm-counter))

(defun nelisp-gnu-bytecode-vm-identity (value)
  value)

(defun nelisp-gnu-bytecode-vm-plus-one (value)
  (1+ value))

(defun nelisp-gnu-bytecode-vm-target (value)
  value)

(defun nelisp-gnu-bytecode-vm-call-target (value)
  (nelisp-gnu-bytecode-vm-target value))

(defun nelisp-gnu-bytecode-vm-special-call ()
  (let ((nelisp-gnu-bytecode-vm-counter 77))
    (nelisp-gnu-bytecode-vm-target nelisp-gnu-bytecode-vm-counter)))

(defun nelisp-gnu-bytecode-vm-target2 (left right)
  left)

(defun nelisp-gnu-bytecode-vm-call-target2 (left right)
  (nelisp-gnu-bytecode-vm-target2 left right))

(defun nelisp-gnu-bytecode-vm-target3 (first second third)
  first)

(defun nelisp-gnu-bytecode-vm-call-target3 (first second third)
  (nelisp-gnu-bytecode-vm-target3 first second third))

(provide 'nelisp-gnu-bytecode-vm-shared-store-fixture)
;;; shared-store.el ends here
