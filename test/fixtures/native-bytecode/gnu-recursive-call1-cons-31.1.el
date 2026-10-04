;;; -*- lexical-binding: t; -*-
;;; GNU Emacs 31.1 bytecode fixture for recursive CALL1 and CONS proof.
(defun nelisp-recursive-vm-callback (value)
  value)
(defun nelisp-recursive-call1-wrapper (value)
  (nelisp-recursive-vm-callback value))

(defun nelisp-recursive-cons-template (car-value cdr-value)
  (cons car-value cdr-value))

(provide 'nelisp-recursive-call1-cons-fixture)
