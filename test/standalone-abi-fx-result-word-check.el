;;; standalone-abi-fx-result-word-check.el --- VM result roots -*- lexical-binding: t; -*-
(defun abi-fx-result-gc (value) (garbage-collect) value)
(defun abi-fx-result-caller (value) (abi-fx-result-gc value))
(let ((checked 0))
  (dolist (value (list nil t 0 -7 2305843009213693952 9223372036854775807
                      -9223372036854775808 1.25 "vm result" '(a . b) [nil t -7]))
    (dolist (call (list (list 'abi-fx-result-gc (list 'quote value))
                       (list 'abi-fx-result-caller (list 'quote value))
                       (list 'funcall (list 'quote 'abi-fx-result-gc) (list 'quote value))
                       (list 'apply (list 'quote 'abi-fx-result-gc) (list 'quote (list value)))))
      (unless (equal (eval call t) value) (error "VM result changed: %S" call))
      (setq checked (1+ checked))))
  (when (boundp 'nelisp--fx-table)
    (dolist (fn '(abi-fx-result-gc abi-fx-result-caller))
      (unless (vectorp (gethash (nthcdr 3 (symbol-function fn)) nelisp--fx-table))
        (error "VM result fixture was not compiled: %S" fn))))
  (princ (format "ABI-FX-RESULT checked=%d PASS\n" checked)))
nil
