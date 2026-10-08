;;; native-call-v2.el --- GNU 31.1 source-free CALL callees -*- lexical-binding: t; -*-
;; The diagnostic builder byte-compiles this into its output directory.
(defun nelisp-call-fixture-vm0 () nil)
(defun nelisp-call-fixture-vm1 (a) (list a))
(defun nelisp-call-fixture-vm2 (a b) (list a b))
(defun nelisp-call-fixture-vm3 (a b c) (list a b c))
(defun nelisp-call-fixture-vm4 (a b c d) (list a b c d))
(defun nelisp-call-fixture-vm5 (a b c d e) (list a b c d e))
(defun nelisp-call-fixture-gc (object) (garbage-collect) object)
(defun nelisp-call-fixture-signal (data) (signal 'error data))
(defun nelisp-call-fixture-throw (tag value) (throw tag value))
(defun nelisp-call-fixture-quit () (signal 'quit nil))
(provide 'native-call-v2-fixture-callees)
;;; native-call-v2.el ends here
