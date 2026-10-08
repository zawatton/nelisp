;;; -*- lexical-binding: t; -*-

(defun nl_native_boxed_eq ((left :type sexp) (right :type sexp))
  (eq left right))

(defun nl_native_boxed_mutate_collect_return ((value :type sexp))
  (seq
   (garbage-collect)
   (setcar value 'native-mutated)
   value))

(defun nl_native_boxed_hidden_mutate_collect_return ((value :type sexp))
  (seq
   (garbage-collect)
   (setcar value 'native-mutated)
   value))

(defun nl_native_boxed_raw_add (left right)
  (+ left right))

(provide 'native-boxed-unit-values)
