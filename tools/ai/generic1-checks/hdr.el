;;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'pcase nil t)
(defmacro T (&rest f) `(condition-case e (let ((r (eval ',(car f) t))) (prin1 ',(car f)) (princ " => ") (prin1 r) (terpri)) (error (prin1 ',(car f)) (princ (format " => ERR %S\n" (car e))))))
