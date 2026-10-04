;;; nelisp-vector-bounds-parity-cases.el --- one form per line  -*- lexical-binding: t; -*-
;; Bounds and length validation of the native vector/record constructors and
;; `aset'; every out-of-range write must signal instead of touching memory.
(condition-case e (aset (vector 1 2) 2 0) (error e))
(condition-case e (aset (vector 1 2) -1 0) (error e))
(condition-case e (aset (vector) 0 1) (error e))
(condition-case e (aset (make-vector 7 nil) 65 122) (error e))
(let ((v (vector 1 2 3))) (list (aset v 0 9) (aset v 2 7) v))
(condition-case e (make-vector -1 nil) (error e))
(condition-case e (make-vector "bad" nil) (error e))
(condition-case e (make-vector 1.5 nil) (error e))
(list (make-vector 0 t) (make-vector 3 'x))
(condition-case e (make-record 'probe -1 nil) (error e))
(condition-case e (make-record 'probe "bad" nil) (error e))
(make-record 'probe 2 7)
