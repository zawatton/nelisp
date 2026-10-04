;;; nelisp-n3-set-reference.el --- Lisp setter validation reference -*- lexical-binding: t; -*-
;; Run unchanged on GNU and the reader; validation remains in the prelude.
(defvar n3set-boundary-x 1)
(defmacro n3set-row (name form)
  `(progn (princ ,name) (princ "|")
          (prin1 (condition-case e ,form (error e))) (terpri)))
(n3set-row "set-global" (list (set 'n3set-boundary-x 2) n3set-boundary-x))
(n3set-row "set-dynamic" (let ((n3set-boundary-x 3)) (list (set 'n3set-boundary-x 4) n3set-boundary-x)))
(n3set-row "set-wrong-type" (set 23 4))
(n3set-row "set-nil" (set nil 4))
(n3set-row "set-t" (set t 4))
(n3set-row "set-keyword" (set :n3set-boundary 4))
(n3set-row "set-uninterned-colon" (set (make-symbol ":n3set-boundary") 5))
(princ "N3-SET-DONE\n")
nil
