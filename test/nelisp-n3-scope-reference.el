;;; nelisp-n3-scope-reference.el --- Formal scope reference -*- lexical-binding: t; -*-
(defmacro n3scope-row (name form)
  `(progn (princ ,name) (princ "|") (prin1 ,form) (terpri)))
(setq n3scope-x 10)
(fset 'n3scope-required (eval '(lambda (n3scope-x) (symbol-value 'n3scope-x)) t))
(fset 'n3scope-optional (eval '(lambda (&optional n3scope-x) (symbol-value 'n3scope-x)) t))
(fset 'n3scope-rest (eval '(lambda (&rest n3scope-x) (symbol-value 'n3scope-x)) t))
(fset 'n3scope-captured (eval '(lambda (n3scope-x) (symbol-value 'n3scope-x)) '(n3scope-x)))
(n3scope-row "required-formal-isolation" (eval '(let ((n3scope-x 99)) (n3scope-required 7)) '(n3scope-x)))
(n3scope-row "optional-formal-isolation" (eval '(let ((n3scope-x 99)) (n3scope-optional 7)) '(n3scope-x)))
(n3scope-row "rest-formal-isolation" (eval '(let ((n3scope-x 99)) (n3scope-rest 7)) '(n3scope-x)))
(n3scope-row "captured-special-control" (eval '(let ((n3scope-x 99)) (n3scope-captured 7)) '(n3scope-x)))
(princ "N3-SCOPE-DONE\n")
nil
