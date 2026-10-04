;;; nelisp-n3-explicit-special-formal-reference.el --- Explicit lexical formal -*- lexical-binding: t; -*-
;; Explicit eval-alist binding shadows the global special declaration even for a same-named closure formal.
(defvar n3explicit-special 1)
(let ((fn (eval '(lambda (n3explicit-special)
                  (list n3explicit-special (symbol-value 'n3explicit-special)))
                '((n3explicit-special . 4)))))
  (prin1 (funcall fn 7)) (terpri))
(princ "N3-EXPLICIT-SPECIAL-FORMAL-DONE\n")
nil
