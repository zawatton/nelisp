;;; nelisp-n3-explicit-special-formal-reference.el --- Known eval gap -*- lexical-binding: t; -*-
;; An explicit lexical alist entry overrides the global special declaration
;; while binding a same-named closure parameter. GNU prints (7 1); N3-11
;; currently prints (7 7). Keep this known-failing reference separate.
(defvar n3explicit-special 1)
(let ((fn (eval '(lambda (n3explicit-special)
                  (list n3explicit-special (symbol-value 'n3explicit-special)))
                '((n3explicit-special . 4)))))
  (prin1 (funcall fn 7)) (terpri))
(princ "N3-EXPLICIT-SPECIAL-FORMAL-DONE\n")
nil
