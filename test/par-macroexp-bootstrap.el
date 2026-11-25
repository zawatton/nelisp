;;; par-macroexp-bootstrap.el --- Macro expansion after runtime preload -*- lexical-binding: t; -*-

(require 'macroexp)
(require 'cl-lib)
(require 'rx)

(defmacro par-macroexp-when (test value)
  `(when ,test (list ,value)))

(defalias 'par-macroexp-alias 'par-macroexp-when)

(defun par-macroexp-row (name value)
  (prin1 (list name value))
  (terpri))

(let ((form '(list (quote (par-macroexp-when t 9)) (+ 1 2))))
  (par-macroexp-row 'unchanged (eq form (macroexpand-all form))))

(par-macroexp-row
 'alias
 (eval (macroexpand-all '(list (par-macroexp-alias t 8)
                               (par-macroexp-alias nil 9)))
       t))

(par-macroexp-row
 'compiler-macro
 (eval (macroexpand-all
        '(cl-block outer (let ((value 21)) (cl-return-from outer value)) 0))
       t))

(par-macroexp-row
 'lambda
 (eval (macroexpand-all
        '(funcall #'(lambda (x) (par-macroexp-alias x x)) 7))
       t))

(par-macroexp-row
 'rx-locals
 (eval (macroexpand-all '(rx-let ((token "abc")) (rx token (one-or-more digit))))
       t))

(fset 'par-macroexp-alias (cons 'macro (lambda (_test value) (list '1+ value))))
(par-macroexp-row 'redefined (eval (macroexpand-all '(par-macroexp-alias t 9)) t))
