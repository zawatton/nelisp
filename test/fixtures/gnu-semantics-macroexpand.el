;;; -*- lexical-binding: t; -*-
(defmacro gs-expand-first (x) (list 'gs-expand-second x))
(defmacro gs-expand-second (x) (list '+ x 1))
(fset 'gs-expand-alias 'gs-expand-first)
(let ((form '(ordinary 3)))
  (prin1 (list 'ordinary-identity (eq form (macroexpand form)))) (terpri))
(prin1 (list 'chain (macroexpand '(gs-expand-first 3)))) (terpri)
(prin1 (list 'alias (macroexpand '(gs-expand-alias 3)))) (terpri)
(prin1 (list 'environment
             (macroexpand '(gs-expand-first 3)
                          (list (cons 'gs-expand-first
                                      (lambda (x) (list 'quote x))))))) (terpri)
(let ((form '(gs-expand-first 3)))
  (prin1 (list 'nil-shadow
               (eq form (macroexpand form '((gs-expand-first)))))) (terpri))
(prin1 (list 'nonsymbol-head (macroexpand '(1) "invalid environment"))) (terpri)

(prin1 (list 'alias-target-environment
             (macroexpand '(gs-expand-alias 3)
                          (list (cons 'gs-expand-first
                                      (lambda (x) (list 'quote x))))))) (terpri)
(let ((form '(gs-expand-alias 3)))
  (prin1 (list 'alias-target-nil-shadow
               (eq form (macroexpand form '((gs-expand-first)))))) (terpri))
(prin1 (list 'malformed-environment
             (condition-case e (macroexpand '(gs-expand-first 3) "bad")
               (error e)))) (terpri)
(prin1 (list 'cons-macro-head
             (macroexpand (cons (cons 'macro (lambda (x) (list '+ x 2))) '(3)))))
(terpri)
(autoload 'gs-expand-autoload
  (expand-file-name "test/fixtures/gnu-semantics-macroexpand-library.el") nil nil 'macro)
(prin1 (list 'autoload (macroexpand '(gs-expand-autoload 4)))) (terpri)
(kill-emacs 0)
