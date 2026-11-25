;;; par-eval-core-macros.el --- Lazy eval and macro expansion -*- lexical-binding: t; -*-

(defvar par-eval-macro-count 0)

(defmacro par-eval-macro (x)
  (setq par-eval-macro-count (1+ par-eval-macro-count))
  `(list ,x ,par-eval-macro-count))

;; Eval sees the same cons, but each call to the macro sees current state.
(let* ((form (list 'par-eval-macro 11))
       (a (eval form t))
       (b (eval form t)))
  (fset 'par-eval-macro '(macro lambda (x) (list 'list x :new)))
  (prin1 (list 'lazy a b (eval form t) par-eval-macro-count))
  (terpri))

;; The eager macroexpander substitutes the form once. Its result keeps the
;; expansion, even when the source macro has since been replaced.
(let* ((form (macroexpand-all '(lambda (x) (par-eval-macro x))))
       (fn (eval form t)))
  (fset 'par-eval-macro '(macro lambda (x) (list 'list x :later)))
  (prin1 (list 'expanded (funcall fn 17) (apply fn '(23))
               (eval '(par-eval-macro 29) t) par-eval-macro-count))
  (terpri))
