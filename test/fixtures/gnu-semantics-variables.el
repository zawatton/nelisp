;;; gnu-semantics-variables.el --- GNU declaration fixture -*- lexical-binding: t; -*-

(defun gs-variables-row (name form)
  (prin1 (list name (condition-case err (eval form t) (error err))))
  (terpri))

(gs-variables-row 'initialize-top-level
 '(progn
    (condition-case nil (defvar gs-var-uninitialized (error "init")) (error nil))
    (list (let ((gs-var-uninitialized 7))
            (defvar gs-var-uninitialized 2) gs-var-uninitialized)
          gs-var-uninitialized)))
(gs-variables-row 'oldest-sequential-binding
 '(progn
    (condition-case nil (defvar gs-var-sequential (error "init")) (error nil))
    (list (let* ((gs-var-sequential 7) (gs-var-sequential 8))
            (defvar gs-var-sequential 9) gs-var-sequential)
          gs-var-sequential)))
(gs-variables-row 'oldest-nested-binding
 '(progn
    (condition-case nil (defvar gs-var-nested (error "init")) (error nil))
    (list (let ((gs-var-nested 7))
            (list (let ((gs-var-nested 8))
                    (defvar gs-var-nested 9) gs-var-nested)
                  gs-var-nested))
          gs-var-nested)))
(gs-variables-row 'preserve-initialized-top-level
 '(progn (defvar gs-var-initialized 3)
         (list (let ((gs-var-initialized 7))
                 (defvar gs-var-initialized (error "must not run"))
                 gs-var-initialized)
               gs-var-initialized)))
(gs-variables-row 'initialize-void-current-binding
 '(progn (defvar gs-var-void 3)
         (list (let ((gs-var-void 7))
                 (makunbound 'gs-var-void)
                 (defvar gs-var-void 9) gs-var-void)
               gs-var-void)))
(gs-variables-row 'declaration-order
 '(progn
    (defvar gs-var-order (special-variable-p 'gs-var-order))
    (defconst gs-const-order (special-variable-p 'gs-const-order))
    (list gs-var-order gs-const-order (special-variable-p 'gs-const-order))))
(gs-variables-row 'declaration-special-objects
 '(list (special-form-p 'defvar) (special-form-p 'defconst)
        (macroexpand '(defvar gs-var-expansion 7))
        (macroexpand '(defconst gs-const-expansion 9))))
(gs-variables-row 'ordinary-marker-name
 '(let ((form 'nelisp--unbound-marker))
    (list form (funcall (lambda () form)))))
(gs-variables-row 'ordinary-marker-global-value
 '(progn (setq gs-var-marker-global 'nelisp--unbound-marker)
         (list (boundp 'gs-var-marker-global)
               (symbol-value 'gs-var-marker-global) gs-var-marker-global)))
(gs-variables-row 'ordinary-marker-dynamic-value
 '(progn (defvar gs-var-marker-dynamic nil)
         (let ((gs-var-marker-dynamic 'nelisp--unbound-marker))
           (list (boundp 'gs-var-marker-dynamic)
                 (symbol-value 'gs-var-marker-dynamic)
                 (funcall (lambda () gs-var-marker-dynamic))))))
(gs-variables-row 'fresh-marker-name
 '(let ((value (make-symbol "nelisp--unbound-marker")))
    (list (symbol-name value) (eq value 'nelisp--unbound-marker)
          (funcall (lambda () (symbol-name value))))))
(gs-variables-row 'ordinary-marker-function-cell
 '(progn (fset 'gs-var-marker-function 'nelisp--unbound-marker)
         (list (fboundp 'gs-var-marker-function)
               (symbol-function 'gs-var-marker-function))))

;; Avoid the standalone --load driver's printing of the final form's value.
(kill-emacs 0)

;;; gnu-semantics-variables.el ends here
