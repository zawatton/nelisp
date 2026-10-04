;;; emacs-cc-module-predicates-1.el --- Opaque module object predicates -*- lexical-binding: t; -*-

(defun emacs-cc-module-predicates-1--test (name type arguments)
  "Check NAME's arity and recognize an opaque object of TYPE."
  (unless (= (length arguments) 1)
    (signal 'wrong-number-of-arguments
            (list name (length arguments))))
  (let ((object (car arguments)))
    ;; GNU type-of also returns a record's user-selected type name.  Such
    ;; records must not impersonate opaque module objects (src/data.c).
    (and (not (recordp object)) (eq (type-of object) type))))

(unless (fboundp 'user-ptrp)
  (defun user-ptrp (&rest arguments)
    "Return t if OBJECT is an opaque module user pointer."
    (emacs-cc-module-predicates-1--test 'user-ptrp 'user-ptr arguments)))

(unless (fboundp 'module-function-p)
  (defun module-function-p (&rest arguments)
    "Return t if OBJECT is a function loaded from a dynamic module."
    (emacs-cc-module-predicates-1--test
     'module-function-p 'module-function arguments)))

(provide 'emacs-cc-module-predicates-1)
;;; emacs-cc-module-predicates-1.el ends here
