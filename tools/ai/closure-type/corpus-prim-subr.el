;;; corpus-prim-subr.el --- Lisp-defined builtins are subrs (GNU 31.1 differential)  -*- lexical-binding: t; -*-
;; Run with tools/ai/cl-gnu/run-corpus.el; expected output is GNU 31.1's.
;; NeLisp defines some GNU C primitives in Lisp; they must still look like subrs.
(let ((f (symbol-function 'get-buffer-create))) (list (subrp f) (functionp f) (type-of f) (closurep f) (interpreted-function-p f) (primitive-function-p f) (subr-primitive-p f)))
(subr-arity (symbol-function 'get-buffer-create))
(subr-name (symbol-function 'get-buffer-create))
(if (= 2 (cdr (subr-arity (symbol-function 'get-buffer-create)))) 'two 'other)
(mapcar (lambda (s) (subrp (symbol-function s))) '(buffer-name string-match expand-file-name message current-buffer looking-at re-search-forward file-exists-p set-buffer buffer-string))
(subr-arity (symbol-function 'message))
(func-arity 'get-buffer-create)
(format "%S" (symbol-function 'buffer-name))
(cl-type-of (symbol-function 'get-buffer-create))
(list (special-form-p 'get-buffer-create) (macrop 'get-buffer-create) (commandp 'buffer-name))
(progn (defun nl-s3-user-fn (x) x) (list (subrp (symbol-function 'nl-s3-user-fn)) (closurep (symbol-function 'nl-s3-user-fn))))
(condition-case e (subr-arity (lambda (x) x)) (error (car e)))
(list (funcall (symbol-function 'buffer-name) (get-buffer-create " s3")) (apply #'string-match '("b" "abc")))
(equal (symbol-function 'buffer-name) (symbol-function 'buffer-name))
