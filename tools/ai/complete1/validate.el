;;; validate.el --- host-side registration check for the complete1 native arms  -*- lexical-binding: t; -*-
;; usage: emacs --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build -l tools/ai/complete1/validate.el
(let ((arities (nelisp-standalone--builtin-arities)))
  (dolist (n '("try-completion" "all-completions" "test-completion" "unintern" "obarrayp"))
    (unless (assoc n arities) (error "no arity: %s" n))
    (unless (member n nelisp-standalone--reader-builtins) (error "not a reader builtin: %s" n))
    (unless (cl-find-if (lambda (e) (equal (cadar e) n)) (nelisp-standalone--applyfn-reader-table))
      (error "no arm: %s" n))))
(nelisp-standalone--validate-reader-registrations)
(message "complete1 validate OK")
