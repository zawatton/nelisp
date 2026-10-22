;;; standalone-eval-call-check.el --- calling a subr from the evaluator -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-eval-call-check.el
;;
;; GNU's eval_sub (eval.c:2540-2800) reads the function cell of the form's car
;; and, for a subr, checks the argument count against min_args and max_args
;; (wrong-number-of-arguments with the ORIGINAL symbol), calls an UNEVALLED
;; subr (a special form) with the unevaluated list, evaluates the arguments of
;; any other subr left to right into an array and calls it.  A special form in
;; a function cell stays a special form when it is called through another name,
;; and funcall and apply of one signal invalid-function.  The cases below pin
;; those behaviours; the output is GNU Emacs's own
;; (test/standalone-eval-call-check.expected).

(defvar ec-log nil)
(defun ec-note (x) (push x ec-log) x)
(defun ec-err (thunk) (condition-case e (funcall thunk) (error e)))
(defmacro ec-case (name form) `(princ (format "%s %S\n" ',name (ec-err (lambda () ,form)))))

;; Argument counts of subrs called from the evaluator, funcall and apply.  (GNU's eager
;; macroexpansion at load rewrites (funcall #'f ...) to (f ...), so a variable carries #'f.)
(ec-case car-0 (car))
(ec-case car-2 (car 1 2))
(ec-case cons-1 (cons 1))
(ec-case cons-3 (cons 1 2 3))
(ec-case make-vector-1 (make-vector 1))
(ec-case substring-0 (substring))
(ec-case funcall-car-0 (funcall 'car))
(ec-case funcall-cons-1 (let ((f #'cons)) (funcall f 1)))
(ec-case apply-cons-1 (apply 'cons '(1)))
(ec-case apply-car-nil (apply #'car nil))
(ec-case eval-car-0 (eval '(car) t))
(ec-case if-0 (eval '(if) t))
(ec-case setq-odd (eval '(setq a) t))
(ec-case quote-2 (eval '(quote 1 2) t))

;; Left-to-right evaluation, MANY and fixed-arity subrs.
(setq ec-log nil)
(ec-case order (list (ec-note 1) (ec-note 2) (ec-note 3)))
(princ (format "order-log %S\n" (reverse ec-log)))
(ec-case plus-many (+ 1 2 3 4 5 6 7 8 9 10))
(ec-case list-nine (list 1 2 3 4 5 6 7 8 9))
(ec-case list-ten (list 1 2 3 4 5 6 7 8 9 10))
(ec-case concat-3 (concat "a" "b" "c"))
(ec-case max-3 (max 1 5 3))
(ec-case format-2 (format "%d-%s" 3 "x"))
(ec-case substring-optional (list (substring "abcde" 1) (substring "abcde" 1 3)))
(ec-case nested (+ (* 2 3) (- 10 (1+ 4)) (car '(7 8))))
(ec-case wrong-type-plus (+ 1 'a))
(ec-case wrong-type-car (car 1))

;; A special form kept in a function cell is still a special form.
(defalias 'ec-if (symbol-function 'if))
(ec-case alias-if (list (ec-if t 1 2) (ec-if nil 1 2)))
(defalias 'ec-quote (symbol-function 'quote))
(ec-case alias-quote (ec-quote (a b c)))
(defalias 'ec-and (symbol-function 'and))
(ec-case alias-and (list (ec-and 1 2 3) (ec-and 1 nil 3) (ec-and)))

;; A special form called through funcall or apply is an invalid function.
(ec-case funcall-if (funcall 'if t 1 2))
(ec-case funcall-if-obj (funcall (symbol-function 'if) t 1 2))
(ec-case apply-quote (apply 'quote '(1)))
(ec-case apply-while (apply #'while '(nil)))
(ec-case funcall-setq (funcall 'setq 'a 1))

;; A special form redefined as a function stops being special.
(let ((saved (symbol-function 'prog1)))
  (unwind-protect
      (progn
        (fset 'prog1 (lambda (&rest args) (list 'shadowed args)))
        (princ (format "shadow-prog1 %S\n" (condition-case e (eval '(prog1 (+ 1 2) (+ 3 4)) t) (error e)))))
    (fset 'prog1 saved)))
(ec-case prog1-restored (prog1 (+ 1 2) (+ 3 4)))

;; A builtin redefined as a function runs the new definition (string-bytes is a subr in both runtimes
;; and no runtime library calls it, unlike car).
(let ((saved (symbol-function 'string-bytes)))
  (unwind-protect
      (progn
        (fset 'string-bytes (lambda (&rest args) (list 'my-string-bytes args)))
        (princ (format "shadow-subr %S\n" (eval '(string-bytes "abc") t))))
    (fset 'string-bytes saved)))
(ec-case subr-restored (string-bytes "abc"))
(defun ec-later (x) (list 'later x))
(ec-case later-defined (ec-later 5))
(fset 'ec-later nil)
(ec-case later-void (ec-later 5))
;; Variables named like special forms are not functions; local functions named like them
;; (cl-flet and cl-labels rewrite the calls when the body is macroexpanded) win over the special form.
(require 'cl-lib)
(ec-case var-named-if (let ((if 5) (quote 6) (progn 7)) (list (if t 1 2) 'a if quote progn)))
(ec-case flet-if (cl-flet ((if (a b c) (list 'local-if a b c))) (if 1 2 3)))
(ec-case labels-while (cl-labels ((while (n) (if (> n 0) (while (1- n)) 'done))) (while 3)))

;; The car of a form that is not a function, a lambda in car position, a dotted argument list and
;; an error raised while an argument is evaluated.
(ec-case void-fn (eval '(ec-no-such-function 1) t))
(ec-case invalid-int (eval '(1 2) t))
(ec-case invalid-str (eval '("s") t))
(ec-case lambda-head (eval '((lambda (x) (* x 2)) 21) t))
(ec-case dotted-plus (eval '(+ 1 . 2) t))
(ec-case dotted-car (eval '(car . 1) t))
(ec-case dotted-if (eval '(if . 1) t))
(ec-case arg-error (eval '(+ 1 (error "boom")) t))
(ec-case macro-head (eval '(when t 1 2) t))

(kill-emacs 0)
