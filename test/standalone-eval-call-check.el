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

;; Every special form eval_sub calls: too few arguments, an improper argument list (list_length
;; signals wrong-type-argument listp on the last tail before anything is evaluated), too many.
(defvar ec-v nil)
(defvar ec-c nil)
(ec-case few-let (eval '(let) t))
(ec-case few-letstar (eval '(let*) t))
(ec-case few-while (eval '(while) t))
(ec-case few-prog1 (eval '(prog1) t))
(ec-case few-condition-case (eval '(condition-case) t))
(ec-case few-condition-case-x (eval '(condition-case x) t))
(ec-case few-unwind-protect (eval '(unwind-protect) t))
(ec-case few-catch (eval '(catch) t))
(ec-case few-function (eval '(function) t))
(ec-case few-defvar (eval '(defvar) t))
(ec-case few-prog2 (eval '(prog2) t))
(ec-case few-prog2-1 (eval '(prog2 1) t))
(ec-case few-if (eval '(if) t))
(ec-case few-if-t (eval '(if t) t))
(ec-case few-defconst (eval '(defconst) t))
(ec-case few-defconst-a (eval '(defconst a) t))
(ec-case few-quote (eval '(quote) t))

(ec-case improper-let-dot-1 (eval '(let . 1) t))
(ec-case improper-letstar-dot-1 (eval '(let* . 1) t))
(ec-case improper-while-dot-1 (eval '(while . 1) t))
(ec-case improper-cond-dot-1 (eval '(cond . 1) t))
(ec-case improper-and-dot-1 (eval '(and . 1) t))
(ec-case improper-or-dot-1 (eval '(or . 1) t))
(ec-case improper-progn-dot-1 (eval '(progn . 1) t))
(ec-case improper-prog1-dot-1 (eval '(prog1 . 1) t))
(ec-case improper-prog2-dot-1 (eval '(prog2 . 1) t))
(ec-case improper-condition-case-dot-1 (eval '(condition-case . 1) t))
(ec-case improper-unwind-protect-dot-1 (eval '(unwind-protect . 1) t))
(ec-case improper-catch-dot-1 (eval '(catch . 1) t))
(ec-case improper-save-excursion-dot-1 (eval '(save-excursion . 1) t))
(ec-case improper-save-restriction-dot-1 (eval '(save-restriction . 1) t))
(ec-case improper-save-current-buffer-dot-1 (eval '(save-current-buffer . 1) t))
(ec-case improper-function-dot-1 (eval '(function . 1) t))
(ec-case improper-defvar-dot-1 (eval '(defvar . 1) t))
(ec-case improper-if-dot-1 (eval '(if . 1) t))
(ec-case improper-setq-dot-1 (eval '(setq . 1) t))
(ec-case improper-quote-dot-1 (eval '(quote . 1) t))
(ec-case improper-defconst-dot-1 (eval '(defconst . 1) t))
(ec-case improper-progn-1-dot-2 (eval '(progn 1 . 2) t))
(ec-case improper-and-t-dot-2 (eval '(and t . 2) t))
(ec-case improper-or-nil-dot-2 (eval '(or nil . 2) t))
(ec-case improper-cond-t-1-dot-2 (eval '(cond (t 1) . 2) t))
(ec-case improper-prog1-1-dot-2 (eval '(prog1 1 . 2) t))
(ec-case improper-let-nil-dot-2 (eval '(let nil . 2) t))
(ec-case improper-letstar-nil-dot-2 (eval '(let* nil . 2) t))
(ec-case improper-while-nil-dot-2 (eval '(while nil . 2) t))
(ec-case improper-catch-x-dot-2 (eval '(catch 'x . 2) t))
(ec-case improper-unwind-protect-1-dot-2 (eval '(unwind-protect 1 . 2) t))
(ec-case improper-condition-case-nil-1-dot-2 (eval '(condition-case nil 1 . 2) t))
(ec-case improper-save-excursion-1-dot-2 (eval '(save-excursion 1 . 2) t))
(ec-case improper-function-f-dot-2 (eval '(function f . 2) t))
(ec-case improper-defvar-ec-v-1-dot-2 (eval '(defvar ec-v 1 . 2) t))
(ec-case improper-if-t-1-dot-2 (eval '(if t 1 . 2) t))
(ec-case improper-setq-ec-v-1-dot-2 (eval '(setq ec-v 1 . 2) t))

(ec-case many-function-f-g (eval '(function f g) t))
(ec-case many-defvar-ec-v-1-d-4 (eval '(defvar ec-v 1 "d" 4) t))
(ec-case many-defconst-ec-c-1-d-4 (eval '(defconst ec-c 1 "d" 4) t))

(ec-case ok-let-nil (eval '(let nil) t))
(ec-case ok-letstar-nil (eval '(let* nil) t))
(ec-case ok-while-nil (eval '(while nil) t))
(ec-case ok-prog1-1 (eval '(prog1 1) t))
(ec-case ok-prog2-1-2 (eval '(prog2 1 2) t))
(ec-case ok-condition-case-nil-1 (eval '(condition-case nil 1) t))
(ec-case ok-unwind-protect-1 (eval '(unwind-protect 1) t))
(ec-case ok-catch-x (eval '(catch 'x) t))
(ec-case ok-save-excursion (eval '(save-excursion) t))
(ec-case ok-save-restriction (eval '(save-restriction) t))
(ec-case ok-and (eval '(and) t))
(ec-case ok-or (eval '(or) t))
(ec-case ok-progn (eval '(progn) t))
(ec-case ok-cond (eval '(cond) t))
(ec-case ok-quote-x (eval '(quote x) t))
(ec-case ok-defvar-ec-v2 (eval '(defvar ec-v2) t))
(ec-case ok-function-car (eval '(function car) t))

;; Macros the evaluator expands: GNU reports the byte-compiled macro's (MIN . NONREST) arity.
(ec-case macro-when (eval '(when) t))
(ec-case macro-unless (eval '(unless) t))
(ec-case macro-dolist (eval '(dolist) t))
(ec-case macro-defun (eval '(defun) t))
(ec-case macro-defun-f (eval '(defun f) t))
(ec-case macro-defmacro (eval '(defmacro) t))
(ec-case macro-when-dot-1 (eval '(when . 1) t))
(ec-case macro-dolist-dot-1 (eval '(dolist . 1) t))

;; eval_sub of a symbol is Fassq over the lexical environment, then Fsymbol_value (eval.c:2585-2594): a
;; keyword is its own value, and a parameter named like one is an ordinary lexical binding that
;; funcall_lambda does not refuse, so the binding wins (and `setq' stores into it, eval.c:496-501).
;; The parameter cases go through `eval': GNU's eager macroexpansion at load rewrites a `setq' of a
;; keyword to a constant-symbol check before the evaluator sees it.
(ec-case keyword-self (list :foo (let ((x :bar)) x) (symbol-value :foo)))
(ec-case keyword-param (eval '(funcall (lambda (:k) :k) 1) t))
(ec-case keyword-param-setq (eval '(funcall (lambda (:k) (setq :k 2) :k) 1) t))
(ec-case keyword-setq-const (eval '(setq :foo 3) t))
(ec-case keyword-setq-self (eval '(setq :foo :foo) t))

;; funcall_lambda (eval.c:3350): a closure called with as many arguments as it has parameters binds each
;; as `Fcons (Fcons (sym, arg), lexenv)' (eval.c:3401) and runs its body as Fprogn.  A parameter named
;; like a special variable is bound lexically, so the function still sees the global value; the closures
;; made in one scope share their captured variables; a signal, a throw or a redefinition of the function
;; inside its body leaves the caller's bindings alone.
(defvar ec-dyn 10)
(defun ec-f0 () 'zero)
(defun ec-f1 (x) (+ x 1))
(defun ec-f2 (a b) (list a b))
(defun ec-f3 (a b c) (list c b a))
(defun ec-f4 (a b c d) (list a (+ b c) d))
(defun ec-fempty (x))
(defun ec-flit (x) "doc" 5)
(defun ec-fdyn (ec-dyn) (symbol-value 'ec-dyn))
(defun ec-fread () ec-dyn)
(defun ec-fbody (x) (setq x (1+ x)) (setq x (* x 2)) x)
(defun ec-fsig (x) (error "boom %s" x))
(defun ec-fthrow (x) (throw 'ec-tag x))
(defun ec-fself (x) (fset 'ec-fself nil) x)
(defun ec-count (n) (if (= n 0) 0 (1+ (ec-count (1- n)))))
(defun ec-fact (n) (if (< n 2) 1 (* n (ec-fact (1- n)))))
(defun ec-mk (x) (lambda () x))
(defun ec-mk2 (x) (lambda () (setq x (1+ x))))
(fset 'ec-counter (let ((c 0)) (lambda () (setq c (1+ c)))))
(fset 'ec-adder (let ((base 100)) (lambda (n) (+ base n))))
(let ((shared 0))
  (fset 'ec-inc (lambda () (setq shared (1+ shared))))
  (fset 'ec-get (lambda () shared)))
(ec-case closure-0 (ec-f0))
(ec-case closure-1 (ec-f1 4))
(ec-case closure-2 (ec-f2 1 2))
(ec-case closure-3 (ec-f3 1 2 3))
(ec-case closure-4 (ec-f4 1 2 3 4))
(ec-case closure-empty-body (ec-fempty 1))
(ec-case closure-literal-body (ec-flit 1))
(ec-case closure-param-special (list (ec-fdyn 5) ec-dyn))
(ec-case closure-sees-dynamic (list (ec-fread) (let ((ec-dyn 20)) (ec-fread)) (ec-fread)))
(ec-case closure-param-setq (let ((x 1)) (list (ec-fbody x) x)))
(ec-case closure-nested-args (ec-f2 (ec-f1 1) (ec-f1 (ec-f1 2))))
(ec-case closure-arg-order (progn (setq ec-log nil) (ec-f3 (ec-note 'a) (ec-note 'b) (ec-note 'c)) (reverse ec-log)))
(ec-case closure-arg-signal
  (progn (setq ec-log nil)
         (list (car (ec-err (lambda () (ec-f2 (ec-note 1) (car 5))))) (reverse ec-log))))
(ec-case closure-signal (list (ec-err (lambda () (ec-fsig 3))) (let ((x 5)) (ignore-errors (ec-fsig x)) x)))
(ec-case closure-throw (list (catch 'ec-tag (ec-fthrow 9) 10) (catch 'ec-tag 11)))
(ec-case closure-redefine-self (list (ec-fself 3) (fboundp 'ec-fself)))
(ec-case closure-recursion (list (ec-count 200) (ec-fact 10)))
(ec-case closure-captured (list (ec-counter) (ec-counter) (ec-counter)))
(ec-case closure-captured-param (list (ec-adder 5) (ec-adder 6)))
(ec-case closure-shared-capture (list (ec-inc) (ec-inc) (ec-get)))
(ec-case closure-returned-param
  (list (funcall (ec-mk 7)) (let ((f (ec-mk2 1))) (list (funcall f) (funcall f)))))
(ec-case closure-wrong-args
  (list (car (ec-err (lambda () (ec-f1)))) (car (ec-err (lambda () (ec-f1 1 2))))
        (car (ec-err (lambda () (ec-f0 1))))))
(ec-case closure-in-condition-case (condition-case e (ec-f1 'a) (wrong-type-argument (list 'wta (cadr e)))))

(kill-emacs 0)
