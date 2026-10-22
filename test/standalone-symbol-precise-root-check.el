;;; standalone-symbol-precise-root-check.el --- evaluator transients are not held only by the stack -*- lexical-binding: t; -*-

;; Usage: nelisp --load test/standalone-symbol-precise-root-check.el
;;
;; Doc 226 D9 stage 3.2 stores an interned symbol in a form as its object
;; WORD.  An evaluator step that wants the symbol as a 32-byte view makes a
;; fresh one (`nl_cons_car_ptr' of a vectorlike WORD); the view is a transient
;; box that only the native stack keeps alive.  With the conservative stack
;; scan switched off (`nelisp--debug-switch' 28; 29 restores it) only precise
;; roots count, so a special form that holds such a view across the evaluation
;; of a sub-form loses it to a collection in that sub-form, and then names a
;; freed cell.  Each case below calls `garbage-collect' inside a position
;; evaluated while the construct under test holds its symbols, and checks the
;; result.  Prints one SYM-PRECISE line, exits non-zero on a failure.

(defvar spr-checks 0)
(defvar spr-failures nil)
(defmacro spr-case (name expected &rest body)
  `(let ((got (condition-case err (progn ,@body) (error (list 'error err)))))
     (setq spr-checks (1+ spr-checks))
     (unless (equal got ,expected)
       (push (list ',name got ,expected) spr-failures))))

(defun spr-gc () (garbage-collect) 1)
(defvar spr-dyn 5)
(defvar spr-sink nil)
(defun spr-id (x) x)
(defun spr-add (a b) (+ a b))
(defmacro spr-twice (x) (list '* 2 x))

(nelisp--debug-switch 28)

(spr-case setq nil
  (progn (setq spr-sink (spr-gc)) (if (eql spr-sink 1) nil spr-sink)))
(spr-case setq-pairs '(1 2)
  (progn (setq spr-a (spr-gc) spr-b (progn (spr-gc) 2)) (list spr-a spr-b)))
(spr-case setq-value 3 (setq spr-sink (progn (spr-gc) 3)))
(spr-case setq-lexical 4
  (let ((x 0)) (setq x (progn (spr-gc) 4)) x))
(spr-case let 3
  (let ((a (progn (spr-gc) 1)) (b (progn (spr-gc) 2))) (+ a b)))
(spr-case let-star 3
  (let* ((a (progn (spr-gc) 1)) (b (+ a (progn (spr-gc) 1))) (c (progn (spr-gc) 0))) (+ a b c)))
(spr-case let-dynamic 6
  (let ((spr-dyn (progn (spr-gc) 6))) (spr-id spr-dyn)))
(spr-case let-dynamic-restored 5 spr-dyn)
(spr-case defvar-new 7
  (progn (defvar spr-defvar-v (progn (spr-gc) 7)) spr-defvar-v))
(spr-case defvar-existing 5
  (progn (defvar spr-dyn (progn (spr-gc) 99)) spr-dyn))
(spr-case defconst 8
  (progn (defconst spr-const-c (progn (spr-gc) 8)) spr-const-c))
(spr-case defun 9
  (progn (defun spr-def-f (x) (progn (spr-gc) (+ x 1))) (spr-def-f 8)))
(spr-case defmacro 10
  (progn (defmacro spr-def-m (x) (progn (spr-gc) (list '+ x 1))) (spr-def-m 9)))
(spr-case defalias 11
  (progn (defalias 'spr-alias-f (lambda (x) (progn (spr-gc) (+ x 1)))) (spr-alias-f 10)))
(spr-case call-user 3 (spr-add (progn (spr-gc) 1) (progn (spr-gc) 2)))
(spr-case call-builtin 3 (+ (progn (spr-gc) 1) (progn (spr-gc) 2)))
(spr-case call-macro 12 (spr-twice (progn (spr-gc) 6)))
(spr-case funcall 3 (funcall 'spr-add (progn (spr-gc) 1) (progn (spr-gc) 2)))
(spr-case funcall-lambda 3 (funcall (lambda (a b) (spr-gc) (+ a b)) 1 2))
(spr-case apply 6 (apply 'spr-add (progn (spr-gc) 1) (list (progn (spr-gc) 5))))
(spr-case lambda-call 3 ((lambda (a b) (spr-gc) (+ a b)) 1 2))
(spr-case lambda-optional 5 (funcall (lambda (a &optional b) (spr-gc) (+ a (or b 4))) 1))
(spr-case lambda-rest 6 (funcall (lambda (a &rest r) (spr-gc) (+ a (apply '+ r))) 1 2 3))
(spr-case closure 15
  (let ((k 10)) (funcall (lambda (x) (spr-gc) (+ x k)) 5)))
(spr-case closure-set 3
  (let ((c 0)) (let ((f (lambda () (spr-gc) (setq c (1+ c))))) (funcall f) (funcall f) (funcall f))))
(spr-case condition-case 'caught
  (condition-case e (progn (spr-gc) (signal 'spr-error '(1))) (spr-error (spr-gc) 'caught)))
(spr-case condition-case-var 'wrong-type-argument
  (condition-case e (progn (spr-gc) (car 5)) (error (spr-gc) (car e))))
(spr-case condition-case-success 'ok
  (condition-case e (progn (spr-gc) 'ok) (error 'bad)))
(spr-case catch 7
  (catch 'spr-tag (progn (spr-gc) (throw 'spr-tag 7))))
(spr-case catch-nested 3
  (catch 'a (catch 'b (spr-gc) (throw 'a 3)) 4))
(spr-case unwind-protect 1
  (let ((x 0)) (unwind-protect (progn (spr-gc) 2) (spr-gc) (setq x 1)) x))
(spr-case while 10
  (let ((i 0) (s 0)) (while (< i 5) (spr-gc) (setq s (+ s i) i (1+ i))) s))
(spr-case if 2 (if (progn (spr-gc) nil) 1 (progn (spr-gc) 2)))
(spr-case cond 'b (cond ((progn (spr-gc) nil) 'a) ((progn (spr-gc) t) 'b)))
(spr-case and-or 3 (or (progn (spr-gc) nil) (and (progn (spr-gc) 1) 3)))
(spr-case progn 3 (progn (spr-gc) (spr-gc) 3))
(spr-case prog1 1 (prog1 1 (spr-gc)))
(spr-case quote-function '(a b) (progn (spr-gc) '(a b)))
(spr-case function-lambda 4 (funcall (function (lambda (x) (spr-gc) (+ x 1))) 3))
(spr-case dolist 6
  (let ((s 0)) (dolist (e '(1 2 3)) (spr-gc) (setq s (+ s e))) s))
(spr-case dotimes 6
  (let ((s 0)) (dotimes (i 4) (spr-gc) (setq s (+ s i))) s))
(spr-case mapcar '(2 3 4)
  (mapcar (lambda (x) (spr-gc) (1+ x)) '(1 2 3)))
(spr-case dynamic-via-function 5
  (progn (defun spr-read-dyn () spr-dyn) (let ((spr-dyn 5)) (spr-gc) (spr-read-dyn))))
(spr-case set 3 (progn (set 'spr-set-v (progn (spr-gc) 3)) spr-set-v))
(spr-case fset 4 (progn (fset 'spr-fset-f (lambda () 4)) (spr-gc) (spr-fset-f)))
(spr-case plist 5 (progn (put 'spr-plist-s 'p (progn (spr-gc) 5)) (get 'spr-plist-s 'p)))
(spr-case intern-eq t (eq (progn (spr-gc) (intern "spr-interned")) 'spr-interned))
(spr-case symbol-name "spr-sym-name" (progn (spr-gc) (symbol-name 'spr-sym-name)))
(spr-case hash-table 2
  (let ((h (make-hash-table :test 'eq))) (puthash 'k (progn (spr-gc) 2) h) (gethash 'k h)))
(spr-case macroexpand '(* 2 3) (progn (spr-gc) (macroexpand '(spr-twice 3))))
(spr-case eval-form 7 (eval '(let ((x 3)) (setq x (progn (spr-gc) 7)) x) t))

(nelisp--debug-switch 29)

(dolist (f (reverse spr-failures))
  (princ (format "  MISMATCH %S\n" f)))
(princ (format "SYM-PRECISE checks=%d failures=%d %s\n" spr-checks (length spr-failures)
               (if spr-failures "FAIL" "PASS")))
(kill-emacs (if spr-failures 1 0))

;;; standalone-symbol-precise-root-check.el ends here
