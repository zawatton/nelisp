;;; standalone-argv-precise-root-check.el --- argument values are rooted through the activation -*- lexical-binding: t; -*-

;; Usage: nelisp --load test/standalone-argv-precise-root-check.el
;;
;; GNU eval_sub evaluates the arguments of a call into an array and its backtrace entry
;; points at it, which is how mark_specpdl roots them (eval.c:2626-2693, alloc.c).  The
;; interpreter does the same: a builtin's (up to five) and a closure's arguments are
;; eight words of the activation chunk's argument stack, registered in the call form's
;; record before the first argument is evaluated and marked through it.  With the
;; conservative stack scan off (`nelisp--debug-switch 28'; 29 restores it) only precise
;; roots count, so every heap value an argument produced must survive a collection in a
;; LATER argument or in the callee.  The values here are fresh conses, strings and
;; vectors, which a fixnum would not test.  Prints one ARGV-PRECISE line, exits non-zero
;; on a failure.

(defvar ap-checks 0)
(defvar ap-failures nil)
(defmacro ap-case (name expected &rest body)
  `(let ((got (condition-case err (progn ,@body) (error (list 'error err)))))
     (setq ap-checks (1+ ap-checks))
     (unless (equal got ,expected)
       (push (list ',name got ,expected) ap-failures))))

(defun ap-gc () (garbage-collect) 1)
(defun ap-id (x) x)
(defun ap-list3 (a b c) (list a b c))
(defun ap-gc-body (a b) (ap-gc) (list a b))
(defun ap-gc-body3 (a b c) (ap-gc) (ap-gc) (list c b a))
(defun ap-mk (n) (cons n (list n n)))
(defun ap-deep (n acc) (if (= n 0) acc (progn (ap-gc) (ap-deep (1- n) (cons (ap-mk n) acc)))))
(defvar ap-sink nil)

(nelisp--debug-switch 28)

;; Builtins, up to five arguments (the argument stack).
(ap-case cons-then-gc '((1 . 2) (3 . 4))
  (list (cons 1 2) (progn (ap-gc) (cons 3 4))))
(ap-case three-fresh '((1 . 2) (3 . 4) (5 . 6))
  (list (cons 1 2) (progn (ap-gc) (cons 3 4)) (progn (ap-gc) (cons 5 6))))
(ap-case five-fresh '((1) (2) (3) (4) (5))
  (list (list 1) (progn (ap-gc) (list 2)) (progn (ap-gc) (list 3))
        (progn (ap-gc) (list 4)) (progn (ap-gc) (list 5))))
(ap-case strings "ab1cd2"
  (concat (format "ab%d" 1) (progn (ap-gc) (format "cd%d" 2))))
(ap-case vectors [1 2 3 4]
  (vconcat (vector 1 2) (progn (ap-gc) (vector 3 4))))
(ap-case plus-fresh 6
  (+ (car (cons 1 2)) (progn (ap-gc) (car (cons 2 3))) (progn (ap-gc) 3)))
(ap-case nested '(((1 . 2) 3) (4 . 5))
  (list (list (cons 1 2) (progn (ap-gc) 3)) (progn (ap-gc) (cons 4 5))))
(ap-case first-survives-twice '((a . b) (c . d) (e . f))
  (list (cons 'a 'b) (progn (ap-gc) (cons 'c 'd)) (progn (ap-gc) (ap-gc) (cons 'e 'f))))

;; More than five arguments (the root-frame path).
(ap-case seven-fresh '((1) (2) (3) (4) (5) (6) (7))
  (list (list 1) (progn (ap-gc) (list 2)) (list 3) (progn (ap-gc) (list 4)) (list 5)
        (progn (ap-gc) (list 6)) (progn (ap-gc) (list 7))))

;; Closures (the argument array of the closure route, the parameters, the body).
(ap-case closure-three '((1 . 2) (3 . 4) (5 . 6))
  (ap-list3 (cons 1 2) (progn (ap-gc) (cons 3 4)) (progn (ap-gc) (cons 5 6))))
(ap-case closure-gc-in-body '((1) (2))
  (ap-gc-body (list 1) (progn (ap-gc) (list 2))))
(ap-case closure-gc-in-body-3 '((5) (4) (3))
  (ap-gc-body3 (list 3) (progn (ap-gc) (list 4)) (progn (ap-gc) (list 5))))
(ap-case closure-as-argument '((1) (2))
  (list (ap-id (list 1)) (progn (ap-gc) (ap-id (list 2)))))
(ap-case closure-recursion 300
  (length (ap-deep 300 nil)))
(ap-case closure-recursion-value '((1 1 1) (2 2 2) (3 3 3))
  (ap-deep 3 nil))

;; The calls that still take a rooted frame.
(ap-case funcall-fresh '((1 . 2) (3 . 4))
  (funcall (lambda (a b) (list a b)) (cons 1 2) (progn (ap-gc) (cons 3 4))))
(ap-case apply-fresh '((1 . 2) (3 . 4) (5 . 6))
  (apply #'list (cons 1 2) (progn (ap-gc) (cons 3 4)) (list (progn (ap-gc) (cons 5 6)))))
(ap-case let-and-call '((7 . 8) (9 . 10))
  (let ((a (cons 7 8))) (list a (progn (ap-gc) (cons 9 10)))))
(ap-case mapcar-fresh '((1) (2) (3))
  (mapcar (lambda (x) (ap-gc) (list x)) '(1 2 3)))

(nelisp--debug-switch 29)

(dolist (f (reverse ap-failures))
  (princ (format "  MISMATCH %S\n" f)))
(princ (format "ARGV-PRECISE checks=%d failures=%d %s\n" ap-checks (length ap-failures)
               (if ap-failures "FAIL" "PASS")))
(kill-emacs (if ap-failures 1 0))

;;; standalone-argv-precise-root-check.el ends here
