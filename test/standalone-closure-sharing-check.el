;;; standalone-closure-sharing-check.el --- what lexical closures share -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-closure-sharing-check.el
;;
;; GNU's lexical environment is an alist of (SYMBOL . VALUE) conses
;; (Vinternal_interpreter_environment); a closure captures the alist, lexical
;; `setq' is a setcdr on the binding cons, so a closure, its creator and every
;; other closure over the binding share it (eval.c funcall_lambda, Flet,
;; Fsetq).  Each case below observes that sharing from a different side.  The
;; same functions run interpreted and then through the VM
;; (`nelisp--fx-register'); the output is GNU Emacs's own
;; (test/standalone-closure-sharing-check.expected).

(defvar cs-dyn 'global)
(defun cs-dyn-get () cs-dyn)

;; A closure sees the creator's later setq, and the creator sees the closure's.
(defun cs-c1 ()
  (let* ((x 1) (f (lambda () x)) (g (lambda (v) (setq x v))))
    (let ((r1 (funcall f)))
      (setq x 2)
      (garbage-collect)
      (let ((r2 (funcall f)))
        (funcall g 3)
        (garbage-collect)
        (list r1 r2 (funcall f) x)))))

;; Two closures over one binding; a second binding of the same function.
(defun cs-make-counter ()
  (let ((n 0)) (garbage-collect) (cons (lambda () (setq n (1+ n))) (lambda () n))))
(defun cs-c2 ()
  (let* ((c (cs-make-counter)) (a (car c)) (b (cdr c)))
    (funcall a) (garbage-collect) (funcall a)
    (let ((c2 (cs-make-counter)))
      (funcall (car c2))
      (list (funcall b) (funcall (cdr c2))))))

;; Nested lambdas; a shadowing let at the call site does not reach the closure.
(defun cs-c3 ()
  (let ((x 10))
    (let ((adder (lambda (y) (lambda (z) (+ x y z)))))
      (setq x 20)
      (list (funcall (funcall adder 1) 2)
            (let ((x 100)) (funcall (funcall adder 3) 4))))))

;; One binding per iteration of dotimes, dolist and a while with a let inside.
(defun cs-c4 ()
  (let (fs ws (k 0))
    (dotimes (i 3)
      (let ((j i)) (push (lambda () (setq j (1+ j))) fs)))
    (while (< k 3)
      (let ((m k)) (push (lambda () (setq m (+ m 10))) ws))
      (setq k (1+ k)))
    (list (mapcar #'funcall fs) (mapcar #'funcall fs)
          (mapcar #'funcall ws) (mapcar #'funcall ws))))
(defun cs-c5 ()
  (let (fs gs)
    (dotimes (i 3) (push (lambda () i) fs))
    (dolist (v '(a b c)) (push (lambda () v) gs))
    (list (mapcar #'funcall fs) (mapcar #'funcall gs))))

;; Lexical and dynamic bindings together: a special variable stays dynamic.
(defun cs-c6 ()
  (let ((lex 'lexical) (cs-dyn 'bound))
    (let ((f (lambda () (list lex cs-dyn (cs-dyn-get)))))
      (list (funcall f)
            (let ((cs-dyn 'rebound)) (funcall f))
            (let ((lex 'shadow)) (funcall f))))))

;; A condition-case variable and an unwind-protect cleanup reading and
;; changing lexical variables.
(defun cs-c7 ()
  (let ((log nil) (x 1))
    (condition-case err
        (unwind-protect (progn (setq x 2) (error "boom %d" x))
          (push (list 'cleanup x) log)
          (setq x (1+ x)))
      (error (push (list 'handler (car err) x (cdr err)) log)))
    (list x (reverse log))))
(defun cs-c8 ()
  (let (fs)
    (dolist (v '(1 2))
      (condition-case e (error "e%d" v)
        (error (push (lambda () (list v (cadr e))) fs))))
    (mapcar #'funcall fs)))

;; Closures that outlive the function that bound their variable.
(defun cs-make-acc (init)
  (let ((total init)) (garbage-collect) (lambda (d) (setq total (+ total d)))))
(defun cs-c9 ()
  (let ((a (cs-make-acc 10)) (b (cs-make-acc 100)))
    (list (funcall a 1) (funcall b 1) (funcall a 1) (funcall b 5))))

;; Parameters: &optional and &rest bindings are shared like any other.
(defun cs-c10 (&optional a &rest r)
  (let ((f (lambda () (list a r))))
    (setq a (list 'changed a))
    (garbage-collect)
    (funcall f)))
(defun cs-c10-run () (list (cs-c10) (cs-c10 1) (cs-c10 1 2 3)))

;; A closure per recursive call.
(defun cs-c11 (n)
  (if (= n 0) nil
    (let ((x n)) (garbage-collect) (cons (lambda () (setq x (* 2 x))) (cs-c11 (1- n))))))
(defun cs-c11-run ()
  (let ((l (cs-c11 3))) (list (mapcar #'funcall l) (mapcar #'funcall l))))

;; A self-referring closure and a throw through a closure.
(defun cs-c12 ()
  (let (f)
    (setq f (lambda (n) (if (= n 0) 'done (funcall f (1- n)))))
    (list (funcall f 3)
          (let ((x 1))
            (catch 'tag
              (let ((g (lambda () (throw 'tag x))))
                (setq x 5)
                (funcall g)))))))

;; let* chains and a shared binding across sibling closures made in a loop.
(defun cs-c13 ()
  (let* ((a 1) (b (lambda () a)) (c (progn (setq a 2) (lambda () (list a (funcall b))))))
    (setq a 3)
    (funcall c)))

(defconst cs-cases
  '(cs-c1 cs-c2 cs-c3 cs-c4 cs-c5 cs-c6 cs-c7 cs-c8 cs-c9 cs-c10-run cs-c11-run cs-c12 cs-c13))

(defun cs-run-all (label)
  (dolist (c cs-cases)
    (princ (format "%s %s %S\n" label c (condition-case e (funcall c) (error (list 'error e)))))))

(cs-run-all 'interp)
(when (fboundp 'nelisp--fx-register)
  (dolist (c (append cs-cases '(cs-make-counter cs-make-acc cs-c10 cs-c11)))
    (nelisp--fx-register c)))
(cs-run-all 'vm)
(cs-run-all 'again)
(kill-emacs 0)

;;; standalone-closure-sharing-check.el ends here
