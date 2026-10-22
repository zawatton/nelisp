;;; standalone-closure-params-check.el --- parameter bindings: setq, capture, shadowing -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-closure-params-check.el
;;
;; A parameter is GNU's (SYMBOL . ARG) cons on the lexical environment
;; (eval.c:3350 funcall_lambda).  These cases change a parameter by setq
;; before, between and after closures capture it, with &optional and &rest
;; parameters, more than eight parameters, a parameter named like a special
;; variable, recursion and nested closures.  Interpreted, then through the VM
;; (`nelisp--fx-register'), then once more; the output is GNU Emacs's own
;; (test/standalone-closure-params-check.expected).

(defvar cp-sp-var 'global)
(defun cp-sp-read () cp-sp-var)

(defun cp-loop (n)
  (let ((s 0))
    (while (> n 0) (setq s (+ s n)) (garbage-collect) (setq n (1- n)))
    (list s n)))
(defun cp-shadow (x)
  (let* ((f (lambda () x)) (x (* x 10)))
    (garbage-collect)
    (list x (funcall f) (let ((x 1)) (funcall f)))))
(defun cp-set-capture (x)
  (setq x (1+ x))
  (garbage-collect)
  (let ((f (lambda () x)))
    (garbage-collect)
    (setq x (* x 2))
    (garbage-collect)
    (list x (funcall f))))
(defun cp-capture-set (x)
  (let ((f (lambda (v) (setq x v))))
    (garbage-collect)
    (funcall f (list x))
    (garbage-collect)
    x))
(defun cp-rest (a &rest r)
  (let ((f (lambda () (list a r))))
    (setq r (cons 'changed r))
    (garbage-collect)
    (list (funcall f) r)))
(defun cp-opt (a &optional b c)
  (let ((g (lambda (v) (setq c v))))
    (funcall g 5)
    (list a b c)))
(defun cp-many (a b c d e f g h i j)
  (let ((l (lambda () (list a j))))
    (setq a 100 j 200)
    (list a b c d e f g h i j (funcall l))))
(defun cp-sp (cp-sp-var)
  (list cp-sp-var (cp-sp-read) (let ((cp-sp-var 'dyn)) (cp-sp-read))))
(defun cp-recur (n acc)
  (if (= n 0) acc (progn (garbage-collect) (cp-recur (1- n) (cons (lambda () n) acc)))))
(defun cp-nested (x)
  (lambda (y) (lambda (z) (setq x (+ x y z)))))
(defun cp-accum (n)
  (let ((fs nil))
    (dotimes (i n) (push (lambda (k) (setq n (+ n k i))) fs) (garbage-collect))
    (list (mapcar (lambda (f) (garbage-collect) (funcall f 1)) fs) n)))

(defun cp-run ()
  (list (cp-loop 5)
        (cp-shadow 3)
        (cp-set-capture 4)
        (cp-capture-set 5)
        (cp-rest 1 2 3)
        (cp-rest 1)
        (cp-opt 1)
        (cp-opt 1 2 3)
        (cp-many 1 2 3 4 5 6 7 8 9 10)
        (cp-sp 'arg)
        (mapcar #'funcall (cp-recur 4 nil))
        (let* ((f (cp-nested 1)) (g (funcall f 10)))
          (list (funcall g 100) (funcall g 1000) (funcall (funcall f 0) 5)))
        (cp-accum 3)))

(defun cp-print (label)
  (princ (format "%s %S\n" label (condition-case e (cp-run) (error (list 'error e))))))

(cp-print 'interp)
(when (fboundp 'nelisp--fx-register)
  (dolist (c '(cp-loop cp-shadow cp-set-capture cp-capture-set cp-rest cp-opt
               cp-many cp-sp cp-recur cp-nested cp-accum cp-run))
    (nelisp--fx-register c)))
(cp-print 'vm)
(cp-print 'again)
(kill-emacs 0)

;;; standalone-closure-params-check.el ends here
