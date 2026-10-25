;;; nelisp-subr-object-standalone.el --- Subr function cells vs GNU -*- lexical-binding: t; -*-
;; w2subr: runs unchanged on GNU Emacs and the NeLisp binary; the two stdout
;; streams must be byte-identical.  A directcall primitive's cell holds an
;; immutable Subr object in NeLisp; every Lisp-visible behaviour stays GNU's:
;; calls through funcall/apply/mapcar/bytecode, redefinition mid-loop to a
;; lambda, to another builtin and to nil, advice, defalias, restoring the cell.
(fset 'ws-bc1 #[514 "\1\1!\207" [] 4])
(fset 'ws-bc2 #[771 "\2\2\2\"\207" [] 6])
(defvar ws-n 0)
(defun ws-row (name v) (setq ws-n (1+ ws-n)) (princ (format "SUBR CASE %s %S\n" name v)))
(defun ws-cc (thunk) (condition-case e (funcall thunk) (error (list 'err e))))
(defmacro ws-case (name form) `(ws-row ,name (ws-cc (lambda () ,form))))
(defmacro ws-restoring (sym &rest body)
  `(let ((orig (symbol-function ,sym)))
     (unwind-protect (progn ,@body) (fset ,sym orig))))

;; Plain hits, every route.
(dolist (fn '(car cdr identity))
  (ws-case (format "hit1-%s" fn)
           (list (funcall fn '(1 2)) (apply fn '((1 2))) (ws-bc1 fn '(3 4)) (mapcar fn '((5) (6))))))
(dolist (fn '(1+ 1-))
  (ws-case (format "hit1-%s" fn)
           (list (funcall fn 7) (apply fn '(8)) (ws-bc1 fn 9) (mapcar fn '(5 6))))
  (ws-case (format "arity-%s" fn) (func-arity fn)))
(dolist (fn '(+ - logand))
  (ws-case (format "hit2-%s" fn) (list (funcall fn 7 2) (ws-bc2 fn 3 4) (apply fn '(5 6)))))
(ws-case "hit2-cons" (list (funcall 'cons 7 '(8 9)) (ws-bc2 'cons 3 4) (apply 'cons '(5 6))))
(ws-case "hit2-nth" (list (funcall 'nth 1 '(8 9)) (ws-bc2 'nth 0 '(3 4)) (apply 'nth '(1 (5 6)))))
(ws-case "subrp-car" (subrp (symbol-function 'car)))
(ws-case "subrp-lambda" (subrp (symbol-function 'ws-row)))
(ws-case "eq-cell" (eq (symbol-function 'car) (symbol-function 'car)))
(dolist (fn '(car cdr cons nth length aset))
  (ws-case (format "func-arity-%s" fn) (func-arity fn)))

;; Redefinition mid-loop: a lambda, another builtin, nil, then the original.
(ws-case "fset-lambda-mid-loop"
  (ws-restoring 'car
    (let ((r nil))
      (dotimes (i 6)
        (when (= i 3) (fset 'car (lambda (x) (list 'new x))))
        (push (funcall 'car (list i)) r))
      (nreverse r))))
(ws-case "fset-builtin-mid-loop"
  (ws-restoring 'car
    (let ((r nil))
      (dotimes (i 6)
        (when (= i 2) (fset 'car (symbol-function 'cdr)))
        (push (funcall 'car (list i (* i i))) r))
      (nreverse r))))
(ws-case "fset-nil-mid-loop"
  (ws-restoring 'car
    (let ((r nil))
      (dotimes (i 4)
        (when (= i 2) (fset 'car nil))
        (push (ws-cc (lambda () (funcall 'car (list i)))) r))
      (nreverse r))))
(ws-case "fset-bytecode-mid-loop"
  (ws-restoring 'length
    (let ((r nil))
      (dotimes (i 5)
        (when (= i 2) (fset 'length #[257 "\300\207" [seen] 2]))
        (push (ws-bc1 'length (list i i)) r))
      (nreverse r))))
(ws-case "mapcar-redefined-during-walk"
  (ws-restoring 'car
    (let ((n 0))
      (mapcar (lambda (x) (setq n (1+ n)) (when (= n 2) (fset 'car (lambda (y) 'late))) x)
              '(1 2 3))
      (mapcar 'car '((1) (2) (3))))))
(ws-case "walk-with-redefinition-between"
  (ws-restoring 'car
    (list (mapcar 'car '((1) (2)))
          (progn (fset 'car (lambda (y) (list 'f y))) (mapcar 'car '((1) (2))))
          (progn (fset 'car nil) (ws-cc (lambda () (mapcar 'car '((1)))))))))
(ws-case "defalias-then-call"
  (ws-restoring 'cdr
    (defalias 'cdr 'car)
    (list (funcall 'cdr '(1 2)) (ws-bc1 'cdr '(3 4)))))
(ws-case "alias-of-builtin"
  (progn (defalias 'ws-first 'car)
         (list (funcall 'ws-first '(1 2)) (ws-bc1 'ws-first '(3 4)) (mapcar 'ws-first '((5) (6))))))
(ws-case "advice-around"
  (progn
    (defun ws-adv (f x) (list 'adv (funcall f x)))
    (advice-add 'logand :around #'ws-adv)
    (unwind-protect (list (funcall 'logand 7 3) (mapcar 'logand '(5 6)))
      (advice-remove 'logand #'ws-adv))))
(ws-case "restored-cell-still-works"
  (list (funcall 'car '(1 2)) (apply 'car '((3 4))) (ws-bc1 'car '(5 6))
        (let ((s 0)) (dolist (x '((1) (2) (3))) (setq s (+ s (car x)))) s)))
(ws-case "errors-unchanged"
  (list (ws-cc (lambda () (funcall 'car 5)))
        (car (ws-cc (lambda () (funcall 'car))))
        (car (cadr (ws-cc (lambda () (funcall 'car 1 2)))))
        (ws-cc (lambda () (apply 'nth '(a (1)))))
        (ws-cc (lambda () (ws-bc2 'nth 1 5)))))
(princ (format "SUBR COMPLETE checked=%d\n" ws-n))
