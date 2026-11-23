;;; -*- lexical-binding: t; -*-
;; fx record vs closure env/formals (F391).  Spec = GNU 31.1 (par-fxenv.gnu).  Rows E5* are in par-condcase-kd.el (KNOWN-DIV F392); P1 (timing) is not a parity row.
(defmacro row (n f) `(princ (format "%-26s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(defun fe-rd (s) (if (boundp s) (symbol-value s) 'unbound))
(defun fe-vw (c) (delq t (copy-sequence (aref c 2))))
(defun fe-two (form e1 e2 warm-first &rest args)
  "Closures of one FORM under envs E1, E2; warm one 300x; call both."
  (let ((a (eval form e1)) (b (eval form e2)))
    (dotimes (_ 300) (apply (if warm-first a b) args))
    (cons a b)))
;; E1 let specialness (declared marker decides dynamic vs lexical let)
(row "E1 let a-warm" (let ((p (fe-two '(lambda () (let ((fe-m 9)) (fe-rd 'fe-m))) '(fe-m t) '(t) t))) (list (funcall (car p)) (funcall (cdr p)))))
(row "E1r let b-warm" (let ((p (fe-two '(lambda () (let ((fe-m 9)) (fe-rd 'fe-m))) '(fe-m t) '(t) nil))) (list (funcall (car p)) (funcall (cdr p)))))
;; E2 let* lone binding (VARBIND path)
(row "E2 let* b-warm" (let ((p (fe-two '(lambda () (let* ((fe-m2 1) (fe-x 2)) (list fe-x (fe-rd 'fe-m2)))) '(fe-m2 t) '(t) nil))) (list (funcall (car p)) (funcall (cdr p)))))
;; E3 nested lambda: outer env operand baked as a constant
(row "E3 view a-warm" (let ((p (fe-two '(lambda () (lambda () 1)) '(fe-k1 t) '(fe-k2 fe-k3 t) t))) (list (fe-vw (funcall (car p))) (fe-vw (funcall (cdr p))))))
(row "E3r view b-warm" (let ((p (fe-two '(lambda () (lambda () 1)) '(fe-k1 t) '(fe-k2 fe-k3 t) nil))) (list (fe-vw (funcall (car p))) (fe-vw (funcall (cdr p))))))
;; E4 inner closure inherits specialness via baked env
(row "E4 inner let a-warm" (let ((p (fe-two '(lambda () (funcall (lambda () (let ((fe-m4 4)) (fe-rd 'fe-m4))))) '(fe-m4 t) '(t) t))) (list (funcall (car p)) (funcall (cdr p)))))
;; E6 special formal
(row "E6 formal b-warm" (let ((p (fe-two '(lambda (fe-f) (list fe-f (fe-rd 'fe-f))) '(fe-f t) '(t) nil 6))) (list (funcall (car p) 6) (funcall (cdr p) 6))))
;; N1 negative control: copied body (distinct key) under the two envs
(row "N1 copy-tree a-warm" (let* ((f '(lambda () (let ((fe-n 9)) (fe-rd 'fe-n)))) (a (eval f '(fe-n t))) (b (eval (copy-tree f) '(t)))) (dotimes (_ 300) (funcall a)) (list (funcall a) (funcall b))))
;; N2 negative control: same body, equal (not eq) envs
(row "N2 same env" (let ((p (fe-two '(lambda () (let ((fe-n2 9)) (fe-rd 'fe-n2))) '(fe-n2 t) (list 'fe-n2 t) t))) (list (funcall (car p)) (funcall (cdr p)))))
(row "F1 formals order" (let* ((tail '((list a b))) (f1 (eval `(lambda (a b) ,@tail) t)) (f2 (eval `(lambda (b a) ,@tail) t))) (dotimes (_ 300) (funcall f1 1 2)) (list (funcall f1 1 2) (funcall f2 1 2))))
(row "F2 arity" (let* ((tail '((list a (and (boundp 'b) 'dyn)))) (f1 (eval `(lambda (a) ,@tail) t)) (f2 (eval `(lambda (a &optional b) ,@tail) t))) (dotimes (_ 300) (funcall f1 1)) (list (funcall f1 1) (funcall f2 1 2))))
(row "D1 defalias shared body" (let ((form '(lambda () (let ((fe-d 9)) (fe-rd 'fe-d))))) (defalias 'fe-d1 (eval form '(fe-d t))) (defalias 'fe-d2 (eval form '(t))) (dotimes (_ 300) (fe-d1)) (list (fe-d1) (fe-d2))))
(row "M1 mapcar" (let ((p (fe-two '(lambda (_) (let ((fe-mm 9)) (fe-rd 'fe-mm))) '(fe-mm t) '(t) t 0))) (list (mapcar (car p) '(1 2)) (mapcar (cdr p) '(1 2)))))
(defun fe-caller (f) (let ((s 0)) (dotimes (_ 3) (setq s (+ s 0))) (funcall f)))
(row "V1 from VM code" (let ((p (fe-two '(lambda () (let ((fe-v 9)) (fe-rd 'fe-v))) '(fe-v t) '(t) t))) (dotimes (_ 300) (fe-caller (car p))) (list (fe-caller (car p)) (fe-caller (cdr p)))))
(row "C1 cap env" (let ((p (fe-two '(lambda () (let ((fe-c 9)) (list x (fe-rd 'fe-c)))) '((x . 1) fe-c t) '((x . 1) t) t))) (list (funcall (car p)) (funcall (cdr p)))))
(row "O1 order only view" (let ((p (fe-two '(lambda () (lambda () 1)) '(fe-o1 fe-o2 t) '(fe-o2 fe-o1 t) t))) (list (fe-vw (funcall (car p))) (fe-vw (funcall (cdr p))))))
(row "O2 dup marker view" (let ((p (fe-two '(lambda () (lambda () 1)) '(fe-o1 t) '(fe-o1 fe-o1 t) t))) (list (fe-vw (funcall (car p))) (fe-vw (funcall (cdr p))))))
(row "F2b arity &rest" (let* ((tail '((list a b))) (f1 (eval `(lambda (a b) ,@tail) t)) (f2 (eval `(lambda (a &rest b) ,@tail) t))) (dotimes (_ 300) (funcall f1 1 2)) (list (funcall f1 1 2) (funcall f2 1 2))))
(row "D2 redefine name" (let ((form '(lambda () (let ((fe-d3 9)) (fe-rd 'fe-d3))))) (defalias 'fe-d3f (eval form '(fe-d3 t))) (dotimes (_ 300) (fe-d3f)) (let ((r1 (fe-d3f))) (defalias 'fe-d3f (eval form '(t))) (list r1 (fe-d3f)))))
(row "P2 equal env ratio" (let* ((form '(lambda () (let ((fe-n3 9) (s 0)) (dotimes (i 20) (setq s (+ s i))) (+ s (fe-rd 'fe-n3))))) (a (eval form '(fe-n3 t))) (b (eval form (list 'fe-n3 t))) ta tb) (dotimes (_ 300) (funcall a)) (setq ta (float-time)) (dotimes (_ 20000) (funcall a)) (setq ta (- (float-time) ta) tb (float-time)) (dotimes (_ 20000) (funcall b)) (setq tb (- (float-time) tb)) (< (/ tb ta) 1.5)))
