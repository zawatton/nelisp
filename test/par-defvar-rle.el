;;; -*- lexical-binding: t; -*-
;; Body defvar run-length marker exactness (F402).  GNU 31.1 is the spec (par-defvar-rle.gnu).
;; GNU Fdefvar pushes a bare marker per call, so the closure env view must show every repeat.
;; NeLisp stores an adjacent repeat as a fixnum run marker in frame slot 3 and expands it in the view.
;; The cost row (time ratio, not parity) is in par-defvar-rle-cost.el.
(defmacro row (n f) `(princ (format "%-28s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(defvar rl-n 0)
(defun rl-view (body) (let ((o (eval `(lambda () ,body) '(t)))) (delq t (copy-sequence (aref (funcall o) 2)))))
(row "R1 a a a" (rl-view '(progn (defvar rl-a) (defvar rl-a) (defvar rl-a) (lambda () 1))))
(row "R2 a b a" (rl-view '(progn (defvar rl-a) (defvar rl-b) (defvar rl-a) (lambda () 1))))
(row "R3 a a b b a a" (rl-view '(progn (defvar rl-a) (defvar rl-a) (defvar rl-b) (defvar rl-b) (defvar rl-a) (defvar rl-a) (lambda () 1))))
(row "R4 loop 5 then closure"
     (let ((v (rl-view '(progn (setq rl-n 0) (while (< (setq rl-n (1+ rl-n)) 6) (defvar rl-a)) (lambda () 1)))))
       (list (length v) v)))
(row "R5 loop 40 length"
     (length (rl-view '(progn (setq rl-n 0) (while (< (setq rl-n (1+ rl-n)) 41) (defvar rl-a)) (lambda () 1)))))
(row "R6 loop then other name"
     (rl-view '(progn (setq rl-n 0) (while (< (setq rl-n (1+ rl-n)) 4) (defvar rl-a)) (defvar rl-b) (lambda () 1))))
(row "R7 snapshot unchanged"
     (let* ((o (eval '(lambda ()
                        (defvar rl-a) (defvar rl-a)
                        (let ((c1 (lambda () 1)))
                          (defvar rl-a) (defvar rl-a)
                          (list c1 (lambda () 2)))) '(t)))
            (r (funcall o)))
       (delq t (copy-sequence (aref (car r) 2)))))
(row "R8 let after run is dynamic"
     (eval '(progn (defvar rl-d) (defvar rl-d) (defvar rl-d) (let ((rl-d 1)) (symbol-value 'rl-d))) t))
(row "R9 let other name lexical"
     (eval '(progn (defvar rl-d2) (defvar rl-d2) (let ((rl-e 2)) (funcall (lambda () (boundp 'rl-e))))) t))
(row "R10 closure sees run special"
     (eval '(progn (defvar rl-d3) (defvar rl-d3) (let ((rl-d3 5)) (funcall (lambda () (symbol-value 'rl-d3))))) t))
(row "R11 two runs then closure"
     (rl-view '(progn (defvar rl-a) (defvar rl-a) (defvar rl-a) (defvar rl-b) (defvar rl-b) (lambda () 1))))
(row "R12 nested closure view"
     (let ((o (eval '(lambda () (defvar rl-a) (defvar rl-a) (defvar rl-a) (lambda () (lambda () 1))) '(t))))
       (delq t (copy-sequence (aref (funcall (funcall o)) 2)))))
