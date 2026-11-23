;;; -*- lexical-binding: t; -*-
;; Nested closure env view parity (F298). GNU 31.1 is the spec; expected output in par-nested-env.gnu.
;; Regime A (no lexical pair in the creating frame): the nested closure's env view is the outer env
;; unchanged (order, duplicates, special names).  Regime B rows (a lexical pair present) are KNOWN-DIV:
;; GNU's cconv trims, NeLisp never trimmed; they are printed but excluded from the .gnu comparison.
;; Rows drop the trailing t (pre-existing divergence).  The "nv-big" rows use 400+ names, ~90% special.
(defvar nv-s1 1) (defvar nv-s2 2)
(defmacro row (n f) `(princ (format "%-26s %S\n" ,n (condition-case e ,f (error (list 'ERR e))))))
(defun nv (al body) (let ((o (eval `(lambda () ,body) al))) (delq t (copy-sequence (aref (funcall o) 2)))))
(row "A1 plain nested" (nv '(nv-s1 nv-ns1 nv-s2 t) '(lambda () 1)))
(row "A2 order+dups" (nv '(nv-ns2 nv-s1 nv-ns2 nv-s2 nv-s1 nv-ns3 t) '(lambda () 1)))
(row "A3 dyn let special" (nv '(nv-s1 nv-ns1 t) '(let ((nv-s1 7)) (lambda () nv-s1))))
(row "A4 body defvar nonspecial" (nv '(nv-s1 nv-ns1 t) '(progn (defvar nv-late) (lambda () 1))))
(row "A5 body defvar special" (nv '(nv-s1 nv-ns1 t) '(progn (defvar nv-s2) (lambda () 1))))
;; A6 is a KNOWN-DIV (GNU pushes a duplicate marker, NeLisp dedupes in declare_one): not in the .gnu comparison
(row "A7 3-deep" (let ((o (eval '(lambda () (lambda () (lambda () 1))) '(nv-s1 nv-ns1 t)))) (delq t (copy-sequence (aref (funcall (funcall o)) 2)))))
(row "A8 only specials" (nv '(nv-s1 nv-s2 t) '(lambda () 1)))
(row "A9 no t" (nv '(nv-s1 nv-ns1) '(lambda () 1)))
(row "A10 call twice then nested"
     (let* ((o (eval '(lambda () (lambda () 1)) '(nv-ns2 nv-s1 nv-ns2 nv-s2 nv-s1 t)))
            (v1 (delq t (copy-sequence (aref (funcall o) 2))))
            (v2 (delq t (copy-sequence (aref (funcall o) 2)))))
       (list v1 (equal v1 v2) (delq t (copy-sequence (aref o 2))))))
(row "A11 special-only body let" (nv '(nv-s1 nv-ns1 t) '(let ((nv-s1 7)) (funcall (lambda () nv-s1)) (lambda () 1))))
(row "A12 outer view unchanged" (let ((o (eval '(lambda () (lambda () 1)) '(nv-s1 nv-ns1 nv-s2 t)))) (funcall o) (delq t (copy-sequence (aref o 2)))))
(row "A13 special read in body" (let ((o (eval '(lambda () (list nv-s1 (lambda () nv-s2))) '(nv-s1 nv-ns1 t)))) (let ((r (funcall o))) (list (car r) (funcall (cadr r))))))
;; 400+ names: ~90% special, in a fixed pseudo-random order with duplicates of markers
(defun nvb-names (n) (let (l (i 0)) (dotimes (_ n) (push (intern (format "nvb-v%d" (setq i (1+ i)))) l)) (nreverse l)))
(defvar nvb-all (let ((l (nvb-names 450)) (r nil) (i 0))
                  (dolist (s l) (setq i (1+ i)) (when (/= (% i 10) 0) (eval `(defvar ,s 0) t)) (push s r))
                  (nconc (nreverse r) (list (nth 3 l) (nth 9 l) (nth 449 l) 'nvb-extra))))
(defun nvb (body) (let ((o (eval `(lambda () ,body) (append nvb-all '(t))))) (delq t (copy-sequence (aref (funcall o) 2)))))
(let ((v (nvb '(lambda () 1))))
  (row "big nested equal" (equal v nvb-all))
  (row "big nested length" (list (length v) (length nvb-all) (car v) (car (last v)) (nth 3 v) (nth 9 v) (nth 453 v))))
(let ((v (nvb '(progn (defvar nvb-late) (lambda () 1)))))
  (row "big body defvar" (list (car v) (equal (cdr v) nvb-all))))
(row "big call twice" (let* ((o (eval '(lambda () (lambda () 1)) (append nvb-all '(t)))) (a (delq t (copy-sequence (aref (funcall o) 2)))) (b (delq t (copy-sequence (aref (funcall o) 2))))) (list (equal a b) (equal a nvb-all) (= (length (aref o 2)) (1+ (length nvb-all))))))
(row "big dyn let special" (let* ((o (eval '(lambda () (let ((nvb-v1 5)) (list (symbol-value 'nvb-v1) (lambda () 1)))) (append nvb-all '(t)))) (r (funcall o))) (list (car r) (equal (delq t (copy-sequence (aref (cadr r) 2))) nvb-all) (symbol-value 'nvb-v1))))
