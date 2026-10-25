;;; nelisp-fxvm-runtime-fixture.el --- FX/interpreter arithmetic parity -*- lexical-binding: t -*-

(defun fxvm-rem (x y) (% x y))
(defun fxvm-and2 (x y) (logand x y))
(defun fxvm-and0 () (logand))
(defun fxvm-and1 (x) (logand x))
(defun fxvm-and3 (x y z) (logand x y z))
(defun fxvm-length (v) (length v))
(defun fxvm-aref (v i) (aref v i))
(defun fxvm-aset (v i x) (aset v i x))

;; First-call registration runs before the checked cases, as in R5.
(fxvm-rem 9 4) (fxvm-and2 9 4) (fxvm-and0) (fxvm-and1 9) (fxvm-and3 9 4 3)
(fxvm-length [3 5 7 11]) (fxvm-aref [3 5 7 11] 2) (fxvm-aset [3 5 7 11] 2 7)

(defun fxvm-outcome (fn args)
  (condition-case e (list 'ok (apply fn args))
    (error (list 'error (car e)))))

(let* ((marker (make-marker))
       (big (* most-positive-fixnum 8))
       (cases (list (list 0 3) (list 17 0) (list -17 4) (list 17 -4) (list -17 -4)
                    (list most-negative-fixnum -1) (list 17.5 4) (list 17 4.5)
                    (list big 13) (list 13 big) (list marker 3) (list 13 marker))))
  (with-temp-buffer
    (insert "abcdefghijklmnop") (set-marker marker 6)
    (dolist (fn '(fxvm-rem fxvm-and2))
      (dolist (args cases)
        (prin1 (list fn (fxvm-outcome fn args))) (terpri)))
    (prin1 (list 'arity0 (fxvm-outcome 'fxvm-and0 nil))) (terpri)
    (dolist (v (list -17 0 17 big marker 1.5))
      (prin1 (list 'arity1 (fxvm-outcome 'fxvm-and1 (list v)))) (terpri)
      (prin1 (list 'arity3 (fxvm-outcome 'fxvm-and3 (list v 15 3)))) (terpri)))
  (set-marker marker nil))

;; A fresh vector used as the funcall argument retains the same identity,
;; mutation and error behavior as a bound vector.
(prin1 (list 'fresh-length (funcall 'fxvm-length (vector 3 5 7 11)))) (terpri)
(prin1 (list 'fresh-aref (funcall 'fxvm-aref (vector 3 5 7 11) 2))) (terpri)
(prin1 (list 'fresh-aset (funcall 'fxvm-aset (vector 3 5 7 11) 2 31))) (terpri)
(let ((v (vector 3 5 7 11)))
  (funcall 'fxvm-aset v 1 23)
  (prin1 (list 'same-vector (funcall 'fxvm-length v) (funcall 'fxvm-aref v 1) v)) (terpri))

;; The resolved function cell still controls primitive admission.
(fset 'fxvm-saved-rem (symbol-function '%))
(fset '% (lambda (_x _y) 123))
(prin1 (list 'redefined-rem (fxvm-rem 17 4))) (terpri)
(fset '% (symbol-function 'fxvm-saved-rem))
(prin1 (list 'restored-rem (fxvm-rem 17 4))) (terpri)
(princ "FXVM-RUNTIME COMPLETE\n")
(kill-emacs 0)
