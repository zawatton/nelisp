;;; -*- lexical-binding: t; -*-
;;; Untimed source contracts; run with NELISP_FX_DISABLE=1.
(defun nl-leaf-check (name actual expected)
  (unless (equal actual expected) (error "Leaf contract %s: %S != %S" name actual expected))
  (princ (format "LEAF-CHECK %s\n" name)))
(princ "LEAF-SMOKE-BEGIN v1\n")
(nl-leaf-check 'arrays
 (let ((a (vector 3 5 7 11)))
  (list (length a) (aref a 2) (aset a 1 17) a))
 '(4 7 17 [3 17 7 11]))
(nl-leaf-check 'shared-vector-init
 (let* ((x (vector 7)) (a (make-vector 3 x)))
  (aset x 0 19)
  (list (eq (aref a 0) (aref a 2)) (aref (aref a 1) 0)))
 '(t 19))
(nl-leaf-check 'arity-before-effects
 (let ((touched nil))
  (list (condition-case err (length [] (setq touched t))
          (error (car err))) touched))
 '(wrong-number-of-arguments nil))
(nl-leaf-check 'wrong-type
 (condition-case err (aref 3 0) (error (car err)))
 'wrong-type-argument)
(nl-leaf-check 'bounds
 (condition-case err (aref [] 0) (error (car err)))
 'args-out-of-range)
(defalias 'nl-leaf-length-alias 'length)
(nl-leaf-check 'alias (nl-leaf-length-alias [1 2 3]) 3)
(let ((old (symbol-function 'length)) (observed nil))
 (unwind-protect
  (progn
   (fset 'length (lambda (_a) 'redefined))
   (setq observed (length [])))
  (fset 'length old))
 (nl-leaf-check 'redefinition observed 'redefined))
(nl-leaf-check 'restored-function-cell (length [1 2]) 2)
(nl-leaf-check 'callback-and-collection
 (mapcar (lambda (x) (let ((v (vector x))) (garbage-collect) v)) '(1 2 3))
 '([1] [2] [3]))
(nl-leaf-check 'nonlocal-exit
 (catch 'nl-leaf-exit
  (mapcar (lambda (x) (if (= x 2) (throw 'nl-leaf-exit x) x)) '(1 2 3)))
 2)
(princ "LEAF-SMOKE-DONE v1\n")
(kill-emacs 0)
