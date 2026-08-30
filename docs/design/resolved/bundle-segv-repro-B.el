(defmacro mp (name args &rest body)
  `(progn
     (defun ,name ,args ,@body)
     7))
(mp foo (x) x)
