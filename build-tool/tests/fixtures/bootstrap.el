(defun bootstrap-add (x) (+ x 1))

(defun bootstrap-main ()
  (let ((cell (cons 1 2))
        (vec (vector "image" 41)))
    (setcar cell (bootstrap-add (car cell)))
    (list (car cell) (aref vec 0) (bootstrap-add (aref vec 1)))))

(bootstrap-main)
