(current-cpu-time
  (let ((v (current-cpu-time)))
    (list (consp v) (integerp (car v)) (cdr v)))
  (with-temp-buffer
    (insert "CPU probe")
    (let ((v (current-cpu-time)))
      (list (consp v) (integerp (car v)) (cdr v))))
  (condition-case e (current-cpu-time 1)
    (wrong-number-of-arguments
     (list 'wrong-number-of-arguments 'current-cpu-time (car (last e))))
    (error e)))
