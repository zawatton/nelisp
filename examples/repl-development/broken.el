(defun nelisp-repl-demo-step (x)
  (if (= x 1)
      (error "demo bug: input 1")
    (+ x 10)))
