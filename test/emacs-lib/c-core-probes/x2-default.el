;;; GNU comparison: explicit defaults must synchronize live symbol bindings.
(set-default
 (let ((s 'x2-unbound-default))
   (makunbound s)
   (list (set-default s 42) (boundp s) (symbol-value s)
         (default-boundp s) (default-value s)))
 (let ((s 'x2-bound-default))
   (set s 1)
   (list (set-default s nil) (boundp s) (symbol-value s) (default-value s)))
 (let ((s 'x2-local-default))
   (set-default s 10)
   (with-temp-buffer
     (set (make-local-variable s) 20)
     (list (set-default s 30) (symbol-value s) (default-value s)
           (local-variable-p s)))))
