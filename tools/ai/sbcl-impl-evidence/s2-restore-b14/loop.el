(let ((t0 (float-time)) (i 0) v) (while (< i 300000) (setq v (make-vector 64 nil) i (1+ i)))
  (princ (format "LOOP300k %.3f\n" (- (float-time) t0))))
(kill-emacs 0)
