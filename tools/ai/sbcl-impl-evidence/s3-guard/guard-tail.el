
;; ---- guard phases (appended after the normal validated run) ----
(let ((out (getenv "NELISP_GUARD_OUT")))
  (when out
    (let* ((wrong (equal (getenv "GUARD_SELFTEST_WRONG") "1"))
           (r (condition-case e (guard-run-all 64 10 wrong) (error (list (list 'driver-error e)))))
           (all (let ((ok t)) (dolist (x r) (unless (or (memq (cadr x) '(pass unavailable)) (eq (car x) 'advice) (eq (car x) 'timing-info)) (setq ok nil))) ok)))
      (let* ((orig (symbol-function 'bench-call-helper)) t0 a b c)
        (setq t0 (float-time)) (bench-function-call2 262144 nil) (setq a (- (float-time) t0))
        (fset 'bench-call-helper (lambda (x y) (+ x y 1)))
        (setq t0 (float-time)) (bench-function-call2 262144 nil) (setq b (- (float-time) t0))
        (fset 'bench-call-helper orig)
        (setq t0 (float-time)) (bench-function-call2 262144 nil) (setq c (- (float-time) t0))
        (setq r (append r (list (list 'timing-info 'n 262144 'orig-s (format "%.4f" a) 'fset-lisp-s (format "%.4f" b) 'restored-s (format "%.4f" c))))))
      (write-region (concat (prin1-to-string (list 'guard-summary 'overall (if all 'pass 'fail) 'broken (getenv "BENCH_BROKEN") 'wrong (and wrong t) 'phases r)) "\n") nil out nil 'silent))))
