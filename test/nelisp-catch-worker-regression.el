;;; nelisp-catch-worker-regression.el --- Private worker catch targets -*- lexical-binding: t; -*-

;; Tier-3 evaluator workers must start without the parent's active catch,
;; then keep their own target registry through nested evaluator calls.
(let ((storage (nelisp-thread-shared-alloc 32)))
  (unless (> storage 4095) (error "Shared worker storage unavailable"))
  (unless (= (nelisp-thread-gc-inhibit 1) 1)
    (error "Worker GC inhibition unavailable"))
  (unwind-protect
      (let ((answer
             (catch 'parent
               (dolist
                   (case
                    '(((condition-case nil (throw 'parent 9) (no-catch 41)) 41)
                      ((catch 'worker (throw 'worker 42)) 42)
                      ((progn (catch 'completed 0)
                              (condition-case nil (throw 'completed 9)
                                (no-catch 43))) 43)))
                 (let ((done (+ storage 8)))
                   ;; Each spawn publishes one completion to the same counter;
                   ;; join and all parent reads use atomic completion ordering.
                   (let* ((expected (1+ (nelisp-thread-atomic-read done)))
                          (tid (nelisp-thread-spawn 2 0 (car case) storage done)))
                     (unless (> tid 0) (error "Worker spawn failed: %S" tid))
                     (nelisp-thread-join done expected)
                     (let ((value (nelisp-thread-atomic-read storage)))
                       (unless (= value (cadr case))
                         (error "Worker catch mismatch: %S != %S"
                                value (cadr case)))
                       (prin1 value)
                       (terpri)))))
               (throw 'parent 44))))
        (unless (= answer 44) (error "Parent catch was lost: %S" answer))
        (princ "WORKER-CATCH-PASS\n"))
    (unless (= (nelisp-thread-gc-inhibit 0) 1)
      (error "Worker GC inhibition cleanup failed"))))

;;; nelisp-catch-worker-regression.el ends here
