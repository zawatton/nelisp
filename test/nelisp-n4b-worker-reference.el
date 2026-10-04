;;; -*- lexical-binding: t; -*-
(defun n4-worker-target (x)
  (setq x nil)
  ;; Native collection completes before its Lisp census formatter attempts
  ;; a global write. Registered workers intentionally reject that write.
  (condition-case nil (garbage-collect) (nelisp-worker-mirror-mutation nil))
  (let ((f (backtrace-frame 0 'n4-worker-target)))
    (if (and (equal f '(t n4-worker-target (7)))
             (null (backtrace-frame 0 'n4-parent-target)))
        (car (car (cddr f))) -99)))
(defun n4-parent-target ()
  (let ((done (nelisp-thread-shared-alloc 4096))
        (result (nelisp-thread-shared-alloc 4096)))
    (unless (= (nelisp-thread-gc-inhibit 1) 1) (error "Worker setup failed"))
    (unwind-protect
        (progn (nelisp-thread-spawn 2 0 '(n4-worker-target (list 7)) result done)
               (nelisp-thread-join done 1)
               (list (nelisp-thread-atomic-read result)
                     (car (cdr (backtrace-frame 0 'n4-parent-target)))))
      (unless (= (nelisp-thread-gc-inhibit 0) 1) (error "Worker teardown failed")))))
(prin1 (n4-parent-target)) (terpri)
(princ "N4-WORKER-REFERENCE-DONE\n")
nil
