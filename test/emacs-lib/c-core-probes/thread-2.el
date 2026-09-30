(thread-last-error
 (thread-last-error)
 (thread-last-error t))
(thread-live-p
 (condition-case e (thread-live-p nil) (error (list (car e) (cdr e))))
 (condition-case e (thread-live-p 'not-a-thread) (error (list (car e) (cdr e)))))
(thread-name
 (condition-case e (thread-name nil) (error (list (car e) (cdr e))))
 (condition-case e (thread-name 'not-a-thread) (error (list (car e) (cdr e)))))
(thread-set-buffer-disposition
 (condition-case e (thread-set-buffer-disposition nil nil) (error (list (car e) (cdr e))))
 (condition-case e (thread-set-buffer-disposition nil t) (error (list (car e) (cdr e)))))
(thread-signal
 (condition-case e (thread-signal nil 'error '("thread-2-probe"))
   (error (list (car e) (cdr e))))
 (condition-case e (thread-signal nil 7 nil)
   (error (list (car e) (cdr e))))
 (condition-case e (thread-signal 'not-a-thread 'error nil)
   (error (list (car e) (cdr e)))))
(thread-yield
 (thread-yield)
 (let ((b (generate-new-buffer " *thread-2-yield*")))
   (unwind-protect (with-current-buffer b (thread-yield))
     (kill-buffer b))))
