(daemon-initialized
 (condition-case e (daemon-initialized) (error (list (car e) (cdr e))))
 (let ((b (get-buffer-create " *daemon-init-probe*")))
   (unwind-protect
       (progn (with-current-buffer b (insert "state changed"))
              (condition-case e (daemon-initialized)
                (error (list (car e) (cdr e)))))
     (kill-buffer b))))
