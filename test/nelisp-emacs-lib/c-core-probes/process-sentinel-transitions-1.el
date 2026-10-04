;;; process-sentinel-transitions-1.el --- Child lifecycle parity -*- lexical-binding: t; -*-

(set-process-sentinel
 (let* ((events nil)
        (process (make-process
                  :name "ccore-sentinel-child" :command '("cat")
                  :connection-type 'pipe :noquery t
                  :sentinel (lambda (child event)
                              (push (list (process-status child)
                                          (string-to-list event)) events))))
        (wait (lambda (count)
                (let ((deadline (+ (float-time) 2)))
                  (while (and (< (length events) count)
                              (< (float-time) deadline))
                    (accept-process-output process 0.02))
                  (unless (= (length events) count)
                    (error "Sentinel transition count %s: %s" count events))))))
   (unwind-protect
       (progn
         (signal-process process 'STOP)
         (funcall wait 1)
         ;; Read status before draining notifications to cover observed waits.
         (process-status process)
         (signal-process process 'CONT)
         (funcall wait 2)
         (process-send-eof process)
         (funcall wait 3)
         (accept-process-output process 0.02)
         (accept-process-output process 0.02)
         (nreverse events))
     (ignore-errors (signal-process process 'CONT))
     (ignore-errors (delete-process process)))))
