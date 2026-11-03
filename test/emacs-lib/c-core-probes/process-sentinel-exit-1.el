;;; process-sentinel-exit-1.el --- Terminal notifications -*- lexical-binding: t; -*-

(process-exit-status
 (mapcar
  (lambda (action)
    (let* ((events nil)
           (process (make-process
                     :name "ccore-terminal-child" :connection-type 'pipe
                     :command (if (eq action 'exit7) '("sh" "-c" "exit 7") '("cat"))
                     :noquery t
                     :sentinel (lambda (child event)
                                 (push (list (process-status child)
                                             (process-exit-status child)
                                             (string-to-list event)) events)))))
      (unwind-protect
          (progn
            (if (eq action 'delete) (delete-process process)
              (unless (eq action 'exit7) (signal-process process action)))
            (let ((deadline (+ (float-time) 2)))
              (while (and (null events) (< (float-time) deadline))
                (accept-process-output process 0.02)))
            (accept-process-output process 0.02)
            (list (process-status process) (process-exit-status process)
                  (nreverse events)))
        (ignore-errors (delete-process process)))))
  '(TERM INT delete exit7)))
