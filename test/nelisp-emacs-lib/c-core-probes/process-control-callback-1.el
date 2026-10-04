;;; process-control-callback-1.el --- Immediate/reentrant notifications -*- lexical-binding: t; -*-
(continue-process
 (let* ((events nil) (origin (current-buffer))
        (other (generate-new-buffer " *ccore-sentinel-context*"))
        (process (make-process :name "ccore-callback" :command '("cat")
                              :connection-type 'pipe :noquery t)))
   (unwind-protect
       (progn
         (set-process-sentinel
          process (lambda (child event)
                    (push (list (process-status child) (string-to-list event)
                                inhibit-quit last-nonmenu-event) events)
                    (set-buffer other)
                    (set-process-sentinel child (lambda (p event)
                                                  (push (list (process-status p)
                                                              (string-to-list event)) events)))
                    (accept-process-output child 0.01)
                    (delete-process child)))
         (let ((returned (eq (continue-process process) process)))
           (list returned (eq origin (current-buffer)) (nreverse events))))
     (ignore-errors (delete-process process))
     (kill-buffer other)))
 (mapcar
  (lambda (debug)
    (let* ((debug-on-error debug) (reported nil)
           (command-error-function (lambda (data context _caller)
                                     (setq reported (list data context))))
           (process-error-pause-time 0)
           (process (make-process :name "ccore-callback-error" :command '("cat")
                                 :connection-type 'pipe :noquery t
                                 :sentinel (lambda (_p _event) (error "callback failure")))))
      (unwind-protect
          (let ((result (condition-case err
                            (progn (continue-process process) 'returned)
                          (error err))))
            (set-process-sentinel process #'ignore)
            (list result reported))
        (set-process-sentinel process #'ignore)
        (ignore-errors (delete-process process)))))
  '(nil t)))
