;;; process-event-owner-1.el --- event process owner dispatch probe -*- lexical-binding: t; -*-

(process-status
 (let* ((path (make-temp-name
               (expand-file-name "process-event-owner-" temporary-file-directory)))
        (buffer (get-buffer-create " *process-event-owner*"))
        (scratch (get-buffer-create " *process-event-owner-scratch*"))
        (capture (lambda (function &rest arguments)
                   (condition-case err
                       (let ((value (apply function arguments)))
                         (list 'ok
                               (if (bufferp value)
                                   (list 'buffer (buffer-name value)
                                         (eq value buffer))
                                 value)))
                     (error (list 'error (prin1-to-string err))))))
        process child result fd)
   (unwind-protect
       (progn
         (setq process
               (make-network-process :name "owner-audit" :family 'local
                                     :service path :server t :buffer buffer
                                     :coding 'binary))
         (setq fd (and (vectorp process) (>= (length process) 14)
                       (aref process 2)))
         (setq child (start-process "owner-child" nil "/bin/sh" "-c"
                                    "sleep 30"))
         (setq result
               (list
                :event-processp (and (processp process) t)
                :event-list-membership (and (memq process (process-list)) t)
                :event-name (funcall capture #'process-name process)
                :event-status (funcall capture #'process-status process)
                :event-buffer (funcall capture #'process-buffer process)
                :event-query-initial
                (funcall capture #'process-query-on-exit-flag process)
                :event-fd-owned
                (if fd
                    (and (integerp fd) (>= fd 0)
                         (eq (gethash fd emacs-process-events--by-fd) process)
                         (memq process emacs-process-events--all)
                         t)
                  (and (file-exists-p path)
                       (eq (plist-get (process-contact process t) :family)
                           'local)))
                :name-errors
                (list (funcall capture #'process-name "owner-audit")
                      (funcall capture #'process-name buffer)
                      (funcall capture #'process-name nil))
                :buffer-errors
                (list (funcall capture #'process-buffer "owner-audit")
                      (funcall capture #'process-buffer buffer)
                      (funcall capture #'process-buffer nil))
                :query-errors
                (list (funcall capture #'process-query-on-exit-flag "owner-audit")
                      (funcall capture #'process-query-on-exit-flag buffer)
                      (funcall capture #'process-query-on-exit-flag nil))
                :status-reference-results
                (list (funcall capture #'process-status "owner-audit")
                      (funcall capture #'process-status buffer)
                      (funcall capture #'process-status "owner-missing")
                      (funcall capture #'process-status (buffer-name buffer))
                      (funcall capture #'process-status nil)
                      (funcall capture #'process-status scratch))
                :setter-results
                (list (funcall capture #'set-process-query-on-exit-flag process nil)
                      (process-query-on-exit-flag process)
                      (funcall capture #'set-process-query-on-exit-flag
                               process 'enabled)
                      (process-query-on-exit-flag process)
                      (funcall capture #'set-process-query-on-exit-flag
                               "owner-audit" nil))
                :child-processp (and (processp child) t)
                :child-list-membership (and (memq child (process-list)) t)
                :child-name (process-name child)
                :child-status (process-status child)
                :child-named-status (process-status "owner-child")
                :delete-buffer-name
                (funcall capture #'delete-process (buffer-name buffer))))
         result)
     (when child (ignore-errors (delete-process child)))
     (when (and process fd
                (eq (gethash fd emacs-process-events--by-fd) process))
       (let ((owner (get 'delete-process 'emacs-process-events--event-owner)))
         (when (functionp owner) (ignore-errors (funcall owner process)))))
     (when (file-exists-p path) (ignore-errors (delete-file path)))
     (when (buffer-live-p buffer) (kill-buffer buffer))
     (when (buffer-live-p scratch) (kill-buffer scratch)))))
