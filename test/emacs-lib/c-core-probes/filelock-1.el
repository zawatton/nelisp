(lock-buffer
 (let ((create-lockfiles nil))
   (lock-buffer))
 (condition-case e (lock-buffer 42) (error (list (car e) (cdr e))))
 (let ((create-lockfiles nil)
       (buffer-file-name "/tmp/c-core-filelock-1-probe"))
   (set-buffer-modified-p t)
   (prog1 (lock-buffer "/tmp/c-core-filelock-1-explicit")
     (set-buffer-modified-p nil))))
(unlock-buffer
 (let ((buffer-file-name nil))
   (set-buffer-modified-p t)
   (prog1 (unlock-buffer) (set-buffer-modified-p nil)))
 (let ((b (get-buffer-create " *cc-filelock-1-unlock*")))
   (unwind-protect
       (with-current-buffer b
         (setq buffer-file-name "/tmp/c-core-filelock-1-visited")
         (insert "changed")
         (prog1 (unlock-buffer) (set-buffer-modified-p nil)))
     (kill-buffer b))))
