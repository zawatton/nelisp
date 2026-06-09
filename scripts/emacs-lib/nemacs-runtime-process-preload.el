;;; nemacs-runtime-process-preload.el --- source-v1 process facades -*- lexical-binding: nil; -*-

;; Keep this file source-v1 safe: top-level `setq' and `fset' only.
;; The standalone reader's runtime-image replay does not reliably install
;; `defun' bodies from loaded helper files.

(setq shell-file-name "/bin/sh")
(setq shell-command-switch "-c")
(setq emacs-process-shell-file-name "/bin/sh")
(setq emacs-process-shell-command-switch "-c")
(setq emacs-process-call-process-region-input-file
      "/tmp/nemacs-call-process-region-input")
(setq emacs-process-shell-command-on-region-output-file
      "/tmp/nemacs-shell-command-on-region-output")
(setq emacs-process--fallback-tag 'emacs-process-fallback)
(setq emacs-process--fallback-processes nil)
(setq emacs-process--fallback-next-pid 10000)
(setq emacs-process--native-process-metadata nil)

(fset 'emacs-process--fallback-process-p
      '(lambda (object)
         (if (vectorp object)
             (if (<= 10 (length object))
                 (eq (aref object 0) emacs-process--fallback-tag)
               nil)
           nil)))

(fset 'emacs-process--native-process-p
      '(lambda (object)
         (if (fboundp 'nelisp-process-object-p)
             (nelisp-process-object-p object)
           nil)))

(fset 'emacs-process--process-object-p
      '(lambda (object)
         (if (emacs-process--fallback-process-p object)
             t
           (emacs-process--native-process-p object))))

(fset 'emacs-process--native-start-available-p
      '(lambda ()
         (if (fboundp 'nelisp-process-start-process)
             t
           (fboundp 'nelisp-process-start))))

(fset 'emacs-process--fallback-plist-get
      '(lambda (plist prop)
         (let ((value nil)
               (found nil))
           (while (if plist (not found) nil)
             (if (eq (car plist) prop)
                 (progn
                   (setq value (car (cdr plist)))
                   (setq found t))
             (setq plist (cdr (cdr plist)))))
           value)))

(fset 'emacs-process--native-metadata-cell
      '(lambda (process)
         (let ((items emacs-process--native-process-metadata)
               (cell nil))
           (while (if items (not cell) nil)
             (if (eq (car (car items)) process)
                 (setq cell (car items))
               (setq items (cdr items))))
           cell)))

(fset 'emacs-process--native-metadata
      '(lambda (process key)
         (let ((cell (emacs-process--native-metadata-cell process)))
           (if cell
               (emacs-process--fallback-plist-get (cdr cell) key)
             nil))))

(fset 'emacs-process--native-put-metadata
      '(lambda (process plist)
         (let ((cell (emacs-process--native-metadata-cell process)))
           (if cell
               (setcdr cell plist)
             (setq emacs-process--native-process-metadata
                   (cons (cons process plist)
                         emacs-process--native-process-metadata)))
           process)))

(fset 'emacs-process--native-plist-put
      '(lambda (plist key value)
         (let ((head plist)
               (items plist)
               (done nil))
           (while (if items (not done) nil)
             (if (eq (car items) key)
                 (progn
                   (setcar (cdr items) value)
                   (setq done t))
               (setq items (cdr (cdr items)))))
           (if done
               head
             (cons key (cons value head))))))

(fset 'emacs-process--native-set-metadata
      '(lambda (process key value)
         (let ((cell (emacs-process--native-metadata-cell process)))
           (if cell
               (setcdr cell
                       (emacs-process--native-plist-put
                        (cdr cell) key value))
             (emacs-process--native-put-metadata
              process (list key value)))
           value)))

(fset 'emacs-process--fallback-buffer
      '(lambda (buffer)
         (if (if (stringp buffer) (fboundp 'get-buffer-create) nil)
             (get-buffer-create buffer)
           buffer)))

(fset 'emacs-process--native-status-code
      '(lambda (process)
         (if (fboundp 'nelisp-process-status)
             (nelisp-process-status process)
           3)))

(fset 'emacs-process--native-status-symbol
      '(lambda (process)
         (let ((code (emacs-process--native-status-code process)))
           (if (= code 0)
               'run
             (if (= code 1)
                 'exit
               (if (= code 2)
                   'signal
                 'closed))))))

(fset 'emacs-process--native-exit-status
      '(lambda (process)
         (if (fboundp 'nelisp-process-exit-status)
             (nelisp-process-exit-status process)
           0)))

(fset 'emacs-process--native-start
      '(lambda (name buffer command filter sentinel)
         (let ((process nil))
           (if (fboundp 'nelisp-process-start-process)
               (setq process (apply 'nelisp-process-start-process command))
             (if (fboundp 'nelisp-process-start)
                 (setq process (apply 'nelisp-process-start command))
               nil))
           (if process
               (emacs-process--native-put-metadata
                process
                (list :name name
                      :buffer (emacs-process--fallback-buffer buffer)
                      :command command
                      :filter filter
                      :sentinel sentinel
                      :sentinel-fired nil
                      :deleted nil))
             nil)
           process)))

(fset 'emacs-process--native-drain-output
      '(lambda (process)
         (let ((observed nil)
               (chunk t)
               (buffer (emacs-process--native-metadata process :buffer))
               (filter (emacs-process--native-metadata process :filter)))
           (while (if (fboundp 'nelisp-process-read-output) chunk nil)
             (setq chunk (nelisp-process-read-output process 4096))
             (if (if (stringp chunk) (> (length chunk) 0) nil)
                 (progn
                   (setq observed t)
                   (if buffer
                       (with-current-buffer buffer
                         (goto-char (point-max))
                         (insert chunk))
                     nil)
                   (if (functionp filter)
                       (funcall filter process chunk)
                     nil))
               nil))
           observed)))

(fset 'emacs-process--native-maybe-fire-sentinel
      '(lambda (process)
         (let ((status (emacs-process--native-status-symbol process)))
           (if (if (eq status 'run)
                   t
                 (emacs-process--native-metadata process :sentinel-fired))
               nil
             (let ((sentinel (emacs-process--native-metadata process :sentinel))
                   (event (if (eq status 'exit)
                              "finished\n"
                            (concat "exited abnormally with code "
                                    (number-to-string
                                     (emacs-process--native-exit-status
                                      process))
                                    "\n"))))
               (emacs-process--native-set-metadata
                process :sentinel-fired t)
               (if (functionp sentinel)
                   (funcall sentinel process event)
                 nil)
               t)))))

(fset 'emacs-process--native-live-processes
      '(lambda ()
         (let ((items emacs-process--native-process-metadata)
               (result nil))
           (while items
             (if (emacs-process--native-metadata (car (car items)) :deleted)
                 nil
               (setq result (cons (car (car items)) result)))
             (setq items (cdr items)))
           result)))

(fset 'emacs-process--native-accept
      '(lambda (processes)
         (let ((observed nil)
               (items processes))
           (while items
             (if (emacs-process--native-process-p (car items))
                 (progn
                   (if (emacs-process--native-drain-output (car items))
                       (setq observed t)
                     nil)
                   (if (emacs-process--native-maybe-fire-sentinel (car items))
                       (setq observed t)
                     nil))
               nil)
             (setq items (cdr items)))
           observed)))

(fset 'emacs-process--native-delete
      '(lambda (process)
         (if (fboundp 'nelisp-process-delete)
             (nelisp-process-delete process)
           nil)
         (emacs-process--native-set-metadata process :deleted t)
         process))

(fset 'emacs-process--fallback-sentinel-event
      '(lambda (status)
         (if (if (integerp status) (= status 0) nil)
             "finished\n"
           (concat "exited abnormally with code "
                   (number-to-string status)
                   "\n"))))

(fset 'emacs-process--fallback-make-process
      '(lambda (&rest plist)
         (let ((name (or (emacs-process--fallback-plist-get plist :name)
                         "process"))
               (buffer (emacs-process--fallback-buffer
                        (emacs-process--fallback-plist-get plist :buffer)))
               (command (emacs-process--fallback-plist-get plist :command))
               (sentinel (emacs-process--fallback-plist-get plist :sentinel))
               (filter (emacs-process--fallback-plist-get plist :filter))
               (pid emacs-process--fallback-next-pid)
               (process nil)
               (status 1))
           (setq process
                 (vector emacs-process--fallback-tag name buffer command
                         'run nil filter sentinel nil pid))
           (setq emacs-process--fallback-next-pid
                 (+ emacs-process--fallback-next-pid 1))
           (setq emacs-process--fallback-processes
                 (cons process emacs-process--fallback-processes))
           (setq status
                 (if (if (consp command) (car command) nil)
                     (apply 'call-process
                            (car command) nil buffer nil (cdr command))
                   1))
           (aset process 4 'exit)
           (aset process 5 status)
           (if (functionp sentinel)
               (funcall sentinel process
                        (emacs-process--fallback-sentinel-event status))
             nil)
           process)))

(fset 'emacs-process-call-process
      '(lambda (&rest args)
         (if (fboundp 'nelisp-process-call-process)
             (apply 'nelisp-process-call-process args)
           (if (fboundp 'nelisp-call-process)
               (apply 'nelisp-call-process args)
             1))))

(fset 'call-process
      '(lambda (&rest args)
         (apply 'emacs-process-call-process args)))

(fset 'emacs-process-call-process-region
      '(lambda (start end program &optional delete destination display &rest args)
         (if (fboundp 'nelisp-process-call-process-region)
             (apply 'nelisp-process-call-process-region
                    start end program delete destination display args)
           (if (fboundp 'nelisp-call-process-region)
               (apply 'nelisp-call-process-region
                      start end program delete destination display args)
             (if (if (fboundp 'buffer-substring-no-properties)
                     (fboundp 'nl-write-file)
                   nil)
                 (progn
                   (nl-write-file
                    emacs-process-call-process-region-input-file
                    (buffer-substring-no-properties start end))
                   (if (if delete (fboundp 'delete-region) nil)
                       (delete-region start end)
                     0)
                   (apply 'call-process
                          program
                          emacs-process-call-process-region-input-file
                          destination
                          display
                          args))
               1)))))

(fset 'call-process-region
      '(lambda (&rest args)
         (apply 'emacs-process-call-process-region args)))

(fset 'emacs-process-start-process
      '(lambda (name buffer program &rest program-args)
         (if (emacs-process--native-start-available-p)
             (or (emacs-process--native-start
                  name buffer (cons program program-args) nil nil)
                 (emacs-process--fallback-make-process
                  :name name
                  :buffer buffer
                  :command (cons program program-args)))
           (emacs-process--fallback-make-process
            :name name
            :buffer buffer
            :command (cons program program-args)))))

(fset 'start-process
      '(lambda (&rest args)
         (apply 'emacs-process-start-process args)))

(fset 'emacs-process-make-process
      '(lambda (&rest plist)
         (if (emacs-process--native-start-available-p)
             (or (emacs-process--native-start
                  (or (emacs-process--fallback-plist-get plist :name)
                      "process")
                  (emacs-process--fallback-plist-get plist :buffer)
                  (emacs-process--fallback-plist-get plist :command)
                  (emacs-process--fallback-plist-get plist :filter)
                  (emacs-process--fallback-plist-get plist :sentinel))
                 (apply 'emacs-process--fallback-make-process plist))
           (apply 'emacs-process--fallback-make-process plist))))

(fset 'make-process
      '(lambda (&rest plist)
         (apply 'emacs-process-make-process plist)))

(fset 'emacs-process-processp
      '(lambda (object)
         (emacs-process--process-object-p object)))

(fset 'processp
      '(lambda (object)
         (emacs-process-processp object)))

(fset 'emacs-process-process-list
      '(lambda ()
         (let ((items emacs-process--fallback-processes)
               (result (emacs-process--native-live-processes)))
           (while items
             (if (aref (car items) 8)
                 nil
               (setq result (cons (car items) result)))
             (setq items (cdr items)))
           result)))

(fset 'process-list
      '(lambda ()
         (emacs-process-process-list)))

(fset 'emacs-process-process-status
      '(lambda (process)
         (if (emacs-process--fallback-process-p process)
             (aref process 4)
           (if (emacs-process--native-process-p process)
               (emacs-process--native-status-symbol process)
             nil))))

(fset 'process-status
      '(lambda (process)
         (emacs-process-process-status process)))

(fset 'emacs-process-process-exit-status
      '(lambda (process)
         (if (emacs-process--fallback-process-p process)
             (or (aref process 5) 0)
           (if (emacs-process--native-process-p process)
               (emacs-process--native-exit-status process)
             0))))

(fset 'process-exit-status
      '(lambda (process)
         (emacs-process-process-exit-status process)))

(fset 'emacs-process-process-buffer
      '(lambda (process)
         (if (emacs-process--fallback-process-p process)
             (aref process 2)
           (if (emacs-process--native-process-p process)
               (emacs-process--native-metadata process :buffer)
             nil))))

(fset 'process-buffer
      '(lambda (process)
         (emacs-process-process-buffer process)))

(fset 'emacs-process-process-name
      '(lambda (process)
         (if (emacs-process--fallback-process-p process)
             (aref process 1)
           (if (emacs-process--native-process-p process)
               (emacs-process--native-metadata process :name)
             ""))))

(fset 'process-name
      '(lambda (process)
         (emacs-process-process-name process)))

(fset 'emacs-process-process-command
      '(lambda (process)
         (if (emacs-process--fallback-process-p process)
             (aref process 3)
           (if (emacs-process--native-process-p process)
               (emacs-process--native-metadata process :command)
             nil))))

(fset 'process-command
      '(lambda (process)
         (emacs-process-process-command process)))

(fset 'emacs-process-process-live-p
      '(lambda (process)
         (memq (process-status process) '(run open listen connect stop))))

(fset 'process-live-p
      '(lambda (process)
         (emacs-process-process-live-p process)))

(fset 'emacs-process-process-id
      '(lambda (process)
         (if (emacs-process--fallback-process-p process)
             (aref process 9)
           (if (if (emacs-process--native-process-p process)
                   (fboundp 'nelisp-process-pid)
                 nil)
               (nelisp-process-pid process)
             nil))))

(fset 'process-id
      '(lambda (process)
         (emacs-process-process-id process)))

(fset 'emacs-process-process-mark
      '(lambda (process)
         nil))

(fset 'process-mark
      '(lambda (process)
         (emacs-process-process-mark process)))

(fset 'emacs-process-set-process-filter
      '(lambda (process filter)
         (if (emacs-process--fallback-process-p process)
             (progn
               (aset process 6 filter)
               filter)
           (if (emacs-process--native-process-p process)
               (emacs-process--native-set-metadata process :filter filter)
             nil))))

(fset 'set-process-filter
      '(lambda (process filter)
         (emacs-process-set-process-filter process filter)))

(fset 'emacs-process-set-process-sentinel
      '(lambda (process sentinel)
         (if (emacs-process--fallback-process-p process)
             (progn
               (aset process 7 sentinel)
               sentinel)
           (if (emacs-process--native-process-p process)
               (emacs-process--native-set-metadata
                process :sentinel sentinel)
             nil))))

(fset 'set-process-sentinel
      '(lambda (process sentinel)
         (emacs-process-set-process-sentinel process sentinel)))

(fset 'emacs-process-accept-process-output
      '(lambda (&optional process seconds millisec just-this-one)
         (if (if process
                 (emacs-process--native-process-p process)
               emacs-process--native-process-metadata)
             (emacs-process--native-accept
              (if process
                  (list process)
                (emacs-process--native-live-processes)))
           nil)))

(fset 'accept-process-output
      '(lambda (&optional process seconds millisec just-this-one)
         (emacs-process-accept-process-output
          process seconds millisec just-this-one)))

(fset 'emacs-process-signal-process
      '(lambda (process-or-pid signum)
         (if (emacs-process--fallback-process-p process-or-pid)
             (progn
               (aset process-or-pid 4 'signal)
               (aset process-or-pid 5 1)
               process-or-pid)
           (if (emacs-process--native-process-p process-or-pid)
               (emacs-process--native-delete process-or-pid)
             nil))))

(fset 'signal-process
      '(lambda (process-or-pid signum)
         (emacs-process-signal-process process-or-pid signum)))

(fset 'emacs-process-kill-process
      '(lambda (process)
         (signal-process process 'KILL)))

(fset 'kill-process
      '(lambda (process)
         (emacs-process-kill-process process)))

(fset 'emacs-process-process-send-string
      '(lambda (process string)
         nil))

(fset 'process-send-string
      '(lambda (process string)
         (emacs-process-process-send-string process string)))

(fset 'emacs-process-process-send-eof
      '(lambda (&optional process)
         nil))

(fset 'process-send-eof
      '(lambda (&optional process)
         (emacs-process-process-send-eof process)))

(fset 'emacs-process-delete-process
      '(lambda (process)
         (if (emacs-process--fallback-process-p process)
             (progn
               (aset process 8 t)
               process)
           (if (emacs-process--native-process-p process)
               (emacs-process--native-delete process)
             nil))))

(fset 'delete-process
      '(lambda (process)
         (emacs-process-delete-process process)))

(fset 'emacs-process-shell-command
      '(lambda (command &optional output-buffer error-buffer)
         (call-process emacs-process-shell-file-name
                       nil
                       (if output-buffer output-buffer t)
                       nil
                       emacs-process-shell-command-switch
                       command)))

(fset 'shell-command
      '(lambda (command &optional output-buffer error-buffer)
         (emacs-process-shell-command command output-buffer error-buffer)))

(fset 'emacs-process-shell-command-on-region
      '(lambda (start end command &optional output-buffer replace-flag
                      error-buffer display-error-buffer region-noncontiguous-p)
         (let ((destination
                (if replace-flag
                    emacs-process-shell-command-on-region-output-file
                  (if output-buffer
                      output-buffer
                    emacs-process-shell-command-on-region-output-file)))
               (status 1))
           (if (fboundp 'nl-write-file)
               (nl-write-file destination "")
             0)
           (setq status
                 (call-process-region
                  start end emacs-process-shell-file-name
                  nil destination nil
                  emacs-process-shell-command-switch command))
           (if (if replace-flag
                   (if (fboundp 'delete-region) (fboundp 'insert) nil)
                 nil)
               (let ((text (if (fboundp 'rdf) (rdf destination) "")))
                 (delete-region start end)
                 (insert text))
             0)
           status)))

(fset 'shell-command-on-region
      '(lambda (start end command &optional output-buffer replace-flag
                      error-buffer display-error-buffer region-noncontiguous-p)
         (emacs-process-shell-command-on-region
          start end command output-buffer replace-flag error-buffer
          display-error-buffer region-noncontiguous-p)))

(fset 'emacs-process-async-shell-command
      '(lambda (command &optional output-buffer error-buffer)
         (make-process
          :name (concat "async-shell-command<" command ">")
          :buffer (or output-buffer "*Async Shell Command*")
          :command (list emacs-process-shell-file-name
                         emacs-process-shell-command-switch
                         command))))

(fset 'async-shell-command
      '(lambda (command &optional output-buffer error-buffer)
         (emacs-process-async-shell-command
          command output-buffer error-buffer)))

(fset 'emacs-process-shell-command-to-string
      '(lambda (command)
         (if (fboundp 'with-temp-buffer)
             (with-temp-buffer
               (call-process emacs-process-shell-file-name
                             nil t nil
                             emacs-process-shell-command-switch
                             command)
               (buffer-string))
           "")))

(fset 'shell-command-to-string
      '(lambda (command)
         (emacs-process-shell-command-to-string command)))

(provide 'emacs-process)
(provide 'emacs-process-builtins)

;;; nemacs-runtime-process-preload.el ends here
