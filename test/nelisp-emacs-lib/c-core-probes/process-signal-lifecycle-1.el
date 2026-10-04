;;; process-signal-lifecycle-1.el --- Real child signals and preserved pipes -*- lexical-binding: t; -*-

(signal-process
 (signal-process 99999999 0)
 (condition-case err (signal-process 99999999 'SIGBOGUS) (error (car err)))
 (signal-process "99999999" 0)
 (signal-process "ccore-missing" 0))

(stop-process
 (let* ((output "")
        (program "import os,signal,sys\ndef stopped(*args):\n print('STOP',flush=True)\n os.kill(os.getpid(),signal.SIGSTOP)\nsignal.signal(signal.SIGTSTP,stopped)\nsignal.signal(signal.SIGUSR1,lambda *args: print('USR1',flush=True))\nprint('READY',flush=True)\nfor line in sys.stdin:\n print('ECHO:'+line.strip(),flush=True)\n")
        (process (make-process
                  :name "ccore-signal-child" :command (list (executable-find "python3") "-u" "-c" program)
                  :connection-type 'pipe :noquery t
                  :filter (lambda (_process text) (setq output (concat output text)))
                  :sentinel (lambda (&rest _args) nil)))
        (wait (lambda (predicate)
                (let ((deadline (+ (float-time) 5)))
                  (while (and (not (funcall predicate)) (< (float-time) deadline))
                    (accept-process-output process 0.02))
                  (unless (funcall predicate) (error "Child protocol timed out"))))))
   (unwind-protect
       (progn
         (funcall wait (lambda () (string-match-p "READY" output)))
         (let* ((alive (= (signal-process process 0) 0))
                (name (process-name process))
                (stop-result (equal (stop-process name) name)))
           (funcall wait (lambda () (eq (process-status process) 'stop)))
           (let ((stopped (eq (process-status process) 'stop))
                 (continue-result (equal (continue-process name) name)))
             (funcall wait (lambda () (eq (process-status process) 'run)))
             (signal-process process 'USR1)
             (funcall wait (lambda () (string-match-p "USR1" output)))
             (process-send-string process "ping\n")
             (funcall wait (lambda () (string-match-p "ECHO:ping" output)))
             (let ((pid (process-id process))
                   (result (list alive stop-result stopped continue-result
                                 (and (process-live-p process) t)
                                 (and (string-match-p "STOP" output) t)
                                 (and (string-match-p "ECHO:ping" output) t))))
               (signal-process process 'STOP)
               (funcall wait (lambda () (eq (process-status process) 'stop)))
               (delete-process process)
               (funcall wait (lambda () (= (signal-process pid 0) -1)))
               (append result (list (= (signal-process pid 0) -1)))))))
     (when (process-live-p process)
       (ignore-errors (signal-process process 'CONT))
       (ignore-errors (delete-process process))))))

(make-process
 (let ((process (start-process "ccore-path-child" nil "cat")))
   (unwind-protect
       (progn
         (accept-process-output process 0.02)
         (list (process-status process) (= (signal-process process 0) 0)))
     (ignore-errors (delete-process process)))))

(processp
 (with-temp-buffer
   (let* ((buffer (current-buffer))
          (filter (lambda (&rest _args) nil))
          (sentinel (lambda (&rest _args) nil))
          ;; This form checks owner routing, independently of socket I/O.
          (process (if (fboundp 'emacs-process-events--make-vec)
                       (emacs-process-events--register
                        (emacs-process-events--make-vec
                         "ccore-event-owner" -1 'pipe 'open filter sentinel
                         buffer nil nil nil nil))
                     (make-pipe-process :name "ccore-event-owner" :buffer buffer
                                        :filter filter :sentinel sentinel))))
     (unwind-protect
         (progn
           (set-process-query-on-exit-flag process nil)
           (list (and (processp process) t) (process-name process)
                 (process-status process) (process-id process)
                 (eq (process-buffer process) buffer)
                 (eq (process-filter process) filter)
                 (eq (process-sentinel process) sentinel)
                 (null (process-query-on-exit-flag process))
                 (and (memq process (process-list)) t)))
       (delete-process process)))))
