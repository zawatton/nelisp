;;; process-control-group-1.el --- Default job-control stops -*- lexical-binding: t; -*-
(stop-process
 (mapcar
  (lambda (group)
    (let* ((output "")
           (process (make-process :name "ccore-group" :connection-type 'pipe
                                  :command '("python3" "-u" "-c" "import os,time,signal; signal.signal(signal.SIGTSTP,lambda *_:os.kill(os.getpid(),signal.SIGSTOP)); print(os.getpgrp()==os.getpid(), flush=True); time.sleep(30)")
                                  :filter (lambda (_p text) (setq output (concat output text)))
                                  :sentinel #'ignore :noquery t))
           (wait (lambda (predicate)
                   ;; Fixture readiness wait, not product behaviour: the loop
                   ;; returns as soon as PREDICATE holds, so a generous bound
                   ;; only matters on a loaded machine (2 s failed ~1 in 4
                   ;; runs at load average ~20).
                   (let ((deadline (+ (float-time) 15)))
                     (while (and (not (funcall predicate)) (< (float-time) deadline))
                       (accept-process-output process 0.02))
                     (unless (funcall predicate) (error "Group transition timed out"))))))
      (unwind-protect
          (progn
            (funcall wait (lambda () (string-match-p "True" output)))
            (stop-process process group)
            (funcall wait (lambda () (eq (process-status process) 'stop)))
            (let ((stopped (eq (process-status process) 'stop)))
              (continue-process process group)
              (funcall wait (lambda () (eq (process-status process) 'run)))
              (list stopped (eq (process-status process) 'run))))
        (ignore-errors (signal-process process 'CONT))
        (ignore-errors (delete-process process)))))
  '(nil t lambda))
 (let* ((output "")
        (process (make-process
                  :name "ccore-foreground" :connection-type 'pty :noquery t
                  :sentinel #'ignore
                  :filter (lambda (_p text) (setq output (concat output text)))
                  :command '("python3" "-u" "-c"
                             "import os,signal,subprocess,time,ctypes
signal.signal(signal.SIGTTOU,signal.SIG_IGN)
job=subprocess.Popen(['sleep','30'],preexec_fn=lambda:(os.setpgrp(),ctypes.CDLL(None).prctl(1,9)))
os.tcsetpgrp(0,job.pid)
print('READY',flush=True)
while True:
 p,s=os.waitpid(job.pid,os.WNOHANG|os.WUNTRACED|os.WCONTINUED)
 if p:
  if os.WIFSTOPPED(s): print('JOB_STOP',flush=True)
  elif os.WIFCONTINUED(s):
   print('JOB_CONT',flush=True)
   os.tcsetpgrp(0,os.getpid())
   print('SHELL',flush=True)
  else: break
 time.sleep(.01)")))
        (wait (lambda (token)
                (let ((deadline (+ (float-time) 3)))
                  (while (and (not (string-match-p token output)) (< (float-time) deadline))
                    (accept-process-output process .02))
                  (unless (string-match-p token output) (error "PTY protocol timed out: %s" token))))))
   (unwind-protect
       (progn
         (funcall wait "READY")
         (stop-process process t)
         (funcall wait "JOB_STOP")
         (let ((parent-running (eq (process-status process) 'run)))
           (continue-process process t)
           (funcall wait "SHELL")
           (stop-process process 'lambda)
           (accept-process-output process .05)
           (list parent-running (eq (process-status process) 'run))))
     (ignore-errors (delete-process process)))))
