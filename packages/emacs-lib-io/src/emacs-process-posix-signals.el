;;; emacs-process-posix-signals.el --- POSIX signal and wait status reference -*- lexical-binding: t; -*-

(require 'emacs-process)

(defun emacs-process-posix--available-p ()
  (and (fboundp 'syscall-direct) (memq system-type '(gnu/linux darwin))))

(defun emacs-process-posix--syscall (linux darwin a b c)
  (syscall-direct (if (eq system-type 'darwin) darwin linux) a b c 0 0 0))

(defun emacs-process-posix--signal-number (signal)
  (if (integerp signal) signal
    (let* ((name (upcase (if (symbolp signal) (symbol-name signal) signal)))
           (name (if (string-prefix-p "SIG" name) (substring name 3) name))
           (table (if (eq system-type 'darwin)
                      '(("HUP" . 1) ("INT" . 2) ("QUIT" . 3) ("KILL" . 9)
                        ("ILL" . 4) ("TRAP" . 5) ("ABRT" . 6) ("IOT" . 6)
                        ("EMT" . 7) ("FPE" . 8) ("BUS" . 10) ("SEGV" . 11)
                        ("SYS" . 12) ("PIPE" . 13) ("ALRM" . 14) ("TERM" . 15)
                        ("URG" . 16) ("STOP" . 17) ("TSTP" . 18) ("CONT" . 19)
                        ("CHLD" . 20) ("TTIN" . 21) ("TTOU" . 22) ("IO" . 23)
                        ("XCPU" . 24) ("XFSZ" . 25) ("VTALRM" . 26) ("PROF" . 27)
                        ("WINCH" . 28) ("INFO" . 29) ("USR1" . 30) ("USR2" . 31))
                    '(("HUP" . 1) ("INT" . 2) ("QUIT" . 3) ("KILL" . 9)
                      ("ILL" . 4) ("TRAP" . 5) ("ABRT" . 6) ("IOT" . 6)
                      ("BUS" . 7) ("FPE" . 8) ("USR1" . 10) ("SEGV" . 11)
                      ("USR2" . 12) ("PIPE" . 13) ("ALRM" . 14) ("TERM" . 15)
                      ("STKFLT" . 16) ("CHLD" . 17) ("CONT" . 18) ("STOP" . 19)
                      ("TSTP" . 20) ("TTIN" . 21) ("TTOU" . 22) ("URG" . 23)
                      ("XCPU" . 24) ("XFSZ" . 25) ("VTALRM" . 26) ("PROF" . 27)
                      ("WINCH" . 28) ("IO" . 29) ("POLL" . 29) ("PWR" . 30)
                      ("SYS" . 31) ("RTMIN" . 34) ("RTMAX" . 64))))
           (entry (assoc name table)))
      (if entry (cdr entry) (error "Unknown signal %s" signal)))))

(defun emacs-process-posix--signal (process-or-pid signal)
  (let ((pid (if (emacs-process--native-process-p process-or-pid)
                 (nelisp-process-pid process-or-pid) process-or-pid)))
    (unless (integerp pid) (signal 'wrong-type-argument (list 'integerp pid)))
    (let ((result (emacs-process-posix--syscall
                   62 #x2000025 pid (emacs-process-posix--signal-number signal) 0)))
      (if (= result 0) 0 -1))))

(defun emacs-process-posix--refresh (process)
  "Observe child state without treating stop/continue as terminal exit."
  (when (= (aref process 3) 0)
    (let* ((status-ptr (or (emacs-process--native-metadata process :wait-status-pointer)
                           (emacs-process--native-set-metadata
                            process :wait-status-pointer (alloc-bytes 8 8))))
           ;; Linux WCONTINUED=8; Darwin WCONTINUED=16.
           (options (logior 1 2 (if (eq system-type 'darwin) 16 8))))
      (ptr-write-u64 status-ptr 0 0)
      (let ((result (emacs-process-posix--syscall
                     61 #x2000007 (nelisp-process-pid process) status-ptr options)))
        (when (= result (nelisp-process-pid process))
          (let* ((status (ptr-read-u64 status-ptr 0))
                 (signal (logand status 127)))
            (cond
             ((= status (if (eq system-type 'darwin) #x137f #xffff))
              (emacs-process--native-set-metadata process :stop-signal nil))
             ((= (logand status 255) 127)
              (emacs-process--native-set-metadata
               process :stop-signal (logand (ash status -8) 255)))
             (t
              (emacs-process--native-set-metadata process :stop-signal nil)
              (aset process 3 (if (= signal 0) 1 2))
              (aset process 4 (if (= signal 0) (logand (ash status -8) 255)
                                (+ 128 signal)))))
            ;; A status accessor may reap the transition before the event
            ;; loop runs. Preserve that notification until sentinel dispatch.
            (emacs-process--native-set-metadata
             process :pending-status-events
             (append (emacs-process--native-metadata process :pending-status-events)
                     (list (cons (cond ((= (aref process 3) 1) 'exit)
                                       ((= (aref process 3) 2) 'signal)
                                       ((emacs-process--native-metadata
                                         process :stop-signal) 'stop)
                                       (t 'run))
                                 (or (emacs-process--native-metadata process :stop-signal)
                                     (aref process 4)))))))))))
  (if (and (= (aref process 3) 0)
           (emacs-process--native-metadata process :stop-signal))
      4 (aref process 3)))

(defun emacs-process-posix--send (process signal current-group)
  ;; Like GNU, pipe connections always address the child's own group,
  ;; including t/lambda CURRENT-GROUP. PTY routing needs a tty descriptor.
  (let* ((pid (nelisp-process-pid process))
         (private (emacs-process-posix--syscall 121 #x2000097 pid 0 0))
         (master (emacs-process--native-metadata process :pty-master))
         (group private))
    (emacs-process--native-set-metadata process :control-inhibited nil)
    (when (and current-group master)
      (let ((word (alloc-bytes 8 8)))
        (ptr-write-u64 word 0 0)
        (when (= (emacs-process-posix--syscall 16 #x2000036 master #x540f word) 0)
          (setq group (ptr-read-u64 word 0)))))
    (unless (= private pid)
      (error "Child has no private process group"))
    (if (and master (eq current-group 'lambda) (= group pid))
        (emacs-process--native-set-metadata process :control-inhibited t)
      (unless (and (> group 0) (= (emacs-process-posix--signal (- group) signal) 0))
        (error "Cannot signal process %s" (process-name process))))
    process))

(provide 'emacs-process-posix-signals)
