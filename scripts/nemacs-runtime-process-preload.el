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

;; --- Pure-elisp bidirectional subprocess layer (Doc 142 anvil-wl) -------
;;
;; The standalone reader's native `nelisp-process-start' object is
;; output-only: it wires a single pipe to the child's stdout/stderr and
;; leaves the child's stdin inherited from the parent, so there is no way
;; to `process-send-string' to a live child.  The interactive IMAP engine
;; (anvil-wl-imap.el) drives a bidirectional tunnel: send a command line,
;; then `accept-process-output' until the tagged reply arrives, while the
;; process is live.
;;
;; This layer closes the gap entirely in pure elisp, using only the
;; standalone reader's own builtins (`syscall-direct', `alloc-bytes',
;; `ptr-read-u32', `ptr-read-u8', `ptr-write-u8', `ptr-write-u64') — no
;; Rust, no `nelisp-os-*' FFI (which targets a different runtime).  It
;; forks, sets up TWO pipes (parent->child stdin, child->stdout parent),
;; dup2's both in the child, then execve's the program.  The parent keeps
;; the stdin write fd and the (nonblocking) stdout read fd in a tagged
;; vector that the facades below recognise.
;;
;; Linux x86_64 syscall numbers used (no libc dependency):
;;   read=0 write=1 close=3 fcntl=72 pipe=22 dup2=33 execve=59 fork=57
;;   wait4=61 kill=62 nanosleep=35 exit=60
(setq emacs-process--bidi-tag 'emacs-process-bidi)
(setq emacs-process--bidi-processes nil)
(setq emacs-process--bidi-next-pid 20000)
;; Slot layout of the bidi vector:
;;  0 tag  1 name  2 buffer  3 command  4 status(symbol) 5 exit-code
;;  6 filter 7 sentinel 8 deleted 9 pid 10 in-fd(write) 11 out-fd(read)
;;  12 sentinel-fired
(setq emacs-process--bidi-NR-read 0)
(setq emacs-process--bidi-NR-write 1)
(setq emacs-process--bidi-NR-close 3)
(setq emacs-process--bidi-NR-fcntl 72)
(setq emacs-process--bidi-NR-pipe 22)
(setq emacs-process--bidi-NR-dup2 33)
(setq emacs-process--bidi-NR-execve 59)
(setq emacs-process--bidi-NR-fork 57)
(setq emacs-process--bidi-NR-wait4 61)
(setq emacs-process--bidi-NR-kill 62)
(setq emacs-process--bidi-NR-nanosleep 35)
(setq emacs-process--bidi-NR-exit 60)
(setq emacs-process--bidi-F-SETFL 4)
(setq emacs-process--bidi-O-NONBLOCK 2048)
(setq emacs-process--bidi-SIGTERM 15)
(setq emacs-process--bidi-SIGKILL 9)
(setq emacs-process--bidi-WNOHANG 1)
(setq emacs-process--bidi-read-chunk 4096)

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
         (if (emacs-process--bidi-process-p object)
             t
           (if (emacs-process--fallback-process-p object)
               t
             (emacs-process--native-process-p object)))))

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

;; --- bidi layer helpers --------------------------------------------------

;; Callable predicate that recognises closures.  The standalone reader's
;; `functionp' returns nil for a `(closure ...)' object (even though it is
;; perfectly funcall-able), so a plain `(functionp x)' gate silently drops
;; lambda :filter / :sentinel handlers — which is exactly what
;; anvil-wl-imap.el passes.  Accept symbols, `(closure ...)' and
;; `(lambda ...)' forms.
(fset 'emacs-process--callable-p
      '(lambda (object)
         (cond
          ((null object) nil)
          ((functionp object) t)
          ((if (symbolp object) (fboundp object) nil) t)
          ((if (consp object)
               (if (eq (car object) 'closure) t (eq (car object) 'lambda))
             nil)
           t)
          (t nil))))

(fset 'emacs-process--bidi-available-p
      '(lambda ()
         (if (fboundp 'syscall-direct)
             (if (fboundp 'alloc-bytes)
                 (if (fboundp 'ptr-read-u32)
                     (if (fboundp 'ptr-read-u8)
                         (if (fboundp 'ptr-write-u8)
                             (fboundp 'ptr-write-u64)
                           nil)
                       nil)
                   nil)
               nil)
           nil)))

(fset 'emacs-process--bidi-process-p
      '(lambda (object)
         (if (vectorp object)
             (if (<= 13 (length object))
                 (eq (aref object 0) emacs-process--bidi-tag)
               nil)
           nil)))

;; Build a NUL-terminated C string buffer for S; return its pointer.
(fset 'emacs-process--bidi-cstr
      '(lambda (s)
         (let ((n (length s))
               (buf nil)
               (i 0))
           (setq buf (alloc-bytes (+ n 1) 1))
           (while (< i n)
             (ptr-write-u8 buf i (aref s i))
             (setq i (+ i 1)))
           (ptr-write-u8 buf n 0)
           buf)))

;; Build a NULL-terminated array of C-string pointers for STRS; return ptr.
(fset 'emacs-process--bidi-cstr-array
      '(lambda (strs)
         (let ((n (length strs))
               (arr nil)
               (i 0)
               (items strs))
           (setq arr (alloc-bytes (* (+ n 1) 8) 8))
           (while items
             (ptr-write-u64 arr (* i 8) (emacs-process--bidi-cstr (car items)))
             (setq i (+ i 1))
             (setq items (cdr items)))
           (ptr-write-u64 arr (* n 8) 0)
           arr)))

;; pipe(2) -> cons (READ-FD . WRITE-FD), or nil on failure.
(fset 'emacs-process--bidi-pipe
      '(lambda ()
         (let ((buf (alloc-bytes 8 4))
               (rc 0))
           (setq rc (syscall-direct emacs-process--bidi-NR-pipe buf 0 0 0 0 0))
           (if (< rc 0)
               nil
             (cons (ptr-read-u32 buf 0) (ptr-read-u32 buf 4))))))

(fset 'emacs-process--bidi-close
      '(lambda (fd)
         (if (>= fd 0)
             (syscall-direct emacs-process--bidi-NR-close fd 0 0 0 0 0)
           0)))

;; Build the program path + argv from a COMMAND list (PROGRAM ARG...).
;; PROGRAM is used verbatim as the execve path (callers pass an absolute
;; path or a PATH-resolved one; the smoke + IMAP engine pass absolutes).
(fset 'emacs-process--bidi-spawn
      '(lambda (command)
         (let ((program (car command))
               (in-pipe (emacs-process--bidi-pipe))
               (out-pipe nil)
               (in-r -1) (in-w -1) (out-r -1) (out-w -1)
               (path nil) (argv nil) (envp nil)
               (pid -1)
               (result nil))
           (if (null in-pipe)
               nil
             (progn
               (setq out-pipe (emacs-process--bidi-pipe))
               (if (null out-pipe)
                   (progn
                     (emacs-process--bidi-close (car in-pipe))
                     (emacs-process--bidi-close (cdr in-pipe))
                     nil)
                 (progn
                   (setq in-r (car in-pipe) in-w (cdr in-pipe)
                         out-r (car out-pipe) out-w (cdr out-pipe))
                   (setq path (emacs-process--bidi-cstr program))
                   (setq argv (emacs-process--bidi-cstr-array command))
                   (setq envp (emacs-process--bidi-cstr-array nil))
                   (setq pid (syscall-direct emacs-process--bidi-NR-fork
                                             0 0 0 0 0 0))
                   (cond
                    ((= pid 0)
                     ;; Child: wire stdin/stdout, drop parent-side fds, exec.
                     (syscall-direct emacs-process--bidi-NR-dup2 in-r 0 0 0 0 0)
                     (syscall-direct emacs-process--bidi-NR-dup2 out-w 1 0 0 0 0)
                     (emacs-process--bidi-close in-w)
                     (emacs-process--bidi-close out-r)
                     (emacs-process--bidi-close in-r)
                     (emacs-process--bidi-close out-w)
                     (syscall-direct emacs-process--bidi-NR-execve
                                     path argv envp 0 0 0)
                     ;; execve only returns on failure.
                     (syscall-direct emacs-process--bidi-NR-exit 127 0 0 0 0 0))
                    ((< pid 0)
                     (emacs-process--bidi-close in-r)
                     (emacs-process--bidi-close in-w)
                     (emacs-process--bidi-close out-r)
                     (emacs-process--bidi-close out-w)
                     nil)
                    (t
                     ;; Parent: close child-side fds, set stdout nonblocking.
                     (emacs-process--bidi-close in-r)
                     (emacs-process--bidi-close out-w)
                     (syscall-direct emacs-process--bidi-NR-fcntl
                                     out-r emacs-process--bidi-F-SETFL
                                     emacs-process--bidi-O-NONBLOCK 0 0 0)
                     (setq result (list pid in-w out-r))
                     result)))))))))

;; nonblocking read(2) of up to NBYTES from FD -> string (may be ""), or
;; nil when the pipe returned EOF / unrecoverable error.  EAGAIN (no data
;; pending) is reported as "" so callers keep polling.
(fset 'emacs-process--bidi-read
      '(lambda (fd nbytes)
         (let ((buf (alloc-bytes nbytes 1))
               (n 0)
               (s "")
               (i 0))
           (setq n (syscall-direct emacs-process--bidi-NR-read
                                   fd buf nbytes 0 0 0))
           (cond
            ((= n 0) nil)                 ; EOF
            ((< n 0) "")                  ; EAGAIN / transient
            (t
             (while (< i n)
               (setq s (concat s (char-to-string (ptr-read-u8 buf i))))
               (setq i (+ i 1)))
             s)))))

;; nanosleep for MS milliseconds (best-effort yield so the child can run).
(fset 'emacs-process--bidi-sleep-ms
      '(lambda (ms)
         (if (> ms 0)
             (let ((ts (alloc-bytes 16 8)))
               (ptr-write-u64 ts 0 (/ ms 1000))
               (ptr-write-u64 ts 8 (* (- ms (* (/ ms 1000) 1000)) 1000000))
               (syscall-direct emacs-process--bidi-NR-nanosleep ts 0 0 0 0 0))
           0)))

;; reap: wait4(pid, &status, WNOHANG).  Returns t when the child exited
;; (status mirrored into the vector), nil while still running.
(fset 'emacs-process--bidi-reap
      '(lambda (process)
         (if (eq (aref process 4) 'run)
             (let ((statusp (alloc-bytes 8 8))
                   (rc 0)
                   (raw 0)
                   (sig 0)
                   (code 0))
               (ptr-write-u64 statusp 0 0)
               (setq rc (syscall-direct emacs-process--bidi-NR-wait4
                                        (aref process 9) statusp
                                        emacs-process--bidi-WNOHANG 0 0 0))
               (cond
                ((= rc 0) nil)            ; still running
                ((< rc 0)
                 (aset process 4 'exit)
                 (aset process 5 -1)
                 t)
                (t
                 (setq raw (ptr-read-u32 statusp 0))
                 (setq sig (logand raw 127))
                 (if (= sig 0)
                     (progn
                       (setq code (logand (ash raw -8) 255))
                       (aset process 4 'exit)
                       (aset process 5 code))
                   (progn
                     (aset process 4 'signal)
                     (aset process 5 (+ 128 sig))))
                 t)))
           nil)))

;; Drain all currently-pending stdout bytes into the filter; reap exit.
;; Returns t when output was observed OR the sentinel fired.
;;
;; Ordering matters: a fast child can be reaped by `wait4' while the kernel
;; still has buffered pipe bytes that read(2) reports as EAGAIN on the same
;; tick.  So once the child has exited we keep reading past EAGAIN until a
;; genuine EOF (read == 0) before firing the sentinel — otherwise a final
;; reply line is lost (e.g. the IMAP tagged completion that arrives just as
;; the tunnel dies).
(fset 'emacs-process--bidi-drain
      '(lambda (process)
         (let ((observed nil)
               (out-fd (aref process 11))
               (buffer (aref process 2))
               (filter (aref process 6))
               (chunk t)
               (exited nil)
               (eof nil)
               (eagain-after-exit 0)
               (loop t))
           ;; Learn the exit state up front (non-blocking).  `bidi-reap'
           ;; only signals the *transition*, so derive `exited' from the
           ;; current status symbol (which reap has just refreshed).
           (emacs-process--bidi-reap process)
           (if (eq (aref process 4) 'run)
               nil
             (setq exited t))
           (if (< out-fd 0)
               (setq eof t loop nil)
             nil)
           (while loop
             (setq chunk (emacs-process--bidi-read
                          out-fd emacs-process--bidi-read-chunk))
             (cond
              ((null chunk)
               ;; Genuine EOF on stdout.
               (setq eof t)
               (setq loop nil))
              ((= (length chunk) 0)
               ;; EAGAIN: no data pending right now.
               (if exited
                   ;; Child gone — buffered bytes may still be in flight.
                   ;; Yield briefly and retry a few times, then accept EOF.
                   (if (< eagain-after-exit 8)
                       (progn
                         (emacs-process--bidi-sleep-ms 5)
                         (setq eagain-after-exit (+ eagain-after-exit 1)))
                     (setq loop nil))
                 ;; Still running — stop here, caller will pump again.
                 (setq loop nil)))
              (t
               (setq observed t)
               (setq eagain-after-exit 0)
               (if buffer
                   (with-current-buffer buffer
                     (goto-char (point-max))
                     (insert chunk))
                 nil)
               (if (emacs-process--callable-p filter)
                   (funcall filter process chunk)
                 nil))))
           ;; Fire the sentinel only after the child exited AND the pipe is
           ;; fully drained (EOF or fd already closed).
           (if (if exited
                   (if eof
                       (if (aref process 12) nil t)
                     nil)
                 nil)
               (let ((sentinel (aref process 7))
                     (event (if (eq (aref process 4) 'exit)
                                "finished\n"
                              (concat "exited abnormally with code "
                                      (number-to-string (aref process 5))
                                      "\n"))))
                 (aset process 12 t)
                 (if (emacs-process--callable-p sentinel)
                     (funcall sentinel process event)
                   nil)
                 (setq observed t))
             nil)
           observed)))

(fset 'emacs-process--bidi-make-process
      '(lambda (&rest plist)
         (let ((name (or (emacs-process--fallback-plist-get plist :name)
                         "process"))
               (buffer (emacs-process--fallback-buffer
                        (emacs-process--fallback-plist-get plist :buffer)))
               (command (emacs-process--fallback-plist-get plist :command))
               (filter (emacs-process--fallback-plist-get plist :filter))
               (sentinel (emacs-process--fallback-plist-get plist :sentinel))
               (spawn nil)
               (process nil))
           (setq spawn (emacs-process--bidi-spawn command))
           (if (null spawn)
               nil
             (progn
               (setq process
                     (vector emacs-process--bidi-tag name buffer command
                             'run nil filter sentinel nil
                             (nth 0 spawn) (nth 1 spawn) (nth 2 spawn)
                             nil))
               (setq emacs-process--bidi-processes
                     (cons process emacs-process--bidi-processes))
               process)))))

(fset 'emacs-process--bidi-live-processes
      '(lambda ()
         (let ((items emacs-process--bidi-processes)
               (result nil))
           (while items
             (if (aref (car items) 8)
                 nil
               (setq result (cons (car items) result)))
             (setq items (cdr items)))
           result)))

(fset 'emacs-process--bidi-accept
      '(lambda (processes)
         (let ((observed nil)
               (items processes))
           (while items
             (if (emacs-process--bidi-process-p (car items))
                 (if (emacs-process--bidi-drain (car items))
                     (setq observed t)
                   nil)
               nil)
             (setq items (cdr items)))
           observed)))

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
         ;; Prefer the pure-elisp bidirectional layer: it is the only path
         ;; that supports `process-send-string' to a live child (the
         ;; interactive IMAP pattern).  Fall back to the output-only native
         ;; object, then to the synchronous fallback.
         (or (if (emacs-process--bidi-available-p)
                 (apply 'emacs-process--bidi-make-process plist)
               nil)
             (if (emacs-process--native-start-available-p)
                 (emacs-process--native-start
                  (or (emacs-process--fallback-plist-get plist :name)
                      "process")
                  (emacs-process--fallback-plist-get plist :buffer)
                  (emacs-process--fallback-plist-get plist :command)
                  (emacs-process--fallback-plist-get plist :filter)
                  (emacs-process--fallback-plist-get plist :sentinel))
               nil)
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
               (result (append (emacs-process--bidi-live-processes)
                               (emacs-process--native-live-processes))))
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
         (if (emacs-process--bidi-process-p process)
             (if (aref process 8)
                 'closed              ; deleted
               (progn
                 ;; Refresh exit state without blocking so `process-live-p'
                 ;; reflects a child that exited between pumps.
                 (emacs-process--bidi-reap process)
                 (aref process 4)))
           (if (emacs-process--fallback-process-p process)
               (aref process 4)
             (if (emacs-process--native-process-p process)
                 (emacs-process--native-status-symbol process)
               nil)))))

(fset 'process-status
      '(lambda (process)
         (emacs-process-process-status process)))

(fset 'emacs-process-process-exit-status
      '(lambda (process)
         (if (emacs-process--bidi-process-p process)
             (progn
               (emacs-process--bidi-reap process)
               (or (aref process 5) 0))
           (if (emacs-process--fallback-process-p process)
               (or (aref process 5) 0)
             (if (emacs-process--native-process-p process)
                 (emacs-process--native-exit-status process)
               0)))))

(fset 'process-exit-status
      '(lambda (process)
         (emacs-process-process-exit-status process)))

(fset 'emacs-process-process-buffer
      '(lambda (process)
         (if (emacs-process--bidi-process-p process)
             (aref process 2)
           (if (emacs-process--fallback-process-p process)
               (aref process 2)
             (if (emacs-process--native-process-p process)
                 (emacs-process--native-metadata process :buffer)
               nil)))))

(fset 'process-buffer
      '(lambda (process)
         (emacs-process-process-buffer process)))

(fset 'emacs-process-process-name
      '(lambda (process)
         (if (emacs-process--bidi-process-p process)
             (aref process 1)
           (if (emacs-process--fallback-process-p process)
               (aref process 1)
             (if (emacs-process--native-process-p process)
                 (emacs-process--native-metadata process :name)
               "")))))

(fset 'process-name
      '(lambda (process)
         (emacs-process-process-name process)))

(fset 'emacs-process-process-command
      '(lambda (process)
         (if (emacs-process--bidi-process-p process)
             (aref process 3)
           (if (emacs-process--fallback-process-p process)
               (aref process 3)
             (if (emacs-process--native-process-p process)
                 (emacs-process--native-metadata process :command)
               nil)))))

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
         (if (emacs-process--bidi-process-p process)
             (aref process 9)
           (if (emacs-process--fallback-process-p process)
               (aref process 9)
             (if (if (emacs-process--native-process-p process)
                     (fboundp 'nelisp-process-pid)
                   nil)
                 (nelisp-process-pid process)
               nil)))))

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
         (if (emacs-process--bidi-process-p process)
             (progn
               (aset process 6 filter)
               filter)
           (if (emacs-process--fallback-process-p process)
               (progn
                 (aset process 6 filter)
                 filter)
             (if (emacs-process--native-process-p process)
                 (emacs-process--native-set-metadata process :filter filter)
               nil)))))

(fset 'set-process-filter
      '(lambda (process filter)
         (emacs-process-set-process-filter process filter)))

(fset 'emacs-process-set-process-sentinel
      '(lambda (process sentinel)
         (if (emacs-process--bidi-process-p process)
             (progn
               (aset process 7 sentinel)
               sentinel)
           (if (emacs-process--fallback-process-p process)
               (progn
                 (aset process 7 sentinel)
                 sentinel)
             (if (emacs-process--native-process-p process)
                 (emacs-process--native-set-metadata
                  process :sentinel sentinel)
               nil)))))

(fset 'set-process-sentinel
      '(lambda (process sentinel)
         (emacs-process-set-process-sentinel process sentinel)))

(fset 'emacs-process-accept-process-output
      '(lambda (&optional process seconds millisec just-this-one)
         ;; Bidirectional layer: drain pending stdout into the filter,
         ;; sleeping in short slices up to SECONDS+MILLISEC so a live child
         ;; has time to answer between pumps (the IMAP --pump contract).
         (if (if process
                 (emacs-process--bidi-process-p process)
               emacs-process--bidi-processes)
             (let ((budget-ms (+ (* (or seconds 0) 1000) (or millisec 0)))
                   (slice 10)
                   (targets (if process
                                (list process)
                              (emacs-process--bidi-live-processes)))
                   (observed nil)
                   (waited 0)
                   (loop t))
               ;; Always do one immediate drain.
               (if (emacs-process--bidi-accept targets)
                   (setq observed t)
                 nil)
               ;; Then poll for the remaining budget until something arrives.
               (while (if loop
                          (if observed
                              nil
                            (< waited budget-ms))
                        nil)
                 (emacs-process--bidi-sleep-ms slice)
                 (setq waited (+ waited slice))
                 (if (emacs-process--bidi-accept targets)
                     (setq observed t)
                   nil))
               observed)
           (if (if process
                   (emacs-process--native-process-p process)
                 emacs-process--native-process-metadata)
               (emacs-process--native-accept
                (if process
                    (list process)
                  (emacs-process--native-live-processes)))
             nil))))

(fset 'accept-process-output
      '(lambda (&optional process seconds millisec just-this-one)
         (emacs-process-accept-process-output
          process seconds millisec just-this-one)))

(fset 'emacs-process--bidi-signal-number
      '(lambda (signum)
         (cond
          ((integerp signum) signum)
          ((eq signum 'TERM) emacs-process--bidi-SIGTERM)
          ((eq signum 'KILL) emacs-process--bidi-SIGKILL)
          (t emacs-process--bidi-SIGTERM))))

(fset 'emacs-process-signal-process
      '(lambda (process-or-pid signum)
         (if (emacs-process--bidi-process-p process-or-pid)
             (progn
               (syscall-direct emacs-process--bidi-NR-kill
                               (aref process-or-pid 9)
                               (emacs-process--bidi-signal-number signum)
                               0 0 0 0)
               process-or-pid)
           (if (emacs-process--fallback-process-p process-or-pid)
               (progn
                 (aset process-or-pid 4 'signal)
                 (aset process-or-pid 5 1)
                 process-or-pid)
             (if (emacs-process--native-process-p process-or-pid)
                 (emacs-process--native-delete process-or-pid)
               nil)))))

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
         (if (emacs-process--bidi-process-p process)
             ;; Write STRING to the child's stdin fd via raw write(2).
             ;; Loop so a short write (partial pipe buffer) still delivers
             ;; the whole line.  ASCII / base64 MIME is in-scope; raw 8-bit
             ;; binary bodies are a known string-model limitation.
             (let ((fd (aref process 10))
                   (remaining string)
                   (n 0)
                   (loop t))
               (while (if loop (> (length remaining) 0) nil)
                 (setq n (syscall-direct
                          emacs-process--bidi-NR-write
                          fd (emacs-process--bidi-cstr remaining)
                          (length remaining) 0 0 0))
                 (cond
                  ((< n 0) (setq loop nil))       ; error — stop
                  ((= n 0) (setq loop nil))       ; nothing written — stop
                  ((>= n (length remaining)) (setq remaining ""))
                  (t (setq remaining (substring remaining n)))))
               nil)
           nil)))

(fset 'process-send-string
      '(lambda (process string)
         (emacs-process-process-send-string process string)))

(fset 'emacs-process-process-send-eof
      '(lambda (&optional process)
         (if (emacs-process--bidi-process-p process)
             ;; Close the stdin write fd so the child sees EOF on stdin.
             (let ((fd (aref process 10)))
               (if (>= fd 0)
                   (progn
                     (emacs-process--bidi-close fd)
                     (aset process 10 -1))
                 nil)
               nil)
           nil)))

(fset 'process-send-eof
      '(lambda (&optional process)
         (emacs-process-process-send-eof process)))

(fset 'emacs-process-delete-process
      '(lambda (process)
         (if (emacs-process--bidi-process-p process)
             (progn
               ;; SIGTERM the child if still running, close fds, reap, mark.
               (if (eq (aref process 4) 'run)
                   (syscall-direct emacs-process--bidi-NR-kill
                                   (aref process 9)
                                   emacs-process--bidi-SIGTERM 0 0 0 0)
                 nil)
               (if (>= (aref process 10) 0)
                   (progn (emacs-process--bidi-close (aref process 10))
                          (aset process 10 -1))
                 nil)
               (if (>= (aref process 11) 0)
                   (progn (emacs-process--bidi-close (aref process 11))
                          (aset process 11 -1))
                 nil)
               (emacs-process--bidi-reap process)
               (aset process 8 t)
               process)
           (if (emacs-process--fallback-process-p process)
               (progn
                 (aset process 8 t)
                 process)
             (if (emacs-process--native-process-p process)
                 (emacs-process--native-delete process)
               nil)))))

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
