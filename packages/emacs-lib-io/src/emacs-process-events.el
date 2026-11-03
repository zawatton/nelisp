;;; emacs-process-events.el --- process objects + filter/sentinel for NeLisp -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 7 — Layer 2 process-object port for NeLisp standalone.
;;
;; Builds on `emacs-network-ffi.el' (= libc socket FFI) to provide the
;; `make-network-process' + `process-*' surface that vendor `server.el'
;; / `jsonrpc.el' / anvil socket bridges expect.
;;
;; Process objects are vectors with a sentinel `:emacs-process-events'
;; head element so `processp' identifies them — Emacs's C process
;; objects are opaque so we cannot match them precisely, but the
;; surface (`set-process-filter' / `process-send-string' / etc) is
;; what consumers use.
;;
;;   [:emacs-process-events name fd kind status filter sentinel buffer
;;    plist coding-system child-of]
;;
;; - kind         = `network-server' / `network-connection' / `pipe'
;; - status       = `open' / `closed' / `connect' / `listen'
;; - child-of     = parent listener fd for accepted children, or nil
;;
;; Two registries:
;;   `emacs-process-events--by-fd'  hash fd → process (eventloop dispatch)
;;   `emacs-process-events--all'    list of all live processes
;;
;; Events delivery: `emacs-eventloop.el' loops on `poll(2)', and for
;; each ready fd looks up the process here, then dispatches:
;;   - listener fd ready → `accept', create child process, fire its
;;     sentinel with "open from server\n"
;;   - connection fd ready → `recv', fire filter with bytes (or fire
;;     sentinel with "connection broken by remote peer\n" on EOF).
;;
;; Each polyfill is gated on `unless (fboundp ...)' so under host Emacs
;; this file is a no-op.

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- process filter/sentinel state lives in the nemacs event-loop bridge.
;;; Code:

(require 'emacs-process-coding)

(require 'emacs-network-ffi)


(defun emacs-process-events--signal-network-error (failure)
  "Signal a network FAILURE using GNU's server-bind error data.
The FFI adapter returns (:error STRING); preserve other diagnostics."
  (let ((diagnostic (cadr failure)) reason)
    (save-match-data
      (when (string-match "\\`bind(.*) failed: errno=\\([0-9]+\\)\\'" diagnostic)
        (setq reason
              (cdr (assq (string-to-number (match-string 1 diagnostic))
                         '((1 . "Operation not permitted")
                           (2 . "No such file or directory")
                           (13 . "Permission denied")
                           (20 . "Not a directory")
                           (22 . "Invalid argument")))))))
    (signal 'file-error (if reason (list "Cannot bind server socket" reason)
                         (list diagnostic)))))

;;;; --- registry ---------------------------------------------------------

(defvar emacs-process-events--by-fd (make-hash-table :test #'eql)
  "fd → process-vector lookup for the eventloop dispatcher.")

(defvar emacs-process-events--all nil
  "List of every live process-events vector.  Used by `process-list'.")

(defvar emacs-process-events--id-counter 0
  "Monotonic counter for `process-id'.  Process objects we create in
elisp do not have a kernel pid; we hand out ids from this counter.")


;;;; --- vector layout helpers --------------------------------------------

(defconst emacs-process-events--vec-tag :emacs-process-events
  "Sentinel value at index 0 to identify our process vectors.")

;; Index 0 = tag, 1 = name, 2 = fd, 3 = kind, 4 = status,
;; 5 = filter, 6 = sentinel, 7 = buffer, 8 = plist,
;; 9 = coding-system, 10 = child-of, 11 = id, 12 = recv-buffer,
;; 13 = on-exit-flag.

(defun emacs-process-events--make-vec (name fd kind status filter sentinel
                                            buffer plist coding child-of id)
  (vector emacs-process-events--vec-tag
          name fd kind status filter sentinel
          buffer plist coding child-of id "" t))

(defsubst emacs-process-events--processp (x)
  (and (vectorp x)
       (>= (length x) 14)
       (eq (aref x 0) emacs-process-events--vec-tag)))

(defun emacs-process-events--pipe-p (process)
  "Recognize the pipe connection representation supplied by the prelude."
  (and (vectorp process) (= (length process) 7)
       (eq (aref process 0) 'pipe-process)))

(defun emacs-process-events--require-process (process)
  "Validate a process owned by this provider."
  (unless (or (emacs-process-events--processp process)
              (emacs-process-events--pipe-p process))
    (signal 'wrong-type-argument (list 'processp process)))
  process)

(defun emacs-process-events--resolve-process (process)
  "Resolve a process object, process name, buffer, or current buffer."
  (if (stringp process)
      (or (get-process process)
          (error "Process %s does not exist" process))
    (emacs-process-builtins--resolve-process process)))

(defun emacs-process-events--get (proc idx)
  (emacs-process-events--require-process proc)
  (if (emacs-process-events--processp proc)
      (aref proc idx)
    ;; The prelude retains only name, status, fd, filter and output.
    ;; Slot 4 is available for attributes subsequently set by this API.
    ;; Creation-time buffer, coding and query options are not retained.
    (cond ((= idx 1) (aref proc 1))
          ((= idx 2) (aref proc 3))
          ((= idx 3) 'pipe)
          ((= idx 4) (aref proc 2))
          ((= idx 5) (or (aref proc 5) 'internal-default-process-filter))
          ((= idx 6) (or (plist-get (aref proc 4) idx)
                        'internal-default-process-sentinel))
          ((= idx 11) nil)
          ((= idx 12) (aref proc 6))
          (t (plist-get (aref proc 4) idx)))))

(defun emacs-process-events--set (proc idx val)
  (emacs-process-events--require-process proc)
  (if (emacs-process-events--processp proc)
      (aset proc idx val)
    (cond ((= idx 1) (aset proc 1 val))
          ((= idx 2) (aset proc 3 val))
          ((= idx 4) (aset proc 2 val))
          ((= idx 5) (aset proc 5 val))
          ((= idx 12) (aset proc 6 val))
          (t (aset proc 4 (plist-put (aref proc 4) idx val)))))
  val)


;;;; --- public API: query / accessor ------------------------------------
;;
;; Note: every name below has a no-op stub in `emacs-stub-bulk.el' (=
;; loaded before us via `emacs-stub.el').  `(unless (fboundp ...))'
;; would therefore SKIP every polyfill below — the stubs are not
;; useful, just placeholders for the host-Emacs C surface.  Switch to
;; a runtime gate keyed on `nl-ffi-call' (= present only on NeLisp
;; standalone) so we override the stubs only when we know we're not
;; actually running under host Emacs (= where the C builtins are
;; canonical and our `defun' would clobber them).

(defconst emacs-process-events--standalone-p
  (fboundp 'nl-ffi-call)
  "Non-nil when the in-process NeLisp FFI primitive exists, signalling
that we are running under standalone NeLisp and the stub-bulk
process-* placeholders should be overridden with real
implementations.")

;;;; --- process-coding-system conversion (Doc 06 C4) --------------------
;;
;; Slot 9 holds a (DECODING . ENCODING) cons (or, for back-compat, a bare
;; symbol meaning both directions).  Incoming bytes are decoded before the
;; filter sees them; outgoing strings are encoded before `send'.  These
;; helpers are defined ungated (no `nl-ffi-call' dependency) so they run and
;; are testable under host Emacs too; only the recv/send call sites that use
;; them are standalone-gated.

(defun emacs-process-events--coding-decoder (coding)
  "Decoding coding-system from CODING (a (DECODE . ENCODE) cons / symbol / nil)."
  (if (consp coding) (car coding) coding))

(defun emacs-process-events--coding-encoder (coding)
  "Encoding coding-system from CODING (a (DECODE . ENCODE) cons / symbol / nil)."
  (if (consp coding) (cdr coding) coding))

(defun emacs-process-events--no-conversion-p (cs)
  "Non-nil when coding-system CS means \"pass raw bytes through unchanged\"."
  (memq cs '(nil binary no-conversion raw-text raw-text-unix)))

(defun emacs-process-events--decode-output (chunk coding)
  "Decode raw output CHUNK per CODING's decoder via the coding machinery.
Identity when the decoder is a no-conversion system, when CHUNK is not a
string, or when `decode-coding-string' is unavailable (Doc 06 C4)."
  (let ((dec (emacs-process-events--coding-decoder coding)))
    (if (or (not (stringp chunk))
            (emacs-process-events--no-conversion-p dec)
            (not (fboundp 'decode-coding-string)))
        chunk
      (emacs-process-coding-convert chunk dec nil))))

(defun emacs-process-events--encode-input (string coding)
  "Encode input STRING per CODING's encoder via the coding machinery.
Identity when the encoder is a no-conversion system, when STRING is not a
string, or when `encode-coding-string' is unavailable (Doc 06 C4)."
  (let ((enc (emacs-process-events--coding-encoder coding)))
    (if (or (not (stringp string))
            (emacs-process-events--no-conversion-p enc)
            (not (fboundp 'encode-coding-string)))
        string
      (emacs-process-coding-convert string enc t))))

(when emacs-process-events--standalone-p

  (defun processp (object)
    "Polyfill: return non-nil iff OBJECT is one of our process vectors."
    (emacs-process-events--processp object))

  (defun process-name (process) (emacs-process-events--get process 1))
  (defun process-status (process)
    (if (and (stringp process) (not (get-process process)))
        nil
      (setq process (emacs-process-events--resolve-process process))
      (if (emacs-process-events--child-process-p process)
          (emacs-process-process-status process)
        (emacs-process-events--get process 4))))
  (defun process-id (process)
    (if (memq (emacs-process-events--get process 3)
              '(network-server network-connection pipe))
        nil
      (emacs-process-events--get process 11)))
  (defun process-buffer (process) (emacs-process-events--get process 7))
  (defun process-filter (process) (emacs-process-events--get process 5))
  (defun process-sentinel (process) (emacs-process-events--get process 6))
  (defun process-plist (process) (emacs-process-events--get process 8))

  (defun process-list ()
    "Polyfill: return the list of every live process vector."
    (let ((live nil))
      (dolist (p emacs-process-events--all)
        (unless (eq (process-status p) 'closed)
          (push p live)))
      (nreverse live)))


  ;; --- mutator -----

  (defun set-process-filter (process function)
    (setq function (or function 'internal-default-process-filter))
    (emacs-process-events--set process 5 function)
    function)

  (defun set-process-sentinel (process function)
    (setq function (or function 'internal-default-process-sentinel))
    (emacs-process-events--set process 6 function)
    function)

  (defun set-process-buffer (process buffer)
    (emacs-process-events--require-process process)
    (unless (or (null buffer) (bufferp buffer))
      (signal 'wrong-type-argument (list 'bufferp buffer)))
    (emacs-process-events--set process 7 buffer)
    buffer)

  (defun set-process-plist (process plist)
    (emacs-process-events--require-process process)
    (unless (listp plist)
      (signal 'wrong-type-argument (list 'listp plist)))
    (emacs-process-events--set process 8 plist)
    plist)

  (defun set-process-coding-system (process &optional decoding encoding)
    "Set PROCESS's (DECODING . ENCODING) coding systems (Doc 06 C4).
ENCODING is now honored (previously only DECODING was stored)."
    (emacs-process-events--set process 9 (cons decoding encoding))
    (cons decoding encoding))

  (defun process-coding-system (process)
    "Return PROCESS's coding systems as a (DECODING . ENCODING) cons.
A bare-symbol slot value (legacy) is widened to (SYM . SYM)."
    (let ((c (emacs-process-events--get process 9)))
      (if (consp c) c (cons c c))))

  (defun set-process-query-on-exit-flag (process flag)
    (emacs-process-events--set process 13 (and flag t))
    flag)

  (defun process-query-on-exit-flag (process)
    (emacs-process-events--get process 13)))


;;;; --- public API: I/O -------------------------------------------------

(defun process-id-fd (process)
  "Return the underlying libc fd for PROCESS.
Provided so the eventloop can build pollfd arrays."
  (emacs-process-events--get process 2))

(when emacs-process-events--standalone-p

  (defun process-send-string (process string)
    "Polyfill: send STRING to PROCESS via libc `send'.
Loops on partial writes / EAGAIN until all bytes are accepted by
the kernel.  Returns nil; signals on hard error (= ECONNRESET / EPIPE).

Perf (2026-05-12): the common case is that `send(2)' accepts the
whole STRING in one syscall (UNIX socket buffer is ~200 kB).  The
prior unconditional `(substring string i n)' allocation on every
iteration cost ~3 s on 20 kB payloads in NeLisp standalone because
the runtime's `substring' walks/copies the source byte-by-byte.
Fast-path: when no bytes have been sent yet, pass STRING in as-is;
only fall through to `substring' on the rare partial-write retry."
    (unless (stringp string)
      (signal 'wrong-type-argument (list 'stringp string)))
    (setq process (emacs-process-events--resolve-process process))
    (unless (memq (process-status process) '(run open stop))
      (error "Process %s not running: finished\n" (process-name process)))
    ;; C4: encode the string per the process's encoding coding-system before
    ;; the bytes go on the wire (identity for utf-8 / no-conversion).
    (setq string (emacs-process-events--encode-input
                  string (emacs-process-events--get process 9)))
    (let ((fd (if (and (eq (emacs-process-events--get process 3) 'pipe)
                       (> (length process) 18))
                  (aref process 16)
                (process-id-fd process)))
          (pipe (memq (emacs-process-events--get process 3) '(pipe pipe-process)))
          (i 0)
          (n (length string)))
      (while (< i n)
        (let* ((chunk (if (= i 0) string (substring string i n)))
               (sent (if pipe (emacs-network-ffi--write fd chunk)
                       (emacs-network-ffi--send fd chunk 0))))
          (cond
           ((and (integerp sent) (> sent 0)) (setq i (+ i sent)))
           ((and (integerp sent) (zerop sent))
            (signal 'file-error
                    (list "process-send-string: peer closed connection")))
           (t
            (let ((errno (emacs-network-ffi--errno)))
              (cond
               ((or (= errno emacs-network-ffi-EAGAIN)
                    (= errno emacs-network-ffi-EINTR))
                (when (fboundp 'sit-for) (sit-for 0.001)))
               (t (signal 'file-error
                          (list (format "process-send-string: errno=%d"
                                        errno))))))))))
      nil))

  (defun process-send-region (process start end)
    "Send the current buffer's region to PROCESS."
    (setq process (emacs-process-events--resolve-process process))
    (process-send-string
     process (buffer-substring-no-properties start end)))

  (defun delete-process (&optional process)
    "Polyfill: close the underlying fd, fire the sentinel, drop from registry."
    (setq process (emacs-process-events--resolve-process process))
    (let ((fd (process-id-fd process)))
      (when (and (eq (emacs-process-events--get process 3) 'pipe)
                 (> (length process) 18))
        (let ((i 16))
          (while (< i 19)
            (let ((peer (aref process i)))
              (when (and (integerp peer) (>= peer 0))
                (emacs-network-ffi--close peer)
                (aset process i -1)))
            (setq i (1+ i)))))
      (when (and fd (>= fd 0))
        (emacs-network-ffi--close fd)
        (remhash fd emacs-process-events--by-fd))
      (emacs-process-events--set process 4 'closed)
      (emacs-process-events--set process 2 -1)
      (let ((sent (process-sentinel process)))
        (when (functionp sent)
          (condition-case nil
              (funcall sent process "deleted by user\n")
            (error nil))))
      nil)))


;;;; --- private API: child registration / dispatch ----------------------

(defun emacs-process-events--register (proc)
  "Add PROC to the by-fd registry + the all-list."
  (let ((fd (process-id-fd proc)))
    (when (and fd (>= fd 0))
      (puthash fd proc emacs-process-events--by-fd))
    (push proc emacs-process-events--all))
  proc)

(defun emacs-process-events--lookup-by-fd (fd)
  (gethash fd emacs-process-events--by-fd))

(defun emacs-process-events--all-fds ()
  "Return the list of fds for live registered processes.
Derived from the all-process list rather than `maphash' over the by-fd table:
the pure-elisp standalone reader's `maphash' does not iterate (it returns with
the callback never run), which silently emptied the eventloop poll set and made
async filters never fire (Doc 06 C1)."
  (let ((fds nil))
    (dolist (proc emacs-process-events--all)
      (let ((fd (process-id-fd proc)))
        (when (and (integerp fd) (>= fd 0)
                   (not (memq (emacs-process-events--get proc 4) '(closed stop exit signal)))
                   (not (eq (process-filter proc) t)))
          (push fd fds))))
    fds))

(defun emacs-process-events-dispatch-pending (&optional process just-this-one)
  "Poll registered network/pipe fds without waiting; dispatch ready callbacks.
Return activity for PROCESS, or any activity when nil.  Native subprocess
reaping belongs to emacs-process; this registry never reaps unrelated PIDs."
  (let* ((fds (if (and process just-this-one)
                  (list (process-id-fd process))
                (emacs-process-events--all-fds)))
         (count (length fds)) (any nil))
    (when (> count 0)
      (let ((buf (nl-ffi-malloc (* count 8))) (index 0))
        (unwind-protect
            (progn
              (dolist (fd fds)
                (nl-ffi-write-i32 buf (* index 8) fd)
                (nl-ffi-write-i16 buf (+ (* index 8) 4) 1)
                (nl-ffi-write-i16 buf (+ (* index 8) 6) 0)
                (setq index (1+ index)))
              (when (> (emacs-network-ffi--call
                        "poll" [:sint32 :pointer :sint32 :sint32] buf count 0) 0)
                (setq index 0)
                (dolist (fd fds)
                  (let ((target (emacs-process-events--lookup-by-fd fd)))
                    (when (and target (/= 0 (nl-ffi-read-i16 buf (+ (* index 8) 6))))
                      (let ((activity nil))
                        (if (eq (emacs-process-events--get target 3) 'network-server)
                            (let ((child t))
                              (while child
                                (setq child (emacs-process-events--accept-child target))
                                (when child (setq activity t))))
                          (setq activity (emacs-process-events--read-and-dispatch target)))
                        (when (and activity (or (null process) (eq process target)))
                          (setq any t)))))
                  (setq index (1+ index)))))
          (nl-ffi-free buf))))
    (let ((reaped (emacs-process-events--reap-children)))
      (when (and reaped
                 (or (null process)
                     (and (vectorp process) (> (length process) 8)
                          (eq (aref process 0) :emacs-process-events)
                          (assoc (plist-get (aref process 8) :pid) reaped))))
        (setq any t)))
    any))

;;;; --- SIGCHLD-fallback child reaping (Doc 06 C2) ----------------------
;;
;; The pure-elisp standalone reader cannot run Lisp from a C signal handler, so
;; a true async SIGCHLD reaper is out of reach; instead we poll `wait4(-1,
;; WNOHANG)' from the event loop (the fallback the design calls for).  Sub-
;; process procs carry their OS pid in the plist (`:pid'); a reaped pid is
;; matched back to its proc to update status + fire the sentinel.

(defun emacs-process-events--wait-exited-p (status)
  "Non-nil if wait(2) STATUS indicates a normal exit (WIFEXITED)."
  (= 0 (logand status #x7f)))

(defun emacs-process-events--wait-exit-code (status)
  "WEXITSTATUS(STATUS): the child's exit code (low 8 bits of status >> 8)."
  (logand (ash status -8) #xff))

(defun emacs-process-events--wait-signal (status)
  "WTERMSIG(STATUS): the signal that killed the child, or 0 on normal exit."
  (logand status #x7f))

(defun emacs-process-events--lookup-by-pid (pid)
  "Find a registered process whose plist `:pid' equals PID, or nil.
Scans the all-process list (the standalone reader's `maphash' does not
iterate; see `emacs-process-events--all-fds')."
  (let ((found nil) (rest emacs-process-events--all))
    (while (and rest (not found))
      (let ((p (car rest)))
        (when (eql pid (plist-get (emacs-process-events--get p 8) :pid))
          (setq found p)))
      (setq rest (cdr rest)))
    found))

(defun emacs-process-events--reap-children ()
  "Reap only tracked pipe children; leave other process owners' PIDs alone."
  (let ((reaped nil))
    (dolist (proc (copy-sequence emacs-process-events--all))
      (let ((pid (plist-get (emacs-process-events--get proc 8) :pid)))
        (when (and (integerp pid) (> pid 0)
                   (not (memq (emacs-process-events--get proc 4) '(exit signal))))
          (let ((buf (nl-ffi-malloc 8)))
            (unwind-protect
                (when (= pid (emacs-network-ffi--call
                              "wait4" [:sint32 :sint32 :pointer :sint32 :sint32]
                              pid buf 1 0))
                  (let* ((status (nl-ffi-read-i32 buf 0))
                         (exited (emacs-process-events--wait-exited-p status))
                         (code (emacs-process-events--wait-exit-code status)))
                    (push (cons pid code) reaped)
                    (emacs-process-events--set proc 4 (if exited 'exit 'signal))
                    (let ((sent (process-sentinel proc)))
                      (when (functionp sent)
                        (funcall sent proc (if exited
                                               (format "finished with code %d\n" code)
                                             (format "terminated by signal %d\n"
                                                     (emacs-process-events--wait-signal status))))))))
              (nl-ffi-free buf))))))
    (nreverse reaped)))

(defun emacs-process-events--accept-child (server)
  "Server fd is poll-ready: accept the next pending connection.
Creates a connection process inheriting filter / sentinel / coding
/ buffer from SERVER's plist, fires its sentinel with `(:open ...)`,
returns the new process or nil on EAGAIN / error."
  (let* ((sfd (process-id-fd server))
         (cfd (emacs-network-ffi--accept sfd)))
    (cond
     ((or (not (integerp cfd)) (< cfd 0))
      ;; EAGAIN is not interesting — caller will re-poll.
      nil)
     (t
      (emacs-network-ffi--set-nonblocking cfd)
      (let* ((id (cl-incf emacs-process-events--id-counter))
             (cname (format "%s<%d>" (process-name server) cfd))
             (proc (emacs-process-events--make-vec
                    cname cfd 'network-connection 'open
                    (process-filter server)
                    (process-sentinel server)
                    (process-buffer server)
                    ;; Inherit a shallow copy of the listener's plist —
                    ;; vendor server.el authenticates local clients via
                    ;; the child's `(process-get proc :authenticated)'
                    ;; which Emacs copies from the server (M14).
                    (append (process-plist server) nil)
                    (emacs-process-events--get server 9) ; coding
                    sfd
                    id)))
        (emacs-process-events--register proc)
        (let ((sent (process-sentinel server)))
          (when (functionp sent)
            (condition-case err
                (funcall sent proc "open\n")
              (error
               (when (fboundp 'nelisp--write-stderr-line)
                 (nelisp--write-stderr-line
                  (format "[emacs-process-events] sentinel ERR on accept: %S"
                          err)))))))
        proc)))))

(defun emacs-process-events--read-fd (fd max-bytes)
  "FFI: ssize_t read(FD, buf, MAX-BYTES) for a pipe-subprocess fd (Doc 06 C1).
Pipe fds are not sockets, so `recv(2)' (ENOTSOCK) cannot be used; plain
`read(2)' is.  Returns a string (\"\" at EOF), :would-block on EAGAIN,
:interrupted on EINTR, or nil on any other error."
  (let* ((buf (nl-ffi-malloc max-bytes))
         (got (emacs-network-ffi--call
               "read" [:sint64 :sint32 :pointer :sint64] fd buf max-bytes))
         (result nil))
    (cond
     ((and (integerp got) (>= got 0))
      (setq result (if (zerop got) "" (nl-ffi-read-bytes buf got))))
     ((integerp got)
      (let ((errno (emacs-network-ffi--errno)))
        (cond
         ((= errno emacs-network-ffi-EAGAIN) (setq result :would-block))
         ((= errno emacs-network-ffi-EINTR)  (setq result :interrupted))
         (t (setq result nil))))))
    (nl-ffi-free buf)
    result))

(defun emacs-process-events--read-chunk (proc fd)
  "Read up to 4096 bytes from PROC's FD, picking read(2) vs recv(2) by type.
Pipe-subprocess fds (kind `pipe-process') use `read(2)'; socket fds use
`recv(2)' (Doc 06 C1)."
  (if (memq (emacs-process-events--get proc 3) '(pipe pipe-process))
      (emacs-process-events--read-fd fd 4096)
    (emacs-network-ffi--recv fd 4096 0)))

(defun emacs-process-events--read-and-dispatch (proc)
  "Connection PROC has data ready: read + fire filter.
Returns t if dispatch happened, nil if peer closed."
  (let* ((fd (process-id-fd proc))
         (chunk (emacs-process-events--read-chunk proc fd)))
    (cond
     ((eq chunk :would-block) nil)
     ((eq chunk :interrupted) nil)
     ((or (null chunk) (and (stringp chunk) (= 0 (length chunk))))
      (emacs-network-ffi--close fd)
      (remhash fd emacs-process-events--by-fd)
      (emacs-process-events--set proc 2 -1)
      (unless (and (memq (emacs-process-events--get proc 3) '(pipe pipe-process))
                   (integerp (plist-get (emacs-process-events--get proc 8) :pid)))
        (emacs-process-events--set proc 4 'closed)
        (let ((sent (process-sentinel proc)))
          (when (functionp sent)
            (save-current-buffer
              (save-match-data
                (funcall sent proc "connection broken by remote peer\n"))))))
      t)
     ((stringp chunk)
      (let ((filt (process-filter proc))
            ;; C4: decode the raw bytes per the process's decoding
            ;; coding-system before the filter sees them.
            (text (emacs-process-events--decode-output
                   chunk (emacs-process-events--get proc 9))))
        (when (functionp filt)
          (condition-case err
              (funcall filt proc text)
            (error
             (when (fboundp 'nelisp--write-stderr-line)
               (nelisp--write-stderr-line
                (format "[emacs-process-events] filter ERR: %S" err)))))))
      t)
     (t nil))))


;;;; --- public API: make-network-process --------------------------------
;;
;; `make-network-process' has a no-op stub in `emacs-stub-bulk.el', so
;; we override unconditionally on standalone (= same gate as the
;; getters/setters above).  Under host Emacs this whole module never
;; loads (it is only added to load-path by the standalone driver).

(when emacs-process-events--standalone-p
  (defun make-network-process (&rest args)
    "Polyfill: open a network socket process described by keyword ARGS.

Supported keywords (subset of host Emacs):
  :name NAME              — required, used for diagnostic identity
  :family local            — only AF_UNIX is supported in Phase 7
  :service PATH            — required: socket path (UNIX domain)
  :server BACKLOG-OR-T     — non-nil → bind+listen; nil/missing → connect
  :sentinel FN             — process-status change callback
  :filter FN               — incoming-data callback (server CHILD only)
  :buffer BUFFER           — process buffer (passed back to filter)
  :coding CODING           — decoding for incoming bytes (informational)
  :plist PLIST             — initial property list

Unsupported keywords (= AF_INET / TLS / etc) are silently ignored.
Returns the process vector on success, signals `file-error' on failure."
    (let ((name nil) (family nil) (service nil) (server nil)
          (sentinel nil) (filter nil) (buffer nil)
          (coding nil) (plist nil))
      (let ((tail args))
        (while tail
          (let ((k (car tail)) (v (cadr tail)))
            (cond
             ((eq k :name) (setq name v))
             ((eq k :family) (setq family v))
             ((eq k :service) (setq service v))
             ((eq k :server) (setq server v))
             ((eq k :sentinel) (setq sentinel v))
             ((eq k :filter) (setq filter v))
             ((eq k :buffer) (setq buffer v))
             ((eq k :coding) (setq coding v))
             ((eq k :plist) (setq plist v)))
            (setq tail (cddr tail)))))
      (unless (stringp name)
        (error ":name value not a string"))
      (unless (or (null family) (integerp family)
                  (memq family '(local ipv4 ipv6)))
        (error "Unknown address family"))
      (when (or (null family) (eq family 'local))
        (unless (stringp service)
          (signal 'wrong-type-argument (list 'stringp service))))
      (unless (or (null family) (eq family 'local) (eq family 'ipv4))
        (signal 'file-error
                (list (format "make-network-process: only :family local / ipv4 supported in Phase 7b, got %S"
                              family))))
      ;; Pick the right ffi entry per family.  For `local' the
      ;; `service' is the socket-file path; for `ipv4' it is the
      ;; port number (= integer), plus the optional `:host' arg.
      (let* ((id (cl-incf emacs-process-events--id-counter))
             (host-key (plist-get args :host))
             (fd (cond
                  ((and server (eq family 'ipv4))
                   (emacs-network-ffi-server-tcp
                    (or host-key 'local) service
                    (if (numberp server) server 16)))
                  ((eq family 'ipv4)
                   (emacs-network-ffi-client-tcp
                    (or host-key 'local) service))
                  (server
                   (emacs-network-ffi-server-unix
                    service
                    (if (numberp server) server 16)))
                  (t
                   (emacs-network-ffi-client-unix service)))))
        (when (and (consp fd) (eq (car fd) :error))
          (emacs-process-events--signal-network-error fd))
        (let ((proc (emacs-process-events--make-vec
                     name fd
                     (if server 'network-server 'network-connection)
                     (if server 'listen 'open)
                     filter sentinel buffer plist coding nil id)))
          (emacs-process-events--register proc)
          proc)))))


(defun emacs-process-events--child-process-p (process)
  (and (fboundp 'emacs-process--native-process-p)
       (or (emacs-process--native-process-p process)
           (emacs-process--fallback-process-p process))))

(defun emacs-process-events--combined-list ()
  (let ((result (funcall (get 'process-list 'emacs-process-events--event-owner))))
    (dolist (process (emacs-process-process-list))
      (unless (memq process result)
        (setq result (append result (list process)))))
    result))

(defun emacs-process-events--capture-owner-functions ()
  "Save this provider's raw public process functions before merging owners.
This runs on every reload after the raw definitions above have been
re-evaluated, and also runs when the facade has not loaded yet."
  (dolist (name '(processp process-list process-name process-status process-id
                  process-buffer process-filter process-sentinel process-plist
                  process-query-on-exit-flag set-process-filter
                  set-process-sentinel set-process-buffer set-process-plist
                  set-process-query-on-exit-flag process-send-string
                  delete-process))
    (when (fboundp name)
      (put name 'emacs-process-events--event-owner (symbol-function name)))))

(when emacs-process-events--standalone-p
  (emacs-process-events--capture-owner-functions))

(when (and emacs-process-events--standalone-p (fboundp 'emacs-process-processp))
  ;; Keep both owners. A late event-provider load must not erase child
  ;; accessors, while child bridges must not steal event-owned vectors.
  (dolist (entry '((process-name emacs-process-process-name)
                   (process-status emacs-process-process-status)
                   (process-id emacs-process-process-id)
                   (process-buffer emacs-process-process-buffer)
                   (process-filter emacs-process-process-filter)
                   (process-sentinel emacs-process-process-sentinel)
                   (process-plist emacs-process-process-plist)
                   (process-query-on-exit-flag emacs-process-process-query-on-exit-flag)))
    (let ((name (car entry)))
      (put name 'emacs-process-events--event-owner (symbol-function name))
      (put name 'emacs-process-events--child-owner (symbol-function (cadr entry)))
      (fset name
            `(lambda (process)
               (funcall (get ',name
                             (if (emacs-process-events--child-process-p process)
                                 'emacs-process-events--child-owner
                               'emacs-process-events--event-owner)) process)))))
  (dolist (entry '((set-process-filter emacs-process-set-process-filter)
                   (set-process-sentinel emacs-process-set-process-sentinel)
                   (set-process-buffer emacs-process-set-process-buffer)
                   (set-process-plist emacs-process-set-process-plist)
                   (set-process-query-on-exit-flag emacs-process-set-process-query-on-exit-flag)
                   (process-send-string emacs-process-process-send-string)))
    (let ((name (car entry)))
      (put name 'emacs-process-events--event-owner (symbol-function name))
      (put name 'emacs-process-events--child-owner (symbol-function (cadr entry)))
      (fset name
            `(lambda (process value)
               (when (eq ',name 'process-send-string)
                 (unless (stringp value)
                   (signal 'wrong-type-argument (list 'stringp value)))
                 (setq process (emacs-process-events--resolve-process process)))
               (funcall (get ',name
                             (if (emacs-process-events--child-process-p process)
                                 'emacs-process-events--child-owner
                               'emacs-process-events--event-owner)) process value)))))
  (put 'delete-process 'emacs-process-events--event-owner (symbol-function 'delete-process))
  (fset 'delete-process
        '(lambda (&optional process)
           (setq process
                 (if (and (stringp process)
                          (not (get-process process))
                          (fboundp 'emacs-process-builtins--resolve-process))
                     (emacs-process-builtins--resolve-process process)
                   (emacs-process-events--resolve-process process)))
           (if (emacs-process-events--child-process-p process)
               (progn (emacs-process-delete-process process) nil)
             (funcall (get 'delete-process 'emacs-process-events--event-owner) process))))
  (put 'process-list 'emacs-process-events--event-owner (symbol-function 'process-list))
  (fset 'process-list #'emacs-process-events--combined-list)
  (fset 'processp
        '(lambda (process)
           (or (emacs-process-events--processp process)
               (emacs-process-processp process)))))

(when emacs-process-events--standalone-p
  (require 'emacs-process-coding)
  (fset 'process-coding-system #'emacs-process-coding-get)
  (fset 'set-process-coding-system #'emacs-process-coding-set))

(provide 'emacs-process-events)

;;; emacs-process-events.el ends here
