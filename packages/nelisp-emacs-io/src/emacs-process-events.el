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

;;; Code:

(require 'emacs-network-ffi)


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

(defun emacs-process-events--get (proc idx)
  (and (emacs-process-events--processp proc) (aref proc idx)))

(defun emacs-process-events--set (proc idx val)
  (when (emacs-process-events--processp proc)
    (aset proc idx val)))


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

(when emacs-process-events--standalone-p

  (defun processp (object)
    "Polyfill: return non-nil iff OBJECT is one of our process vectors."
    (emacs-process-events--processp object))

  (defun process-name (process) (emacs-process-events--get process 1))
  (defun process-status (process) (emacs-process-events--get process 4))
  (defun process-id (process) (emacs-process-events--get process 11))
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
    (emacs-process-events--set process 5 function)
    function)

  (defun set-process-sentinel (process function)
    (emacs-process-events--set process 6 function)
    function)

  (defun set-process-buffer (process buffer)
    (emacs-process-events--set process 7 buffer)
    buffer)

  (defun set-process-plist (process plist)
    (emacs-process-events--set process 8 plist)
    plist)

  (defun set-process-coding-system (process &optional decoding _encoding)
    (emacs-process-events--set process 9 decoding)
    decoding)

  (defun set-process-query-on-exit-flag (process flag)
    (emacs-process-events--set process 13 flag)
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
    (unless (emacs-process-events--processp process)
      (signal 'wrong-type-argument (list 'processp process)))
    (let ((fd (process-id-fd process))
          (i 0)
          (n (length string)))
      (while (< i n)
        (let* ((chunk (if (= i 0) string (substring string i n)))
               (sent (emacs-network-ffi--send fd chunk 0)))
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
    (process-send-string
     process (buffer-substring-no-properties start end)))

  (defun delete-process (process)
    "Polyfill: close the underlying fd, fire the sentinel, drop from registry."
    (let ((fd (process-id-fd process)))
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
  "Return the list of fds currently registered."
  (let ((fds nil))
    (maphash (lambda (fd _proc) (push fd fds))
             emacs-process-events--by-fd)
    fds))

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

(defun emacs-process-events--read-and-dispatch (proc)
  "Connection PROC has data ready: recv + fire filter.
Returns t if dispatch happened, nil if peer closed."
  (let* ((fd (process-id-fd proc))
         (chunk (emacs-network-ffi--recv fd 4096 0)))
    (cond
     ((eq chunk :would-block) nil)
     ((eq chunk :interrupted) nil)
     ((or (null chunk) (and (stringp chunk) (= 0 (length chunk))))
      (let ((sent (process-sentinel proc)))
        (when (functionp sent)
          (condition-case err
              (funcall sent proc "connection broken by remote peer\n")
            (error
             (when (fboundp 'nelisp--write-stderr-line)
               (nelisp--write-stderr-line
                (format "[emacs-process-events] sentinel ERR on close: %S"
                        err)))))))
      (emacs-network-ffi--close fd)
      (remhash fd emacs-process-events--by-fd)
      (emacs-process-events--set proc 4 'closed)
      (emacs-process-events--set proc 2 -1)
      nil)
     ((stringp chunk)
      (let ((filt (process-filter proc)))
        (when (functionp filt)
          (condition-case err
              (funcall filt proc chunk)
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
      (unless name
        (signal 'wrong-type-argument
                (list 'make-network-process "missing :name")))
      (unless service
        (signal 'wrong-type-argument
                (list 'make-network-process "missing :service")))
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
          (signal 'file-error (list (cadr fd))))
        (let ((proc (emacs-process-events--make-vec
                     name fd
                     (if server 'network-server 'network-connection)
                     (if server 'listen 'open)
                     filter sentinel buffer plist coding nil id)))
          (emacs-process-events--register proc)
          proc)))))


(provide 'emacs-process-events)

;;; emacs-process-events.el ends here
