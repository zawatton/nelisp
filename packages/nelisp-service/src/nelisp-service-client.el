;;; nelisp-service-client.el --- Connect to, auto-start and proxy NeLisp daemons -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 213 §4.3.  Client side of `nelisp-service-daemon':
;;
;;   (nelisp-service-client-connect NAME :version V :start-command CMD)
;;
;; reads NAME's state file, connects over loopback and says hello.  When
;; no daemon answers and CMD is given, it starts one with CMD and waits
;; for it, so the first client of a session pays the start-up once and
;; later clients reuse the running daemon.  Concurrent first clients are
;; safe: the daemon's exclusive lock lets exactly one instance win.
;;
;; A version mismatch makes the daemon exit (by default) and the client
;; start a replacement, so a rebuilt product never talks to stale code.
;;
;; `nelisp-service-client-stdio-proxy' turns a client into an MCP stdio
;; server: it reads one message at a time from stdin (NDJSON line or
;; Content-Length frame), forwards the JSON text as the request payload,
;; and writes the daemon's reply back in the same framing.  An empty reply
;; (a notification) writes nothing.  The proxy is single-threaded and
;; strictly request/response, which is how MCP stdio servers are driven.
;;
;; Spawned daemons are detached by default: on the standalone reader a
;; child started with `make-process' outlives its parent (measured
;; 2026-10-10), so the daemon keeps running after the first client exits.

;;; Code:

(require 'nelisp-service)

(defvar nelisp-service-client--started (float-time)
  "When this client library was loaded; sent in the handshake.
A daemon only lets a client newer than itself replace it.")

(defvar nelisp-service-client-spawn-function #'nelisp-service-client-spawn-detached
  "Function of (NAME COMMAND) that starts a daemon process.")

(defun nelisp-service-client-spawn-detached (name command)
  "Start COMMAND for daemon NAME without tying its life to this process."
  (make-process :name (format "nelisp-service-start-%s" name)
                :command command
                :connection-type 'pipe
                :noquery t
                :filter #'ignore
                :sentinel #'ignore))

;;; Connection ------------------------------------------------------------

(defun nelisp-service-client--on-input (client chunk)
  "Handle CHUNK received by CLIENT."
  (dolist (message (nelisp-service-reader-feed
                    (nelisp-service-get client :reader) chunk))
    (progn
      (when (consp message)
        (let ((kind (car message))
              (a (nth 1 message))
              (b (nth 2 message)))
          (cond
           ((eq kind 'welcome) (nelisp-service-put client :welcome t))
           ((eq kind 'reject) (nelisp-service-put client :rejected a))
           ((eq kind 'pong) (nelisp-service-put client :pong a))
           ((eq kind 'bye) (nelisp-service-put client :bye t))
           ((memq kind '(res err))
            (let ((callback (gethash a (nelisp-service-get client :pending))))
              (remhash a (nelisp-service-get client :pending))
              (when callback
                (funcall callback (if (eq kind 'res) 'ok 'error) b))))))))))

(defun nelisp-service-client--on-close (client proc)
  "Fail CLIENT's outstanding requests once PROC has closed."
  (unless (memq (process-status proc) '(open run))
    (nelisp-service-put client :closed t)
    (let ((pending (nelisp-service-get client :pending))
          (callbacks nil))
      (maphash (lambda (_id cb) (push cb callbacks)) pending)
      (clrhash pending)
      (dolist (cb callbacks)
        (funcall cb 'disconnected "connection to daemon closed")))))

(defun nelisp-service-client--open (name state version)
  "Connect to daemon NAME described by STATE as VERSION.
Return the client on welcome, the rejection reason symbol on
rejection, or nil when nothing answers."
  (let ((client (nelisp-service-record
                 'nelisp-service-client
                 :name name :version version
                 :reader (nelisp-service-reader-create)
                 :pending (make-hash-table :test 'eql)
                 :next-id 0 :welcome nil :rejected nil :closed nil)))
    (condition-case nil
        (let ((proc (make-network-process
                     :name (format "nelisp-service-client-%s" name)
                     :host "127.0.0.1" :service (plist-get state :port)
                     :family 'ipv4 :noquery t
                     :filter (lambda (_p chunk)
                               (nelisp-service-client--on-input client chunk))
                     :sentinel (lambda (p _event)
                                 (nelisp-service-client--on-close client p)))))
          (nelisp-service-put client :process proc)
          (process-send-string
           proc (nelisp-service-encode
                 (list 'hello (plist-get state :token) version
                       nelisp-service-client--started)))
          (nelisp-service-wait-until
           (lambda () (or (nelisp-service-get client :welcome)
                          (nelisp-service-get client :rejected)
                          (nelisp-service-get client :closed)))
           10)
          (cond
           ((nelisp-service-get client :welcome) client)
           (t (condition-case nil (delete-process proc) (error nil))
              (or (nelisp-service-get client :rejected) nil))))
      (error nil))))

(defun nelisp-service-client--lock-age (name)
  "Return the age in seconds of NAME's lock file, or nil without one."
  (let ((lock (nelisp-service-read-plist (nelisp-service-state-file name "lock"))))
    (and lock (- (float-time) (or (plist-get lock :started) 0)))))

(defun nelisp-service-client-connect (name &rest args)
  "Return a client connected to daemon NAME, starting it when needed.
ARGS is a plist:
:version        the version to present (default nil)
:start-command  command list that starts the daemon; nil means fail
                when it is not running
:timeout        seconds to wait overall (default 180)
:stale-after    seconds after which an unanswered lock is cleared
                (default 120)
Signal an error when no daemon can be reached in time."
  (let* ((version (plist-get args :version))
         (command (plist-get args :start-command))
         (stale-after (or (plist-get args :stale-after) 120))
         (deadline (+ (float-time) (or (plist-get args :timeout) 180)))
         (spawned-at nil)
         (client nil))
    (while (and (not client) (< (float-time) deadline))
      (let ((state (nelisp-service-read-plist
                    (nelisp-service-state-file name "state")))
            (age (nelisp-service-client--lock-age name)))
        (cond
         (state
          (let ((result (nelisp-service-client--open name state version)))
            (cond
             ((and result (not (symbolp result))) (setq client result))
             ((eq result 'version)
              ;; The daemon exits to make room; start ours once it is gone.
              (nelisp-service-wait-until
               (lambda () (not (file-exists-p
                                (nelisp-service-state-file name "lock"))))
               15)
              (setq spawned-at nil))
             ((or (null age) (> age stale-after))
              ;; Nothing answers and no live lock: leftovers of a crash.
              (nelisp-service-delete-file (nelisp-service-state-file name "state"))
              (nelisp-service-delete-file (nelisp-service-state-file name "lock")))
             (t (accept-process-output nil 0.25)))))
         ((and age (> age stale-after))
          (nelisp-service-delete-file (nelisp-service-state-file name "lock")))
         (age (accept-process-output nil 0.25))
         ((and command (or (null spawned-at)
                           (> (- (float-time) spawned-at) stale-after)))
          (funcall nelisp-service-client-spawn-function name command)
          (setq spawned-at (float-time)))
         ((null command)
          (error "nelisp-service: daemon %s is not running" name))
         (t (accept-process-output nil 0.25)))))
    (or client
        (error "nelisp-service: could not reach daemon %s" name))))

;;; Requests --------------------------------------------------------------

(defun nelisp-service-client-live-p (client)
  "Return non-nil while CLIENT's connection is open."
  (and client (not (nelisp-service-get client :closed))
       (process-live-p (nelisp-service-get client :process))))

(defun nelisp-service-client-send (client payload callback)
  "Send PAYLOAD through CLIENT; CALLBACK gets (STATUS VALUE) later.
STATUS is `ok', `error' (the handler signalled) or `disconnected'.
Return the request id."
  (let ((id (nelisp-service-incf client :next-id)))
    (puthash id callback (nelisp-service-get client :pending))
    (condition-case nil
        (process-send-string (nelisp-service-get client :process)
                             (nelisp-service-encode (list 'req id payload)))
      (error
       (remhash id (nelisp-service-get client :pending))
       (funcall callback 'disconnected "send failed")))
    id))

(defun nelisp-service-client-request (client payload &optional timeout)
  "Send PAYLOAD through CLIENT and return the reply payload.
Signal an error on a handler error, a lost connection, or after
TIMEOUT seconds (default 600)."
  (let ((result nil))
    (nelisp-service-client-send client payload
                                (lambda (status value)
                                  (setq result (cons status value))))
    (unless (nelisp-service-wait-until (lambda () result) (or timeout 600) 0.01)
      (error "nelisp-service: request to %s timed out"
             (nelisp-service-get client :name)))
    (if (eq (car result) 'ok)
        (cdr result)
      (signal 'error (list (format "nelisp-service: %s: %s"
                                   (car result) (cdr result))
                           (car result))))))

(defun nelisp-service-client-ping (client &optional timeout)
  "Return the daemon's stats plist through CLIENT, or nil on timeout."
  (nelisp-service-put client :pong nil)
  (process-send-string (nelisp-service-get client :process)
                       (nelisp-service-encode '(ping)))
  (nelisp-service-wait-until (lambda () (nelisp-service-get client :pong))
                             (or timeout 10)))

(defun nelisp-service-client-shutdown (client &optional timeout)
  "Ask the daemon behind CLIENT to exit; return non-nil when it agreed."
  (process-send-string (nelisp-service-get client :process)
                       (nelisp-service-encode '(shutdown)))
  (nelisp-service-wait-until (lambda () (nelisp-service-get client :bye))
                             (or timeout 10)))

(defun nelisp-service-client-close (client)
  "Close CLIENT's connection."
  (condition-case nil (delete-process (nelisp-service-get client :process))
    (error nil))
  (nelisp-service-put client :closed t))

;;; MCP stdio proxy -------------------------------------------------------

(defun nelisp-service-client--content-length (line)
  "Return the length announced by header LINE, or nil."
  (let ((prefix "content-length:"))
    (when (and (>= (length line) (length prefix))
               (string= (downcase (substring line 0 (length prefix))) prefix))
      (string-to-number (substring line (length prefix))))))

(defun nelisp-service-client-read-mcp-message ()
  "Read one MCP message from stdin.
Return (FRAMING . JSON) with FRAMING `line' or `framed', or nil at EOF."
  (let ((line ""))
    (while (and line (string= line ""))
      (setq line (nelisp-service-read-stdin-line)))
    (when line
      (let ((length (nelisp-service-client--content-length line)))
        (if (not length)
            (cons 'line line)
          ;; Skip the remaining headers up to the blank separator line.
          (let ((header line))
            (while (and header (not (string= header "")))
              (setq header (nelisp-service-read-stdin-line))))
          (cons 'framed (or (nelisp-service-read-stdin-bytes length) "")))))))

(defun nelisp-service-client-write-mcp-message (framing json)
  "Write JSON to stdout in FRAMING (`line' or `framed')."
  (nelisp-service-write-stdout
   (if (eq framing 'framed)
       (format "Content-Length: %d\r\n\r\n%s"
               (nelisp-service-utf8-length json) json)
     (concat json "\n"))))

(defun nelisp-service-client-ensure-started (name &rest args)
  "Start daemon NAME with ARGS' :start-command unless one exists or is starting.
Return immediately; this only spawns.  Use it to overlap a daemon's
cold start with work the caller can do without the daemon."
  (let ((command (plist-get args :start-command)))
    (when (and command
               (not (file-exists-p (nelisp-service-state-file name "state")))
               (not (file-exists-p (nelisp-service-state-file name "lock"))))
      (funcall nelisp-service-client-spawn-function name command)
      t)))

(defun nelisp-service-client-stdio-proxy (name &rest args)
  "Serve MCP on stdin/stdout by forwarding every message to daemon NAME.
ARGS are passed to `nelisp-service-client-connect', plus:
:request-timeout  seconds per forwarded message (default 3600)
:local-handler    function of (JSON CONNECTED) returning a reply string
                  to answer the message without the daemon (\"\" means
                  answer nothing), or nil to forward it.  CONNECTED is
                  non-nil once a daemon connection exists, so a handler
                  can serve cached answers only while the daemon is
                  still starting.
The daemon is started at once but connected to lazily, on the first
message the local handler does not answer.  A message whose connection
drops is resent once over a fresh connection.  Return 0 at end of input."
  (nelisp-service-setup-stdio)
  (apply #'nelisp-service-client-ensure-started name args)
  (let ((client nil)
        (local (plist-get args :local-handler))
        (timeout (or (plist-get args :request-timeout) 3600))
        (message nil))
    (while (setq message (nelisp-service-client-read-mcp-message))
      (let ((reply (and local (funcall local (cdr message)
                                       (nelisp-service-client-live-p client)))))
        (unless reply
          (unless (nelisp-service-client-live-p client)
            (setq client (apply #'nelisp-service-client-connect name args)))
          (setq reply
                (condition-case err
                    (nelisp-service-client-request client (cdr message) timeout)
                  (error
                   (if (eq (nth 2 err) 'disconnected)
                       (progn
                         (setq client (apply #'nelisp-service-client-connect
                                             name args))
                         (nelisp-service-client-request client (cdr message)
                                                        timeout))
                     (signal (car err) (cdr err)))))))
        (when (and (stringp reply) (> (length reply) 0))
          (nelisp-service-client-write-mcp-message (car message) reply))))
    (when client (nelisp-service-client-close client))
    0))

(provide 'nelisp-service-client)

;;; nelisp-service-client.el ends here
