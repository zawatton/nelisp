;;; nelisp-service-daemon.el --- One resident NeLisp server per user and name -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 213 §4.3.  A daemon listens on 127.0.0.1 and answers framed
;; requests (see `nelisp-service-encode') from any number of clients:
;;
;;   client -> daemon  (hello TOKEN VERSION)
;;   daemon -> client  (welcome VERSION) | (reject REASON VERSION)
;;   client -> daemon  (req ID PAYLOAD)     daemon -> client (res ID PAYLOAD)
;;   client -> daemon  (ping)               daemon -> client (pong STATS)
;;   client -> daemon  (shutdown)           daemon -> client (bye)
;;
;; PAYLOAD is any readable object; the daemon hands it to :handler, a
;; function of (CONNECTION PAYLOAD REPLY) that calls REPLY exactly once,
;; now or later, with the response payload.
;;
;; Transport.  The standalone reader has no named pipes and no Win32 FFI,
;; but `make-network-process' works on Windows over loopback (measured
;; 2026-10-10: listen, connect and a round trip on 127.0.0.1).  Two caveats
;; shape the code:
;;   - `:service 0' listens but `process-contact' still reports port 0, so
;;     the daemon picks a concrete port itself.
;;   - A second listener on the same port succeeds on Windows, so a bind
;;     proves nothing about exclusivity.  Single instance comes from the
;;     lock file (created with `excl'), and a port is taken only after a
;;     client connect to it fails.
;;
;; Files under `nelisp-service-state-directory':
;;   NAME.lock   created exclusively before binding, removed on exit
;;   NAME.state  (:port P :token T :version V :started TIME), written after
;;               binding, removed on exit; clients read it to connect
;;
;; Lifetime.  `nelisp-service-daemon-run' services connections until
;; `nelisp-service-daemon-stop', a `(shutdown)' request, a version
;; mismatch when :replace-on-mismatch is set, or :idle-timeout seconds with
;; no connection.  It returns instead of calling `kill-emacs', because the
;; standalone reader's `kill-emacs' does not end the process.

;;; Code:

(require 'nelisp-service)

(defcustom nelisp-service-daemon-port-range '(20000 . 60000)
  "Inclusive range of loopback ports a daemon may listen on."
  :type '(cons integer integer)
  :group 'nelisp-service)

(defun nelisp-service-daemon--port-in-use-p (port)
  "Return non-nil when something accepts connections on loopback PORT."
  (condition-case nil
      (let ((probe (make-network-process
                    :name "nelisp-service-port-probe" :host "127.0.0.1"
                    :service port :family 'ipv4 :noquery t)))
        (delete-process probe)
        t)
    (error nil)))

(defun nelisp-service-daemon--candidate-port (name attempt)
  "Return the ATTEMPT-th candidate port for daemon NAME."
  (let* ((lo (car nelisp-service-daemon-port-range))
         (span (1+ (- (cdr nelisp-service-daemon-port-range) lo)))
         (seed (+ (if (fboundp (quote sxhash-equal)) (sxhash-equal name) (length name))
                  (truncate (* 1000 (float-time)))
                  (random 1000003)
                  (* attempt 7919))))
    (+ lo (mod (abs seed) span))))

(defun nelisp-service-daemon-acquire-lock (name &optional stale-after)
  "Create NAME's lock file, clearing a stale one.  Return non-nil on success.
A lock is stale when no daemon answers on its state file and the lock
is older than STALE-AFTER seconds (default 120).  A daemon whose start-up
is slow calls this first, before loading anything, so concurrent starters
lose fast, then passes :lock-held to `nelisp-service-daemon-start'."
  (setq stale-after (or stale-after 120))
  (let ((lock (nelisp-service-state-file name "lock"))
        (record (list :started (float-time))))
    (or (nelisp-service-write-plist lock record t)
        (let* ((held (nelisp-service-read-plist lock))
               (age (- (float-time) (or (plist-get held :started) 0)))
               (state (nelisp-service-read-plist
                       (nelisp-service-state-file name "state"))))
          (when (and (> age stale-after)
                     (not (and state (nelisp-service-daemon--port-in-use-p
                                      (plist-get state :port)))))
            (nelisp-service-delete-file lock)
            (nelisp-service-delete-file (nelisp-service-state-file name "state"))
            (nelisp-service-write-plist lock record t))))))

(defun nelisp-service-daemon-start (name &rest args)
  "Start daemon NAME and return it, or nil when another instance holds it.
ARGS is a plist:
:handler        function of (CONNECTION PAYLOAD REPLY); required
:version        any readable value clients must match (default nil)
:idle-timeout   seconds without connections before exiting (default 600;
                nil means never)
:replace-on-mismatch  when non-nil (the default), a client with another
                version makes this daemon exit so the client can start
                its own
:stale-after    seconds after which a lock without a live daemon is
                reclaimed (default 120)
:lock-held      non-nil when the caller already holds the lock through
                `nelisp-service-daemon-acquire-lock'
Call `nelisp-service-daemon-run' to serve."
  (unless (plist-get args :handler)
    (error "nelisp-service-daemon-start: :handler is required"))
  (when (or (plist-get args :lock-held)
            (nelisp-service-daemon-acquire-lock name (plist-get args :stale-after)))
    (let ((daemon (nelisp-service-record
                   'nelisp-service-daemon
                   :name name
                   :handler (plist-get args :handler)
                   :version (plist-get args :version)
                   :idle-timeout (if (plist-member args :idle-timeout)
                                     (plist-get args :idle-timeout)
                                   600)
                   :replace-on-mismatch (if (plist-member args :replace-on-mismatch)
                                            (plist-get args :replace-on-mismatch)
                                          t)
                   :token (nelisp-service-make-token)
                   :connections nil
                   :last-activity (float-time)
                   :stopped nil :served 0 :rejected 0
                   :started (float-time))))
      (condition-case err
          (progn
            (nelisp-service-daemon--listen daemon)
            (nelisp-service-write-plist
             (nelisp-service-state-file name "state")
             (list :port (nelisp-service-get daemon :port)
                   :token (nelisp-service-get daemon :token)
                   :version (nelisp-service-get daemon :version)
                   :started (float-time)))
            daemon)
        (error
         (nelisp-service-delete-file (nelisp-service-state-file name "lock"))
         (signal (car err) (cdr err)))))))

(defun nelisp-service-daemon--listen (daemon)
  "Open DAEMON's listening socket on a free loopback port."
  (let ((attempt 0) (server nil))
    (while (and (not server) (< attempt 40))
      (let ((port (nelisp-service-daemon--candidate-port
                   (nelisp-service-get daemon :name) attempt)))
        (unless (nelisp-service-daemon--port-in-use-p port)
          (setq server
                (condition-case nil
                    (make-network-process
                     :name (format "nelisp-service-%s"
                                   (nelisp-service-get daemon :name))
                     :server t :host "127.0.0.1" :service port :family 'ipv4
                     :noquery t
                     :filter (lambda (conn chunk)
                               (nelisp-service-daemon--on-input daemon conn chunk))
                     :sentinel (lambda (conn _event)
                                 (nelisp-service-daemon--on-change daemon conn)))
                  (error nil)))
          (when server (nelisp-service-put daemon :port port))))
      (setq attempt (1+ attempt)))
    (unless server
      (error "nelisp-service-daemon: no free port for %s"
             (nelisp-service-get daemon :name)))
    (nelisp-service-put daemon :server server)))

;;; Connections -----------------------------------------------------------

(defun nelisp-service-daemon--connection (daemon conn)
  "Return DAEMON's record for network process CONN, creating it."
  (or (assq conn (nelisp-service-get daemon :connections))
      (let ((record (list conn
                          :reader (nelisp-service-reader-create)
                          :authenticated nil)))
        (nelisp-service-put daemon :connections
                            (cons record (nelisp-service-get daemon :connections)))
        record)))

(defun nelisp-service-daemon--forget (daemon conn)
  "Drop CONN from DAEMON's connections."
  (nelisp-service-put daemon :connections
                      (let ((kept nil))
                        (dolist (c (nelisp-service-get daemon :connections))
                          (unless (eq (car c) conn) (push c kept)))
                        (nreverse kept))))

(defun nelisp-service-daemon--send (conn object)
  "Send OBJECT to CONN, ignoring a connection that has gone away."
  (condition-case nil
      (process-send-string conn (nelisp-service-encode object))
    (error nil)))

(defun nelisp-service-daemon--on-change (daemon conn)
  "Track the end of connection CONN of DAEMON."
  (unless (or (eq conn (nelisp-service-get daemon :server))
              (memq (process-status conn) '(open run listen)))
    (nelisp-service-daemon--forget daemon conn)
    (nelisp-service-put daemon :last-activity (float-time))))

(defun nelisp-service-daemon--on-input (daemon conn chunk)
  "Handle CHUNK received by DAEMON on connection CONN."
  (let ((record (nelisp-service-daemon--connection daemon conn)))
    (nelisp-service-put daemon :last-activity (float-time))
    (dolist (message (nelisp-service-reader-feed
                      (plist-get (cdr record) :reader) chunk))
      (nelisp-service-daemon--on-message daemon conn record message))))

(defun nelisp-service-daemon--reject (daemon conn reason)
  "Refuse CONN for REASON and close it."
  (nelisp-service-incf daemon :rejected)
  (nelisp-service-daemon--send
   conn (list 'reject reason (nelisp-service-get daemon :version)))
  (delete-process conn)
  (nelisp-service-daemon--forget daemon conn))

(defun nelisp-service-daemon--on-message (daemon conn record message)
  "Act on MESSAGE from CONN, whose connection RECORD DAEMON keeps."
  (cond
   ((not (consp message)) nil)
   ((eq (car message) 'hello)
    (cond
     ((not (equal (nth 1 message) (nelisp-service-get daemon :token)))
      (nelisp-service-daemon--reject daemon conn 'token))
     ;; Another version.  Only a client that started after this daemon
     ;; can carry newer code, so only it may replace the daemon.  An
     ;; older client -- a session left running across an update, or one
     ;; whose hello carries no start time -- is served instead: rejecting
     ;; it would make it start a daemon of the current version, which it
     ;; would reject again, restarting daemons forever.
     ((and (not (equal (nth 2 message) (nelisp-service-get daemon :version)))
           (numberp (nth 3 message))
           (> (nth 3 message) (nelisp-service-get daemon :started)))
      (nelisp-service-daemon--reject daemon conn 'version)
      (when (nelisp-service-get daemon :replace-on-mismatch)
        (nelisp-service-daemon-stop daemon)))
     (t
      (setcdr record (plist-put (cdr record) :authenticated t))
      (nelisp-service-daemon--send
       conn (list 'welcome (nelisp-service-get daemon :version))))))
   ((not (plist-get (cdr record) :authenticated))
    (nelisp-service-daemon--reject daemon conn 'unauthenticated))
   ((eq (car message) 'req)
    (let ((id (nth 1 message))
          (answered nil))
      (nelisp-service-incf daemon :served)
      (condition-case err
          (funcall (nelisp-service-get daemon :handler)
                   conn (nth 2 message)
                   (lambda (payload)
                     (unless answered
                       (setq answered t)
                       (nelisp-service-daemon--send conn (list 'res id payload)))))
        (error
         (unless answered
           (setq answered t)
           (nelisp-service-daemon--send
            conn (list 'err id (error-message-string err))))))))
   ((equal message '(ping))
    (nelisp-service-daemon--send
     conn (list 'pong (nelisp-service-daemon-stats daemon))))
   ((equal message '(shutdown))
    (nelisp-service-daemon--send conn '(bye))
    (nelisp-service-daemon-stop daemon))))

;;; Running ---------------------------------------------------------------

(defun nelisp-service-daemon-stats (daemon)
  "Return a plist describing DAEMON."
  (list :name (nelisp-service-get daemon :name)
        :port (nelisp-service-get daemon :port)
        :version (nelisp-service-get daemon :version)
        :connections (length (nelisp-service-get daemon :connections))
        :served (nelisp-service-get daemon :served)
        :rejected (nelisp-service-get daemon :rejected)))

(defun nelisp-service-daemon-stop (daemon)
  "Make `nelisp-service-daemon-run' return after the current event."
  (nelisp-service-put daemon :stopped t))

(defun nelisp-service-daemon--idle-p (daemon)
  "Return non-nil when DAEMON has been without connections long enough."
  (let ((limit (nelisp-service-get daemon :idle-timeout)))
    (and limit
         (null (nelisp-service-get daemon :connections))
         (> (- (float-time) (nelisp-service-get daemon :last-activity)) limit))))

(defun nelisp-service-daemon-run (daemon &optional poll)
  "Serve DAEMON until it stops, then release its port and files.
POLL is the event-loop interval in seconds (default 0.5).  Return
the reason: `stopped' or `idle'."
  (let ((reason nil))
    (while (not reason)
      (accept-process-output nil (or poll 0.5))
      (cond
       ((nelisp-service-get daemon :stopped) (setq reason 'stopped))
       ((nelisp-service-daemon--idle-p daemon) (setq reason 'idle))))
    ;; Let replies queued by the last handler reach their clients.
    (accept-process-output nil 0.05)
    (nelisp-service-daemon-close daemon)
    reason))

(defun nelisp-service-daemon-close (daemon)
  "Close DAEMON's sockets and remove its state and lock files."
  (let ((name (nelisp-service-get daemon :name)))
    (nelisp-service-delete-file (nelisp-service-state-file name "state"))
    (dolist (c (nelisp-service-get daemon :connections))
      (condition-case nil (delete-process (car c)) (error nil)))
    (nelisp-service-put daemon :connections nil)
    (condition-case nil (delete-process (nelisp-service-get daemon :server))
      (error nil))
    (nelisp-service-delete-file (nelisp-service-state-file name "lock"))
    t))

(provide 'nelisp-service-daemon)

;;; nelisp-service-daemon.el ends here
