;;; w32-reconnect-probe.el --- Raw TCP reconnect ordering probe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercise loopback TCP close/reconnect ordering without loading service code.
;; Default cycles run the three reconnect controls.  Opt-in mode sets run the
;; paired EOF controls or compare a retained listener with listener churn.

;;; Code:

(require 'cl-lib)

(defconst w32-probe--event-limit 128)
(defconst w32-probe--event-length 1024)
(defvar w32-probe--events nil)
(defvar w32-probe--event-sequence 0)
(defvar w32-probe--event-dropped 0)
(defvar w32-probe--next-id 0)
(defvar w32-probe--pending-context nil)
(defvar w32-probe--accepted nil)
(defvar w32-probe--clients-by-local (make-hash-table :test #'equal))
(defvar w32-probe--connections (make-hash-table :test #'eql))
(defvar w32-probe--closed (make-hash-table :test #'eql))
(defvar w32-probe--listener-profile nil)

(defun w32-probe--endpoint-key (endpoint)
  (format "%S" endpoint))

(defun w32-probe--context (process)
  (process-get process 'w32-probe-context))

(defun w32-probe--record (context id event process detail)
  (let* ((contact (and (processp process) (process-contact process t)))
         (local (or (plist-get contact :local) (plist-get contact :service) "?"))
         (remote (or (plist-get contact :remote) "?"))
         (status (if (processp process) (process-status process) 'none))
         (line (format "seq=%d time=%.6f cycle=%s mode=%s conn=%s event=%s local=%S remote=%S status=%s detail=%S"
                       (setq w32-probe--event-sequence (1+ w32-probe--event-sequence))
                       (float-time) (or (plist-get context :cycle) "?")
                       (or (plist-get context :mode) "?") (or id "-") event
                       local remote status detail)))
    (setq line (if (> (length line) w32-probe--event-length)
                   (concat (substring line 0 (- w32-probe--event-length 3)) "...")
                 line))
    (if (< (length w32-probe--events) w32-probe--event-limit)
        (setq w32-probe--events (append w32-probe--events (list line)))
      (setq w32-probe--events (append (cdr w32-probe--events) (list line))
            w32-probe--event-dropped (1+ w32-probe--event-dropped)))))

(defun w32-probe--reset-events ()
  (setq w32-probe--events nil w32-probe--event-sequence 0 w32-probe--event-dropped 0))

(defun w32-probe--flush-failure (directory context reason)
  (let ((file (expand-file-name
               (format "w32-probe-failure-c%s-%s.log"
                       (plist-get context :cycle) (plist-get context :mode)) directory)))
    (with-temp-file file
      (insert (format "FAIL context=%S reason=%s retained=%d dropped=%d\n"
                      context reason (length w32-probe--events) w32-probe--event-dropped))
      (dolist (event w32-probe--events) (insert event "\n")))
    file))

(defun w32-probe--link-client (client)
  (let* ((local (plist-get (process-contact client t) :local))
         (key (w32-probe--endpoint-key local)))
    (puthash key client w32-probe--clients-by-local)
    ;; Accept callbacks can run during connect on some hosts, before the
    ;; client process has returned. Link either arrival order by socket tuple.
    (dolist (server-client w32-probe--accepted)
      (when (equal key (process-get server-client 'w32-probe-remote-key))
        (process-put client 'w32-probe-id (process-get server-client 'w32-probe-id))
        (process-put server-client 'w32-probe-client client)))
    key))

(defun w32-probe--server-log (_server client _message)
  (let* ((contact (process-contact client t))
         (remote (plist-get contact :remote))
         (client-process (gethash (w32-probe--endpoint-key remote)
                                  w32-probe--clients-by-local))
         (context (copy-sequence
                   (or (and client-process (w32-probe--context client-process))
                       w32-probe--pending-context)))
         (id (plist-get context :conn-id)))
    (process-put client 'w32-probe-id id)
    (puthash id client w32-probe--connections)
    (process-put client 'w32-probe-context context)
    (process-put client 'w32-probe-remote-key (w32-probe--endpoint-key remote))
    (process-put client 'w32-probe-input "")
    (when client-process
      (process-put client-process 'w32-probe-id id)
      (process-put client 'w32-probe-client client-process))
    (set-process-filter client #'w32-probe--server-filter)
    (set-process-sentinel client #'w32-probe--server-sentinel)
    (push client w32-probe--accepted)
    (w32-probe--record context id "accept-callback" client contact)))

(defun w32-probe--make-listener (generation)
  (let ((process
         (make-network-process
          :name "w32-probe-listener"
          :server t :host "127.0.0.1" :service 0 :family 'ipv4
          :coding 'binary :noquery t :log #'w32-probe--server-log)))
    (process-put process 'w32-probe-generation generation)
    process))

(defun w32-probe--server-filter (process bytes)
  (let* ((context (w32-probe--context process))
         (id (process-get process 'w32-probe-id))
         (input (concat (or (process-get process 'w32-probe-input) "") bytes))
         (newline (string-match "\n" input)))
    (process-put process 'w32-probe-input (if newline (substring input (1+ newline)) input))
    (when newline
      (let* ((line (substring input 0 newline))
             (reply (cond ((equal line "OLD") "WELCOME\n")
                          ((equal line "NEW") "REJECT\n")
                          (t "REJECT\n"))))
        (w32-probe--record context id "input" process line)
        (w32-probe--record context id "send" process (string-trim reply))
        ;; This probe server never closes a connection before replying.
        (process-send-string process reply)))))

(defun w32-probe--server-sentinel (process event)
  (let* ((status (process-status process))
         (id (process-get process 'w32-probe-id)))
    (w32-probe--record (w32-probe--context process) id "server-sentinel"
                       process (string-trim event))
    (when (and id (memq status '(closed failed exit signal)))
      (puthash id t w32-probe--closed))))

(defun w32-probe--client-filter (process bytes)
  (process-put process 'w32-probe-input
               (concat (or (process-get process 'w32-probe-input) "") bytes)))

(defun w32-probe--client-sentinel (process event)
  (w32-probe--record (w32-probe--context process)
                     (or (process-get process 'w32-probe-id)
                         (plist-get (w32-probe--context process) :conn-id))
                     "client-sentinel"
                     process (string-trim event)))

(defun w32-probe--wait-for (predicate process seconds)
  (let ((deadline (+ (float-time) seconds))
        (event-pump (or (getenv "W32_PROBE_EVENT_PUMP") "client")))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (if (equal event-pump "all")
          (accept-process-output nil 0.05)
        (accept-process-output process 0.02)))
    (funcall predicate)))

(defun w32-probe--client (server port context process-name payload)
  (let ((client nil) (complete nil)
        (context (plist-put (copy-sequence context) :conn-id
                            (setq w32-probe--next-id (1+ w32-probe--next-id)))))
    (setq w32-probe--pending-context context)
    (unwind-protect
        (progn
          (setq client (make-network-process :name process-name :host "127.0.0.1"
                                             :service port :family 'ipv4 :coding 'binary
                                             :noquery t :buffer nil :nowait nil))
          (process-put client 'w32-probe-context context)
          (process-put client 'w32-probe-input "")
          (set-process-filter client #'w32-probe--client-filter)
          (set-process-sentinel client #'w32-probe--client-sentinel)
          (w32-probe--link-client client)
          ;; Send immediately after connect; do not wait for accept or old-close.
          (process-send-string client (concat payload "\n"))
          (w32-probe--record context (or (process-get client 'w32-probe-id)
                                          (plist-get context :conn-id))
                             "client-send" client payload)
          (unless (w32-probe--wait-for
                   (lambda () (string-match "\n" (process-get client 'w32-probe-input)))
                   client 2.0)
            (error "missed reply for %s" payload))
          (let ((reply (car (split-string (process-get client 'w32-probe-input) "\n"))))
            (process-put client 'w32-probe-reply reply)
            (w32-probe--record context (or (process-get client 'w32-probe-id)
                                            (plist-get context :conn-id))
                               "client-reply" client reply))
          ;; A response followed by an immediate peer reset is not a healthy
          ;; round trip. Drain already-queued status events before accepting it.
          (accept-process-output client 0)
          (unless (process-live-p client)
            (error "peer closed after reply (status=%s)"
                   (process-status client)))
          (setq complete t)
          client)
      (setq w32-probe--pending-context nil)
      (unless complete
        (when (process-live-p client) (delete-process client))
        (when client
          (let ((id (process-get client 'w32-probe-id)))
            (let ((server-client (and id (gethash id w32-probe--connections))))
              (when server-client
                (w32-probe--wait-for (lambda () (gethash id w32-probe--closed))
                                     server-client 1.0)))))))))

(defun w32-probe--wait-close (client server seconds)
  (let ((id (process-get client 'w32-probe-id)))
    (let ((server-client (and id (gethash id w32-probe--connections))))
      (and server-client
           (w32-probe--wait-for (lambda () (gethash id w32-probe--closed))
                                server-client seconds)))))

(defun w32-probe--stats ()
  (list :attempted 0 :completed 0 :incomplete 0 :old-welcome 0
        :new-reject 0 :close-observed 0))

(defun w32-probe--bump (counts mode key)
  (let ((stats (or (gethash mode counts) (w32-probe--stats))))
    (puthash mode (plist-put stats key (1+ (or (plist-get stats key) 0))) counts)))

(defun w32-probe--run-one (server port directory counts cycle mode)
  (w32-probe--reset-events)
  (let* ((mode-name (symbol-name mode))
         (same-name (not (eq mode 'unique-name-immediate)))
         (process-mode-name
          (if (memq mode '(same-name-after-client-eof
                           same-name-after-client-eof-delete))
              "same-name-after-client-eof"
            mode-name))
         (base-name (format "w32-probe-%s-%d" process-mode-name cycle))
         (old-process-name (if same-name base-name (concat base-name "-old")))
         (new-process-name (if same-name base-name (concat base-name "-new")))
         (old-context (list :cycle cycle :mode mode-name :role "old"))
         (new-context (list :cycle cycle :mode mode-name :role "new"))
         (old nil) (old-reference nil) (old-object-id nil)
         (new nil) (reason nil) (failure-context nil)
         (body-complete nil) (new-peer-closed nil) (old-peer-closed nil)
         (cleanup-complete nil)
         (result nil))
    (w32-probe--bump counts mode :attempted)
    (setq result
          (unwind-protect
        (condition-case err
            (progn
              (setq old (w32-probe--client server port old-context old-process-name "OLD"))
              (when (memq mode '(same-name-after-client-eof
                                 same-name-after-client-eof-delete))
                (setq old-reference old
                      old-object-id (sxhash-eq old)))
              (unless (equal (process-get old 'w32-probe-reply) "WELCOME")
                (error "old response was %S" (process-get old 'w32-probe-reply)))
              (w32-probe--bump counts mode :old-welcome)
              (cond
               ((memq mode '(same-name-after-client-eof
                             same-name-after-client-eof-delete))
                (let* ((id (process-get old 'w32-probe-id))
                       (peer (and id (gethash id w32-probe--connections))))
                  (w32-probe--record old-context id "client-eof-before" old
                                     (process-status old))
                  (process-send-eof old)
                  (w32-probe--record old-context id "client-eof-immediate" old
                                     (process-status old))
                  (w32-probe--record old-context id "peer-status-after-eof" peer
                                     (and peer (process-status peer)))
                  (unless (process-live-p old)
                    (error "EOF control unsupported: client became terminal immediately"))
                  (unless (w32-probe--wait-for
                           (lambda ()
                             (memq (process-status old) '(closed failed exit signal)))
                           old 2.0)
                    (error "EOF control incomplete: old client did not become terminal"))
                  (w32-probe--record old-context id "old-client-terminal-after-eof" old
                                     (process-status old))
                  (w32-probe--record old-context id "peer-status-after-client-terminal"
                                     peer (and peer (process-status peer)))
                  (w32-probe--bump counts mode :close-observed)
                  (let ((status-before (process-status old))
                        (pid-before (process-id old)))
                    (w32-probe--record
                     old-context id "old-terminal-before-reconnect" old
                     (list :pid pid-before :object-id old-object-id
                           :same-object (eq old old-reference)))
                    (w32-probe--bump
                     counts mode (intern (format ":old-before-%s" status-before)))
                    (when (eq old old-reference)
                      (w32-probe--bump counts mode :old-same-object-before))
                    (when (eq mode 'same-name-after-client-eof-delete)
                      (delete-process old))
                    (let ((status-after (process-status old))
                          (pid-after (process-id old)))
                      (w32-probe--record
                       old-context id "old-terminal-after-optional-delete" old
                       (list :pid pid-after :object-id old-object-id
                             :same-object (eq old old-reference)))
                      (w32-probe--bump
                       counts mode (intern (format ":old-after-%s" status-after)))
                      (when (eq old old-reference)
                        (w32-probe--bump counts mode :old-same-object-after))))))
               (t
                (delete-process old)
                (when (eq mode 'same-name-after-server-close)
                  (unless (w32-probe--wait-close old server 2.0)
                    (error "old server close not observed before reconnect"))
                  (w32-probe--bump counts mode :close-observed))))
              (setq new (w32-probe--client server port new-context new-process-name "NEW"))
              (unless (equal (process-get new 'w32-probe-reply) "REJECT")
                (error "new response was %S" (process-get new 'w32-probe-reply)))
              (w32-probe--bump counts mode :new-reject)
              (w32-probe--bump counts mode :completed)
              (setq body-complete t)
              t)
          (error
           (setq reason (error-message-string err))
           (setq failure-context (if new new-context old-context))
           (w32-probe--bump counts mode :incomplete)
           (w32-probe--flush-failure directory failure-context reason)
           nil))
      ;; Close every client and wait briefly for both server terminal events.
      (when (and new (process-live-p new)) (delete-process new))
      (when (and old (process-live-p old)) (delete-process old))
      (when new
        (if w32-probe--listener-profile
            (setq new-peer-closed
                  (or (ignore-errors (w32-probe--wait-close new server 1.0))
                      (gethash (process-get new 'w32-probe-id) w32-probe--closed)))
          (ignore-errors (w32-probe--wait-close new server 1.0))))
      (when old
        (if w32-probe--listener-profile
            (setq old-peer-closed
                  (or (ignore-errors (w32-probe--wait-close old server 1.0))
                      (gethash (process-get old 'w32-probe-id) w32-probe--closed)))
          (ignore-errors (w32-probe--wait-close old server 1.0))))
      (when w32-probe--listener-profile
        (let* ((new-id (and new (process-get new 'w32-probe-id)))
               (old-id (and old (process-get old 'w32-probe-id)))
               (new-peer (and new-id (gethash new-id w32-probe--connections)))
               (old-peer (and old-id (gethash old-id w32-probe--connections))))
          (w32-probe--record new-context new-id "listener-boundary-peer-terminal"
                             new-peer (list :observed (and new-peer-closed t)
                                            :status (and new-peer (process-status new-peer))))
          (w32-probe--record old-context old-id "listener-boundary-peer-terminal"
                             old-peer (list :observed (and old-peer-closed t)
                                            :status (and old-peer (process-status old-peer))))
          (when (and old-peer-closed new-peer-closed)
            (w32-probe--bump counts mode :listener-peer-terminal-pairs))
          (setq cleanup-complete (and old-peer-closed new-peer-closed))))))
    (when (and w32-probe--listener-profile (not body-complete) reason)
      (w32-probe--flush-failure directory failure-context reason))
    (when (and w32-probe--listener-profile body-complete
               (not (and old-peer-closed new-peer-closed)))
      (let ((stats (gethash mode counts)))
        (puthash mode (plist-put stats :completed
                                 (max 0 (1- (or (plist-get stats :completed) 0))))
                 counts))
      (w32-probe--bump counts mode :incomplete)
      (setq reason "accepted peer terminal not observed at listener boundary"
            failure-context (if new new-context old-context)
            result nil)
      (w32-probe--flush-failure directory failure-context reason))
    (if w32-probe--listener-profile
        (list :transaction-complete (and body-complete t)
              :cleanup-complete (and cleanup-complete t)
              :reason reason :failure-context failure-context)
      result)))

(defun w32-probe--summary (directory cycles counts failures modes)
  (let ((file (expand-file-name "w32-probe-summary.log" directory)))
    (with-temp-file file
      (insert (format "cycles-per-mode=%d total-incomplete=%d\n" cycles failures))
      (dolist (mode modes)
        (let ((stats (or (gethash mode counts) (w32-probe--stats))))
          (insert (format "mode=%s attempted=%d completed=%d incomplete=%d old-welcome=%d new-reject=%d close-observed=%d\n"
                          mode (plist-get stats :attempted) (plist-get stats :completed)
                          (plist-get stats :incomplete) (plist-get stats :old-welcome)
                          (plist-get stats :new-reject) (plist-get stats :close-observed)))
          (when (memq mode '(same-name-after-client-eof
                             same-name-after-client-eof-delete))
            (insert (format "mode=%s edge-before-closed=%d edge-before-failed=%d edge-before-exit=%d edge-before-signal=%d edge-after-closed=%d edge-after-failed=%d edge-after-exit=%d edge-after-signal=%d same-object-before=%d same-object-after=%d\n"
                            mode (or (plist-get stats :old-before-closed) 0)
                            (or (plist-get stats :old-before-failed) 0)
                            (or (plist-get stats :old-before-exit) 0)
                            (or (plist-get stats :old-before-signal) 0)
                            (or (plist-get stats :old-after-closed) 0)
                            (or (plist-get stats :old-after-failed) 0)
                            (or (plist-get stats :old-after-exit) 0)
                            (or (plist-get stats :old-after-signal) 0)
                            (or (plist-get stats :old-same-object-before) 0)
                            (or (plist-get stats :old-same-object-after) 0))))
          (when w32-probe--listener-profile
            (insert (format "listener-arm=%s created=%d deleted=%d boundaries=%d before-listen=%d after-listen=%d after-closed=%d after-failed=%d same-object-after=%d replacements-distinct=%d replacement-port-changed=%d replacement-port-same=%d peer-terminal-pairs=%d\n"
                            (getenv "W32_PROBE_LISTENER_ARM")
                            (or (plist-get stats :listener-created) 0)
                            (or (plist-get stats :listener-deleted) 0)
                            (or (plist-get stats :listener-boundaries) 0)
                            (or (plist-get stats :listener-before-listen) 0)
                            (or (plist-get stats :listener-after-listen) 0)
                            (or (plist-get stats :listener-after-closed) 0)
                            (or (plist-get stats :listener-after-failed) 0)
                            (or (plist-get stats :listener-same-object-after) 0)
                            (or (plist-get stats :listener-replacements-distinct) 0)
                            (or (plist-get stats :listener-replacement-port-changed) 0)
                            (or (plist-get stats :listener-replacement-port-same) 0)
                            (or (plist-get stats :listener-peer-terminal-pairs) 0)))))))
    file))

(defun w32-probe--args ()
  (let ((args command-line-args-left) (cycles 200) (directory "target/service-flake"))
    (while args
      (pcase (pop args)
        ("--" nil)
        ("--cycles" (setq cycles (string-to-number (or (pop args) ""))))
        ("--artifact-dir" (setq directory (or (pop args) directory)))
        (arg (error "unknown argument %s" arg))))
    (unless (<= 1 cycles 10000) (error "cycles must be between 1 and 10000"))
    (list cycles directory)))

(defun w32-probe-main ()
  (pcase-let* ((`(,cycles ,directory) (w32-probe--args))
               (event-pump (or (getenv "W32_PROBE_EVENT_PUMP") "client"))
               (eof-control (or (getenv "W32_PROBE_EOF_CONTROL") "false"))
               (mode-set (or (getenv "W32_PROBE_MODE_SET") "default"))
               (listener-arm (getenv "W32_PROBE_LISTENER_ARM"))
               (w32-probe--listener-profile
                (equal mode-set "listener-lifetime-pair"))
               (counts (make-hash-table :test #'eq))
               (failures 0)
               (w32-probe--clients-by-local (make-hash-table :test #'equal))
               (w32-probe--connections (make-hash-table :test #'eql))
               (w32-probe--closed (make-hash-table :test #'eql))
               (w32-probe--next-id 0)
               (w32-probe--pending-context nil)
               (w32-probe--accepted nil)
               (default-directory (expand-file-name directory))
               (modes '(same-name-immediate unique-name-immediate
                        same-name-after-server-close)))
    (unless (member event-pump '("client" "all"))
      (error "W32_PROBE_EVENT_PUMP must be client or all, got %S" event-pump))
    (unless (member eof-control '("true" "false"))
      (error "W32_PROBE_EOF_CONTROL must be true or false, got %S" eof-control))
    (unless (member mode-set '("default" "eof-delete-pair" "listener-lifetime-pair"))
      (error "W32_PROBE_MODE_SET must be default, eof-delete-pair, or listener-lifetime-pair, got %S"
             mode-set))
    (cond
     ((equal mode-set "eof-delete-pair")
      (unless (equal eof-control "true")
        (error "eof-delete-pair requires W32_PROBE_EOF_CONTROL=true"))
      (setq modes '(same-name-after-client-eof
                    same-name-after-client-eof-delete)))
     (w32-probe--listener-profile
      (unless (equal eof-control "true")
        (error "listener-lifetime-pair requires W32_PROBE_EOF_CONTROL=true"))
      (unless (member listener-arm '("reference" "churn"))
        (error "W32_PROBE_LISTENER_ARM must be reference or churn, got %S"
               listener-arm))
      (setq modes '(same-name-after-client-eof-delete)))
     (t
      (when (equal eof-control "true")
        (setq modes (append modes '(same-name-after-client-eof))))))
    (princ (format "W32_PROBE_EVENT_PUMP %s\n" event-pump))
    (princ (format "W32_PROBE_EOF_CONTROL %s\n" eof-control))
    (princ (format "W32_PROBE_MODE_SET %s\n" mode-set))
    (when w32-probe--listener-profile
      (princ (format "W32_PROBE_LISTENER_ARM %s\n" listener-arm)))
    (make-directory default-directory t)
    (let* ((server (w32-probe--make-listener 0))
           (port (plist-get (process-contact server t) :service)))
      (when w32-probe--listener-profile
        (w32-probe--bump counts 'same-name-after-client-eof-delete :listener-created))
      (unwind-protect
          (progn
            (unless port (error "listener did not report an ephemeral port"))
            (princ (format "W32_PROBE_START emacs=%s cycles=%d port=%s\n"
                           emacs-version cycles port))
            (catch 'w32-probe-stop-arm
              (dotimes (cycle-index cycles)
              (let* ((cycle (1+ cycle-index))
                     (rotation (if w32-probe--listener-profile 0
                                 (% cycle (length modes))))
                     (ordered (if w32-probe--listener-profile modes
                                (append (nthcdr rotation modes)
                                        (cl-subseq modes 0 rotation)))))
                (dolist (mode ordered)
                  (let* ((outcome
                          (w32-probe--run-one server port default-directory counts cycle mode))
                         (success (if w32-probe--listener-profile
                                      (plist-get outcome :transaction-complete)
                                    outcome))
                         (cleanup-complete
                          (if w32-probe--listener-profile
                              (plist-get outcome :cleanup-complete)
                            success)))
                    (unless (if w32-probe--listener-profile
                                (and success cleanup-complete)
                              success)
                      (setq failures (1+ failures)))
                    (when (and w32-probe--listener-profile cleanup-complete)
                      (let* ((context (list :cycle cycle :mode (symbol-name mode)
                                            :role "listener"))
                             (old-server server)
                             (old-port port)
                             (old-id (sxhash-eq old-server))
                             (before-status (process-status old-server))
                             (delete-for-next
                              (and (equal listener-arm "churn") (< cycle cycles))))
                        (w32-probe--record context nil "listener-before-boundary" old-server
                                           (list :identity old-id :port old-port
                                                 :status before-status))
                        (w32-probe--bump counts mode :listener-boundaries)
                        (w32-probe--bump counts mode
                                         (intern (format ":listener-before-%s" before-status)))
                        (when delete-for-next
                          (delete-process old-server)
                          (w32-probe--bump counts mode :listener-deleted)
                          (w32-probe--bump counts mode :listener-boundary-deletes))
                        (let ((after-status (process-status old-server)))
                          (w32-probe--record context nil "listener-after-optional-delete"
                                             old-server
                                             (list :identity old-id :port old-port
                                                   :status after-status
                                                   :same-object (eq old-server server)))
                          (w32-probe--bump counts mode
                                           (intern (format ":listener-after-%s" after-status)))
                          (when (eq old-server server)
                            (w32-probe--bump counts mode :listener-same-object-after)))
                        (when delete-for-next
                          (setq server (w32-probe--make-listener cycle)
                                port (plist-get (process-contact server t) :service))
                          (unless port (error "replacement listener did not report an ephemeral port"))
                          (w32-probe--bump counts mode :listener-created)
                          (unless (eq old-server server)
                            (w32-probe--bump counts mode :listener-replacements-distinct))
                          (w32-probe--bump counts mode
                                           (if (equal old-port port)
                                               :listener-replacement-port-same
                                             :listener-replacement-port-changed))
                          (w32-probe--record context nil "listener-replacement-created" server
                                             (list :identity (sxhash-eq server) :port port
                                                   :previous-identity old-id
                                                   :previous-port old-port)))))
                    (when (and w32-probe--listener-profile
                               (not cleanup-complete))
                      (throw 'w32-probe-stop-arm t))
                    (when (and w32-probe--listener-profile
                               (not success)
                               (plist-get outcome :reason))
                      (w32-probe--flush-failure
                       default-directory (plist-get outcome :failure-context)
                       (plist-get outcome :reason))))
                (when (= (% cycle 10) 0)
                  (princ (format "W32_PROBE_PROGRESS cycles=%d/%d incomplete=%d\n"
                                 cycle cycles failures)))))))
            (when w32-probe--listener-profile
              (let* ((final-server server)
                     (status-before (process-status final-server))
                     (identity (sxhash-eq final-server)))
                (w32-probe--record (list :cycle cycles :role "listener") nil
                                   "listener-final-shutdown-before" final-server
                                   (list :identity identity :port port :status status-before))
                (delete-process final-server)
                (setq server nil)
                (w32-probe--bump counts 'same-name-after-client-eof-delete
                                 :listener-deleted)
                (w32-probe--record (list :cycle cycles :role "listener") nil
                                   "listener-final-shutdown-after" final-server
                                   (list :identity identity :port port
                                         :status (process-status final-server)))))
            (let ((summary (w32-probe--summary default-directory cycles counts failures modes)))
              (when (and (equal eof-control "true")
                         (not w32-probe--listener-profile))
                (let ((stats (gethash 'same-name-after-client-eof counts)))
                  (princ (format "W32_PROBE_EOF_MODE attempted=%d completed=%d incomplete=%d old-welcome=%d new-reject=%d client-terminal=%d\n"
                                 (plist-get stats :attempted) (plist-get stats :completed)
                                 (plist-get stats :incomplete) (plist-get stats :old-welcome)
                                 (plist-get stats :new-reject) (plist-get stats :close-observed)))))
              (when (member mode-set '("eof-delete-pair" "listener-lifetime-pair"))
                (let ((stats (gethash 'same-name-after-client-eof-delete counts)))
                  (princ (format "W32_PROBE_EOF_DELETE_MODE attempted=%d completed=%d incomplete=%d old-welcome=%d new-reject=%d client-terminal=%d\n"
                                 (plist-get stats :attempted) (plist-get stats :completed)
                                 (plist-get stats :incomplete) (plist-get stats :old-welcome)
                                 (plist-get stats :new-reject) (plist-get stats :close-observed)))))
              (when w32-probe--listener-profile
                (let ((stats (gethash 'same-name-after-client-eof-delete counts)))
                  (princ (format "W32_PROBE_LISTENER_LIFETIME arm=%s created=%d deleted=%d boundaries=%d peer-terminal-pairs=%d\n"
                                 listener-arm (or (plist-get stats :listener-created) 0)
                                 (or (plist-get stats :listener-deleted) 0)
                                 (or (plist-get stats :listener-boundaries) 0)
                                 (or (plist-get stats :listener-peer-terminal-pairs) 0)))))
              (princ (format "W32_PROBE_DONE incomplete=%d summary=%s\n" failures summary)))
            (when (> failures 0) (kill-emacs 1)))
        (when server (delete-process server))))))

(condition-case err
    (w32-probe-main)
  (error
   (princ (format "W32_PROBE_FATAL %s\n" (error-message-string err)))
   (kill-emacs 1)))

;;; w32-reconnect-probe.el ends here
