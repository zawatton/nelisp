;;; w32-reconnect-probe.el --- Raw TCP reconnect ordering probe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercise loopback TCP close/reconnect ordering without loading service code.
;; Each cycle runs three controls: same process name with immediate reconnect,
;; distinct process names with immediate reconnect, and the same-name reconnect
;; only after the listener observed the prior socket's terminal close event.

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
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output process 0.02))
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
         (base-name (format "w32-probe-%s-%d" mode-name cycle))
         (old-process-name (if same-name base-name (concat base-name "-old")))
         (new-process-name (if same-name base-name (concat base-name "-new")))
         (old-context (list :cycle cycle :mode mode-name :role "old"))
         (new-context (list :cycle cycle :mode mode-name :role "new"))
         (old nil) (new nil) (reason nil) (failure-context nil))
    (w32-probe--bump counts mode :attempted)
    (unwind-protect
        (condition-case err
            (progn
              (setq old (w32-probe--client server port old-context old-process-name "OLD"))
              (unless (equal (process-get old 'w32-probe-reply) "WELCOME")
                (error "old response was %S" (process-get old 'w32-probe-reply)))
              (w32-probe--bump counts mode :old-welcome)
              (delete-process old)
              (when (eq mode 'same-name-after-server-close)
                (unless (w32-probe--wait-close old server 2.0)
                  (error "old server close not observed before reconnect"))
                (w32-probe--bump counts mode :close-observed))
              (setq new (w32-probe--client server port new-context new-process-name "NEW"))
              (unless (equal (process-get new 'w32-probe-reply) "REJECT")
                (error "new response was %S" (process-get new 'w32-probe-reply)))
              (w32-probe--bump counts mode :new-reject)
              (w32-probe--bump counts mode :completed)
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
      (when new (ignore-errors (w32-probe--wait-close new server 1.0)))
      (when old (ignore-errors (w32-probe--wait-close old server 1.0))))))

(defun w32-probe--summary (directory cycles counts failures)
  (let ((file (expand-file-name "w32-probe-summary.log" directory)))
    (with-temp-file file
      (insert (format "cycles-per-mode=%d total-incomplete=%d\n" cycles failures))
      (dolist (mode '(same-name-immediate unique-name-immediate
                      same-name-after-server-close))
        (let ((stats (or (gethash mode counts) (w32-probe--stats))))
          (insert (format "mode=%s attempted=%d completed=%d incomplete=%d old-welcome=%d new-reject=%d close-observed=%d\n"
                          mode (plist-get stats :attempted) (plist-get stats :completed)
                          (plist-get stats :incomplete) (plist-get stats :old-welcome)
                          (plist-get stats :new-reject) (plist-get stats :close-observed))))))
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
    (make-directory default-directory t)
    (let* ((server (make-network-process :name "w32-probe-listener" :server t
                                         :host "127.0.0.1" :service 0 :family 'ipv4
                                         :coding 'binary :noquery t
                                         :log #'w32-probe--server-log))
           (port (plist-get (process-contact server t) :service)))
      (unwind-protect
          (progn
            (unless port (error "listener did not report an ephemeral port"))
            (princ (format "W32_PROBE_START emacs=%s cycles=%d port=%s\n"
                           emacs-version cycles port))
            (dotimes (cycle-index cycles)
              (let* ((cycle (1+ cycle-index))
                     (rotation (% cycle 3))
                     (ordered (append (nthcdr rotation modes)
                                      (cl-subseq modes 0 rotation))))
                (dolist (mode ordered)
                  (unless (w32-probe--run-one server port default-directory counts cycle mode)
                    (setq failures (1+ failures))))
                (when (= (% cycle 10) 0)
                  (princ (format "W32_PROBE_PROGRESS cycles=%d/%d incomplete=%d\n"
                                 cycle cycles failures)))))
            (let ((summary (w32-probe--summary default-directory cycles counts failures)))
              (princ (format "W32_PROBE_DONE incomplete=%d summary=%s\n" failures summary)))
            (when (> failures 0) (kill-emacs 1)))
        (delete-process server)))))

(condition-case err
    (w32-probe-main)
  (error
   (princ (format "W32_PROBE_FATAL %s\n" (error-message-string err)))
   (kill-emacs 1)))

;;; w32-reconnect-probe.el ends here
