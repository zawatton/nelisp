;;; nelisp-service-worker.el --- Bounded pool of NeLisp worker processes -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 213 §4.2.  A pool owns at most :max-workers child processes that
;; speak the protocol in `nelisp-service-worker-child'.  It
;;
;;   - queues requests beyond the ceiling instead of spawning more,
;;   - reuses a worker for many requests (a NeLisp start costs seconds),
;;   - retires a worker after :recycle-after requests, which bounds the
;;     memory growth of a long-lived reader,
;;   - replaces a worker that dies; only the request in flight on it fails,
;;   - gives up spawning after three workers in a row die before becoming
;;     ready, failing the queued requests instead of looping,
;;   - drains and replaces workers when `nelisp-service-pool-set-version'
;;     changes the version (a rebuilt binary, a reloaded product).
;;
;; Workers default to the running executable, so a pool created on the
;; standalone reader spawns NeLisp children and one created on host Emacs
;; spawns Emacs children.  Children exit at end of file on stdin, so a
;; pool that dies does not leave them behind.
;;
;; Asynchronous use:  (nelisp-service-pool-submit POOL FORM CALLBACK)
;; where CALLBACK receives (STATUS VALUE), STATUS one of `ok', `error',
;; `crashed', `spawn-failed', `shutdown'.  Synchronous use:
;; (nelisp-service-pool-call POOL FORM TIMEOUT).

;;; Code:

(require 'nelisp-service)

(defconst nelisp-service-pool--main-file
  (expand-file-name "../bin/nelisp-service-worker-main.el"
                    (file-name-directory
                     (or load-file-name
                         (and (boundp 'byte-compile-current-file)
                              byte-compile-current-file)
                         default-directory)))
  "Entry script that starts a worker child.")

(defconst nelisp-service-pool--max-spawn-failures 3
  "Consecutive workers dying before ready that stop further spawning.")

(defun nelisp-service-pool-default-command ()
  "Return the command that starts a worker on the current substrate."
  (if (nelisp-service-standalone-p)
      (list (nelisp-service-self-command) "--load"
            nelisp-service-pool--main-file)
    (list (nelisp-service-self-command) "--batch" "-Q"
          "-l" nelisp-service-pool--main-file)))

(defun nelisp-service-pool-create (&rest args)
  "Create a worker pool.  ARGS is a plist:
:name           string used in process names (default \"nelisp\")
:command        worker command list (default
                `nelisp-service-pool-default-command')
:max-workers    ceiling on live workers (default 2)
:recycle-after  retire a worker after this many requests (nil: never)
:init-forms     forms evaluated in each new worker before it serves
:version        any value; see `nelisp-service-pool-set-version'
Workers start lazily, on the first request that needs one."
  (nelisp-service-record
   'nelisp-service-pool
   :name (or (plist-get args :name) "nelisp")
   :command (or (plist-get args :command)
                (nelisp-service-pool-default-command))
   :max-workers (or (plist-get args :max-workers) 2)
   :recycle-after (plist-get args :recycle-after)
   :init-forms (plist-get args :init-forms)
   :version (plist-get args :version)
   :workers nil :queue nil
   :requests (make-hash-table :test 'eql)
   :next-id 0 :worker-seq 0 :spawn-failures 0 :shut nil
   :spawned 0 :retired 0 :crashed 0 :completed 0))

;;; Workers ---------------------------------------------------------------

(defun nelisp-service-pool--send (worker object)
  "Send OBJECT to WORKER's stdin."
  (process-send-string (nelisp-service-get worker :process)
                       (nelisp-service-encode object)))

(defun nelisp-service-pool--spawn (pool)
  "Start one worker for POOL and return it."
  (let* ((seq (nelisp-service-incf pool :worker-seq))
         (worker (nelisp-service-record
                  'nelisp-service-worker
                  :seq seq :ready nil :busy nil :retiring nil :served 0
                  :version (nelisp-service-get pool :version)
                  :splitter (nelisp-service-splitter-create)))
         (args (list :name (format "%s-worker-%d"
                                   (nelisp-service-get pool :name) seq)
                     :command (nelisp-service-get pool :command)
                     :connection-type 'pipe
                     :noquery t
                     :filter (lambda (_proc chunk)
                               (nelisp-service-pool--on-output
                                pool worker chunk))
                     :sentinel (lambda (proc _event)
                                 (nelisp-service-pool--on-exit
                                  pool worker proc)))))
    (unless (nelisp-service-standalone-p)
      (setq args (append args (list :coding 'utf-8))))
    (nelisp-service-put worker :process (apply #'make-process args))
    (nelisp-service-incf pool :spawned)
    (nelisp-service-put pool :workers
                        (append (nelisp-service-get pool :workers)
                                (list worker)))
    worker))

(defun nelisp-service-pool--retire (pool worker)
  "Ask WORKER to exit; POOL forgets it when the process ends."
  (unless (nelisp-service-get worker :retiring)
    (nelisp-service-put worker :retiring t)
    (nelisp-service-incf pool :retired)
    (let ((proc (nelisp-service-get worker :process)))
      (when (process-live-p proc)
        (condition-case nil
            (progn
              (nelisp-service-pool--send worker '(quit))
              (process-send-eof proc))
          (error nil))))))

(defun nelisp-service-pool--stale-p (pool worker)
  "Return non-nil when WORKER should not take another request."
  (or (not (equal (nelisp-service-get worker :version)
                  (nelisp-service-get pool :version)))
      (let ((limit (nelisp-service-get pool :recycle-after)))
        (and limit (>= (nelisp-service-get worker :served) limit)))))

(defun nelisp-service-pool--on-output (pool worker chunk)
  "Handle CHUNK of stdout from WORKER in POOL."
  (dolist (line (nelisp-service-splitter-feed
                 (nelisp-service-get worker :splitter) chunk))
    (let ((message (nelisp-service-decode line)))
      (cond
       ((equal message '(ready))
        (let ((init (nelisp-service-get pool :init-forms)))
          (if init
              (progn
                (nelisp-service-put worker :busy 'init)
                (nelisp-service-pool--send
                 worker (list 'req 'init (cons 'progn init))))
            (nelisp-service-put worker :ready t))))
       ((and (consp message) (eq (car message) 'res)
             (eq (nth 1 message) 'init))
        (nelisp-service-put worker :busy nil)
        (if (eq (nth 2 message) 'ok)
            (nelisp-service-put worker :ready t)
          ;; A worker that cannot initialise is a failed start, not a
          ;; recycled one: count it, keep the reason for the caller.
          (nelisp-service-put worker :init-error (nth 3 message))
          (nelisp-service-put pool :last-start-error (nth 3 message))
          (nelisp-service-incf pool :spawn-failures)
          (nelisp-service-put worker :retiring t)
          (let ((proc (nelisp-service-get worker :process)))
            (condition-case nil
                (progn (nelisp-service-pool--send worker '(quit))
                       (process-send-eof proc))
              (error nil)))))
       ((and (consp message) (eq (car message) 'res))
        (let* ((id (nth 1 message))
               (requests (nelisp-service-get pool :requests))
               (request (gethash id requests)))
          (remhash id requests)
          (nelisp-service-put worker :busy nil)
          (nelisp-service-incf worker :served)
          (nelisp-service-incf pool :completed)
          (when (nelisp-service-pool--stale-p pool worker)
            (nelisp-service-pool--retire pool worker))
          (when request
            (funcall (cdr request) (nth 2 message) (nth 3 message))))))))
  (nelisp-service-pool--dispatch pool))

(defun nelisp-service-pool--on-exit (pool worker proc)
  "Handle the end of WORKER's process PROC in POOL."
  (unless (process-live-p proc)
    (when (memq worker (nelisp-service-get pool :workers))
      (nelisp-service-put pool :workers
                          (delq worker (nelisp-service-get pool :workers)))
      (cond
       ((nelisp-service-get worker :init-error))
       ((nelisp-service-get worker :retiring)
        (nelisp-service-put pool :spawn-failures 0))
       ((not (nelisp-service-get worker :ready))
        (nelisp-service-incf pool :spawn-failures))
       (t (nelisp-service-put pool :spawn-failures 0)))
      (unless (or (nelisp-service-get worker :retiring)
                  (nelisp-service-get worker :ready))
        (nelisp-service-put pool :last-start-error
                            (or (nelisp-service-get pool :last-start-error)
                                "worker exited before becoming ready")))
      (unless (nelisp-service-get worker :retiring)
        (nelisp-service-incf pool :crashed))
      (let ((busy (nelisp-service-get worker :busy)))
        (when (and busy (not (eq busy 'init)))
          (let ((request (gethash busy (nelisp-service-get pool :requests))))
            (remhash busy (nelisp-service-get pool :requests))
            (when request
              (funcall (cdr request) 'crashed
                       (format "worker %d exited"
                               (nelisp-service-get worker :seq)))))))
      (nelisp-service-pool--dispatch pool))))

;;; Dispatch --------------------------------------------------------------

(defun nelisp-service-pool--idle-worker (pool)
  "Return a ready, idle, current worker of POOL, or nil."
  (let ((found nil))
    (dolist (w (nelisp-service-get pool :workers))
      (when (and (not found)
                 (nelisp-service-get w :ready)
                 (not (nelisp-service-get w :busy))
                 (not (nelisp-service-get w :retiring))
                 (not (nelisp-service-pool--stale-p pool w)))
        (setq found w)))
    found))

(defun nelisp-service-pool--count (pool predicate)
  "Count POOL's non-retiring workers satisfying PREDICATE."
  (let ((n 0))
    (dolist (w (nelisp-service-get pool :workers))
      (when (and (not (nelisp-service-get w :retiring)) (funcall predicate w))
        (setq n (1+ n))))
    n))

(defun nelisp-service-pool--fail-queue (pool status message)
  "Fail every queued request of POOL with STATUS and MESSAGE."
  (let ((queue (nelisp-service-get pool :queue)))
    (nelisp-service-put pool :queue nil)
    (dolist (entry queue)
      (remhash (car entry) (nelisp-service-get pool :requests))
      (funcall (nth 2 entry) status message))))

(defun nelisp-service-pool--dispatch (pool)
  "Assign queued requests of POOL to workers, spawning within the ceiling."
  (unless (nelisp-service-get pool :shut)
    ;; Retire idle workers made stale by a version change.
    (dolist (w (nelisp-service-get pool :workers))
      (when (and (nelisp-service-get w :ready)
                 (not (nelisp-service-get w :busy))
                 (nelisp-service-pool--stale-p pool w))
        (nelisp-service-pool--retire pool w)))
    (let ((worker nil))
      (while (and (nelisp-service-get pool :queue)
                  (setq worker (nelisp-service-pool--idle-worker pool)))
        (let ((entry (car (nelisp-service-get pool :queue))))
          (nelisp-service-put pool :queue
                              (cdr (nelisp-service-get pool :queue)))
          (nelisp-service-put worker :busy (car entry))
          (nelisp-service-pool--send
           worker (list 'req (car entry) (nth 1 entry))))))
    (if (>= (nelisp-service-get pool :spawn-failures)
            nelisp-service-pool--max-spawn-failures)
        (progn
          (nelisp-service-put pool :spawn-failures 0)
          (nelisp-service-pool--fail-queue
           pool 'spawn-failed
           (format "workers failed to start: %s"
                   (or (nelisp-service-get pool :last-start-error) "unknown")))
          (nelisp-service-put pool :last-start-error nil))
      (let ((starting (nelisp-service-pool--count
                       pool (lambda (w) (not (nelisp-service-get w :ready)))))
            ;; Retiring workers still hold memory until they exit, so the
            ;; ceiling counts them.
            (live (length (nelisp-service-get pool :workers))))
        (while (and (< starting (length (nelisp-service-get pool :queue)))
                    (< live (nelisp-service-get pool :max-workers)))
          (nelisp-service-pool--spawn pool)
          (setq starting (1+ starting) live (1+ live)))))))

;;; Public API ------------------------------------------------------------

(defun nelisp-service-pool-submit (pool form callback)
  "Queue FORM for evaluation in a worker of POOL.
CALLBACK is called with (STATUS VALUE) when the request ends.
Return the request id."
  (when (nelisp-service-get pool :shut)
    (error "nelisp-service: pool %s is shut down"
           (nelisp-service-get pool :name)))
  (let ((id (nelisp-service-incf pool :next-id)))
    (puthash id (cons form callback) (nelisp-service-get pool :requests))
    (nelisp-service-put pool :queue
                        (append (nelisp-service-get pool :queue)
                                (list (list id form callback))))
    (nelisp-service-pool--dispatch pool)
    id))

(defun nelisp-service-pool-call (pool form &optional timeout)
  "Evaluate FORM in a worker of POOL and return its value.
Signal an error when the worker signals, dies, or TIMEOUT seconds
(default 60) pass first."
  (let* ((result nil)
         (id (nelisp-service-pool-submit
              pool form (lambda (status value)
                          (setq result (cons status value))))))
    (unless (nelisp-service-wait-until (lambda () result) (or timeout 60))
      (nelisp-service-pool-cancel pool id)
      (error "nelisp-service: request %d timed out" id))
    (if (eq (car result) 'ok)
        (cdr result)
      (error "nelisp-service: %s: %s" (car result) (cdr result)))))

(defun nelisp-service-pool-cancel (pool id)
  "Drop request ID from POOL; a reply that arrives later is ignored."
  (remhash id (nelisp-service-get pool :requests))
  (nelisp-service-put pool :queue
                      (let ((kept nil))
                        (dolist (entry (nelisp-service-get pool :queue))
                          (unless (eql (car entry) id) (push entry kept)))
                        (nreverse kept))))

(defun nelisp-service-pool-set-version (pool version &optional command)
  "Set POOL's VERSION (and COMMAND); workers of another version are replaced.
Idle workers are retired at once, busy ones after their current request."
  (nelisp-service-put pool :version version)
  (when command (nelisp-service-put pool :command command))
  (nelisp-service-pool--dispatch pool))

(defun nelisp-service-pool-stats (pool)
  "Return a plist describing POOL."
  (let ((workers (nelisp-service-get pool :workers)))
    (list :workers (length workers)
          :ready (length (delq nil (mapcar (lambda (w) (nelisp-service-get w :ready))
                                           workers)))
          :busy (length (delq nil (mapcar (lambda (w) (nelisp-service-get w :busy))
                                          workers)))
          :queued (length (nelisp-service-get pool :queue))
          :spawned (nelisp-service-get pool :spawned)
          :retired (nelisp-service-get pool :retired)
          :crashed (nelisp-service-get pool :crashed)
          :completed (nelisp-service-get pool :completed))))

(defun nelisp-service-pool-processes (pool)
  "Return the live worker processes of POOL."
  (delq nil (mapcar (lambda (w)
                      (let ((p (nelisp-service-get w :process)))
                        (and (process-live-p p) p)))
                    (nelisp-service-get pool :workers))))

(defun nelisp-service-pool-shutdown (pool &optional timeout)
  "Stop POOL: fail queued requests and end every worker.
Workers get TIMEOUT seconds (default 3) to exit before being killed."
  (nelisp-service-pool--fail-queue pool 'shutdown "pool shut down")
  (nelisp-service-put pool :shut t)
  (dolist (w (nelisp-service-get pool :workers))
    (nelisp-service-pool--retire pool w))
  (nelisp-service-wait-until
   (lambda () (null (nelisp-service-pool-processes pool))) (or timeout 3))
  (dolist (p (nelisp-service-pool-processes pool))
    (delete-process p))
  t)

(provide 'nelisp-service-worker)

;;; nelisp-service-worker.el ends here
