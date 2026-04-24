;;; nelisp-worker.el --- NeLisp lane-aware worker pool -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5-D.1 per Doc 15.  Lane-aware worker pool built on top of
;; `nelisp-process-pool' (§3.1b) that adds:
;;
;;   - three lanes (read / write / batch) per §2.1 B, each backed by
;;     a `nelisp-process-pool' instance
;;   - pipe-stdio protocol per §2.7 B — the child runs
;;     `nelisp-worker-child-loop' and speaks line-delimited sexps
;;   - synchronous request/response via correlation id per §2.3 A —
;;     `nelisp-worker-call' blocks on `accept-process-output' until
;;     the reply lands or the timeout elapses
;;
;; §3.1 base scope (deliberately narrow):
;;
;;   - no classifier (§3.2 will add regex dispatch);
;;     `nelisp-worker-call' accepts `:lane' or defaults to `:write'
;;   - no health check (§3.3)
;;   - no batch warmup (§3.4)
;;   - no latency metrics (§3.5)
;;   - no actor-context call surface; the sync wait is host Elisp
;;     polling.  Actor-friendly variant is follow-up once Phase 4
;;     channel integration stabilises under pipe traffic.
;;
;; The child script path defaults to the bundled
;; `src/nelisp-worker-child.el' relative to this file.  Callers can
;; override via `:command' to plug in a different REPL loop (e.g.
;; an anvil-aware child in Phase 5-E).

;;; Code:

(require 'cl-lib)
(require 'nelisp-process)
(require 'nelisp-process-pool)

(defgroup nelisp-worker nil
  "Lane-aware NeLisp worker pool."
  :group 'nelisp)

(defcustom nelisp-worker-emacs-bin
  (or (executable-find "emacs") "emacs")
  "Path to the Emacs binary used to spawn worker children."
  :type 'string
  :group 'nelisp-worker)

(defcustom nelisp-worker-call-timeout 60.0
  "Default timeout (seconds) for `nelisp-worker-call'.
A caller may override this per-call via `:timeout'."
  :type 'number
  :group 'nelisp-worker)

(defconst nelisp-worker--lanes '(:read :write :batch)
  "All lane keywords known to this module.  Order is stable.")

(defvar nelisp-worker--child-script
  (expand-file-name
   "nelisp-worker-child.el"
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Absolute path to the bundled child loop script.")

;;; Struct -----------------------------------------------------------

(cl-defstruct (nelisp-worker-pool
               (:constructor nelisp-worker-pool--make)
               (:copier nil))
  name
  read-pool
  write-pool
  batch-pool
  (correlation-counter 0)
  (pending (make-hash-table :test 'equal))
  (line-buffers (make-hash-table :test 'eq)))

(defun nelisp-worker--default-command (&optional script-path)
  "Return the command list for spawning a worker child.
SCRIPT-PATH defaults to the bundled child script."
  (list nelisp-worker-emacs-bin
        "--batch" "-Q"
        "-l" (or script-path nelisp-worker--child-script)
        "-f" "nelisp-worker-child-loop"))

;;; Spawn / tear-down ------------------------------------------------

(defun nelisp-worker--make-filter (wrap-pool)
  "Return a filter closure that routes reply lines to the POOL's
correlation table.  WRAP-POOL is the owning `nelisp-worker-pool'."
  (lambda (wrap chunk)
    (let* ((host-proc (nelisp-process-host-proc wrap))
           (prev (gethash host-proc
                          (nelisp-worker-pool-line-buffers wrap-pool) ""))
           (combined (concat prev chunk))
           (lines (split-string combined "\n"))
           (tail (car (last lines)))
           (complete (butlast lines)))
      (puthash host-proc tail
               (nelisp-worker-pool-line-buffers wrap-pool))
      (dolist (line complete)
        (unless (string-empty-p line)
          (nelisp-worker--deliver wrap-pool line))))))

(defun nelisp-worker--deliver (pool line)
  "Parse LINE as (ID TAG RESULT) and set the pending entry."
  (let ((parsed (condition-case _
                    (car (read-from-string line))
                  (error nil))))
    (when (and (listp parsed) (>= (length parsed) 2)
               (stringp (car parsed)))
      (let* ((id (nth 0 parsed))
             (tag (nth 1 parsed))
             (result (nth 2 parsed))
             (entry (gethash id (nelisp-worker-pool-pending pool))))
        (when entry
          (setf (plist-get entry :tag) tag)
          (setf (plist-get entry :result) result)
          (setf (plist-get entry :done) t)
          (puthash id entry (nelisp-worker-pool-pending pool)))))))

(cl-defun nelisp-worker-pool-create (name &key
                                          (read-size 2)
                                          (write-size 1)
                                          (batch-size 1)
                                          command
                                          (prespawn t))
  "Create a 3-lane worker pool named NAME and return it.

Arguments:
  READ-SIZE / WRITE-SIZE / BATCH-SIZE  Per-lane max worker count.
  COMMAND      Optional override for the spawn command.  Default
               uses the bundled child loop.
  PRESPAWN     Non-nil eagerly spawns every lane; nil defers per
               `nelisp-process-pool' semantics."
  (let* ((cmd (or command (nelisp-worker--default-command)))
         (pool (nelisp-worker-pool--make :name (format "%s" name)))
         (mk (lambda (lane size)
               (nelisp-process-pool-create
                (format "%s-%s" name lane)
                :command cmd
                :size size
                :connection-type 'pipe
                :coding 'utf-8
                :prespawn prespawn))))
    (setf (nelisp-worker-pool-read-pool pool) (funcall mk :read read-size))
    (setf (nelisp-worker-pool-write-pool pool) (funcall mk :write write-size))
    (setf (nelisp-worker-pool-batch-pool pool) (funcall mk :batch batch-size))
    ;; Reinstall a pool-aware filter on every spawned worker so replies
    ;; land in this pool's correlation table.
    (let ((filter (nelisp-worker--make-filter pool)))
      (dolist (p (list (nelisp-worker-pool-read-pool pool)
                       (nelisp-worker-pool-write-pool pool)
                       (nelisp-worker-pool-batch-pool pool)))
        (dolist (w (nelisp-process-pool-workers p))
          (setf (nelisp-process-user-filter
                 (nelisp-process-pool-worker-process w))
                filter))))
    pool))

(defun nelisp-worker-pool-kill (pool)
  "Terminate every worker in POOL and clear pending state."
  (dolist (p (list (nelisp-worker-pool-read-pool pool)
                   (nelisp-worker-pool-write-pool pool)
                   (nelisp-worker-pool-batch-pool pool)))
    (ignore-errors (nelisp-process-pool-kill p)))
  (clrhash (nelisp-worker-pool-pending pool))
  (clrhash (nelisp-worker-pool-line-buffers pool))
  nil)

;;; Lane selection ---------------------------------------------------

(defun nelisp-worker--lane-pool (pool lane)
  (pcase lane
    (:read  (nelisp-worker-pool-read-pool pool))
    (:write (nelisp-worker-pool-write-pool pool))
    (:batch (nelisp-worker-pool-batch-pool pool))
    (_ (error "nelisp-worker: unknown lane %S" lane))))

;;; Correlation id ---------------------------------------------------

(defun nelisp-worker--next-id (pool)
  (let ((n (1+ (nelisp-worker-pool-correlation-counter pool))))
    (setf (nelisp-worker-pool-correlation-counter pool) n)
    (format "nw-%d-%d"
            (random 1000000)
            n)))

;;; Request / response ----------------------------------------------

(cl-defun nelisp-worker-call (pool expression &key
                                              (lane :write)
                                              timeout)
  "Evaluate EXPRESSION inside a POOL worker on LANE and return the
result value.  Blocks the current host thread up to TIMEOUT
seconds (default `nelisp-worker-call-timeout').  Signals
`nelisp-worker-timeout' on wedge or `nelisp-worker-error' on the
child's `:error' reply."
  (let* ((lane-pool (nelisp-worker--lane-pool pool lane))
         (worker (nelisp-process-pool-get lane-pool)))
    (unless worker
      (signal 'nelisp-worker-busy (list lane)))
    (let* ((wrap (nelisp-process-pool-worker-process worker))
           (id (nelisp-worker--next-id pool))
           (entry (list :done nil :tag nil :result nil))
           (deadline (+ (float-time)
                        (or timeout nelisp-worker-call-timeout)))
           (request (cons id expression)))
      (puthash id entry (nelisp-worker-pool-pending pool))
      (unwind-protect
          (progn
            (nelisp-process-send-string
             wrap (concat (prin1-to-string request) "\n"))
            (while (and (not (plist-get entry :done))
                        (< (float-time) deadline))
              (accept-process-output
               (nelisp-process-host-proc wrap) 0.05))
            (cond
             ((not (plist-get entry :done))
              (signal 'nelisp-worker-timeout
                      (list :id id :lane lane :expression expression)))
             ((eq (plist-get entry :tag) :ok)
              (plist-get entry :result))
             (t
              (signal 'nelisp-worker-error
                      (list :id id
                            :lane lane
                            :message (plist-get entry :result))))))
        (remhash id (nelisp-worker-pool-pending pool))
        (nelisp-process-pool-return lane-pool worker)))))

(define-error 'nelisp-worker-error
  "NeLisp worker child reported an evaluation error")

(define-error 'nelisp-worker-timeout
  "NeLisp worker call exceeded its timeout")

(define-error 'nelisp-worker-busy
  "NeLisp worker lane is saturated")

;;; Diagnostics ------------------------------------------------------

(defun nelisp-worker-pool-stats (pool)
  "Return a plist with per-lane `nelisp-process-pool-stats' snapshots."
  (list :read  (nelisp-process-pool-stats (nelisp-worker-pool-read-pool pool))
        :write (nelisp-process-pool-stats (nelisp-worker-pool-write-pool pool))
        :batch (nelisp-process-pool-stats (nelisp-worker-pool-batch-pool pool))
        :pending (hash-table-count (nelisp-worker-pool-pending pool))))

(provide 'nelisp-worker)
;;; nelisp-worker.el ends here
