;;; nelisp-process-pool.el --- NeLisp subprocess worker pool -*- lexical-binding: t; -*-
;;
;; Phase 5-C.1b per Doc 14.  Pre-spawned worker pool built on top
;; of `nelisp-make-process'.  Port of anvil-worker's shape:
;;
;;   - fixed-size roster; workers created eagerly by default
;;   - `get' returns an idle worker (or spawns a replacement if one
;;     has died), marking it busy; `return' flips it back to idle
;;   - sentinel-driven dead marking avoids polling; `get' sweeps
;;     the roster first so a crashed worker is silently replaced
;;     before the caller receives a live one
;;   - no actor-driven event dispatch here — the pool is a plain
;;     data structure; event-loop integration is on the caller
;;     side via `:actor' on `nelisp-make-process' (see §3.1)
;;
;; Intentional MVP simplifications (Doc 14 notes):
;;   - eager synchronous spawn (not deferred via run-at-time 0).
;;     Deferred spawn is the anvil-worker startup-freeze workaround
;;     for Windows file-notify; here we expose a `:prespawn nil'
;;     switch so callers who hit that problem can lazy-spawn
;;   - no periodic idle-timeout sweep (reuse is unbounded)
;;   - no actor-protocol request/response layer — caller decides

;;; Code:

(require 'cl-lib)
(require 'nelisp-eval)
(require 'nelisp-actor)
(require 'nelisp-process)

(cl-defstruct (nelisp-process-pool
               (:constructor nelisp-process-pool--make)
               (:copier nil))
  name
  command            ; list — command vector for spawned workers
  (size 2)           ; max roster length
  (connection-type 'pipe)
  (coding 'utf-8)
  (workers nil))     ; list of nelisp-process-pool-worker

(cl-defstruct (nelisp-process-pool-worker
               (:constructor nelisp-process-pool-worker--make)
               (:copier nil))
  pool
  process
  (state 'idle)      ; idle / busy / dead
  (last-used 0))

;;; Spawn + sweep -----------------------------------------------------

(defun nelisp-process-pool--spawn-worker (pool)
  "Spawn a new worker for POOL and return it."
  (let* ((n (1+ (length (nelisp-process-pool-workers pool))))
         (worker (nelisp-process-pool-worker--make
                  :pool pool
                  :state 'idle
                  :last-used (float-time)))
         (proc (nelisp-make-process
                :name (format "%s-w%d"
                              (nelisp-process-pool-name pool) n)
                :command (nelisp-process-pool-command pool)
                :connection-type (nelisp-process-pool-connection-type pool)
                :coding (nelisp-process-pool-coding pool)
                :sentinel
                (lambda (_w _ev)
                  (setf (nelisp-process-pool-worker-state worker) 'dead)))))
    (setf (nelisp-process-pool-worker-process worker) proc)
    (push worker (nelisp-process-pool-workers pool))
    worker))

(defun nelisp-process-pool--quick-alive-p (worker)
  "Cheap health check: host proc is alive + worker state is not dead.
Mirrors anvil-worker's quick-alive-p — no probe, just `process-live-p'."
  (let ((proc (nelisp-process-pool-worker-process worker)))
    (and proc
         (nelisp-process-live-p proc)
         (not (eq (nelisp-process-pool-worker-state worker) 'dead)))))

(defun nelisp-process-pool--sweep-dead (pool)
  "Mark workers whose host proc has exited as `dead'."
  (dolist (w (nelisp-process-pool-workers pool))
    (unless (nelisp-process-pool--quick-alive-p w)
      (setf (nelisp-process-pool-worker-state w) 'dead))))

;;; Public API --------------------------------------------------------

(cl-defun nelisp-process-pool-create (name &key command (size 2)
                                            (connection-type 'pipe)
                                            (coding 'utf-8)
                                            (prespawn t))
  "Create a new process pool named NAME and return it.

Arguments:
  COMMAND          Command vector (list of strings).  Required.
  SIZE             Max worker count (default 2).
  CONNECTION-TYPE  `pipe' (default) or `pty'.
  CODING           Default `utf-8'.
  PRESPAWN         Non-nil eagerly spawns SIZE workers up front;
                   nil defers spawning until the first `pool-get'."
  (unless command
    (signal 'wrong-type-argument (list 'listp command)))
  (let ((pool (nelisp-process-pool--make
               :name (format "%s" name)
               :command command
               :size size
               :connection-type connection-type
               :coding coding)))
    (when prespawn
      (dotimes (_ size)
        (nelisp-process-pool--spawn-worker pool)))
    pool))

(defun nelisp-process-pool-get (pool)
  "Return an idle worker for POOL, spawning or replacing as needed.

Sweep dead workers first.  Prefer an existing idle slot, otherwise
spawn one if the roster is below SIZE, otherwise replace the first
dead worker.  Returns nil when no idle slot is available (all
workers are busy and the roster is full)."
  (nelisp-process-pool--sweep-dead pool)
  (let* ((workers (nelisp-process-pool-workers pool))
         (idle (cl-find 'idle workers
                        :key #'nelisp-process-pool-worker-state))
         (worker
          (cond
           (idle idle)
           ((< (length workers) (nelisp-process-pool-size pool))
            (nelisp-process-pool--spawn-worker pool))
           (t
            (let ((dead (cl-find 'dead workers
                                 :key #'nelisp-process-pool-worker-state)))
              (when dead
                (let ((old-proc (nelisp-process-pool-worker-process dead)))
                  (when (and old-proc
                             (nelisp-process-p old-proc))
                    (ignore-errors (nelisp-delete-process old-proc))))
                (setf (nelisp-process-pool-workers pool)
                      (delq dead workers))
                (nelisp-process-pool--spawn-worker pool)))))))
    (when worker
      (setf (nelisp-process-pool-worker-state worker) 'busy)
      (setf (nelisp-process-pool-worker-last-used worker) (float-time)))
    worker))

(defun nelisp-process-pool-return (_pool worker)
  "Mark WORKER idle again.  If its host proc has died in the
meantime, leave it as `dead' so the next `-get' sweeps it."
  (if (nelisp-process-pool--quick-alive-p worker)
      (setf (nelisp-process-pool-worker-state worker) 'idle)
    (setf (nelisp-process-pool-worker-state worker) 'dead))
  worker)

(defun nelisp-process-pool-kill (pool)
  "Terminate every worker of POOL and empty the roster."
  (dolist (w (nelisp-process-pool-workers pool))
    (let ((p (nelisp-process-pool-worker-process w)))
      (when (and p (nelisp-process-p p))
        (when (nelisp-process-live-p p)
          (ignore-errors (nelisp-kill-process p)))
        (ignore-errors (nelisp-delete-process p)))
      (setf (nelisp-process-pool-worker-state w) 'dead)))
  (setf (nelisp-process-pool-workers pool) nil)
  nil)

(defun nelisp-process-pool-stats (pool)
  "Return a plist summarising POOL state:
 :total / :idle / :busy / :dead / :size."
  (let ((idle 0) (busy 0) (dead 0) (total 0))
    (dolist (w (nelisp-process-pool-workers pool))
      (cl-incf total)
      (pcase (nelisp-process-pool-worker-state w)
        ('idle (cl-incf idle))
        ('busy (cl-incf busy))
        ('dead (cl-incf dead))))
    (list :total total :idle idle :busy busy :dead dead
          :size (nelisp-process-pool-size pool))))

(defun nelisp-process-pool-worker-list (pool)
  "Return a fresh copy of POOL's worker list."
  (copy-sequence (nelisp-process-pool-workers pool)))

(provide 'nelisp-process-pool)
;;; nelisp-process-pool.el ends here
