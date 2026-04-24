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

(defcustom nelisp-worker-health-check-interval 30
  "Seconds between periodic health scans (§2.4 C).
The scan walks every lane of every registered pool and marks any
worker whose host process has exited as `dead', so the next
`nelisp-worker-call' sweeps and replaces it.  A value of 0 (or
non-positive) disables the periodic scan; sentinel-driven dead
marking and on-`-get' sweep continue unchanged."
  :type 'number
  :group 'nelisp-worker)

(defcustom nelisp-worker-batch-warmup-expressions nil
  "Forms fire-and-forget-delivered to every batch-lane worker on spawn.
Mirrors `anvil-worker-batch-warmup-expressions'.  Each form is sent
over the pipe as `(ID . FORM)' with a synthetic correlation id that
we never register, so the child's `(ID :ok RESULT)' reply is
silently dropped by the parent filter.  Nil (the default) disables
warmup entirely."
  :type '(repeat sexp)
  :group 'nelisp-worker)

(defcustom nelisp-worker-batch-warmup-delay 2.0
  "Seconds to wait after spawn before sending warmup expressions.
Matches `anvil-worker-batch-warmup-delay'.  A non-positive value
delivers warmup synchronously during `pool-create'."
  :type 'number
  :group 'nelisp-worker)

(defconst nelisp-worker--lanes '(:read :write :batch)
  "All lane keywords known to this module.  Order is stable.")

;;; Classifier (Phase 5-D.2, ported from anvil-worker.el) ------------
;;
;; Patterns regex-match against the `prin1-to-string' of the request
;; expression.  Precedence is write > batch > read > fallback per
;; anvil-worker.el; an expression that mixes mutations with reads is
;; routed to :write for safety.  The `anvil-' prefix is optional in
;; every pattern so the same rules apply whether the call goes to an
;; anvil-port handler or a native NeLisp primitive.

(defcustom nelisp-worker-classify-read-patterns
  '("(\\(?:anvil-\\)?org-read-\\(?:by-id\\|file\\|outline\\|headline\\)\\b"
    "(\\(?:anvil-\\)?org-index-search\\b"
    "(\\(?:anvil-\\)?file-read\\b"
    "(\\(?:anvil-\\)?file-outline\\b"
    "(\\(?:anvil-\\)?buffer-read\\b"
    "(\\(?:anvil-\\)?buffer-list-modified\\b"
    "(\\(?:anvil-\\)?elisp-describe-\\(?:function\\|variable\\)\\b"
    "(\\(?:anvil-\\)?elisp-info-lookup-symbol\\b"
    "(\\(?:anvil-\\)?elisp-read-source-file\\b"
    "(\\(?:anvil-\\)?elisp-get-function-definition\\b"
    "(\\(?:anvil-\\)?sqlite-query\\b"
    "(\\(?:anvil-\\)?org-get-\\(?:allowed-files\\|tag-config\\|todo-config\\)\\b")
  "Regexes that flag EXPRESSION as a read-only `:read'-lane call.
Matched in document order; the first matching pattern wins.  When
no pattern matches the classifier falls back to
`nelisp-worker-classify-unknown-fallback'."
  :type '(repeat regexp)
  :group 'nelisp-worker)

(defcustom nelisp-worker-classify-write-patterns
  '("(\\(?:anvil-\\)?file-replace-\\(?:string\\|regexp\\)\\b"
    "(\\(?:anvil-\\)?file-insert-at-line\\b"
    "(\\(?:anvil-\\)?file-delete-lines\\b"
    "(\\(?:anvil-\\)?file-append\\b"
    "(\\(?:anvil-\\)?file-prepend\\b"
    "(\\(?:anvil-\\)?file-batch\\(?:-across\\)?\\b"
    "(\\(?:anvil-\\)?file-ensure-import\\b"
    "(\\(?:anvil-\\)?json-object-add\\b"
    "(\\(?:anvil-\\)?buffer-save\\b"
    "(\\(?:anvil-\\)?org-edit-body\\b"
    "(\\(?:anvil-\\)?org-add-todo\\b"
    "(\\(?:anvil-\\)?org-rename-headline\\b"
    "(\\(?:anvil-\\)?org-update-todo-state\\b"
    "(save-buffer\\b"
    "(write-region\\b"
    "(write-file\\b"
    "(delete-file\\b"
    "(rename-file\\b"
    "(make-directory\\b")
  "Regexes that flag EXPRESSION as a mutating `:write'-lane call.
Tested *before* the read patterns so an expression that mixes
read+write operations is still routed to write."
  :type '(repeat regexp)
  :group 'nelisp-worker)

(defcustom nelisp-worker-classify-batch-patterns
  '("(byte-compile\\(?:-file\\)?\\b"
    "(org-babel-tangle\\b"
    "(\\(?:anvil-\\)?elisp-byte-compile-file\\b")
  "Regexes that flag EXPRESSION as a batch-pool candidate.
Routed to `:batch' only when the POOL's batch lane is non-empty;
otherwise the classifier silently downgrades to `:write'."
  :type '(repeat regexp)
  :group 'nelisp-worker)

(defcustom nelisp-worker-classify-unknown-fallback :write
  "Lane chosen by `nelisp-worker-classify' when no pattern matches.
Defaults to `:write' (the safe side) so an unrecognised expression
that happens to mutate state never sneaks into a read replica."
  :type '(choice (const :read) (const :write) (const :batch))
  :group 'nelisp-worker)

(defun nelisp-worker--match-any (expression patterns)
  "Return non-nil if EXPRESSION (a Lisp form) matches any PATTERNS.
Stringifies with `prin1-to-string' first so callers pass raw forms."
  (let ((s (prin1-to-string expression)))
    (cl-some (lambda (re) (string-match-p re s)) patterns)))

(defun nelisp-worker-classify (pool expression)
  "Return the lane symbol that should handle EXPRESSION under POOL.

Resolution order (first match wins):
  1. `nelisp-worker-classify-write-patterns' → `:write'
  2. `nelisp-worker-classify-batch-patterns' → `:batch'
     (downgraded to `:write' when POOL's batch lane is empty)
  3. `nelisp-worker-classify-read-patterns'  → `:read'
  4. otherwise → `nelisp-worker-classify-unknown-fallback'"
  (cond
   ((nelisp-worker--match-any
     expression nelisp-worker-classify-write-patterns)
    :write)
   ((nelisp-worker--match-any
     expression nelisp-worker-classify-batch-patterns)
    (let ((batch-size
           (and (nelisp-worker-pool-batch-pool pool)
                (nelisp-process-pool-size
                 (nelisp-worker-pool-batch-pool pool)))))
      (if (and batch-size (> batch-size 0)) :batch :write)))
   ((nelisp-worker--match-any
     expression nelisp-worker-classify-read-patterns)
    :read)
   (t nelisp-worker-classify-unknown-fallback)))

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

;;; Health check (Phase 5-D.3) -------------------------------------
;;
;; Three-layer dead worker detection per §2.4 C:
;;   1. Sentinel-driven mark  (inherited from `nelisp-process-pool';
;;      the sentinel installed at spawn flips state to `dead' when
;;      the host process exits)
;;   2. Periodic timer scan   (this module; walks every lane and
;;      calls `nelisp-process-pool--sweep-dead' — cheap, no probe)
;;   3. On-`-get' sweep       (inherited from `nelisp-process-pool';
;;      `pool-get' calls sweep-then-spawn so the caller receives a
;;      live worker even if neither of the above has fired)
;;
;; The timer is per-pool and registered in `nelisp-worker--health-
;; timers'.  `nelisp-worker-pool-create' auto-starts when
;; PRESPAWN is non-nil and `nelisp-worker-health-check-interval' is
;; positive; `nelisp-worker-pool-kill' auto-stops.

(defvar nelisp-worker--health-timers (make-hash-table :test 'eq)
  "POOL -> host timer object for its periodic health scan.")

(defun nelisp-worker-health-scan (pool)
  "Mark any dead worker across every lane of POOL as such.
Relies on `nelisp-process-pool--sweep-dead' which only inspects
`nelisp-process-live-p' and the worker's state slot; no subprocess
is spawned here, so the scan is safe to call from a timer context."
  (when (nelisp-worker-pool-p pool)
    (dolist (p (list (nelisp-worker-pool-read-pool pool)
                     (nelisp-worker-pool-write-pool pool)
                     (nelisp-worker-pool-batch-pool pool)))
      (when (nelisp-process-pool-p p)
        (nelisp-process-pool--sweep-dead p)))))

(defun nelisp-worker-health-timer-start (pool &optional interval)
  "Start a periodic health scan timer for POOL.
INTERVAL defaults to `nelisp-worker-health-check-interval'.  A
non-positive INTERVAL disables the timer and returns nil.  Any
previous timer registered for POOL is cancelled first (so this
is safe to call repeatedly)."
  (nelisp-worker-health-timer-stop pool)
  (let ((n (or interval nelisp-worker-health-check-interval)))
    (when (and (numberp n) (> n 0))
      (let ((timer (run-with-timer
                    n n
                    #'nelisp-worker-health-scan pool)))
        (puthash pool timer nelisp-worker--health-timers)
        timer))))

(defun nelisp-worker-health-timer-stop (pool)
  "Cancel POOL's periodic health scan timer, if any.
Idempotent: calling on a pool without a registered timer is a no-op."
  (let ((timer (gethash pool nelisp-worker--health-timers)))
    (when (timerp timer)
      (ignore-errors (cancel-timer timer)))
    (remhash pool nelisp-worker--health-timers))
  nil)

(defun nelisp-worker-health-timer-active-p (pool)
  "Return the active timer for POOL, or nil if none."
  (gethash pool nelisp-worker--health-timers))

;;; Batch warmup (Phase 5-D.4) --------------------------------------
;;
;; Every batch-lane worker can be pre-loaded with a list of host
;; expressions so that the first real `nelisp-worker-call' on that
;; lane lands on an already-hot emacs (init.org, heavy requires,
;; cache warm-up, etc.).  Delivery is fire-and-forget: each expr
;; is written as `(ID . FORM)' with a correlation id that is never
;; registered in the pending table, so the child's reply is dropped
;; silently by `nelisp-worker--deliver' which only updates known ids.
;;
;; Matches anvil-worker.el's warmup pattern (§2.5 B) but uses the
;; pipe-stdio protocol (§2.7 B) instead of emacsclient -e.

(defun nelisp-worker--send-warmup (pool host-proc)
  "Send every `nelisp-worker-batch-warmup-expressions' to HOST-PROC.
POOL is used only to mint a correlation-id prefix so warmup traffic
is identifiable in host traces; it is never registered as pending."
  (when (and host-proc (nelisp-process-p host-proc)
             (nelisp-process-live-p host-proc)
             nelisp-worker-batch-warmup-expressions)
    (let ((sent 0))
      (dolist (expr nelisp-worker-batch-warmup-expressions)
        (cl-incf sent)
        (let* ((id (format "nw-warmup-%s-%d"
                           (nelisp-worker-pool-name pool) sent))
               (request (cons id expr)))
          (ignore-errors
            (nelisp-process-send-string
             host-proc
             (concat (prin1-to-string request) "\n")))))
      sent)))

(defun nelisp-worker--schedule-warmup (pool)
  "Schedule a deferred warmup for every live batch-lane worker in POOL.
A non-positive `nelisp-worker-batch-warmup-delay' delivers warmup
synchronously; otherwise we `run-at-time' so the spawn has time to
settle before the first payload hits the pipe."
  (when (and nelisp-worker-batch-warmup-expressions
             (nelisp-worker-pool-p pool))
    (let ((batch-pool (nelisp-worker-pool-batch-pool pool)))
      (when (nelisp-process-pool-p batch-pool)
        (dolist (w (nelisp-process-pool-workers batch-pool))
          (let ((proc (nelisp-process-pool-worker-process w)))
            (when (and proc (nelisp-process-live-p proc))
              ;; Warmup replies are dropped by the filter anyway, but
              ;; keeping filter install idempotent here means callers
              ;; who explicitly invoke `--schedule-warmup' on a lazy
              ;; pool don't need to wire the filter manually.
              (nelisp-worker--ensure-filter pool proc)
              (cond
               ((and (numberp nelisp-worker-batch-warmup-delay)
                     (> nelisp-worker-batch-warmup-delay 0))
                (run-at-time nelisp-worker-batch-warmup-delay nil
                             #'nelisp-worker--send-warmup pool proc))
               (t
                (nelisp-worker--send-warmup pool proc))))))))))

;;; Spawn / tear-down ------------------------------------------------

(defun nelisp-worker--ensure-filter (pool host-proc)
  "Install the pool-aware reply filter on HOST-PROC if not already tagged.
The cached filter closure is stored in POOL's line-buffers table
under the key `:filter' so every lane worker — prespawned or
lazily spawned — routes replies back to this pool without a
re-allocation per `-call'."
  (when (and host-proc (nelisp-process-p host-proc))
    (let ((filter (gethash :filter
                           (nelisp-worker-pool-line-buffers pool))))
      (unless filter
        (setq filter (nelisp-worker--make-filter pool))
        (puthash :filter filter
                 (nelisp-worker-pool-line-buffers pool)))
      (setf (nelisp-process-user-filter host-proc) filter))))

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
    ;; Install a pool-aware filter on every spawned worker so replies
    ;; land in this pool's correlation table.  `--ensure-filter'
    ;; caches a single closure on POOL so prespawn + lazy-spawn +
    ;; warmup all route through the same instance.
    (dolist (p (list (nelisp-worker-pool-read-pool pool)
                     (nelisp-worker-pool-write-pool pool)
                     (nelisp-worker-pool-batch-pool pool)))
      (dolist (w (nelisp-process-pool-workers p))
        (nelisp-worker--ensure-filter
         pool (nelisp-process-pool-worker-process w))))
    ;; Auto-start the periodic health timer when eagerly spawned —
    ;; a lazy pool has nothing to scan yet and would wake needlessly.
    (when (and prespawn
               (numberp nelisp-worker-health-check-interval)
               (> nelisp-worker-health-check-interval 0))
      (nelisp-worker-health-timer-start pool))
    ;; Schedule batch-lane warmup.  A lazy pool has no batch workers
    ;; to warm; warmup on lazy-spawned batch workers is the caller's
    ;; responsibility (call `nelisp-worker--schedule-warmup' after
    ;; `pool-get' forces a spawn).
    (when prespawn (nelisp-worker--schedule-warmup pool))
    pool))

(defun nelisp-worker-pool-kill (pool)
  "Terminate every worker in POOL and clear pending state."
  (nelisp-worker-health-timer-stop pool)
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

(cl-defun nelisp-worker-call (pool expression &key lane timeout)
  "Evaluate EXPRESSION inside a POOL worker and return the result.

Lane selection:
  - when LANE is supplied (`:read' / `:write' / `:batch') it is
    used verbatim (§2.2 C explicit override)
  - when LANE is nil the `nelisp-worker-classify' regex classifier
    picks the lane (§2.2 C classifier path)

Blocks the current host thread up to TIMEOUT seconds (default
`nelisp-worker-call-timeout').  Signals `nelisp-worker-timeout'
on wedge or `nelisp-worker-error' on the child's `:error' reply."
  (let* ((lane (or lane (nelisp-worker-classify pool expression)))
         (lane-pool (nelisp-worker--lane-pool pool lane))
         (worker (nelisp-process-pool-get lane-pool)))
    (unless worker
      (signal 'nelisp-worker-busy (list lane)))
    (let* ((wrap (nelisp-process-pool-worker-process worker)))
      ;; Lazy-spawned workers (pool-get with `:prespawn nil') are not
      ;; covered by the pool-create install loop — make sure the
      ;; filter is wired before we send the request.
      (nelisp-worker--ensure-filter pool wrap))
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
