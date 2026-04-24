;;; nelisp-process.el --- NeLisp subprocess substrate -*- lexical-binding: t; -*-
;;
;; Phase 5-C.1 per Doc 14.  Thin wrapper over host `make-process'
;; that adds:
;;
;;   - `nelisp-process' cl-defstruct tracking a host process plus
;;     per-process state (owning actor, user callbacks, props plist)
;;   - sentinel / filter trampolines that dispatch NeLisp closures
;;     via `nelisp--apply' (so callbacks may be either host Elisp
;;     lambdas or NeLisp-evaluated closures)
;;   - optional per-process actor integration per Doc 14 §2.3 /
;;     §2.8 — the wrapper posts `nelisp-event' messages of kinds
;;     `process-state' and `process-output' to the given actor
;;   - pipe + PTY selection forwarded to `:connection-type' (§2.2 B)
;;
;; Real tty input, process pool, and anvil-worker-style restart
;; policy live in §3.1b (`nelisp-process-pool.el').

;;; Code:

(require 'cl-lib)
(require 'nelisp-eval)
(require 'nelisp-actor)

;; --- optional eventloop integration helper ------------------------
;;
;; `nelisp-process' does *not* hard-require `nelisp-eventloop.el':
;; callers that want actor-delivered events can pass :actor; we only
;; construct a `nelisp-event' value if `nelisp-make-event' is
;; available.  This keeps the subprocess layer usable in pure
;; non-UI contexts too.
(autoload 'nelisp-make-event "nelisp-eventloop")

(cl-defstruct (nelisp-process
               (:constructor nelisp-process--make)
               (:copier nil))
  id                ; symbol / string name
  host-proc         ; underlying host process object
  (actor nil)       ; owning actor (optional)
  (status 'running) ; mirror of host status, updated by sentinel
  (exit-code nil)   ; filled in when the process exits
  (buffer nil)      ; optional host buffer for stdout/stderr
  (user-sentinel nil)
  (user-filter nil)
  (props nil))

(defvar nelisp-process--registry nil
  "List of every live `nelisp-process' wrap.
Kept ordered newest-first for fast `push'/`delq'.")

(defun nelisp-process--reset-registry ()
  "Discard every wrap in the registry without touching host procs.
Used by test harnesses for a clean slate."
  (setq nelisp-process--registry nil))

(defun nelisp-process--find-by-host (host-proc)
  "Return the wrap whose `host-proc' is eq to HOST-PROC, or nil."
  (cl-find host-proc nelisp-process--registry
           :key #'nelisp-process-host-proc :test #'eq))

(defun nelisp-process--post-event (wrap kind data)
  "Send a `nelisp-event' of KIND carrying DATA to WRAP's actor if any."
  (let ((actor (nelisp-process-actor wrap)))
    (when (and actor
               (nelisp-actor-p actor)
               (fboundp 'nelisp-make-event)
               (not (memq (nelisp-actor-status actor)
                          '(:dead :crashed))))
      (nelisp-send actor (nelisp-make-event kind data)))))

(defun nelisp-process--host-sentinel-trampoline (host-proc event)
  "Sentinel trampoline installed on every host proc created by
`nelisp-make-process'.  Mirrors host state into the wrap, optionally
posts a `process-state' event, then dispatches the user sentinel."
  (let ((wrap (nelisp-process--find-by-host host-proc)))
    (when wrap
      (let ((st (process-status host-proc)))
        (setf (nelisp-process-status wrap) st)
        (when (memq st '(exit signal closed failed))
          (setf (nelisp-process-exit-code wrap)
                (process-exit-status host-proc))))
      (nelisp-process--post-event wrap 'process-state
                                  (list :wrap wrap :event event))
      (let ((sen (nelisp-process-user-sentinel wrap)))
        (when sen
          (condition-case err
              (nelisp--apply sen (list wrap event))
            (error
             (message "nelisp-process sentinel error: %S" err))))))))

(defun nelisp-process--host-filter-trampoline (host-proc chunk)
  "Filter trampoline: posts a `process-output' event and dispatches
the user filter.  The default host behaviour of appending CHUNK to
`process-buffer' is preserved so callers without a filter still see
output via `nelisp-process-buffer'."
  (let ((wrap (nelisp-process--find-by-host host-proc)))
    (when wrap
      (let ((buf (process-buffer host-proc)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (goto-char (point-max))
            (insert chunk))))
      (nelisp-process--post-event wrap 'process-output
                                  (list :wrap wrap :chunk chunk))
      (let ((flt (nelisp-process-user-filter wrap)))
        (when flt
          (condition-case err
              (nelisp--apply flt (list wrap chunk))
            (error
             (message "nelisp-process filter error: %S" err))))))))

(cl-defun nelisp-make-process (&key name command connection-type
                                    coding buffer sentinel filter
                                    actor props stderr)
  "Spawn a subprocess and return the `nelisp-process' wrap.

Arguments:
  NAME             Symbol or string, defaults to \"nelisp-proc\".
  COMMAND          Argument vector (list of strings).  Required.
  CONNECTION-TYPE  `pipe' (default) or `pty'.  Forwarded to host.
  CODING           Coding system, default `utf-8'.
  BUFFER           Optional host buffer for captured stdout.
  SENTINEL         Function of (WRAP EVENT) — host lambda or
                   NeLisp closure (dispatched via `nelisp--apply').
  FILTER           Function of (WRAP CHUNK).  Same dispatch rules
                   as SENTINEL.
  ACTOR            Optional Phase 4 actor.  When non-nil, the
                   trampolines post `process-state' /
                   `process-output' `nelisp-event' messages to
                   it using `nelisp-send'.
  PROPS            Initial plist for `nelisp-process-get/-put'.
  STDERR           Optional stderr buffer/process (forwarded).

The actual filter preserves the host's default \"append to
process-buffer\" behaviour even when FILTER is supplied, so
post-mortem inspection of captured output stays simple."
  (unless command
    (signal 'wrong-type-argument (list 'listp command)))
  (let* ((name (or name "nelisp-proc"))
         (ctype (or connection-type 'pipe))
         (host-args
          (append
           (list :name (format "%s" name)
                 :command command
                 :connection-type ctype
                 :coding (or coding 'utf-8)
                 :sentinel #'nelisp-process--host-sentinel-trampoline
                 :filter #'nelisp-process--host-filter-trampoline)
           (when buffer (list :buffer buffer))
           (when stderr (list :stderr stderr))))
         (host-proc (apply #'make-process host-args))
         (wrap (nelisp-process--make
                :id name
                :host-proc host-proc
                :actor actor
                :status (process-status host-proc)
                :buffer buffer
                :user-sentinel sentinel
                :user-filter filter
                :props props)))
    (push wrap nelisp-process--registry)
    wrap))

(defun nelisp-process-send-string (wrap s)
  "Send S to WRAP's stdin."
  (process-send-string (nelisp-process-host-proc wrap) s))

(defun nelisp-process-send-eof (wrap)
  "Close WRAP's stdin (send EOF)."
  (process-send-eof (nelisp-process-host-proc wrap)))

(defun nelisp-process-live-p (wrap)
  (process-live-p (nelisp-process-host-proc wrap)))

(defun nelisp-process-exit-status-of (wrap)
  "Return the exit status carried by WRAP (shadowed from host at
sentinel time, returning nil if still running)."
  (nelisp-process-exit-code wrap))

(defun nelisp-process-status-of (wrap)
  "Return the current host process status (symbol) via query,
not the cached slot."
  (process-status (nelisp-process-host-proc wrap)))

(defun nelisp-kill-process (wrap)
  "Send SIGTERM to WRAP."
  (kill-process (nelisp-process-host-proc wrap)))

(defun nelisp-delete-process (wrap)
  "Delete WRAP's host process and remove it from the registry."
  (let ((host (nelisp-process-host-proc wrap)))
    (when (processp host)
      (delete-process host)))
  (setq nelisp-process--registry (delq wrap nelisp-process--registry))
  nil)

(defun nelisp-process-list ()
  "Return a fresh copy of the live wrap registry."
  (copy-sequence nelisp-process--registry))

(defun nelisp-process-id-of (wrap)
  "Return the OS PID for WRAP, or nil if unavailable."
  (let ((host (nelisp-process-host-proc wrap)))
    (when (processp host) (process-id host))))

(defun nelisp-process-name-of (wrap)
  (process-name (nelisp-process-host-proc wrap)))

(defun nelisp-process-command-of (wrap)
  (process-command (nelisp-process-host-proc wrap)))

(defun nelisp-process-buffer-of (wrap)
  (or (nelisp-process-buffer wrap)
      (process-buffer (nelisp-process-host-proc wrap))))

(defun nelisp-process-get (wrap key)
  "Lookup KEY in WRAP's property list."
  (plist-get (nelisp-process-props wrap) key))

(defun nelisp-process-put (wrap key value)
  "Store KEY -> VALUE in WRAP's property list."
  (setf (nelisp-process-props wrap)
        (plist-put (nelisp-process-props wrap) key value))
  value)

(defun nelisp-process-accept-output (wrap &optional seconds)
  "Drain pending output from WRAP's host process for up to SECONDS."
  (accept-process-output (nelisp-process-host-proc wrap) seconds))

(defun nelisp-process-wait-for-exit (wrap &optional timeout)
  "Block until WRAP exits or TIMEOUT (seconds, default 5) elapses.
Returns the exit status or nil on timeout."
  (let ((deadline (+ (float-time) (or timeout 5.0))))
    (while (and (nelisp-process-live-p wrap)
                (< (float-time) deadline))
      (nelisp-process-accept-output wrap 0.05))
    (when (not (nelisp-process-live-p wrap))
      (nelisp-process-exit-code wrap))))

(provide 'nelisp-process)
;;; nelisp-process.el ends here
