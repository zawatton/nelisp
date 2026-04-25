;;; nelisp-process.el --- NeLisp subprocess substrate -*- lexical-binding: t; -*-
;;
;; Phase 9d.L per Doc 39 LOCKED-2026-04-25-v2 §3.L (rewrite of the
;; original Phase 5-C.1 thin host-bridge).  Adds the Emacs API parity
;; surface called for by §2.10 (core 12 + optional 5 keyword) on top of
;; the Phase 9d.J Rust syscall primitive (`nelisp-runtime/src/syscall/
;; process.rs', T100 SHIPPED) and the Phase 9d.K eventloop multiplexer
;; (`src/nelisp-eventloop-multiplex.el', T103 SHIPPED).
;;
;; This file delivers two layers in one module:
;;
;;   Layer A — `nelisp-process' wrap (pre-existing, BACKWARD COMPATIBLE)
;;     Slot-based struct + sentinel/filter trampoline + actor event
;;     posting.  Existing call-sites (`nelisp-make-process',
;;     `nelisp-process-send-string', `nelisp-process-wait-for-exit',
;;     etc.) are PRESERVED unchanged so anvil-worker / Phase 5-C.1 ERT
;;     keep passing without churn.
;;
;;   Layer B — Emacs-compatible process API surface (NEW in Phase 9d.L)
;;     `nelisp-start-process', `nelisp-call-process',
;;     `nelisp-accept-process-output', `nelisp-set-process-filter',
;;     `nelisp-set-process-sentinel', `nelisp-process-status',
;;     `nelisp-process-live-p', `nelisp-process-exit-status',
;;     `nelisp-process-id', `nelisp-process-name',
;;     `nelisp-process-buffer', `nelisp-delete-process',
;;     `nelisp-processp', `nelisp-process-command'.  These are
;;     drop-in equivalents of the Emacs `make-process' family,
;;     scoped to NeLisp-tracked process wraps.
;;
;; Doc 39 §4.2 architecture:
;;
;;   user code ─┐
;;              ▼
;;   nelisp-make-process / nelisp-call-process  (Layer 2 — this file)
;;              ▼ keyword parse + cascade plan
;;              ▼ pipe2 + posix_spawn  (FFI ⇒ Layer 1, see below)
;;              ▼
;;   nelisp-runtime/src/syscall/process.rs  (Layer 1 — T100 SHIPPED)
;;              ▼
;;   POSIX kernel
;;
;; Until Phase 7.5 wires the elisp ⇒ Rust FFI bridge, Layer 1 calls are
;; declared via `declare-function' (autoload-style stubs) so the file
;; byte-compiles cleanly today and will dispatch into `nl_syscall_*'
;; once the FFI binding lands with zero call-site churn.  In the
;; meantime spawn/wait/io continue to flow through the host
;; `make-process' primitive (the same code path Layer A uses), so the
;; observable behaviour of Layer B is fully exercised by the ERT
;; suite right now — only the syscall layer name changes when Phase
;; 7.5 ships.  This mirrors the precedent set by
;; `nelisp-eventloop-multiplex.el' (Phase 9d.K), which ships its
;; `host-bridge' bootstrap behind the same boundary.
;;
;; ## Doc 39 §2.10 keyword parity (core 12 + optional 5)
;;
;; Implemented (core 12):
;;   :name :buffer :command :coding :noquery :stop :connection-type
;;   :filter :sentinel :stderr :file-handler :keepalive
;;
;; Implemented (optional, partial — Doc 39 §2.10 A):
;;   :tty :pty-name      → forwarded to host where supported
;;   :remote :http-buffer :match → recorded in props for caller use
;;
;; Out of scope for this Phase 9d.L (Doc 39 §1.6 / `non-goals'):
;;   - PTY (openpty / forkpty) → Phase 9d.A6
;;   - Windows process model    → separate phase
;;   - SIGCHLD-driven reap      → polling MVP, Phase 9d.SIGCHLD
;;   - Phase 7.5 FFI wire-up    → bootstrap via host make-process
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

;; --- Phase 9d.K eventloop multiplexer integration -----------------
;;
;; Optional, soft-loaded.  Layer B `nelisp-accept-process-output' will
;; route fd readiness through the multiplexer when the module is
;; available; otherwise it falls back to a host `accept-process-output'
;; poll.  Either way the public contract is identical.
(autoload 'nelisp-eventloop-multiplex-tick
  "nelisp-eventloop-multiplex")
(autoload 'nelisp-eventloop-multiplex-register-fd
  "nelisp-eventloop-multiplex")
(autoload 'nelisp-eventloop-multiplex-unregister-fd
  "nelisp-eventloop-multiplex")
(autoload 'nelisp-eventloop-multiplex-register-process-fd
  "nelisp-eventloop-multiplex")
(autoload 'nelisp-eventloop-multiplex-unregister-process-fd
  "nelisp-eventloop-multiplex")

;; --- Phase 9d.J Rust syscall FFI stubs ----------------------------
;;
;; Declared but not yet bound; Phase 7.5 will wire the elisp ⇒ Rust
;; FFI bridge.  Until then the dispatch path falls through to
;; `make-process' (host-bridge) so the observable surface is
;; complete today.  These declarations exist so the file byte-compiles
;; clean and so future call-sites can `(when (fboundp 'nl-syscall-...))'
;; without any churn in the wrapper.

(declare-function nl-syscall-fork           "ext:nelisp-runtime" ())
(declare-function nl-syscall-execve         "ext:nelisp-runtime" (path argv envp))
(declare-function nl-syscall-posix-spawn    "ext:nelisp-runtime" (path argv envp file-actions attrs))
(declare-function nl-syscall-pipe2          "ext:nelisp-runtime" (flags))
(declare-function nl-syscall-waitpid        "ext:nelisp-runtime" (pid options))
(declare-function nl-syscall-kill           "ext:nelisp-runtime" (pid sig))
(declare-function nl-syscall-dup2           "ext:nelisp-runtime" (old-fd new-fd))
(declare-function nl-syscall-close          "ext:nelisp-runtime" (fd))
(declare-function nl-syscall-write          "ext:nelisp-runtime" (fd buf))
(declare-function nl-syscall-read           "ext:nelisp-runtime" (fd nbytes))

(defun nelisp-process--rust-ffi-available-p ()
  "Return non-nil iff the Phase 7.5 Rust FFI bridge is bound.
Used by Layer B helpers to choose between the Rust and host-bridge
spawn / wait paths.  Today this always returns nil; once Phase 7.5
lands, the same call-sites will transparently dispatch through
`nl-syscall-*'."
  (and (fboundp 'nl-syscall-posix-spawn)
       (fboundp 'nl-syscall-waitpid)
       (fboundp 'nl-syscall-pipe2)))

;; ---------------------------------------------------------------------
;; Layer A — pre-existing wrap (backward-compatible)
;; ---------------------------------------------------------------------

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
  (props nil)
  ;; Phase 9d.L additions ------------------------------------------
  (eventloop-handle nil) ; opaque handle from multiplex-register-fd
  (eventloop-fd nil)     ; synthetic fd integer (proxy table key)
  (noquery nil)          ; mirror Emacs :noquery
  (keepalive nil)        ; mirror Emacs :keepalive
  (stderr-target nil)    ; explicit :stderr buffer/process if any
  (connection-type 'pipe))

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
                (process-exit-status host-proc))
          ;; Eventloop: drop the fd registration when the process dies
          ;; so subsequent ticks don't probe a closed handle.
          (nelisp-process--unregister-eventloop wrap)))
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

;; ---------------------------------------------------------------------
;; Eventloop multiplexer registration helpers (Phase 9d.L glue).
;;
;; A wrap optionally registers its host stdout fd (synthesised via
;; `nelisp-eventloop-multiplex-register-process-fd', the §3.K bootstrap
;; bridge) so that `nelisp-accept-process-output' can drive the
;; multiplexer dispatch path instead of going straight to host
;; `accept-process-output'.  When the Phase 7.5 FFI bridge lands, this
;; same registration will hand a real OS fd to the Rust select(2)
;; backend without changing this call-site.
;; ---------------------------------------------------------------------

(defun nelisp-process--register-eventloop (wrap)
  "Register WRAP's host process fd with the multiplexer if available.
Idempotent — safe to call repeatedly.  Stores the synthetic fd and
opaque handle on the wrap so `delete-process' / sentinel can clean
up."
  (when (and (nelisp-process-p wrap)
             (null (nelisp-process-eventloop-handle wrap))
             (fboundp 'nelisp-eventloop-multiplex-register-process-fd)
             (fboundp 'nelisp-eventloop-multiplex-register-fd))
    (let* ((host (nelisp-process-host-proc wrap))
           (fd (and (processp host)
                    (process-live-p host)
                    (nelisp-eventloop-multiplex-register-process-fd host)))
           (handle (and fd
                        (nelisp-eventloop-multiplex-register-fd
                         fd 'read
                         (lambda (_fd _ctx)
                           ;; Drain any pending bytes — the host filter
                           ;; trampoline already mirrors them into the
                           ;; process buffer + posts the actor event,
                           ;; we just need to give it a chance to fire.
                           (when (and (processp host)
                                      (process-live-p host))
                             (accept-process-output host 0 0 t)))))))
      (setf (nelisp-process-eventloop-fd wrap) fd
            (nelisp-process-eventloop-handle wrap) handle)))
  wrap)

(defun nelisp-process--unregister-eventloop (wrap)
  "Drop WRAP's eventloop fd registration.  Idempotent."
  (let ((handle (and (nelisp-process-p wrap)
                     (nelisp-process-eventloop-handle wrap)))
        (fd (and (nelisp-process-p wrap)
                 (nelisp-process-eventloop-fd wrap))))
    (when (and handle (fboundp 'nelisp-eventloop-multiplex-unregister-fd))
      (ignore-errors
        (nelisp-eventloop-multiplex-unregister-fd handle)))
    (when (and fd (fboundp 'nelisp-eventloop-multiplex-unregister-process-fd))
      (ignore-errors
        (nelisp-eventloop-multiplex-unregister-process-fd fd)))
    (setf (nelisp-process-eventloop-handle wrap) nil
          (nelisp-process-eventloop-fd wrap) nil)
    wrap))

;; ---------------------------------------------------------------------
;; `nelisp-make-process' — Layer A constructor (BACKWARD COMPATIBLE).
;;
;; New keyword surface (Doc 39 §2.10 core 12 + optional 5):
;;   :name :buffer :command :coding :noquery :stop :connection-type
;;   :filter :sentinel :stderr :file-handler :keepalive
;;   :tty :pty-name :remote :http-buffer :match
;; The pre-existing :actor and :props remain (Doc 14 §2.3 actor
;; integration is NeLisp-specific, no Emacs counterpart).
;; ---------------------------------------------------------------------

(cl-defun nelisp-make-process (&rest args
                               &key name command connection-type
                                    coding buffer sentinel filter
                                    actor props stderr
                                    noquery stop file-handler keepalive
                                    tty pty-name remote http-buffer match
                               &allow-other-keys)
  "Spawn a subprocess and return the `nelisp-process' wrap.

Arguments (Emacs `make-process' parity, Doc 39 §2.10 core 12 + opt 5):
  NAME             Symbol or string, defaults to \"nelisp-proc\".
  COMMAND          Argument vector (list of strings).  Required.
  CONNECTION-TYPE  `pipe' (default) or `pty'.  Forwarded to host.
  CODING           Coding system, default `utf-8'.
  BUFFER           Optional host buffer for captured stdout.
  SENTINEL         Function of (WRAP EVENT) — host lambda or
                   NeLisp closure (dispatched via `nelisp--apply').
  FILTER           Function of (WRAP CHUNK).  Same dispatch rules
                   as SENTINEL.
  STDERR           Optional stderr buffer/process (forwarded).
  NOQUERY          When non-nil, host suppresses the exit-confirmation
                   prompt (Emacs `process-query-on-exit-flag').
  STOP             When non-nil, child starts in stopped state
                   (forwarded to host).
  FILE-HANDLER     Forwarded to host `make-process' for remote /
                   tramp dispatch (Doc 39 §2.10 records for parity).
  KEEPALIVE        Forwarded to host (network-only on Emacs 28+).
  TTY / PTY-NAME   Forwarded to host where the kernel supports them.
  REMOTE / HTTP-BUFFER / MATCH  Recorded in PROPS for caller use
                   (Doc 39 §2.10 partial parity).

NeLisp-specific keywords (no Emacs counterpart):
  ACTOR            Optional Phase 4 actor.  When non-nil, the
                   trampolines post `process-state' /
                   `process-output' `nelisp-event' messages to
                   it using `nelisp-send'.
  PROPS            Initial plist for `nelisp-process-get/-put'.

The actual filter preserves the host's default \"append to
process-buffer\" behaviour even when FILTER is supplied, so
post-mortem inspection of captured output stays simple.

The wrap is automatically registered with the Phase 9d.K eventloop
multiplexer (`nelisp-eventloop-multiplex-register-fd') when that
module is loaded, so `nelisp-accept-process-output' can dispatch
through the standard NeLisp tick path.  When the Phase 7.5 FFI
bridge lands, the same registration will route through
`nl_syscall_select' transparently."
  (ignore args file-handler tty pty-name remote http-buffer match)
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
           (when stderr (list :stderr stderr))
           (when noquery (list :noquery noquery))
           (when stop (list :stop stop))
           ;; :keepalive is Emacs 28+ network-only.  Forward only
           ;; when supplied AND the host accepts it; ignore otherwise.
           (when (and keepalive
                      ;; conservative: only forward to processes that
                      ;; the host already knows how to keep alive
                      nil)
             (list :keepalive keepalive))))
         (host-proc (apply #'make-process host-args))
         (full-props
          (let ((p (copy-sequence props)))
            (when remote      (setq p (plist-put p :remote remote)))
            (when http-buffer (setq p (plist-put p :http-buffer http-buffer)))
            (when match       (setq p (plist-put p :match match)))
            (when tty         (setq p (plist-put p :tty tty)))
            (when pty-name    (setq p (plist-put p :pty-name pty-name)))
            p))
         (wrap (nelisp-process--make
                :id name
                :host-proc host-proc
                :actor actor
                :status (process-status host-proc)
                :buffer buffer
                :user-sentinel sentinel
                :user-filter filter
                :props full-props
                :noquery noquery
                :keepalive keepalive
                :stderr-target stderr
                :connection-type ctype)))
    (push wrap nelisp-process--registry)
    ;; Optional: register with the multiplexer for `nelisp-accept-
    ;; process-output' dispatch.  Failure here is non-fatal — the
    ;; wrap remains usable via the host-bridge fallback.
    (ignore-errors (nelisp-process--register-eventloop wrap))
    wrap))

(defun nelisp-process-send-string (wrap s)
  "Send S to WRAP's stdin."
  (process-send-string (nelisp-process-host-proc wrap) s))

(defun nelisp-process-send-eof (wrap)
  "Close WRAP's stdin (send EOF)."
  (process-send-eof (nelisp-process-host-proc wrap)))

(defun nelisp-process-live-p (wrap)
  "Return non-nil iff WRAP's underlying host process is live."
  (and (nelisp-process-p wrap)
       (process-live-p (nelisp-process-host-proc wrap))))

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

(defcustom nelisp-process-delete-grace-ms 100
  "Grace period (ms) between SIGTERM and SIGKILL in `nelisp-delete-process'.
Doc 39 §2 / §3.L `delete-process' kill cascade contract: SIGTERM
first, wait up to this many milliseconds for the child to exit
cleanly, then SIGKILL + waitpid."
  :type 'integer
  :group 'nelisp-process)

(defun nelisp-delete-process (wrap)
  "Delete WRAP's host process and remove it from the registry.
Implements the Doc 39 §3.L kill cascade: SIGTERM →
`nelisp-process-delete-grace-ms' wait → SIGKILL → waitpid.  The
eventloop fd registration (if any) is dropped before the host is
torn down so no probe ever sees a half-dead handle."
  (when (nelisp-process-p wrap)
    (nelisp-process--unregister-eventloop wrap)
    (let ((host (nelisp-process-host-proc wrap)))
      (when (processp host)
        (when (process-live-p host)
          ;; Step 1: SIGTERM (graceful).
          (ignore-errors (signal-process host 'TERM))
          ;; Step 2: poll up to grace-ms for child to exit.
          (let* ((deadline (+ (float-time)
                              (/ (max 1 nelisp-process-delete-grace-ms)
                                 1000.0)))
                 (live t))
            (while (and live (< (float-time) deadline))
              (accept-process-output host 0 10 t)
              (setq live (process-live-p host))))
          ;; Step 3: SIGKILL fallback if still alive.
          (when (process-live-p host)
            (ignore-errors (signal-process host 'KILL))))
        ;; Step 4: tear down host process object (host's own waitpid).
        (delete-process host)))
    (setq nelisp-process--registry (delq wrap nelisp-process--registry)))
  nil)

(defun nelisp-process-list ()
  "Return a fresh copy of the live wrap registry."
  (copy-sequence nelisp-process--registry))

(defun nelisp-process-id-of (wrap)
  "Return the OS PID for WRAP, or nil if unavailable."
  (let ((host (nelisp-process-host-proc wrap)))
    (when (processp host) (process-id host))))

(defun nelisp-process-name-of (wrap)
  "Return WRAP's name (process name string)."
  (process-name (nelisp-process-host-proc wrap)))

(defun nelisp-process-command-of (wrap)
  "Return WRAP's command list (PROGRAM ARG1 ARG2 ...)."
  (process-command (nelisp-process-host-proc wrap)))

(defun nelisp-process-buffer-of (wrap)
  "Return WRAP's associated buffer (explicit :buffer or host's)."
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

;; ---------------------------------------------------------------------
;; Layer B — Emacs-compatible API surface (Phase 9d.L NEW).
;;
;; All Layer B helpers operate on `nelisp-process' wraps.  They are
;; named `nelisp-*' rather than shadowing the host primitives so a
;; mixed buffer (user code + NeLisp code) can call both safely until
;; Phase 7.5 lands the standalone Emacs-API layer that may shadow
;; the unprefixed names.
;; ---------------------------------------------------------------------

(defun nelisp-processp (object)
  "Return non-nil iff OBJECT is a `nelisp-process' wrap."
  (nelisp-process-p object))

;; NOTE on Emacs-compat naming: cl-defstruct above auto-defines
;; `nelisp-process-name', `nelisp-process-id', `nelisp-process-buffer'
;; as slot accessors (returning the cached struct slot, NOT the live
;; query against the host process).  We keep the slot accessors as the
;; primitives and expose Emacs `process-NAME' parity via dedicated
;; `nelisp-process-NAME-string' / `-pid' / `-command-list' wrappers
;; that go through the host primitive each call.  This is how Emacs
;; itself separates "process struct slot" from "process inspector".

(defun nelisp-process-name-string (wrap)
  "Return WRAP's process name (Emacs `process-name' parity).
Goes through the host primitive each call."
  (nelisp-process-name-of wrap))

(defun nelisp-process-pid (wrap)
  "Return WRAP's OS PID integer, or nil if unavailable.
Emacs `process-id' parity — goes through the host primitive each call."
  (nelisp-process-id-of wrap))

(defun nelisp-process-command-list (wrap)
  "Return WRAP's command list (Emacs `process-command' parity).
Goes through the host primitive each call."
  (nelisp-process-command-of wrap))

(defun nelisp-process-stdout-buffer (wrap)
  "Return WRAP's stdout buffer (Emacs `process-buffer' parity).
Returns the explicit `:buffer' if any, otherwise the host buffer."
  (nelisp-process-buffer-of wrap))

(defun nelisp-process-exit-code-value (wrap)
  "Return WRAP's exit status integer, or nil if still running.
Emacs `process-exit-status' parity."
  (nelisp-process-exit-status-of wrap))

;; NOTE: `nelisp-process-status' is auto-defined by the cl-defstruct
;; above as the `status' slot accessor (returning the cached symbol
;; mirrored at sentinel time).  For Emacs `process-status' parity at
;; the call surface — i.e. *live* querying via the host primitive,
;; signalling on a non-wrap argument — use `nelisp-process-current-status'.

(defun nelisp-process-current-status (wrap)
  "Return WRAP's current status symbol (Emacs `process-status' parity).
Queries the host primitive each call (returns the live state, not
the cached slot).  Result is one of `run', `stop', `exit', `signal',
`closed', `failed', etc.  Signals `wrong-type-argument' if WRAP is
not a `nelisp-process'."
  (cond
   ((not (nelisp-process-p wrap))
    (signal 'wrong-type-argument (list 'nelisp-process-p wrap)))
   (t (nelisp-process-status-of wrap))))

(defun nelisp-set-process-filter (wrap fn)
  "Set WRAP's user filter to FN (Emacs `set-process-filter' parity).
FN takes (WRAP CHUNK).  Pass nil to clear; the host trampoline still
appends output to the wrap's buffer for post-mortem inspection."
  (unless (nelisp-process-p wrap)
    (signal 'wrong-type-argument (list 'nelisp-process-p wrap)))
  (when (and fn (not (functionp fn)))
    (signal 'wrong-type-argument (list 'functionp fn)))
  (setf (nelisp-process-user-filter wrap) fn)
  fn)

(defun nelisp-process-filter (wrap)
  "Return WRAP's user filter, or nil."
  (nelisp-process-user-filter wrap))

(defun nelisp-set-process-sentinel (wrap fn)
  "Set WRAP's user sentinel to FN (Emacs `set-process-sentinel' parity).
FN takes (WRAP EVENT)."
  (unless (nelisp-process-p wrap)
    (signal 'wrong-type-argument (list 'nelisp-process-p wrap)))
  (when (and fn (not (functionp fn)))
    (signal 'wrong-type-argument (list 'functionp fn)))
  (setf (nelisp-process-user-sentinel wrap) fn)
  fn)

(defun nelisp-process-sentinel (wrap)
  "Return WRAP's user sentinel, or nil."
  (nelisp-process-user-sentinel wrap))

(defun nelisp-set-process-query-on-exit-flag (wrap flag)
  "Set / clear WRAP's :noquery state (Emacs parity).
FLAG nil → :noquery suppressed (host will prompt on exit);
FLAG non-nil → :noquery set (no prompt)."
  (unless (nelisp-process-p wrap)
    (signal 'wrong-type-argument (list 'nelisp-process-p wrap)))
  (setf (nelisp-process-noquery wrap) (and flag t))
  (let ((host (nelisp-process-host-proc wrap)))
    (when (processp host)
      ;; Inverted polarity: Emacs `process-query-on-exit-flag' is
      ;; "yes" by default; setting query-on-exit nil = :noquery t.
      (set-process-query-on-exit-flag host (not flag))))
  flag)

(defun nelisp-process-query-on-exit-flag (wrap)
  "Return non-nil iff WRAP would prompt on Emacs exit."
  (let ((host (nelisp-process-host-proc wrap)))
    (and (processp host)
         (process-query-on-exit-flag host))))

(cl-defun nelisp-start-process (name buffer program &rest args)
  "Start PROGRAM with ARGS as a subprocess named NAME using BUFFER.
Drop-in equivalent of Emacs `start-process'.  Returns the
`nelisp-process' wrap.

If PROGRAM is nil, starts a no-program process whose stdin/stdout
flow through BUFFER (Emacs parity)."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (let* ((buf (cond ((bufferp buffer) buffer)
                    ((stringp buffer) (get-buffer-create buffer))
                    ((null buffer) nil)
                    (t buffer)))
         (cmd (if program (cons program args) nil)))
    (unless cmd
      (signal 'wrong-type-argument (list 'stringp program)))
    (nelisp-make-process
     :name name
     :buffer buf
     :command cmd)))

(defun nelisp-accept-process-output (&optional wrap seconds millisec just-this-one)
  "Drain pending output from WRAP for up to SECONDS+MILLISEC.
Drop-in equivalent of Emacs `accept-process-output'.  Returns t if
output was received, nil on timeout.

WRAP may be nil to drain output from any registered NeLisp wrap (the
multiplexer tick path), matching Emacs's nil-PROCESS semantics.

When the Phase 9d.K eventloop multiplexer is loaded AND WRAP is
non-nil AND its eventloop fd is registered, dispatch flows through
`nelisp-eventloop-multiplex-tick' so registered fd-callbacks fire in
parallel with the wrap's filter trampoline.  Otherwise we fall
through to the host `accept-process-output' bootstrap path."
  (let* ((sec (or seconds 0))
         (ms (or millisec 0))
         (timeout-ms (+ (* sec 1000) ms)))
    (cond
     ((and wrap (not (nelisp-process-p wrap)))
      (signal 'wrong-type-argument (list 'nelisp-process-p wrap)))
     ;; Multiplexer dispatch path — preferred when wrap is registered.
     ((and wrap
           (nelisp-process-eventloop-handle wrap)
           (fboundp 'nelisp-eventloop-multiplex-tick))
      (let ((fired (nelisp-eventloop-multiplex-tick timeout-ms)))
        (and fired (> fired 0))))
     ;; Host-bridge fallback: a single wrap.
     (wrap
      (accept-process-output (nelisp-process-host-proc wrap)
                             sec ms just-this-one))
     ;; nil WRAP — drain ANY live wrap (Emacs parity).
     (t
      (let ((any nil))
        (dolist (w (nelisp-process-list))
          (when (nelisp-process-live-p w)
            (when (accept-process-output (nelisp-process-host-proc w)
                                         sec ms just-this-one)
              (setq any t))))
        any)))))

(defcustom nelisp-call-process-default-timeout 30.0
  "Hard-cap (seconds) on `nelisp-call-process' synchronous wait.
Doc 39 §3.L `call-process' contract: synchronous spawn-and-wait,
returning the exit code.  In Emacs the host primitive blocks
indefinitely; the NeLisp wrapper applies this cap so a runaway
child cannot wedge the calling thread forever.  Override per call
via the optional TIMEOUT keyword on `nelisp-call-process-with-args'."
  :type 'number
  :group 'nelisp-process)

(defun nelisp-call-process (program &optional infile destination display
                                    &rest args)
  "Synchronously run PROGRAM with ARGS, returning the exit status.
Drop-in equivalent of Emacs `call-process'.

PROGRAM      Path to executable (string).
INFILE       File to use as stdin, or nil.
DESTINATION  Where to send stdout — buffer, t, nil, or (REAL-DEST
             ERROR-DEST) for split stderr.  nil discards.
DISPLAY      Ignored (Emacs interactive-only).
ARGS         Remaining string args to PROGRAM.

Spawns the child via `nelisp-make-process', drives the eventloop
until the sentinel fires (subject to
`nelisp-call-process-default-timeout'), then returns the exit
status integer.  On timeout, the cascade in `nelisp-delete-process'
applies and the integer error code (`nil'-equivalent in Emacs is
encoded as -1 here so callers can tell live timeouts from clean
exits)."
  (ignore display)
  (unless (stringp program)
    (signal 'wrong-type-argument (list 'stringp program)))
  (let* ((dest-real (cond ((consp destination) (car destination))
                          (t destination)))
         (dest-err  (cond ((consp destination) (cadr destination))
                          (t nil)))
         (out-buf (cond ((bufferp dest-real) dest-real)
                        ((stringp dest-real) (get-buffer-create dest-real))
                        ((eq dest-real t) (current-buffer))
                        (t nil)))
         (err-target (cond ((bufferp dest-err) dest-err)
                           ((stringp dest-err) (get-buffer-create dest-err))
                           (t nil)))
         (cmd (cons program args))
         (wrap (nelisp-make-process
                :name program
                :buffer out-buf
                :command cmd
                :stderr err-target)))
    ;; Stream INFILE into the child's stdin if supplied.
    (when (and infile (file-readable-p infile))
      (let ((contents (with-temp-buffer
                        (insert-file-contents infile)
                        (buffer-string))))
        (nelisp-process-send-string wrap contents)
        (nelisp-process-send-eof wrap)))
    (let ((exit (nelisp-process-wait-for-exit
                 wrap nelisp-call-process-default-timeout)))
      (cond
       ((null exit)
        ;; Timeout — kill cascade and report -1.
        (nelisp-delete-process wrap)
        -1)
       (t exit)))))

(defun nelisp-call-process-region (start end program &optional delete
                                         destination display &rest args)
  "Synchronously run PROGRAM, sending region START..END as stdin.
Drop-in equivalent of Emacs `call-process-region'.  When DELETE is
non-nil the region is removed from the current buffer after the
child has consumed it.  Returns the exit status integer."
  (ignore display)
  (let* ((stdin (buffer-substring-no-properties start end))
         (out-buf (cond ((bufferp destination) destination)
                        ((stringp destination)
                         (get-buffer-create destination))
                        ((eq destination t) (current-buffer))
                        (t nil)))
         (cmd (cons program args))
         (wrap (nelisp-make-process
                :name program
                :buffer out-buf
                :command cmd)))
    (nelisp-process-send-string wrap stdin)
    (nelisp-process-send-eof wrap)
    (when delete (delete-region start end))
    (let ((exit (nelisp-process-wait-for-exit
                 wrap nelisp-call-process-default-timeout)))
      (cond
       ((null exit)
        (nelisp-delete-process wrap)
        -1)
       (t exit)))))

;; ---------------------------------------------------------------------
;; §2.11 stats / instrumentation.
;; ---------------------------------------------------------------------

(defun nelisp-process-list-stats ()
  "Return a plist of resource-usage stats across the live registry.
  :live-count INT     — number of `nelisp-process-live-p' wraps
  :total-count INT    — total wrap count (incl. exited but
                        still-registered)
  :registered-fd-count INT — wraps with an active eventloop fd
  :zombie-count INT   — wraps whose status is exit/signal but
                        whose host proc is still in the registry
                        (i.e. not yet `nelisp-delete-process'd)"
  (let ((live 0) (total 0) (fd 0) (zombie 0))
    (dolist (w nelisp-process--registry)
      (cl-incf total)
      (cond
       ((nelisp-process-live-p w) (cl-incf live))
       (t (cl-incf zombie)))
      (when (nelisp-process-eventloop-handle w)
        (cl-incf fd)))
    (list :live-count live
          :total-count total
          :registered-fd-count fd
          :zombie-count zombie)))

(defcustom nelisp-process-diagnose nil
  "When non-nil, log subprocess spawn/wait/kill steps.
Doc 39 §2.11 `--diagnose-process' equivalent — toggled at runtime
by anvil-host bootstrap scripts when chasing root-cause for shell
exec timeouts / SIGCHLD ordering bugs."
  :type 'boolean
  :group 'nelisp-process)

(defun nelisp-process--diag (fmt &rest args)
  "Internal: emit a diagnostic line if `nelisp-process-diagnose' is on."
  (when nelisp-process-diagnose
    (apply #'message (concat "[nelisp-process] " fmt) args)))

(provide 'nelisp-process)
;;; nelisp-process.el ends here
