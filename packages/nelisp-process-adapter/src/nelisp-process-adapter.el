;;; nelisp-process-adapter.el --- standard-name process API + ONE poll loop -*- lexical-binding: t; -*-
;;
;; Doc 184 P1-P3.  Builds directly on the native `nelisp-process-*'
;; builtins (Doc 184 S1.1, real spawn/pipe/poll/wait, not a stub) and on
;; `nelisp-async-core' (Doc 184 P0, the actor/generator-free timer
;; queue), closing the measured gaps in the prelude's own partial
;; standard-name adapter (`scripts/nelisp-stdlib-prelude.el', Doc 184
;; S1.2/S1.3):
;;
;;   - `make-process' silently dropped `:filter' -- fixed, `:filter' is
;;     stored and actually invoked as bytes arrive;
;;   - `process-filter'/`set-process-filter'/`process-sentinel'/
;;     `set-process-sentinel' did not exist -- added, over the same
;;     `process-put'/`process-get' plist the prelude already used for
;;     `:name'/`:sentinel'/`:stderr';
;;   - `accept-process-output' ignored PROCESS/SECONDS/MILLISEC/
;;     JUST-THIS-ONE and unconditionally drained + blocking-waited every
;;     pending process, then invoked the sentinel with the literal string
;;     "finished\n" no matter what really happened -- fixed: it now
;;     narrows to PROCESS (or every live process when nil), bounds the
;;     wait via `nelisp-async-core''s deadline machinery, honours
;;     JUST-THIS-ONE, and formats the real Emacs-shaped sentinel status
;;     string (finished / exited abnormally with code N / a real signal
;;     name) from the native `nelisp-process-status'/`-exit-status'.
;;
;; This is the file Doc 184 S2 calls the new, minimal "Layer B": it does
;; NOT load `packages/nelisp-process/src/nelisp-process.el' (the richer,
;; generator-blocked adapter, S1.4) -- that file is unreachable standalone
;; and is reference-only for keyword semantics.  Loading THIS file
;; instead UPGRADES the prelude's standard names in place (the same
;; upgrade-on-load pattern `nelisp-async-core.el' already uses for
;; `run-at-time'/`cancel-timer'/`sit-for' -- no `unless (fboundp ...)'
;; guards below, these definitions are meant to replace the prelude's).
;;
;; ONE event loop (Doc 184 S2/S3.3): a poll-set registry of live
;; processes, each polled with a zero-timeout `nelisp-process-poll' (the
;; native primitive is non-blocking by construction -- Doc 184 S3.3), the
;; OUTER blocking behaviour supplied by this file's own bounded
;; poll-and-sleep cycle whose sleep quantum is capped by
;; `nelisp-async-core''s next timer deadline.  Three call sites share
;; that exact loop:
;;
;;   1. `accept-process-output' (explicit, this file);
;;   2. `nelisp--repl-idle-pump' (implicit, wired into the `--repl'
;;      blank-line read step by `nl_repl_loop' in
;;      `scripts/nelisp-standalone-build.el' -- the prelude ships a
;;      default no-op version of this hook so REPL sessions that never
;;      load this file see no behaviour change; loading this file
;;      upgrades it to a real bounded pump);
;;   3. `run-at-time' with REPEAT: upgraded (via `(require
;;      'nelisp-async-core)') to the real deferred/repeating
;;      implementation, fired by both of the above -- the SAME
;;      `nelisp-async-core--fire-due' call this file's own wait loop
;;      already makes.
;;
;; A batch script (`--load'/`--eval', no `--repl') that never calls
;; `accept-process-output' and this file sees exactly today's
;; behaviour: nothing pumps in the background, matching Emacs's own
;; batch-mode contract (Doc 184 S2).
;;
;; `make-network-process' was OUT of scope under Doc 184 (S1.7/P4: no
;; native socket primitive family existed yet).  Doc 194 P0-P5 replaces
;; that stub: `feat/socket-primitives-p1' shipped the six raw-fd
;; primitives this file's `make-network-process'/`open-network-stream'
;; build on directly (P0: the synchronous CLIENT path; P4: `:nowait',
;; driven by THIS FILE's own poll loop via P3's nonblocking primitives;
;; P5: `:server t' auto-accept, same loop, same primitives), plus a
;; pure-elisp DNS resolver (`/etc/hosts' then DNS-over-TCP/53, Doc 194
;; S3.2) so a caller can pass a hostname, not only a literal IPv4/
;; `localhost'.  See docs/design/194-network-process-adapter.org for the
;; full design; this file is P0-P5's own "measured, not asserted"
;; implementation of that doc's S3.1/S3.2/S3.3.
;;
;;; Code:

(require 'nelisp-async-core)

;; Native `nelisp-socket-*' primitives (Doc 184 follow-on / Doc 194 S1.1,
;; `feat/socket-primitives-p1', dispatch-table entries in `scripts/nelisp-
;; standalone-build.el', linux-x86_64 raw SYSCALL, catchable
;; `nelisp-unsupported-primitive' on every other target).  Declared, like
;; the native process primitives above, so `make compile' byte-compiles
;; this file cleanly; real and reachable at runtime in `target/nelisp'.
(declare-function nelisp-socket-listen "ext:nelisp-runtime" (host port &optional nowait))
(declare-function nelisp-socket-accept "ext:nelisp-runtime" (listen-fd &optional nowait))
(declare-function nelisp-socket-connect "ext:nelisp-runtime" (host port &optional nowait))
(declare-function nelisp-socket-send "ext:nelisp-runtime" (fd string))
(declare-function nelisp-socket-recv "ext:nelisp-runtime" (fd max-bytes))
(declare-function nelisp-socket-close "ext:nelisp-runtime" (fd))
;; Doc 194 P3: the two nonblocking-I/O additions this file's poll loop
;; (P4) and auto-accept branch (P5) both drive.
(declare-function nelisp-socket-poll "ext:nelisp-runtime" (fd want-write timeout))
(declare-function nelisp-socket-connect-error "ext:nelisp-runtime" (fd))
;; Byte-level string access (Doc 161 / Doc 194 P2): NOT prelude functions,
;; native dispatch-table entries in scripts/nelisp-standalone-build.el
;; (`(:lit "string-byte")'/`(:lit "string-bytes")'), same declare-function
;; treatment as the socket primitives above.
(declare-function string-byte "ext:nelisp-runtime" (string idx))
(declare-function string-bytes "ext:nelisp-runtime" (string))

;; Native `nelisp-process-*' primitives (Doc 184 S1.1, dispatch-table
;; entries in `scripts/nelisp-standalone-build.el', not elisp defuns
;; anywhere) and the prelude's own already-shipped process helpers
;; (`scripts/nelisp-stdlib-prelude.el', not on this file's compile
;; `-L' path).  Declared so `make compile' byte-compiles this file
;; cleanly; both are real and reachable at runtime in `target/nelisp'.
(declare-function nelisp-process-start "ext:nelisp-runtime" (program &rest args))
(declare-function nelisp-process-object-p "ext:nelisp-runtime" (obj))
(declare-function nelisp-process-poll "ext:nelisp-runtime" (proc))
(declare-function nelisp-process-read-output "ext:nelisp-runtime" (proc limit))
(declare-function nelisp-process-delete "ext:nelisp-runtime" (proc))
(declare-function nelisp-process-exit-status "ext:nelisp-runtime" (proc))
(declare-function executable-find "ext:nelisp-stdlib-prelude" (command &optional remote))
(declare-function process-get "ext:nelisp-stdlib-prelude" (process key))
(declare-function process-put "ext:nelisp-stdlib-prelude" (process key value))
(declare-function process-status "ext:nelisp-stdlib-prelude" (process))

;;; Poll-set registry ---------------------------------------------------

(defvar nelisp-process-adapter--live nil
  "Process objects created by this file's `make-process', not yet fully
drained + sentinel-fired.  Populated by `make-process'; pruned by
`delete-process' and by exit-detection inside the wait loop.")

(defconst nelisp-process-adapter--poll-quantum 0.02
  "Sleep granularity (seconds) between zero-timeout `nelisp-process-poll'
passes.  `nelisp-process-poll' (Doc 184 S1.1/S3.3) is a single
non-blocking `poll(2)' call, not a wait -- the outer blocking behaviour
`accept-process-output'/`nelisp--repl-idle-pump' need comes from this
file's own bounded poll-and-sleep cycle.  Small enough that output/exit
is noticed promptly; coarse enough to cost effectively nothing at rest.")

;;; Sentinel status strings (Doc 184 S1.3/S3.2) --------------------------
;; The native exit-status encoding (`nl_bi_process_decode_status',
;; scripts/nelisp-standalone-build.el) mirrors POSIX wait-status: a
;; normal exit is the plain 0-127 code; a signal-terminated process is
;; 128+SIGNUM.  Verified against host Emacs 30.1 (`signal-process' +
;; `process-sentinel', this doc's own working notes): SIGTERM(15) ->
;; "terminated\n", SIGKILL(9) -> "killed\n".  This substrate's
;; `delete-process'/`kill-process' send SIGTERM (native
;; `nl_bi_process_delete_object' hardcodes signal 15, out of this
;; adapter's scope to change), so a process killed through this API
;; honestly reports "terminated\n", not "killed\n" -- the fix this file
;; ships is READING THE REAL STATUS at all (S1.3's defect: every exit,
;; regardless of cause, was reported as the literal string "finished\n"),
;; not hardcoding a different single wrong string in its place.

(defun nelisp-process-adapter--signal-name (code)
  "Return the lowercase Emacs-style name for signal (CODE - 128).
CODE is the native exit-status integer already known to encode a signal
termination (>= 128, per the decode convention above)."
  (let ((sig (- code 128)))
    (cond
     ((= sig 1) "hangup")
     ((= sig 2) "interrupt")
     ((= sig 3) "quit")
     ((= sig 4) "illegal instruction")
     ((= sig 6) "abort")
     ((= sig 8) "arithmetic error")
     ((= sig 9) "killed")
     ((= sig 11) "segmentation fault")
     ((= sig 13) "broken pipe")
     ((= sig 14) "alarm clock")
     ((= sig 15) "terminated")
     (t (format "signal %d" sig)))))

(defun nelisp-process-adapter--sentinel-message (proc)
  "Format PROC's real Emacs-shaped sentinel status string.
Reads the native `nelisp-process-exit-status' directly (not the
`process-exit-status' wrapper, whose `process-status' companion is not
reliable to read AFTER `delete-process' -- the native
`nl_bi_process_delete_object' always overwrites the status field to
\"deleted\" as its last step, even for a process that had already
exited normally; the exit-status field is not touched by delete and
stays the authoritative signal, per the decode convention above)."
  (let ((code (and (fboundp 'nelisp-process-exit-status)
                    (nelisp-process-exit-status proc))))
    (cond
     ((or (null code) (< code 0)) "finished\n")
     ((>= code 128) (concat (nelisp-process-adapter--signal-name code) "\n"))
     ((= code 0) "finished\n")
     (t (format "exited abnormally with code %d\n" code)))))

;;; make-process: real :filter -------------------------------------------

(defun make-process (&rest plist)
  "Doc 184 P1: standard-name `make-process', built directly on the native
`nelisp-process-*' primitives, with a real `:filter' slot the prelude's
own version (S1.2) silently dropped."
  (let* ((name (or (plist-get plist :name) "process"))
         (command (plist-get plist :command))
         (stderr-buffer (plist-get plist :stderr))
         (sentinel (plist-get plist :sentinel))
         (filter (plist-get plist :filter))
         (resolved (and command
                        (cons (or (executable-find (car command)) (car command))
                              (cdr command)))))
    (unless command (signal 'wrong-type-argument (list 'listp command)))
    (unless (fboundp 'nelisp-process-start)
      (signal 'error (list "make-process: no native process primitive (nelisp-process-start) in this runtime")))
    (let ((proc (apply #'nelisp-process-start resolved)))
      (process-put proc :name name)
      (process-put proc :sentinel sentinel)
      (process-put proc :stderr stderr-buffer)
      (process-put proc :filter filter)
      (process-put proc :adapter-sentinel-fired nil)
      (setq nelisp-process-adapter--live (cons proc nelisp-process-adapter--live))
      proc)))

(defun nelisp-make-process (&rest plist)
  (apply #'make-process plist))

;;; Filter / sentinel accessors (Doc 184 P1) ------------------------------

(defun process-filter (process)
  "Return PROCESS's filter function, or nil if none is set."
  (process-get process :filter))

(defun set-process-filter (process filter)
  "Set PROCESS's filter function to FILTER (a function of PROC and CHUNK)."
  (process-put process :filter filter)
  filter)

(defun process-sentinel (process)
  "Return PROCESS's sentinel function, or nil if none is set."
  (process-get process :sentinel))

(defun set-process-sentinel (process sentinel)
  "Set PROCESS's sentinel function to SENTINEL (a function of PROC and
the status-change STRING, Emacs's own three-shape contract -- see
`nelisp-process-adapter--sentinel-message')."
  (process-put process :sentinel sentinel)
  sentinel)

;;; delete-process: prune the registry, fire a real sentinel ------------

(defun delete-process (&optional process)
  "Doc 184 P2 (native subprocess) + Doc 194 P0 (`network-process'): kill/
reap PROCESS, prune it from the poll-set registry, and -- if it was
still live -- fire its sentinel with the real status string exactly
once, mirroring Emacs's own synchronous `delete-process' -> sentinel
behaviour (measured against host Emacs 30.1 in both docs: a live
process's sentinel fires with the real status string as part of the
same call, not on some later tick).  PROCESS defaults to nil (Emacs's
own \"current buffer's process\" default is not modelled -- this
runtime's buffer/process association is out of scope; nil here is
simply a no-op, not an error)."
  (cond
   ((and process (nelisp--network-process-p process))
    (nelisp--network-process-delete process))
   ((and process (fboundp 'nelisp-process-object-p) (nelisp-process-object-p process))
    (let ((was-running (eq (and (fboundp 'process-status) (process-status process)) 'run))
          (already-fired (process-get process :adapter-sentinel-fired)))
      (when (fboundp 'nelisp-process-delete) (nelisp-process-delete process))
      (setq nelisp-process-adapter--live (delq process nelisp-process-adapter--live))
      (when (and was-running (not already-fired))
        (process-put process :adapter-sentinel-fired t)
        (let ((sentinel (process-get process :sentinel)))
          (when sentinel
            (funcall sentinel process (nelisp-process-adapter--sentinel-message process))))))))
  nil)

(defun kill-process (&optional process _current-group)
  (delete-process process))

;;; The ONE poll loop (Doc 184 S2/S3.3) -----------------------------------

(defun nelisp-process-adapter--drain-and-fire (proc)
  "One zero-timeout poll pass over PROC: dispatch any new output to its
filter, and -- on first-observed exit -- fire its sentinel with the
real status string and prune PROC from the live registry.
Returns non-nil iff real output BYTES were delivered (matching Emacs's
own `accept-process-output' contract, measured against host Emacs
30.1: a status-only change with no output produces a nil return; only
actual bytes produce t).

Doc 194 P0-P3: `make-network-process' adds its `network-process' objects
to this SAME shared `nelisp-process-adapter--live' registry (Doc 194
S3.1's own design).  A `network-process' is dispatched to
`nelisp-process-adapter--drain-and-fire-network' (P4/P5's own real
nonblocking wiring, S3.3) instead of the native `nelisp-process-poll'
path below, which assumes the OTHER tagged-vector shape (a fixed-offset
native process object) and does not know this one."
  (if (nelisp--network-process-p proc)
      (nelisp-process-adapter--drain-and-fire-network proc)
    (let* ((ev (nelisp-process-poll proc))
           (ready (aref ev 0))
           (exited (aref ev 1))
           (got-bytes nil)
           (filter (process-get proc :filter)))
      (when (= ready 1)
        (let ((chunk (nelisp-process-read-output proc 65536)))
          (when (and chunk (> (length chunk) 0))
            (setq got-bytes t)
            (when filter (funcall filter proc chunk)))))
      (when (and (= exited 1) (not (process-get proc :adapter-sentinel-fired)))
        ;; A process can exit with a final chunk still sitting in the pipe;
        ;; drain once more before declaring it done.
        (let ((chunk (nelisp-process-read-output proc 65536)))
          (when (and chunk (> (length chunk) 0))
            (setq got-bytes t)
            (when filter (funcall filter proc chunk))))
        (process-put proc :adapter-sentinel-fired t)
        (setq nelisp-process-adapter--live (delq proc nelisp-process-adapter--live))
        (let ((sentinel (process-get proc :sentinel)))
          (when sentinel
            (funcall sentinel proc (nelisp-process-adapter--sentinel-message proc)))))
      got-bytes)))

;;; network-process nonblocking I/O (Doc 194 P4/P5, S3.3) -----------------
;; `nelisp-process-adapter--drain-and-fire''s `network-process' branch,
;; dispatching on STATUS (S3.1's state machine: `connect' -> `open' ->
;; `closed'/`failed'; `listen' for a `:server t' process's whole
;; lifetime, P5).  Every check below is a ZERO-TIMEOUT `nelisp-socket-
;; poll' probe -- the SAME non-blocking-by-construction contract the
;; native `nelisp-process-poll' path above already has; the OUTER
;; blocking behaviour still comes entirely from this file's own
;; `nelisp-process-adapter--wait' poll-and-sleep cycle, unchanged.

(defun nelisp-process-adapter--network-process-close (proc)
  "Close PROC's fd and prune it from the live registry, WITHOUT firing a
sentinel (the caller already has, or is about to, with its own specific
message -- `closed\\n' at delete time via `nelisp--network-process-
delete', `open\\n'/`failed\\n' at async-connect-completion time via
`nelisp-process-adapter--drain-and-fire-network' below).  Shared by both
so the fd-close + registry-prune step itself is not duplicated."
  (let ((fd (aref proc 3)))
    (when (integerp fd) (ignore-errors (nelisp-socket-close fd))))
  (setq nelisp-process-adapter--live (delq proc nelisp-process-adapter--live)))

(defun nelisp-process-adapter--drain-and-fire-network (proc)
  "The `network-process' counterpart of `nelisp-process-adapter--drain-
and-fire' (Doc 194 P4/P5).  Dispatches on PROC's STATUS slot (aref 2):

- `open': `nelisp-socket-poll FD nil 0' (readable?); on ready,
  `nelisp-socket-recv' up to 65536 bytes (matching the native path's own
  constant just above) and dispatch to `:filter'.  A zero-length `recv'
  (peer closed, TCP EOF) OR the `recv' call itself signalling
  `nelisp-socket-error' (e.g. ECONNRESET) both mean the same thing here
  -- the peer is gone -- and both transition STATUS to `closed' and fire
  the sentinel with `closed\\n', mirroring (not reusing, S3.1's own
  design) `nelisp--network-process-delete''s formatting.
- `connect' (nonblocking connect in flight, P4): `nelisp-socket-poll FD
  t 0' (writable?); on ready, `nelisp-socket-connect-error FD' -- `0'
  transitions to `open' and fires the sentinel with `open\\n' (measured
  against real Emacs 30.1 during this phase's implementation: exactly
  this string, newline included, no other text); a non-zero errno
  transitions to `failed', closes the fd, and fires the sentinel with
  `(format \"failed with code %d\\n\" errno)' -- measured against real
  Emacs 30.1 the SAME way: a bare `failed\\n' (this doc's own S1.3/S3.3
  prose) is NOT what real Emacs sends for an async-connect failure; the
  real string names the errno, and this substrate's own
  `nelisp-socket-connect-error' already returns that exact integer, so
  no separate errno-to-message translation is needed.
- `listen' (P5, `:server t' auto-accept): `nelisp-socket-poll FD nil 0'
  on the LISTENING fd (POLLIN on a listening socket means \"a connection
  is pending\", the same generic `poll(2)' semantics the `open' branch's
  own readable check already relies on); on ready, `nelisp-socket-accept
  FD t' (nonblocking, P3) -- a real fd wraps a NEW child `network-process'
  object, status `open', COPYING (not sharing/aliasing anything else)
  the listener's own `:filter'/`:sentinel' function objects (measured
  against real Emacs 30.1 during this phase's implementation: `(eq
  (process-filter child) (process-filter server))' is `t' -- literal
  reuse of the same function, not a copy of behaviour); calls `:log
  SERVER CHILD \"accept\\n\"' first if the listener was given one
  (measured: real Emacs calls `:log' with `\"accept from HOST\\n\"' --
  simplified here to a bare `\"accept\\n\"', matching this doc's own
  S3.3 prose exactly, because Phase 1's `nl_socket_accept_impl' requests
  no peer address at all, S1.1, so this substrate has no HOST string to
  put there), then fires the CHILD's own sentinel with `\"open\\n\"'
  (ALSO measured against real Emacs: an accepted child fires its
  sentinel immediately, with `\"open from HOST\\n\"' -- again simplified
  to `\"open\\n\"' for the same peer-address-unavailable reason).  The
  child's NAME is `SERVERNAME <fd:N>' -- real Emacs names it
  `SERVERNAME <HOST:PORT>' (the peer's own address:port, S1.3), a shape
  this substrate cannot reproduce for the same reason; `<fd:N>' is
  still a unique, informative suffix per accepted connection.  A NOWAIT
  accept racing an empty queue (`-1' sentinel, P3) between the poll
  check and the accept call -- possible in principle, never observed
  in this single-process cooperative loop, since nothing else can steal
  a pending connection between the two calls -- is simply a no-op this
  pass (nothing to wrap, tried again next pass).

Returns non-nil iff real output BYTES were delivered (`open' status
only), matching the native branch's own contract."
  (let ((status (aref proc 2))
        (fd (aref proc 3)))
    (cond
     ((eq status 'open)
      (if (eq (nelisp-socket-poll fd nil 0) t)
          (let ((chunk (condition-case nil (nelisp-socket-recv fd 65536) (error ""))))
            (if (and chunk (> (string-bytes chunk) 0))
                (progn
                  (let ((filter (process-get proc :filter)))
                    (when filter (funcall filter proc chunk)))
                  t)
              ;; Zero-length recv (peer closed) or a caught recv error
              ;; (ECONNRESET etc.) -- both mean the peer is gone.
              (aset proc 2 'closed)
              (nelisp-process-adapter--network-process-close proc)
              (let ((sentinel (process-get proc :sentinel)))
                (when sentinel (funcall sentinel proc "closed\n")))
              nil))
        nil))
     ((eq status 'connect)
      (when (eq (nelisp-socket-poll fd t 0) t)
        (let ((err (nelisp-socket-connect-error fd)))
          (if (= err 0)
              (progn
                (aset proc 2 'open)
                (let ((sentinel (process-get proc :sentinel)))
                  (when sentinel (funcall sentinel proc "open\n"))))
            (aset proc 2 'failed)
            (nelisp-process-adapter--network-process-close proc)
            (let ((sentinel (process-get proc :sentinel)))
              (when sentinel
                (funcall sentinel proc (format "failed with code %d\n" err)))))))
      nil)
     ((eq status 'listen)
      (when (eq (nelisp-socket-poll fd nil 0) t)
        (let ((cfd (nelisp-socket-accept fd t)))
          (unless (eql cfd -1)
            (let* ((cname (format "%s <fd:%d>" (aref proc 1) cfd))
                   (child (nelisp--make-network-process-object cname 'open cfd)))
              (process-put child :filter (process-get proc :filter))
              (process-put child :sentinel (process-get proc :sentinel))
              (setq nelisp-process-adapter--live (cons child nelisp-process-adapter--live))
              (let ((log (process-get proc :log)))
                (when log (funcall log proc child "accept\n")))
              (let ((sentinel (process-get child :sentinel)))
                (when sentinel (funcall sentinel child "open\n")))))))
      nil)
     (t nil))))

(defun nelisp-process-adapter--pump-once (targets stop-after-first)
  "One non-blocking pass over TARGETS.  Returns non-nil iff any target
delivered real output bytes.  When STOP-AFTER-FIRST is non-nil, stops
polling the remaining targets as soon as one has (Doc 184 S1.3's
measured defect: the old `accept-process-output' drained *every*
pending process unconditionally; this is the fix)."
  (let ((any nil))
    (catch 'nelisp-process-adapter--pump-done
      (dolist (p targets)
        (when (nelisp-process-adapter--drain-and-fire p)
          (setq any t)
          (when stop-after-first (throw 'nelisp-process-adapter--pump-done nil)))))
    any))

(defun nelisp-process-adapter--sleep-gap (next-timer-deadline wait-deadline)
  "Bound this pass's sleep by the poll quantum, the next timer deadline,
and the caller's own wait deadline -- whichever is soonest."
  (let ((now (nelisp-async-core--now))
        (gap nelisp-process-adapter--poll-quantum))
    (when next-timer-deadline (setq gap (min gap (max 0.0 (- next-timer-deadline now)))))
    (when wait-deadline (setq gap (min gap (max 0.0 (- wait-deadline now)))))
    (max gap 0.0)))

(defun nelisp-process-adapter--wait (process seconds millisec just-this-one)
  "The shared wait loop behind `accept-process-output'/
`nelisp--repl-idle-pump' (Doc 184 S2/S3.3).  PROCESS narrows polling to
one process; nil polls every live process.  SECONDS/MILLISEC bound the
wait (both nil = wait indefinitely, matching Emacs).  JUST-THIS-ONE, if
an integer, also suspends firing due timers during the wait (Emacs's
own documented JUST-THIS-ONE nuance); any other non-nil value still
narrows polling to the first ready target (already this loop's default
behaviour -- Doc 184 S1.3's fix).  Returns non-nil iff real output was
received before the deadline."
  (let* ((skip-timers (integerp just-this-one))
         ;; nil TARGETS (no PROCESS given, nothing registered) is legal and
         ;; common -- e.g. a caller waiting purely for a timer to fire
         ;; (Doc 184's own "run-at-time REPEAT through the SAME loop"
         ;; claim: this wait loop still fires due timers with an empty
         ;; poll set).  Only a non-nil PROCESS ever makes TARGETS non-nil.
         (targets (if process (list process) nelisp-process-adapter--live))
         (have-timeout (or (numberp seconds) (numberp millisec)))
         (timeout-secs (and have-timeout
                            (+ (if (numberp seconds) seconds 0)
                               (/ (if (numberp millisec) millisec 0) 1000.0))))
         (deadline (and timeout-secs (+ (nelisp-async-core--now) timeout-secs)))
         (got nil))
    (unless skip-timers (nelisp-async-core--fire-due (nelisp-async-core--now)))
    (catch 'nelisp-process-adapter--wait-done
      (while t
        (when (nelisp-process-adapter--pump-once targets t)
          (setq got t)
          (throw 'nelisp-process-adapter--wait-done nil))
        (when (and deadline (>= (nelisp-async-core--now) deadline))
          (throw 'nelisp-process-adapter--wait-done nil))
        (when (and process
                   (not (eq (and (fboundp 'process-status) (process-status process)) 'run))
                   (process-get process :adapter-sentinel-fired))
          ;; PROCESS is dead and fully drained -- nothing more will
          ;; ever arrive, so stop instead of spinning to the deadline.
          (throw 'nelisp-process-adapter--wait-done nil))
        (unless skip-timers (nelisp-async-core--fire-due (nelisp-async-core--now)))
        (let ((nd (and (not skip-timers) (nelisp-async-core--next-deadline))))
          (when (and (null deadline) (null targets) (null nd))
            ;; Nothing to poll, no timer armed, no timeout given: waiting
            ;; longer can never produce anything.  Stop instead of hanging
            ;; forever.
            (throw 'nelisp-process-adapter--wait-done nil))
          (nelisp-async-core--nanosleep (nelisp-process-adapter--sleep-gap nd deadline)))))
    got))

(defun accept-process-output (&optional process seconds millisec just-this-one)
  "Doc 184 P2: standard-name `accept-process-output' that actually reads
PROCESS/SECONDS/MILLISEC/JUST-THIS-ONE, closing S1.3's measured defect
(the prelude's own version read none of them, via `&rest _args', and
unconditionally blocking-waited + drained every pending process)."
  (nelisp-process-adapter--wait process seconds millisec just-this-one))

;;; REPL idle pump (Doc 184 P3) --------------------------------------------
;; Wired into `--repl''s blank-line read step by `nl_repl_loop'
;; (scripts/nelisp-standalone-build.el).  The prelude ships a default
;; no-op version of this hook (`scripts/nelisp-stdlib-prelude.el') so a
;; REPL session that never loads this file sees no behaviour change --
;; matching Emacs's own batch-mode contract of not pumping outside an
;; explicit wait.  Loading this file upgrades the hook to a real bounded
;; pump, sharing the exact same wait loop `accept-process-output' uses.

(defun nelisp--repl-idle-pump ()
  "Fire due timers and poll every live process once, bounded to a small
fixed window, so a timer armed from the REPL or output from a
backgrounded process becomes visible between prompts without the user
calling `accept-process-output'/`sit-for' by hand."
  (nelisp-process-adapter--wait nil 0 50 nil)
  nil)

;;; network-process: a sibling tagged-vector shape (Doc 194 S3.1) --------
;; [0]='network-process  [1]=NAME  [2]=STATUS  [3]=FD  [4]=PROPS-ALIST
;;
;; A NEW, elisp-only shape, sibling to -- not sharing slots or
;; refresh/delete logic with -- the native `nelisp-process-object-p'
;; shape: that shape's own refresh/delete call `wait4'/`kill' on a PID
;; slot a socket fd does not have (Doc 194 S1.2/S2).  STATUS is a symbol
;; matching real Emacs's own contract (measured against real Emacs 30.1,
;; Doc 194 S1.3): `open' (connected) -> `closed' (peer closed / locally
;; deleted) or `failed'; `listen' for a `:server t' process's whole
;; lifetime (not built this pass, Doc 194 P5).  Never `run'/`exit' -- that
;; is the SUBPROCESS vocabulary and must never leak onto a network
;; process.  FD is the raw integer Phase 1's `nelisp-socket-*' primitives
;; operate on directly -- no wrapping/unwrapping needed at existing call
;; sites, unlike the native process shape's pid indirection.

(defun nelisp--make-network-process-object (name status fd)
  (vector 'network-process name status fd nil))

(defun nelisp--network-process-p (object)
  (and (vectorp object) (> (length object) 0)
       (eq (aref object 0) 'network-process)))

(defun nelisp--network-process-delete (proc)
  "Close PROC's fd (`nelisp-socket-close', Doc 194 S1.1), transition its
status to `closed', prune it from the poll-set registry, and -- if it
had not already transitioned away from `open'/`listen'/`connect' -- fire
its sentinel with the network-process-specific `closed\\n' status string.
Measured against real Emacs 30.1 (Doc 194 S1.3): a network process's
delete contract is a plain `closed\\n', NOT the subprocess family's
`finished'/`exited abnormally'/signal-name shapes
`nelisp-process-adapter--sentinel-message' formats -- this is a
DIFFERENT sentinel-string vocabulary, formatted independently here
rather than reusing that function, by design (Doc 194 S3.1)."
  (let ((fd (aref proc 3))
        (was-live (memq (aref proc 2) '(open listen connect))))
    (when (integerp fd) (ignore-errors (nelisp-socket-close fd)))
    (aset proc 2 'closed)
    (setq nelisp-process-adapter--live (delq proc nelisp-process-adapter--live))
    (when was-live
      (let ((sentinel (process-get proc :sentinel)))
        (when sentinel (funcall sentinel proc "closed\n")))))
  nil)

(defun process-contact (process &optional key _no-block)
  "Doc 194 S3.1: `process-contact', new -- Doc 184 never needed it
(subprocesses have no host/port).  Reads back the `:host'/`:service'
values `make-network-process' stored via `process-put' at construction
time.  Only the shape this phase actually populates is supported; a
non-`network-process' object always answers nil, matching Emacs's own
\"no contact info\" case for e.g. a pipe process."
  ;; No `keywordp' in this runtime's prelude -- any non-nil, non-`t' KEY
  ;; (Emacs's own `:host'/`:service'/etc. convention) is treated as "one
  ;; specific field", matching `process-get''s own untyped key lookup.
  (when (nelisp--network-process-p process)
    (cond
     ((and key (not (eq key t))) (process-get process key))
     (t (list (process-get process :host) (process-get process :service))))))

;;; Host resolution (Doc 194 S3.2) -----------------------------------------
;; Phase A (feat/socket-primitives-p1, unchanged): literal `localhost' and
;; IPv4 dotted-decimal, handled entirely inside `nelisp-socket-connect'/
;; -listen's own native `nl_socket_build_sockaddr'.  Phases B/C below run
;; BEFORE those primitives are ever called, so by the time a host string
;; reaches them it is always already Phase-A-shaped -- no native change,
;; no chicken-and-egg (the DNS resolver itself, Phase C, is addressed by
;; a numeric IPv4 literal, which Phase A already reaches directly).

(defun nelisp--ipv4-dotted-p (host)
  "Return non-nil if HOST is a plausible 4-octet dotted-decimal IPv4
literal.  Mirrors `nl_socket_build_sockaddr''s own native
`nl_ipv4_parse_dotted' accept grammar (Doc 194 S1.1) closely enough to
decide whether host RESOLUTION (Phase B/C below) is needed at all --
exact per-octet range/leading-zero validation is left to that native
parser, which still runs, unchanged, after this decision either way."
  (and (stringp host)
       (let ((parts (split-string host "\\." t)))
         (and (= (length parts) 4)
              (not (memq nil (mapcar (lambda (p) (string-match-p "\\`[0-9]+\\'" p)) parts)))))))

(defun nelisp--net-host-string (host &optional family)
  "Normalize the `:host' plist value HOST the way real Emacs's own
`make-network-process' does before resolution (measured Doc 194 S1.3):
nil or the symbol `local' both mean the loopback interface.  Anything
else must already be a string; the ACTUAL resolution -- literal passthrough,
`/etc/hosts', or DNS -- happens one level up, in `nelisp--resolve-host'.

FAMILY (Doc 194 IPv6 phase, P7) is an optional new argument: when
`\\='ipv6', the nil/`local' loopback default becomes \"::1\" instead of
\"127.0.0.1\".  Every call site before this phase omits FAMILY, so this
addition changes nothing for them -- `(nelisp--net-host-string host)'
with no second argument behaves exactly as it always has."
  (cond
   ((or (null host) (eq host 'local)) (if (eq family 'ipv6) "::1" "127.0.0.1"))
   ((stringp host) host)
   (t (signal 'wrong-type-argument (list 'stringp host)))))

;;; IPv6 literal parsing, pure elisp (Doc 194 IPv6 phase (P7), SCOPE item 3)
;;; -------------------------------------------------------------------------
;; Independent of, and NOT shared with, the native `nl_ipv6_*' family in
;; `scripts/nelisp-standalone-build.el' -- that native layer builds the
;; 28-byte `sockaddr_in6' struct directly from a raw host STRING passed to
;; `nelisp-socket-listen'/-connect (mirroring how `nl_socket_build_
;; sockaddr' already handles IPv4 literals natively, unchanged).  THIS
;; layer exists for everything that needs to REASON about an IPv6 address
;; at the elisp level without a socket call: `:family' auto-detection
;; below, the dual-stack DNS decision (Phase C, further down), and
;; formatting a raw AAAA answer's 16 RDATA bytes back into a literal
;; string.  Two independent implementations of the same RFC 4291 sec 2.2
;; grammar is a real cost, but the raw socket primitives (native, no
;; elisp involved at the call site at all) and this elisp reasoning layer
;; are separate SCOPE items by design (see this doc's own P7 section) --
;; matching how Phase 1's IPv4 dotted-quad grammar ALSO already has two
;; independent checkers, `nl_ipv4_parse_dotted' (native, authoritative
;; for what a raw primitive call accepts) and `nelisp--ipv4-dotted-p'
;; (elisp, above, "close enough to decide whether resolution is needed at
;; all").

(defun nelisp--ipv6-literal-p (text)
  "Return non-nil if TEXT (already bracket-stripped) looks like an IPv6
literal -- contains a colon.  Mirrors the native `nl_ipv6_has_colon_walk'
family-detection convention (Doc 194 IPv6 phase S1.1): neither
\"localhost\" nor an IPv4 dotted-decimal literal ever contains one, so
this check can never mis-route Phase 1's own two literal forms."
  (and (stringp text) (string-match-p ":" text)))

(defun nelisp--ipv6-strip-brackets (host)
  "Strip one bracket pair (\"[::1]\" -> \"::1\"), the `open-network-
stream'/URL display form (Doc 194 IPv6 phase SCOPE item 3).  HOST
without brackets passes through unchanged."
  (if (and (stringp host) (> (length host) 1)
           (string-prefix-p "[" host) (string-suffix-p "]" host))
      (substring host 1 -1)
    host))

(defun nelisp--ipv6-take (n list)
  "First N elements of LIST, or all of LIST if shorter.  A small local
helper rather than `seq-take' -- this file is embedded as executable
prelude source into the default standalone binary (`tools/nelisp-
binary-size-baseline.txt''s own accounting), which does not load
`seq.el'."
  (if (or (<= n 0) (null list))
      nil
    (cons (car list) (nelisp--ipv6-take (1- n) (cdr list)))))

(defun nelisp--ipv6-group-value (text)
  "Parse TEXT as 1-4 hex digits -> an integer 0-65535, or nil if TEXT is
not exactly that shape (empty, too long, or a non-hex character)."
  (and (stringp text)
       (> (length text) 0)
       (<= (length text) 4)
       (string-match-p "\\`[0-9a-fA-F]+\\'" text)
       (string-to-number text 16)))

(defun nelisp--ipv6-find-dcolon-pos (text)
  "Return the character index of the FIRST \"::\" run in TEXT, or nil.
A SECOND \"::\" later in TEXT is deliberately not special-cased here --
it lands inside `nelisp--ipv6-parse-segment''s own `split-string' walk
over whichever half it falls in, which rejects it via an empty group
between two adjacent colons (mirrors the native `nl_ipv6_find_dcolon''s
own first-occurrence-only convention, S1.1 above)."
  (string-match "::" text))

(defun nelisp--ipv6-parse-segment (text)
  "Parse TEXT -- one side of a \"::\" split, or the whole literal body
when there is no \"::\" -- into a list of 16-bit GROUP integers.  An
embedded dotted-IPv4 tail (RFC 4291 sec 2.2, \"::ffff:1.2.3.4\") is only
valid as the LAST colon-separated part and expands to its own two
groups, reusing `nelisp--ipv4-dotted-p' (Phase A, above) for its own
validation.  Returns nil for an EMPTY TEXT (the compressed side of a
leading/trailing \"::\", or a genuinely empty literal) -- the CALLER
distinguishes \"empty, valid\" (a `::' compression side) from
\"malformed\" using TEXT itself, not this return value alone.  Signals
`nelisp-dns-error' on anything that is not a valid group list -- an
out-of-range hex value cannot occur (`nelisp--ipv6-group-value' already
bounds a 1-4 hex digit group to 0-65535, always representable), but a
malformed group (non-hex, too long, or an empty group from an adjacent
`::' this TEXT should never itself contain once the caller has already
removed the real `::') is exactly what this rejects."
  (if (zerop (length text))
      nil
    (let ((parts (split-string text ":"))
          (groups nil))
      (while parts
        (let ((part (car parts)))
          (cond
           ((and (null (cdr parts)) (string-match-p "\\." part))
            (unless (nelisp--ipv4-dotted-p part)
              (signal 'nelisp-dns-error
                      (list "malformed IPv6 literal (bad embedded IPv4 tail)" part)))
            (let ((octets (mapcar #'string-to-number (split-string part "\\."))))
              (when (memq nil (mapcar (lambda (o) (and (>= o 0) (<= o 255))) octets))
                (signal 'nelisp-dns-error
                        (list "malformed IPv6 literal (IPv4 tail octet out of range)" part)))
              (setq groups (append groups
                                    (list (+ (* (nth 0 octets) 256) (nth 1 octets))
                                          (+ (* (nth 2 octets) 256) (nth 3 octets)))))))
           (t
            (let ((v (nelisp--ipv6-group-value part)))
              (unless v
                (signal 'nelisp-dns-error (list "malformed IPv6 literal (bad group)" part)))
              (setq groups (append groups (list v)))))))
        (setq parts (cdr parts)))
      groups)))

(defun nelisp--ipv6-parse-groups (text)
  "Parse TEXT -- a complete RFC 4291 sec 2.2 IPv6 address body, no
brackets -- into a list of exactly 8 16-bit GROUP integers, applying
`::' zero-compression (at any position, including a bare \"::\") when
present.  Signals `nelisp-dns-error' on anything malformed: wrong group
count with no compression, too many groups even WITH compression, or
anything `nelisp--ipv6-parse-segment' itself already rejects."
  (let ((dc (nelisp--ipv6-find-dcolon-pos text)))
    (if (null dc)
        (let ((groups (nelisp--ipv6-parse-segment text)))
          (unless (= (length groups) 8)
            (signal 'nelisp-dns-error
                    (list "malformed IPv6 literal (wrong group count, no `::')" text)))
          groups)
      (let* ((left (substring text 0 dc))
             (right (substring text (+ dc 2)))
             (left-groups (nelisp--ipv6-parse-segment left))
             (right-groups (nelisp--ipv6-parse-segment right))
             (total (+ (length left-groups) (length right-groups))))
        (when (> total 8)
          (signal 'nelisp-dns-error
                  (list "malformed IPv6 literal (too many groups with `::')" text)))
        (append left-groups (make-list (- 8 total) 0) right-groups)))))

(defun nelisp--ipv6-parse (host)
  "Parse HOST (optionally bracketed, e.g. \"[::1]\") as an RFC 4291 sec
2.2 IPv6 literal, returning a list of 8 16-bit GROUP integers.  Signals
`nelisp-dns-error' if HOST is not a valid literal.  The full form, `::'
zero-compression at any position, an embedded dotted-IPv4 tail, and the
bracket form all parse (Doc 194 IPv6 phase, SCOPE item 3).  The exact
inverse of `nelisp--ipv6-unparse'; `test/nelisp-process-adapter-test.el'
composes the two and checks the GROUPS come back unchanged for a
reference table of literals (\"::\", \"::1\", a full form, \"::ffff:v4\",
and the bracketed form)."
  (nelisp--ipv6-parse-groups (nelisp--ipv6-strip-brackets host)))

(defun nelisp--ipv6-unparse (groups)
  "Format an 8-element GROUPS list (16-bit integers, `nelisp--ipv6-parse's
own return shape) as a canonical `::'-compressed IPv6 literal string
(RFC 5952: compress the LONGEST run of two-or-more consecutive zero
groups; lowercase hex, no leading zeros).  The exact inverse of
`nelisp--ipv6-parse' -- see that function's own docstring for the
round-trip test."
  (let ((best-start nil) (best-len 0) (run-start nil) (run-len 0) (i 0))
    (dolist (g groups)
      (if (= g 0)
          (progn
            (when (null run-start) (setq run-start i))
            (setq run-len (1+ run-len))
            (when (> run-len best-len) (setq best-len run-len best-start run-start)))
        (setq run-start nil run-len 0))
      (setq i (1+ i)))
    (if (and best-start (>= best-len 2))
        (concat (mapconcat (lambda (g) (format "%x" g))
                            (nelisp--ipv6-take best-start groups) ":")
                "::"
                (mapconcat (lambda (g) (format "%x" g))
                           (nthcdr (+ best-start best-len) groups) ":"))
      (mapconcat (lambda (g) (format "%x" g)) groups ":"))))

(defun nelisp--ipv6-bytes-to-groups (bytes)
  "BYTES a list of 16 raw integers (a DNS AAAA answer's RDATA, Doc 194
IPv6 phase) -> a list of 8 16-bit GROUP integers, the shared
representation `nelisp--ipv6-unparse'/`nelisp--ipv6-parse' both use."
  (let (groups (rest bytes))
    (while rest
      (push (+ (* (car rest) 256) (cadr rest)) groups)
      (setq rest (cddr rest)))
    (nreverse groups)))

(defun nelisp--ipv6-groups-to-bytes-string (groups)
  "The inverse of `nelisp--ipv6-bytes-to-groups': an 8-element GROUPS
list -> a 16-byte raw STRING.  `unibyte-string', NOT `string' -- measured
against this exact case on `target/nelisp' (not assumed, and not the
same conclusion `nelisp--dns-u16-be' above reached for ITS OWN narrower
value range): `(string 13 184)' -- 184 >= 128, a value THIS function
routinely needs to represent (any IPv6 group byte) but `nelisp--dns-u16-
be''s own callers never do (every one of THEIR values is < 128 by
construction, that function's own docstring) -- comes back as THREE
bytes (13 194 184) through `string-bytes'/`string-byte', not two:
`string' UTF-8-encodes an integer argument >= 128 as a multi-byte
CODEPOINT (exactly like it correctly does for `aref'/`length' access,
which is real Emacs's own ordinary multibyte-string behavior), it does
NOT store it as one raw byte the way `nl_alloc_str' (received socket
bytes) does.  `(unibyte-string 13 184)' measured, on the same binary, to
answer the intended two bytes (13 184) through `string-bytes'/`string-
byte', and to compose correctly under `concat' with itself and with
plain `string' fragments (measured together, not assumed).  So
`nelisp--dns-u16-be''s own docstring claim that plain `string' \"round-
trips the full 0-255 range correctly\" holds only for the narrow set of
values every EXISTING P2 call site restricts itself to (ID capped below
128*256, QTYPE/QCLASS/count fields all small enums) -- not in general;
this function is the first in this file to actually need the >= 128
half of that range, and using `unibyte-string' here is this phase's own
fix for it, out of scope to touch anything upstream of this new
function."
  (apply #'concat
         (mapcar (lambda (g) (unibyte-string (logand (ash g -8) 255) (logand g 255))) groups)))

;; Phase B -- /etc/hosts, pure elisp, zero native change (Doc 194 S3.2).

(defvar nelisp--etc-hosts-file "/etc/hosts"
  "Path consulted by `nelisp--hosts-file-lookup' (Doc 194 P1).  A test can
let-bind this to a fixture file without touching the real system table.")

(defun nelisp--hosts-file-parse-line (line)
  "Return (HOSTNAME . IP) conses for each hostname/alias on LINE, or nil.
LINE is one physical /etc/hosts line: an address followed by one or more
whitespace-separated hostnames; `#' starts a trailing comment (POSIX
hosts(5)).  IPv6 lines parse the same way -- this phase's own IPv4-only
`nl_socket_build_sockaddr' (Doc 194 S1.1) just never matches their
address string later, so a mixed A/AAAA pair for one name still lets the
usable A entry through."
  (let* ((no-comment (car (split-string line "#")))
         (fields (split-string no-comment nil t)))
    (when (>= (length fields) 2)
      (let ((ip (car fields)))
        (mapcar (lambda (h) (cons h ip)) (cdr fields))))))

(defun nelisp--hosts-file-table (&optional file)
  "Parse FILE (default `nelisp--etc-hosts-file') into an alist of
\(HOSTNAME . \"A.B.C.D\"), first matching line wins per hostname --
matching POSIX/glibc `files' lookup order."
  (let ((path (or file nelisp--etc-hosts-file))
        (table nil))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (dolist (line (split-string (buffer-string) "\n"))
          (dolist (pair (nelisp--hosts-file-parse-line line))
            (unless (assoc (car pair) table) (push pair table))))))
    (nreverse table)))

(defun nelisp--hosts-file-lookup (name &optional file)
  "Return NAME's IPv4 address string from `/etc/hosts' (or FILE), or nil
if NAME is not listed there (Doc 194 P1)."
  (cdr (assoc name (nelisp--hosts-file-table file))))

(defun nelisp--hosts-file-lookup-typed (name qtype &optional file)
  "Doc 194 IPv6 phase: like `nelisp--hosts-file-lookup' but restricted to
QTYPE's address family -- `a' (IPv4, an address string with no ':') or
`aaaa' (IPv6, one WITH a ':').  A SEPARATE scan, not built on
`nelisp--hosts-file-table' (which keeps only the FIRST line per NAME
regardless of family, Doc 194 P1's own untyped convention, unchanged) --
this one scans every line for NAME so a file listing e.g. both a
\"::1 localhost\" AND a \"127.0.0.1 localhost\" line resolves correctly
for EITHER family regardless of which line comes first, letting `\\='aaaa'
resolution reach an IPv6 entry even when an earlier IPv4 line for the
same NAME would otherwise shadow it under the untyped lookup's
first-line-wins rule."
  (let ((path (or file nelisp--etc-hosts-file)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (catch 'nelisp--hosts-file-typed-found
          (dolist (line (split-string (buffer-string) "\n"))
            (dolist (pair (nelisp--hosts-file-parse-line line))
              (when (and (equal (car pair) name)
                         (eq (if (nelisp--ipv6-literal-p (cdr pair)) 'aaaa 'a) qtype))
                (throw 'nelisp--hosts-file-typed-found (cdr pair)))))
          nil)))))

;; Phase C -- DNS-over-TCP/53, pure elisp on Phase 1's own primitives
;; (Doc 194 P2/S3.2).  RFC 7766 (mandatory-to-implement in every
;; conformant resolver): a 2-byte big-endian length prefix followed by
;; the identical wire-format message UDP DNS uses (RFC 1035 S4).  Chosen
;; over UDP specifically because `nl_socket_listen_impl'/`nl_socket_
;; connect_impl' hardcode `SOCK_STREAM' at socket-creation time (Doc 194
;; S1.1) -- TCP needs zero native changes; UDP would need a new
;; socket-type parameter (Doc 194 S6, not built here).

(define-error 'nelisp-dns-error "NeLisp DNS resolution error")

(defvar nelisp-dns-resolver-ip nil
  "Numeric IPv4 address of the DNS-over-TCP/53 resolver
`nelisp--dns-resolve-a' connects to.  nil (the default) means \"read
`/etc/resolv.conf''s first `nameserver' line, falling back to
1.1.1.1\" -- see `nelisp--dns-resolver-ip'.  Set this directly to skip
both and pin a specific resolver (e.g. a loopback DNS-over-TCP test
stub).")

(defvar nelisp-dns-resolver-port 53)

(defun nelisp--resolv-conf-nameserver (&optional file)
  "First `nameserver' line's address in /etc/resolv.conf (or FILE), or
nil if the file does not exist or has no such line.  Plain line-split +
`split-string' rather than buffer-based `re-search-forward' (not
available in this runtime's prelude, Doc 143's own regex layer covers
`string-match'/-p only) -- the same style already used by
`nelisp--hosts-file-parse-line' above."
  (let ((path (or file "/etc/resolv.conf")))
    (and (file-exists-p path)
         (with-temp-buffer
           (insert-file-contents path)
           (catch 'nelisp--resolv-conf-found
             (dolist (line (split-string (buffer-string) "\n"))
               (let ((fields (split-string line nil t)))
                 (when (and (equal (car fields) "nameserver") (cadr fields))
                   (throw 'nelisp--resolv-conf-found (cadr fields)))))
             nil)))))

(defun nelisp--dns-resolver-ip ()
  (or nelisp-dns-resolver-ip (nelisp--resolv-conf-nameserver) "1.1.1.1"))

(defun nelisp--dns-u16-be (n)
  "2-byte big-endian encoding of N (0-65535), as a raw byte string.
`string' (scripts/nelisp-stdlib-prelude.el, char-to-string + concat under
the hood), NOT `unibyte-string' -- measured against this exact byte
range on `target/nelisp' (not assumed): `unibyte-string' mishandles byte
values >= 128 in this runtime's own native implementation
(`bf_unibyte_string'/`mut-str-finalize', scripts/nelisp-standalone-
build.el) -- (length (unibyte-string 129)) answers 0 and
(aref (unibyte-string 200 201 202) 0) answers 521, not 200 -- while
`string' round-trips the full 0-255 range correctly on the same binary.
A DNS header's flags/compression-pointer/RDATA bytes routinely need
values >= 128, so this substrate bug is load-bearing here even though it
is out of P0-P2's own scope to FIX (native, scripts/nelisp-standalone-
build.el -- a future doc's concern); this file works around it entirely
in elisp, no native change needed for Doc 194 itself."
  (string (logand (ash n -8) 255) (logand n 255)))

(defun nelisp--dns-encode-qname (name)
  "Encode NAME (a dotted hostname, e.g. \"example.com\") as a DNS QNAME:
each dot-separated label prefixed by its own length byte, terminated by
a zero length byte (RFC 1035 S4.1.2)."
  (apply #'concat
         (append
          (mapcar (lambda (label) (concat (string (length label)) label))
                  (split-string name "\\." t))
          (list (string 0)))))

(defvar nelisp--dns-query-id-counter 0)

(defun nelisp--dns-next-id ()
  "Return a 15-bit query ID (0-32767, not the full 16-bit range), cycling
so consecutive lookups do not all reuse ID 0 -- not a security property
for this synchronous, one-in-flight-request-at-a-time client, just makes
a captured trace easier to read.  Capped below 128*256 deliberately:
this runtime's elisp-level string constructors (`string'/`make-string'/
`char-to-string', and `unibyte-string', independently confirmed broken
for this exact range -- see `nelisp--dns-u16-be') treat every integer
argument as a CODEPOINT and UTF-8-encode it, so a byte VALUE >= 128
built this way becomes TWO raw wire bytes, not one, corrupting this
query's own framing.  Capping the ID's high byte below 128 keeps both ID
bytes in the ASCII range, where codepoint and raw-byte encoding coincide
-- the only place in this client's own OUTGOING message a byte >= 128
could otherwise appear (every other field -- flags, counts, QTYPE/
QCLASS, hostname labels -- is already < 128 by construction)."
  (setq nelisp--dns-query-id-counter (mod (1+ nelisp--dns-query-id-counter) 32768)))

(defun nelisp--dns-encode-query (name &optional qtype)
  "Return the raw DNS-over-TCP QUERY message for NAME: the 2-byte RFC
7766 length prefix, a 12-byte header (ID, flags = recursion desired,
QDCOUNT=1, AN/NS/ARCOUNT=0), and one question (QNAME QTYPE QCLASS=1,
RFC 1035 S4.1.1/S4.1.2).

QTYPE defaults to 1 (A); Doc 194 IPv6 phase passes 28 (AAAA,
`nelisp--dns-resolve-aaaa') for the SAME wire format -- only the two
QTYPE bytes differ.  Every caller before this phase omits QTYPE, so
its own A-record query bytes are unchanged."
  (let* ((qt (or qtype 1))
         (header (concat (nelisp--dns-u16-be (nelisp--dns-next-id))
                          (string 1 0)             ; flags: RD=1
                          (nelisp--dns-u16-be 1)   ; QDCOUNT
                          (nelisp--dns-u16-be 0)   ; ANCOUNT
                          (nelisp--dns-u16-be 0)   ; NSCOUNT
                          (nelisp--dns-u16-be 0))) ; ARCOUNT
         (question (concat (nelisp--dns-encode-qname name)
                            (nelisp--dns-u16-be qt)  ; QTYPE = A (1) or AAAA (28)
                            (nelisp--dns-u16-be 1)))  ; QCLASS = IN
         (message (concat header question)))
    (concat (nelisp--dns-u16-be (string-bytes message)) message)))

(defun nelisp--dns-byte (buf i)
  "Return the raw byte (0-255) at BUF[I], or signal a catchable
`nelisp-dns-error' instead of an uncaught out-of-bounds read -- Doc 194
P2's own exit criterion: a malformed/truncated response must be a
catchable condition, never an uncaught args-out-of-range from a bare
byte-index read past the end of BUF, and never a wild read.

`string-byte'/`string-bytes' (scripts/nelisp-standalone-build.el,
\"Doc 161: byte-level access + count for byte-IO (length is now chars)\"
per that primitive's own comment), NOT `aref'/`length' -- measured
against a REAL DNS-over-TCP response from a live resolver (not assumed):
`aref'/`length' decode every string as UTF-8 to find character
boundaries (`nl_str_charlen'/`nl_u8_decode', Doc 161's own documented,
intentional design -- this runtime has no separate unibyte string type
at all), so a raw byte >= 0x80 that happens to look like a UTF-8
continuation byte gets silently absorbed into the PRECEDING character
and a lead byte gets decoded into a multi-byte codepoint -- reading a
real response's flags byte 0x81 through `aref' returned 64, not 129, and
RDATA bytes came back merged into out-of-range values like 770. Wire
bytes arriving via `nelisp-socket-recv' are stored byte-verbatim
(`nl_alloc_str', a plain memcpy, no encode/decode pass) -- it is only
`aref'/`length' that are UTF-8-aware on top of that byte-clean storage,
and `string-byte'/`string-bytes' read the same storage without going
through that decode at all.  This substrate limitation is out of P0-P2's
own scope to fix (native, scripts/nelisp-standalone-build.el -- a future
doc's concern); this file works around it entirely at the call site."
  (if (or (< i 0) (>= i (string-bytes buf)))
      (signal 'nelisp-dns-error (list "truncated DNS response" i (string-bytes buf)))
    (string-byte buf i)))

(defun nelisp--dns-u16-at (buf i)
  (+ (ash (nelisp--dns-byte buf i) 8) (nelisp--dns-byte buf (1+ i))))

(defun nelisp--dns-skip-name (buf i)
  "Return the offset just past the NAME field starting at BUF[I] (RFC
1035 S4.1.4): a run of length-prefixed labels ending either in a zero
length byte, or in a 2-byte compression pointer (the label's length byte
has both top bits set, 0xC0-0xFF).  A pointer always terminates the name
FIELD AT THIS POSITION -- the two bytes of the pointer itself are all
this occurrence of the field consumes; this client never needs the
POINTED-TO name text (it only extracts A-record RDATA, Doc 194 S3.2), so
there is no recursion into the pointer target and therefore no
compression LOOP possible in this walk at all.  Bounded regardless by
`nelisp--dns-byte''s own out-of-bounds guard: a label length that would
run off the end of BUF signals `nelisp-dns-error' instead of reading
past it."
  (let ((len (nelisp--dns-byte buf i)))
    (cond
     ((= len 0) (1+ i))
     ((= (logand len 192) 192) (+ i 2))
     (t (nelisp--dns-skip-name buf (+ i 1 len))))))

(defun nelisp--dns-parse-response (buf &optional qtype)
  "Parse BUF (one complete DNS-over-TCP response message, the 2-byte TCP
length prefix already stripped by the caller) and return the first
matching-QTYPE/IN-record address string, or nil if the answer section
has none \(NXDOMAIN and an empty ANCOUNT both land here, Doc 194 S3.2 --
neither is a parse ERROR).  Every offset computed while walking
QDCOUNT/ANCOUNT records goes through `nelisp--dns-byte'/`nelisp--dns-
skip-name', so a truncated or adversarially short RDLENGTH signals the
catchable `nelisp-dns-error' rather than reading out of bounds.

QTYPE defaults to 1 (A, 4-byte RDATA -> a dotted-decimal string, RDATA
read byte-by-byte exactly as before this phase).  Doc 194 IPv6 phase
passes 28 (AAAA, 16-byte RDATA -> a canonical IPv6 literal string via
`nelisp--ipv6-bytes-to-groups'+`nelisp--ipv6-unparse') for
`nelisp--dns-resolve-aaaa'.  Every caller before this phase omits
QTYPE, so its own only-type-1-matches condition and its own
dotted-decimal return shape for that case are both unchanged -- this
function's control flow and every A-record code path are untouched by
this addition, only a new sibling condition (QT = 28) sharing the same
bounds-checked walk."
  (let* ((qt (or qtype 1))
         (qdcount (nelisp--dns-u16-at buf 4))
         (ancount (nelisp--dns-u16-at buf 6))
         (pos 12))
    (dotimes (_ qdcount)
      (setq pos (+ (nelisp--dns-skip-name buf pos) 4)))
    (catch 'nelisp--dns-found
      (dotimes (_ ancount)
        (setq pos (nelisp--dns-skip-name buf pos))
        (let* ((rtype (nelisp--dns-u16-at buf pos))
               (rclass (nelisp--dns-u16-at buf (+ pos 2)))
               (rdlength (nelisp--dns-u16-at buf (+ pos 8)))
               (rdata-pos (+ pos 10)))
          (when (> (+ rdata-pos rdlength) (string-bytes buf))
            (signal 'nelisp-dns-error (list "RDATA runs past end of message" rdlength)))
          (when (and (= rtype 1) (= rtype qt) (= rclass 1) (= rdlength 4))
            (throw 'nelisp--dns-found
                   (format "%d.%d.%d.%d"
                           (nelisp--dns-byte buf rdata-pos)
                           (nelisp--dns-byte buf (+ rdata-pos 1))
                           (nelisp--dns-byte buf (+ rdata-pos 2))
                           (nelisp--dns-byte buf (+ rdata-pos 3)))))
          (when (and (= rtype 28) (= rtype qt) (= rclass 1) (= rdlength 16))
            (throw 'nelisp--dns-found
                   (nelisp--ipv6-unparse
                    (nelisp--ipv6-bytes-to-groups
                     (mapcar (lambda (k) (nelisp--dns-byte buf (+ rdata-pos k)))
                             (number-sequence 0 15))))))
          (setq pos (+ rdata-pos rdlength))))
      nil)))

(defun nelisp--socket-recv-exact (fd n)
  "Read exactly N bytes from FD via repeated `nelisp-socket-recv' calls --
TCP does not preserve message boundaries, so a single `recv' may return
fewer bytes than requested (Doc 194 S3.2).  Signals a catchable
`nelisp-dns-error' if the peer closes before N bytes arrive.  Byte
COUNTS throughout use `string-bytes', not `length' -- see
`nelisp--dns-byte''s own comment for why `length' (a UTF-8 character
count on this substrate) cannot be trusted for a raw wire buffer."
  (let ((acc "") (remaining n))
    (while (> remaining 0)
      (let ((chunk (nelisp-socket-recv fd remaining)))
        (when (or (null chunk) (= (string-bytes chunk) 0))
          (signal 'nelisp-dns-error
                  (list "connection closed before N bytes received" n (string-bytes acc))))
        (setq acc (concat acc chunk))
        (setq remaining (- remaining (string-bytes chunk)))))
    acc))

(defun nelisp--dns-resolve-a (name)
  "Resolve NAME's first A record over DNS-over-TCP/53 against
`nelisp--dns-resolver-ip', built entirely on Phase 1's own
`nelisp-socket-connect'/-send/-recv/-close (Doc 194 P2/S3.2).  Returns an
IPv4 dotted-decimal string, or nil if the resolver answered with zero A
records -- NOT an error; the CALLER (`nelisp--resolve-host') is what
turns an overall nil (hosts file AND DNS both came up empty) into a
signalled `nelisp-dns-error'."
  (let* ((resolver (nelisp--dns-resolver-ip))
         (query (nelisp--dns-encode-query name))
         (fd (nelisp-socket-connect resolver nelisp-dns-resolver-port)))
    (unwind-protect
        (progn
          (nelisp-socket-send fd query)
          (let* ((len-prefix (nelisp--socket-recv-exact fd 2))
                 (msg-len (+ (ash (string-byte len-prefix 0) 8) (string-byte len-prefix 1)))
                 (msg (nelisp--socket-recv-exact fd msg-len)))
            (nelisp--dns-parse-response msg)))
      (ignore-errors (nelisp-socket-close fd)))))

(defun nelisp--dns-resolve-aaaa (name)
  "Doc 194 IPv6 phase: like `nelisp--dns-resolve-a' immediately above but
queries a type-28 (AAAA) record and returns a canonical IPv6 literal
string (`nelisp--ipv6-unparse') from the first matching answer, or nil
if the resolver answered with zero AAAA records -- NOT an error, same
convention as `nelisp--dns-resolve-a''s own nil case.  A SEPARATE
function reusing the SAME wire encoder/decoder
(`nelisp--dns-encode-query'/`nelisp--dns-parse-response', both extended
with a backward-compatible optional QTYPE above) rather than a
parameterized rewrite of `nelisp--dns-resolve-a' itself, so that
function's own control flow and A-record wire bytes stay untouched."
  (let* ((resolver (nelisp--dns-resolver-ip))
         (query (nelisp--dns-encode-query name 28))
         (fd (nelisp-socket-connect resolver nelisp-dns-resolver-port)))
    (unwind-protect
        (progn
          (nelisp-socket-send fd query)
          (let* ((len-prefix (nelisp--socket-recv-exact fd 2))
                 (msg-len (+ (ash (string-byte len-prefix 0) 8) (string-byte len-prefix 1)))
                 (msg (nelisp--socket-recv-exact fd msg-len)))
            (nelisp--dns-parse-response msg 28)))
      (ignore-errors (nelisp-socket-close fd)))))

(defun nelisp--resolve-host (host)
  "Return an IPv4 dotted-decimal string or \"localhost\" for HOST, ready
for `nelisp-socket-connect'/-listen (Doc 194 S3.2).  Fallback chain:
already-literal hosts (Phase A, feat/socket-primitives-p1, unchanged)
pass straight through; otherwise `/etc/hosts' (Phase B); otherwise
DNS-over-TCP/53 (Phase C).  Signals a catchable `nelisp-dns-error' when
NO phase resolves it -- never a hang, never a wrong-address connect (Doc
194 P1 exit criterion); a hard failure INSIDE Phase C (resolver
unreachable, malformed response) propagates its own `nelisp-dns-error'
unchanged, it is not re-wrapped as \"unresolvable\" here."
  (cond
   ((or (equal host "localhost") (nelisp--ipv4-dotted-p host)) host)
   ((nelisp--hosts-file-lookup host))
   ((nelisp--dns-resolve-a host))
   (t (signal 'nelisp-dns-error (list "unresolvable host" host)))))

(defun nelisp--net-effective-family (plist)
  "Doc 194 IPv6 phase (P7): decide whether a `make-network-process' PLIST
should route through the IPv6 path.  Real Emacs's own `:family' keyword
(measured Doc 194 S1.3) plus HOST-STRING auto-detection: an explicit
`:family \\='ipv6', or a `:host' that is ALREADY an IPv6 literal
(bracketed or not), both mean `ipv6'.  Anything else -- no `:family', or
`:family \\='ipv4', with a `:host' that is not an IPv6 literal -- means
`ipv4', routing through the EXACT SAME, unmodified code every existing
caller already used before this phase.  This function returning `ipv4'
must never change what happens next, only NAME what already happens --
the TOP CONSTRAINT this whole phase is built around."
  (let ((family (plist-get plist :family))
        (host (plist-get plist :host)))
    (if (or (eq family 'ipv6)
            (and (stringp host)
                 (nelisp--ipv6-literal-p (nelisp--ipv6-strip-brackets host))))
        'ipv6
      'ipv4)))

(defun nelisp--resolve-host-family (host family)
  "Doc 194 IPv6 phase (P7): like `nelisp--resolve-host' immediately above
-- Phase A/B/C's own SAME fallback chain and SAME catchable
`nelisp-dns-error' contract -- but FAMILY-aware, and used ONLY when
`nelisp--net-effective-family' has already decided `ipv6' (never called
for the default/`ipv4' case, so `nelisp--resolve-host' itself needs no
change and every existing caller's behavior is untouched).  FAMILY here
is always `\\='ipv6' in this phase's own callers, but the general
`a'/`aaaa' machinery below (typed hosts-file lookup, separate AAAA/A DNS
queries) is written to support `\\='ipv4' too, for symmetry and so a
`nelisp--net-effective-family' extension never needs to touch this
function's own logic again.

Dual-stack order for a NAME with no family hint reaching this function
at all would be AAAA-then-A (see the ordering below) -- but Doc 194 IPv6
phase's own callers (`make-network-process'/`nelisp--network-process-
connect-nowait') only ever invoke this with FAMILY bound, exactly
`\\='ipv6', because `nelisp--net-effective-family' has already gated the
call; this is recorded here as the general policy this function
implements, not as a currently-reachable no-hint code path.  This is a
deliberate, documented simplification of RFC 6724 (Default Address
Selection for IPv6) -- real `getaddrinfo(3)' with `AF_UNSPEC' queries
BOTH families and SORTS the combined candidate list by RFC 6724 sec 6's
destination-address-selection rules (source-address availability,
policy-table match, longest matching prefix, ...), which needs real
interface/route introspection this substrate does not have.
AAAA-first-then-A is the simpler convention RFC 6724 sec 10.3 itself
notes many implementations already use, and is what this substrate can
implement with only the primitives Doc 194 already has."
  (let ((stripped (nelisp--ipv6-strip-brackets host)))
    (cond
     ((nelisp--ipv6-literal-p stripped) stripped)
     ((and (not (eq family 'ipv6)) (or (equal host "localhost") (nelisp--ipv4-dotted-p host)))
      host)
     ((and (not (eq family 'ipv4)) (nelisp--hosts-file-lookup-typed host 'aaaa)))
     ((and (not (eq family 'ipv6)) (nelisp--hosts-file-lookup host)))
     ((and (not (eq family 'ipv4)) (nelisp--dns-resolve-aaaa host)))
     ((and (not (eq family 'ipv6)) (nelisp--dns-resolve-a host)))
     (t (signal 'nelisp-dns-error (list "unresolvable host" host family))))))

;;; make-network-process / open-network-stream: the synchronous client
;;; path (Doc 194 P0) -------------------------------------------------------
;; `:host'+`:service', no `:server', no `:nowait': a direct, one-to-one
;; wrap of `nelisp-resolve-host'+`nelisp-socket-connect' that needs ZERO
;; poll-loop involvement (measured against real Emacs 30.1, Doc 194 S1.3:
;; a blocking connect completes synchronously inside `make-network-
;; process' itself, `process-status' reads `open' the instant it
;; returns, before any `accept-process-output' call).  `:nowait t'
;; (Doc 194 P4, `nelisp--network-process-connect-nowait' below) and
;; `:server t' (Doc 194 P5, `nelisp--network-process-listen' below) are
;; both real now -- P0-P2's own "signals loudly rather than silently
;; degrading" guard for each is gone, replaced by the real thing.

(defun nelisp--network-process-signal-refused (plist err)
  "Translate a caught error ERR (`nelisp-socket-error' from a refused/
failed connect, or `nelisp-dns-error' from unresolvable host resolution)
into the `file-error' shape real Emacs signals synchronously from
`make-network-process' on a refused connect (measured Doc 194 S1.3):
\(file-error \"make client process failed\" ... :name :host :service
:family).  Exact message-string parity is not attempted (Doc 194 S6) --
this exists so the common `condition-case ((file-error) ...)' idiom
every existing `open-network-stream' caller already uses keeps working."
  (signal 'file-error
          (list "make client process failed"
                (format "%S" err)
                :name (plist-get plist :name)
                :host (plist-get plist :host)
                :service (plist-get plist :service)
                :family (or (plist-get plist :family) 'ipv4))))

(defun nelisp--network-process-populate (proc plist)
  "Store PLIST's `:host'/`:service'/`:filter'/`:sentinel' onto PROC
\(shared by every `make-network-process' branch -- synchronous client,
P4's `:nowait', P5's `:server'/child processes -- so the plist-to-props
mapping is written once)."
  (process-put proc :host (plist-get plist :host))
  (process-put proc :service (plist-get plist :service))
  (process-put proc :filter (plist-get plist :filter))
  (process-put proc :sentinel (plist-get plist :sentinel))
  proc)

(defun nelisp--network-process-connect-nowait (plist)
  "Doc 194 P4: `make-network-process' with `:nowait t' -- a stream
client that returns immediately without waiting for the connect to
complete (measured against real Emacs 30.1 during this phase's
implementation, S1.3/S3.3): `process-status' reads `connect' the
instant this function returns; `nelisp-process-adapter--drain-and-fire-
network''s `connect' branch drives the async connect to completion (via
`nelisp-socket-poll'/`nelisp-socket-connect-error', P3) and fires the
SENTINEL exactly once with `open\\n' (success) or, MEASURED against real
Emacs -- doc 194's own S1.3/S3.3 prose says a bare `failed\\n', which
this measurement found is NOT what real Emacs actually sends --
`(format \"failed with code %d\\n\" ERRNO)' (failure).  Host resolution
(`nelisp--resolve-host', S3.2) still happens SYNCHRONOUSLY here, exactly
like the non-`:nowait' path just above -- a hostname that cannot be
resolved at all never reaches a real connect(2) attempt, so it is
still reported the SAME synchronous `file-error' way (measured Emacs
does not defer THAT class of failure to a sentinel either, since no
connection attempt ever began).  A HARD, SYNCHRONOUS `nelisp-socket-
error' from `nelisp-socket-connect' itself (not the soft EINPROGRESS
outcome `nl_socket_connect_impl' already treats as success under
NOWAIT, P3) is the one case genuinely specific to this branch: real
Emacs's own `:nowait' contract is deferring success/failure reporting
to the sentinel, not raising a synchronous condition, so this creates
the `network-process' object anyway (status `failed', no live fd) and
fires the SAME sentinel shape the async poll-driven path uses,
immediately rather than on a later poll pass.

Doc 194 IPv6 phase (P7): `nelisp--net-effective-family' decides `ipv4'
vs `ipv6' from PLIST exactly as the synchronous client path does
(`make-network-process' below) -- the `ipv4' branch here is the
UNMODIFIED original call, `(nelisp--resolve-host (nelisp--net-host-
string host))', byte-identical to every build before this phase."
  (let* ((host (plist-get plist :host))
         (service (plist-get plist :service))
         (name (or (plist-get plist :name) "network"))
         (family (nelisp--net-effective-family plist))
         (resolved-host (condition-case err
                             (if (eq family 'ipv6)
                                 (nelisp--resolve-host-family
                                  (nelisp--net-host-string host 'ipv6) 'ipv6)
                               (nelisp--resolve-host (nelisp--net-host-string host)))
                           (error (nelisp--network-process-signal-refused plist err)))))
    (condition-case err
        (let* ((fd (nelisp-socket-connect resolved-host service t))
               (proc (nelisp--make-network-process-object name 'connect fd)))
          (nelisp--network-process-populate proc plist)
          (setq nelisp-process-adapter--live (cons proc nelisp-process-adapter--live))
          proc)
      (nelisp-socket-error
       (let* ((errno (- (or (car (cdr err)) 0)))
              (proc (nelisp--make-network-process-object name 'failed -1)))
         (nelisp--network-process-populate proc plist)
         (let ((sentinel (process-get proc :sentinel)))
           (when sentinel (funcall sentinel proc (format "failed with code %d\n" errno))))
         proc)))))

(defun nelisp--network-process-listen (plist)
  "Doc 194 P5: `make-network-process' with `:server t' -- a LISTENING
`network-process' (status `listen' for its whole lifetime, S3.1/S3.3).
`:host' is NOT resolved via `nelisp--resolve-host' the way a client's is
-- a listener binds a LOCAL interface address, it does not connect to a
remote one, so DNS/`/etc/hosts' resolution does not apply; only
`nelisp--net-host-string''s nil/`local' -> \"127.0.0.1\" normalization
runs, matching Phase 1's own bind-time host grammar (S1.1: literal
`localhost'/IPv4 dotted-decimal only) directly.  Built on `nelisp-
socket-listen HOST SERVICE t' (P3's NOWAIT extension -- REQUIRED here,
not optional: P5 depends on P3's nonblocking accept, S4 P5's own text --
a blocking `:server t' would freeze the whole poll loop on every accept
attempt).  `:log' (server-only, real Emacs: `(SERVER CLIENT MESSAGE)' on
each accept, S1.3) is stored on the listener's own props-alist for
`nelisp-process-adapter--drain-and-fire-network''s `listen' branch to
read back.  `:service 0' (kernel-assigned ephemeral port) is explicitly
NOT supported -- `getsockname(2)' to read the real port back does not
exist yet (S6's own open question); pass an explicit port.

Doc 194 IPv6 phase (P7): `nelisp--net-effective-family' decides `ipv4'
vs `ipv6' from PLIST (an explicit `:family \\='ipv6', or an already-IPv6
`:host' literal); the resulting FAMILY is passed to `nelisp--net-host-
string' so the nil/`local' loopback default becomes \"::1\" instead of
\"127.0.0.1\" -- the only behavior change; an explicit `:host' string
(v4 or v6 literal) passes straight through exactly as before, since
`nelisp-socket-listen' itself now decides AF_INET vs AF_INET6 natively
from the string (Doc 194 IPv6 phase, `scripts/nelisp-standalone-
build.el')."
  (let* ((name (or (plist-get plist :name) "network"))
         (host (plist-get plist :host))
         (service (plist-get plist :service))
         (family (nelisp--net-effective-family plist))
         (fd (condition-case err
                 (nelisp-socket-listen (nelisp--net-host-string host family) service t)
               (error (nelisp--network-process-signal-refused plist err))))
         (proc (nelisp--make-network-process-object name 'listen fd)))
    (nelisp--network-process-populate proc plist)
    (process-put proc :log (plist-get plist :log))
    (setq nelisp-process-adapter--live (cons proc nelisp-process-adapter--live))
    proc))

(defun make-network-process (&rest plist)
  "Doc 194 P0/P4/P5: standard-name `make-network-process'.  The
synchronous CLIENT path (no `:server', no `:nowait') is built directly
on `nelisp-socket-connect' plus `nelisp--resolve-host' (S3.1/S3.2).
`:nowait t' delegates to `nelisp--network-process-connect-nowait' (P4).
`:server t' delegates to `nelisp--network-process-listen' (P5).

Doc 194 IPv6 phase (P7): `:family \\='ipv6' (real Emacs's own keyword,
S1.3), OR a `:host' string that is already an IPv6 literal (Emacs's own
`:family' AUTO-DETECTION from the host, SCOPE item 5) routes the
synchronous client path through `nelisp--resolve-host-family'/`::1'
instead of `nelisp--resolve-host'/`127.0.0.1' -- decided once by
`nelisp--net-effective-family', shared with the `:nowait'/`:server'
branches (each function's own docstring above).  Every call that does
NOT trigger either condition takes the exact `ipv4' branch below,
UNCHANGED from before this phase."
  (let* ((service (plist-get plist :service))
         (server (plist-get plist :server))
         (nowait (plist-get plist :nowait)))
    (cond
     (server
      (unless (integerp service) (signal 'wrong-type-argument (list 'integerp service)))
      (nelisp--network-process-listen plist))
     (nowait
      (unless (integerp service) (signal 'wrong-type-argument (list 'integerp service)))
      (nelisp--network-process-connect-nowait plist))
     (t
      (unless (integerp service) (signal 'wrong-type-argument (list 'integerp service)))
      (let* ((name (or (plist-get plist :name) "network"))
             (host (plist-get plist :host))
             (family (nelisp--net-effective-family plist))
             (fd (condition-case err
                     (if (eq family 'ipv6)
                         (nelisp-socket-connect
                          (nelisp--resolve-host-family
                           (nelisp--net-host-string host 'ipv6) 'ipv6)
                          service)
                       (nelisp-socket-connect
                        (nelisp--resolve-host (nelisp--net-host-string host)) service))
                   (error (nelisp--network-process-signal-refused plist err))))
             (proc (nelisp--make-network-process-object name 'open fd)))
        (nelisp--network-process-populate proc plist)
        (setq nelisp-process-adapter--live (cons proc nelisp-process-adapter--live))
        proc)))))

(defun process-send-string (process string)
  "Doc 194 P0: send STRING's raw bytes to PROCESS, a `network-process'
object, via `nelisp-socket-send' (Doc 194 S1.1: byte-clean, UTF-8
multibyte content included, no decode/re-encode pass).  Subprocesses
have no analog under THIS name in this runtime yet -- Doc 184's own
`nelisp-process-write' is that shape's separate entry point -- so
anything that is not a `network-process' signals `wrong-type-argument'
rather than silently doing nothing."
  (if (nelisp--network-process-p process)
      (nelisp-socket-send (aref process 3) string)
    (signal 'wrong-type-argument (list 'nelisp--network-process-p process))))

(defun open-network-stream (name buffer host service &rest parameters)
  "Doc 194 P0: `open-network-stream', a thin wrapper over
`make-network-process' with a positional signature (measured against
real Emacs 30.1, Doc 194 S1.3/S3.1's own documented reduction).  BUFFER
is accepted for signature compatibility but not otherwise used --
buffer/process association is out of this doc's scope, same as Doc
184's own precedent for subprocesses."
  (apply #'make-network-process :name name :buffer buffer
         :host host :service service parameters))

(provide 'nelisp-process-adapter)
;;; nelisp-process-adapter.el ends here
