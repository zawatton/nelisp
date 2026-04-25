;;; nelisp-eventloop-multiplex.el --- Phase 9d.K fd multiplexer -*- lexical-binding: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Doc 39 LOCKED-2026-04-25-v2 §3.K — NeLisp eventloop multiplexer.
;;
;; This is the *standalone* fd polling layer that anchors v1.0 condition
;; #4 (4-core C-runtime: process / eventloop / signal / fd hygiene).
;; It complements `src/nelisp-eventloop.el' (Phase 5-B.4 dispatcher +
;; key/timer/quit binding table) by adding a real-fd multiplexer that
;; — once Phase 7.5 lands the FFI bridge — calls into the Rust
;; `nl_syscall_select' wrapper (T100 SHIPPED, see
;; `nelisp-runtime/src/syscall/process.rs:409').  Until then it ships a
;; *bootstrap backend* that drives the same dispatch through host
;; Emacs process objects WITHOUT delegating the dispatch loop itself
;; to `accept-process-output' — the dispatcher is `nelisp-eventloop-
;; multiplex-tick', readiness probing is the only host hop.
;;
;; The split mirrors the precedent set by `nelisp-filenotify.el'
;; (Phase 9d.A4): public API is final, downstream consumers can
;; integrate today, the Rust path swaps in via `set-backend' once the
;; FFI binding lands with zero call-site churn.
;;
;; Public API:
;;
;;   - `nelisp-eventloop-select-fd READ-FDS WRITE-FDS EXCEPT-FDS &optional TIMEOUT-MS'
;;     → plist (:ready (FD ...) :status :ready | :timeout)
;;     Wraps `nl_syscall_select' once Phase 7.5 lands; the bootstrap
;;     backend interprets FDs as opaque integer handles registered via
;;     `nelisp-eventloop-multiplex-register-process-fd'.
;;
;;   - `nelisp-eventloop-multiplex-register-fd FD TYPE CALLBACK &optional CONTEXT'
;;     TYPE is `read' / `write' / `except'.
;;     CALLBACK is a 2-arg fn (FD CONTEXT) invoked when FD is ready.
;;     Returns an opaque handle.
;;
;;   - `nelisp-eventloop-multiplex-unregister-fd HANDLE'
;;
;;   - `nelisp-eventloop-multiplex-tick &optional TIMEOUT-MS'
;;     One iteration: collect registered fds, select, dispatch ready
;;     callbacks, return number of callbacks fired.
;;
;;   - `nelisp-eventloop-multiplex-run'
;;     Loop until `nelisp-eventloop-multiplex-stop' is called.
;;
;;   - `nelisp-eventloop-multiplex-stop'
;;
;;   - `nelisp-eventloop-as-event-source-pull-fn FD'
;;     Doc 34 §2.4 adapter — returns a 0-arg pull-fn that
;;     consumers can hand to `nelisp-event-source-register'.
;;
;;   - `nelisp-eventloop-multiplex-set-backend BACKEND'
;;     `'host-bridge' (default, ships in this commit) or `'rust-ffi'
;;     (reserved for Phase 7.5).
;;
;; Bootstrap backend ("`'host-bridge'"):
;;
;;   The bootstrap maps NeLisp fd integers (allocated by
;;   `nelisp-eventloop-multiplex-register-process-fd') onto host Emacs
;;   process objects.  Readiness is probed by `process-status' +
;;   `accept-process-output PROC 0' (zero-timeout poll, *not* a
;;   dispatch loop hand-off — just the moral equivalent of a single
;;   `select(2)' / `read' attempt).  The dispatcher itself stays in
;;   `tick' so callbacks fire at ours, not host's, scheduling boundary.
;;
;;   When Phase 7.5 lands the elisp → Rust FFI bridge, the
;;   `'rust-ffi' backend will pack `read-fds' / `write-fds' /
;;   `except-fds' into `libc::fd_set' bitmaps via the Phase 7.0
;;   `nelisp_syscall_mmap' heap-blob path and call
;;   `nl_syscall_select' once per tick.  The Lisp-side API is final.
;;
;; Doc 34 §2.4 contract (LOCKED v2):
;;
;;   `EVENT_SOURCE_CONTRACT_VERSION = 1' is preserved.  The adapter
;;   `nelisp-eventloop-as-event-source-pull-fn' returns a 0-arg
;;   function that, when invoked by `nelisp-event-source-pull',
;;   non-blocking-reads up to one event from the underlying fd and
;;   returns either the event payload or nil.  No new contract
;;   version is introduced; this module is strictly the
;;   implementation source for an existing pull-fn shape.
;;
;; Out of scope for this Phase 9d.K (per Doc 39 §1.6):
;;
;;   - `nelisp-process.el' rewrite (= Phase 9d.L)
;;   - `run-at-time' / `run-with-timer' integration (= Phase 9d.A8)
;;   - multi-thread eventloop (= Phase 11+)
;;   - `epoll(2)' / `kqueue(2)' (= Phase 9d.K' upgrade path)

;;; Code:

(require 'cl-lib)
(require 'nelisp-eventloop)

;; ---------------------------------------------------------------------
;; Backend selection.
;;
;; Phase 9d.K ships exactly one live backend (`'host-bridge').  The
;; `'rust-ffi' branch is reserved for the post Phase 7.5 wire-up; it
;; currently signals `nelisp-eventloop-multiplex-todo' so a premature
;; switch surfaces clearly instead of silently falling back.
;; ---------------------------------------------------------------------

(defvar nelisp-eventloop-multiplex-backend 'host-bridge
  "Active fd-multiplexer backend.  One of:
  `host-bridge' — bootstrap via host Emacs process objects (Phase 9d.K default)
  `rust-ffi'    — `nl_syscall_select' via Phase 7.5 FFI bridge")

(define-error 'nelisp-eventloop-multiplex-todo
  "nelisp-eventloop-multiplex backend not yet wired (Phase 7.5 territory)")

(defun nelisp-eventloop-multiplex-set-backend (backend)
  "Switch the active backend.  See `nelisp-eventloop-multiplex-backend'."
  (unless (memq backend '(host-bridge rust-ffi))
    (signal 'wrong-type-argument
            (list 'nelisp-eventloop-multiplex-backend backend)))
  (setq nelisp-eventloop-multiplex-backend backend))

;; ---------------------------------------------------------------------
;; fd-proxy registry.
;;
;; The `'host-bridge' bootstrap maps synthetic NeLisp fd integers
;; (allocated monotonically) onto host Emacs process objects.  The
;; integer flows through the public select-fd / register-fd API so the
;; surface is identical to what the `'rust-ffi' backend will see once
;; Phase 7.5 lands real OS fds.
;; ---------------------------------------------------------------------

(defvar nelisp-eventloop-multiplex--fd-proxy-table (make-hash-table :test 'eql)
  "Hash table FD-INT -> process object (`'host-bridge' bootstrap only).")

(defvar nelisp-eventloop-multiplex--next-fd 100
  "Counter for synthetic fd integers (start above stdin/out/err).")

(defun nelisp-eventloop-multiplex--reset-fd-proxy ()
  "Test helper — clear the fd-proxy table."
  (clrhash nelisp-eventloop-multiplex--fd-proxy-table)
  (setq nelisp-eventloop-multiplex--next-fd 100))

(defun nelisp-eventloop-multiplex-register-process-fd (proc)
  "Allocate a synthetic NeLisp fd for host process PROC.
Returns the integer fd.  Bootstrap-only entry point."
  (unless (processp proc)
    (signal 'wrong-type-argument (list 'processp proc)))
  (let ((fd (cl-incf nelisp-eventloop-multiplex--next-fd)))
    (puthash fd proc nelisp-eventloop-multiplex--fd-proxy-table)
    fd))

(defun nelisp-eventloop-multiplex-unregister-process-fd (fd)
  "Drop the fd-proxy mapping for FD.  No-op if absent."
  (remhash fd nelisp-eventloop-multiplex--fd-proxy-table))

(defun nelisp-eventloop-multiplex--fd-proc (fd)
  "Return the process bound to FD via `register-process-fd', or nil."
  (gethash fd nelisp-eventloop-multiplex--fd-proxy-table))

;; ---------------------------------------------------------------------
;; select-fd primitive.
;;
;; Returns a plist:
;;   (:ready (FD ...) :status :ready)   — at least one fd ready
;;   (:ready ()      :status :timeout)  — no fd ready within TIMEOUT-MS
;;
;; TIMEOUT-MS:
;;   nil — block indefinitely (capped at 60_000 ms in bootstrap to
;;         keep ERT pathological-case bounded; real `'rust-ffi' backend
;;         will pass NULL timeval through to `select(2)')
;;   0   — non-blocking poll
;;   > 0 — block up to TIMEOUT-MS milliseconds
;; ---------------------------------------------------------------------

(defcustom nelisp-eventloop-multiplex-bootstrap-block-cap-ms 60000
  "Cap on `nelisp-eventloop-select-fd' nil-timeout in `'host-bridge'.
The Rust `'rust-ffi' backend will honour true blocking semantics via
the underlying `select(2)' NULL `struct timeval' path."
  :type 'integer
  :group 'nelisp-eventloop)

(defcustom nelisp-eventloop-multiplex-bootstrap-poll-step-ms 5
  "Inter-poll sleep between bootstrap select-fd readiness rounds.
Smaller = lower latency at higher CPU; larger = higher latency at
lower CPU.  Chosen at 5ms = 200Hz which matches Emacs `display-time'
default cadence and the Doc 39 `process-eventloop-tick-ms' baseline."
  :type 'integer
  :group 'nelisp-eventloop)

(defun nelisp-eventloop--multiplex-fd-ready-p (fd direction)
  "Return non-nil iff FD is ready for DIRECTION (`read' / `write' / `except').
Bootstrap (`'host-bridge'): consults the fd-proxy table and probes
the bound process via `accept-process-output PROC 0' (zero-timeout
single-shot, *not* a dispatch hand-off)."
  (let ((proc (nelisp-eventloop-multiplex--fd-proc fd)))
    (cond
     ((null proc) nil)
     ((not (process-live-p proc))
      ;; Closed / exited processes are read-ready (EOF) — matches
      ;; select(2) semantics where a closed peer returns READ + 0 bytes.
      (eq direction 'read))
     ((eq direction 'read)
      ;; A buffer with pending output, OR accept-process-output returns
      ;; non-nil meaning data was actually received this poll = ready.
      (let* ((buf (process-buffer proc))
             (pending (and buf (buffer-live-p buf)
                           (with-current-buffer buf
                             (> (buffer-size) 0)))))
        (or pending
            (accept-process-output proc 0 0 t))))
     ((eq direction 'write)
      ;; Pipe / network processes are usually writable when live.  For
      ;; bootstrap we treat any live process as write-ready; the
      ;; `'rust-ffi' backend will honour real write-fds via select(2).
      t)
     ((eq direction 'except)
      ;; Bootstrap has no OOB-data abstraction; fall through to nil.
      nil)
     (t nil))))

(defun nelisp-eventloop-select-fd (read-fds write-fds except-fds &optional timeout-ms)
  "Wait until any FD in READ-FDS / WRITE-FDS / EXCEPT-FDS is ready.

READ-FDS, WRITE-FDS, EXCEPT-FDS are lists of fd integers.
TIMEOUT-MS is nil (block, capped by
`nelisp-eventloop-multiplex-bootstrap-block-cap-ms' in `host-bridge')
or a non-negative integer ms.

Returns plist:
  (:ready (FD ...) :status :ready)   — one or more ready fds
  (:ready ()      :status :timeout)  — no fd ready before timeout

The Doc 39 §3.K Rust call format
   nfds = max(all fds)+1
   readfds/writefds/exceptfds = libc::fd_set bitmap pointer
   timeout = struct timeval *
will be honoured by the `'rust-ffi' backend once Phase 7.5 lands the
elisp → Rust FFI bridge.  The `host-bridge' bootstrap implements the
same observable contract via host Emacs process objects."
  (when (and timeout-ms (< timeout-ms 0))
    (signal 'wrong-type-argument (list 'natnump timeout-ms)))
  (pcase nelisp-eventloop-multiplex-backend
    ('host-bridge
     (nelisp-eventloop-multiplex--select-host-bridge
      read-fds write-fds except-fds timeout-ms))
    ('rust-ffi
     (signal 'nelisp-eventloop-multiplex-todo
             (list 'rust-ffi 'select-fd 'phase-7.5)))
    (_ (signal 'wrong-type-argument
               (list 'nelisp-eventloop-multiplex-backend
                     nelisp-eventloop-multiplex-backend)))))

(defun nelisp-eventloop-multiplex--probe-once (read-fds write-fds except-fds)
  "Return list of READY fds (from any of the three sets, deduped)."
  (let ((ready nil))
    (dolist (fd read-fds)
      (when (nelisp-eventloop--multiplex-fd-ready-p fd 'read)
        (push fd ready)))
    (dolist (fd write-fds)
      (when (and (not (memq fd ready))
                 (nelisp-eventloop--multiplex-fd-ready-p fd 'write))
        (push fd ready)))
    (dolist (fd except-fds)
      (when (and (not (memq fd ready))
                 (nelisp-eventloop--multiplex-fd-ready-p fd 'except))
        (push fd ready)))
    (nreverse ready)))

(defun nelisp-eventloop-multiplex--select-host-bridge
    (read-fds write-fds except-fds timeout-ms)
  "Bootstrap `'host-bridge' implementation of `nelisp-eventloop-select-fd'."
  (let* ((cap nelisp-eventloop-multiplex-bootstrap-block-cap-ms)
         (effective (cond ((null timeout-ms) cap)
                          ((zerop timeout-ms) 0)
                          (t (min timeout-ms cap))))
         (step (max 1 nelisp-eventloop-multiplex-bootstrap-poll-step-ms))
         (deadline (+ (* (float-time) 1000.0) effective))
         (ready (nelisp-eventloop-multiplex--probe-once
                 read-fds write-fds except-fds)))
    (cond
     (ready (list :ready ready :status :ready))
     ((zerop effective) (list :ready nil :status :timeout))
     (t
      ;; Spin until ready or deadline.  We sleep `step' ms between
      ;; probes — 0ms here would burn a core for no benefit since
      ;; `accept-process-output ... 0' returns instantly.
      (let ((found nil))
        (while (and (null found)
                    (< (* (float-time) 1000.0) deadline))
          (sleep-for (/ step 1000.0))
          (setq found (nelisp-eventloop-multiplex--probe-once
                       read-fds write-fds except-fds)))
        (if found
            (list :ready found :status :ready)
          (list :ready nil :status :timeout)))))))

;; ---------------------------------------------------------------------
;; fd registration table.
;;
;; A handle is a vector [HANDLE-MARKER FD TYPE CALLBACK CONTEXT ALIVE].
;; ---------------------------------------------------------------------

(defvar nelisp-eventloop-multiplex--registered nil
  "List of registration vectors [`:registration' FD TYPE CALLBACK CONTEXT ALIVE].
Newest first; `nelisp-eventloop-multiplex-tick' walks it in reverse so
registration order = dispatch order for ties.")

(defvar nelisp-eventloop-multiplex--pending-register nil
  "Queue of registrations submitted DURING a tick; merged at tick end.
Per Doc 39 §3.K spec: callback-time register/unregister is deferred
to next tick to keep the dispatch list stable.")

(defvar nelisp-eventloop-multiplex--pending-unregister nil
  "Queue of unregister handles submitted during a tick.")

(defvar nelisp-eventloop-multiplex--in-tick nil
  "Non-nil while `nelisp-eventloop-multiplex-tick' is dispatching.")

(defun nelisp-eventloop-multiplex--reset-registry ()
  "Test helper — clear the fd registration table."
  (setq nelisp-eventloop-multiplex--registered nil
        nelisp-eventloop-multiplex--pending-register nil
        nelisp-eventloop-multiplex--pending-unregister nil
        nelisp-eventloop-multiplex--in-tick nil))

(defun nelisp-eventloop-multiplex-register-fd (fd type callback &optional context)
  "Register CALLBACK to fire when FD is ready for TYPE.
TYPE is `read' / `write' / `except'.  CALLBACK is a 2-arg function
\(FD CONTEXT).  Returns an opaque handle vector.

If called from inside a tick callback, the registration is deferred
until the current tick finishes (Doc 39 §3.K reentrancy safety)."
  (unless (integerp fd)
    (signal 'wrong-type-argument (list 'integerp fd)))
  (unless (memq type '(read write except))
    (signal 'wrong-type-argument (list 'eventloop-fd-type type)))
  (unless (functionp callback)
    (signal 'wrong-type-argument (list 'functionp callback)))
  (let ((h (vector :registration fd type callback context t)))
    (if nelisp-eventloop-multiplex--in-tick
        (push h nelisp-eventloop-multiplex--pending-register)
      (push h nelisp-eventloop-multiplex--registered))
    h))

(defun nelisp-eventloop-multiplex-unregister-fd (handle)
  "Mark HANDLE as no longer interested in fd readiness.
Safe to call from inside a tick callback — the actual list removal is
deferred to the end of the current tick.  Returns t if HANDLE was
alive, nil if already removed."
  (unless (and (vectorp handle)
               (= (length handle) 6)
               (eq (aref handle 0) :registration))
    (signal 'wrong-type-argument
            (list 'nelisp-eventloop-multiplex-handle handle)))
  (let ((was-alive (aref handle 5)))
    (when was-alive
      (aset handle 5 nil)
      (if nelisp-eventloop-multiplex--in-tick
          (push handle nelisp-eventloop-multiplex--pending-unregister)
        (setq nelisp-eventloop-multiplex--registered
              (delq handle nelisp-eventloop-multiplex--registered))))
    was-alive))

(defun nelisp-eventloop-multiplex--flush-pending ()
  "Apply pending register / unregister queues built during a tick."
  (when nelisp-eventloop-multiplex--pending-register
    (setq nelisp-eventloop-multiplex--registered
          (append (nreverse nelisp-eventloop-multiplex--pending-register)
                  nelisp-eventloop-multiplex--registered))
    (setq nelisp-eventloop-multiplex--pending-register nil))
  (when nelisp-eventloop-multiplex--pending-unregister
    (dolist (h nelisp-eventloop-multiplex--pending-unregister)
      (setq nelisp-eventloop-multiplex--registered
            (delq h nelisp-eventloop-multiplex--registered)))
    (setq nelisp-eventloop-multiplex--pending-unregister nil)))

(defun nelisp-eventloop-multiplex-registered-handles ()
  "Return a fresh copy of the live registration list.  Test helper."
  (cl-remove-if-not
   (lambda (h) (aref h 5))
   (copy-sequence nelisp-eventloop-multiplex--registered)))

;; ---------------------------------------------------------------------
;; Dispatch tick.
;;
;; Algorithm (Doc 39 §3.K):
;;   1. Snapshot live registrations into per-type fd lists.
;;   2. select-fd with TIMEOUT-MS.
;;   3. For each ready fd, walk live registrations in registration
;;      order (oldest first), firing callbacks whose (FD, TYPE) matches.
;;   4. Flush pending register/unregister queues.
;;   5. Return number of callbacks fired.
;; ---------------------------------------------------------------------

(defun nelisp-eventloop-multiplex--collect-fds-by-type ()
  "Return plist (:read FDS :write FDS :except FDS) from live registry."
  (let (rd wr ex)
    (dolist (h nelisp-eventloop-multiplex--registered)
      (when (aref h 5)
        (let ((fd (aref h 1))
              (ty (aref h 2)))
          (pcase ty
            ('read   (unless (memq fd rd) (push fd rd)))
            ('write  (unless (memq fd wr) (push fd wr)))
            ('except (unless (memq fd ex) (push fd ex)))))))
    (list :read rd :write wr :except ex)))

(defun nelisp-eventloop-multiplex-tick (&optional timeout-ms)
  "Run one multiplexer iteration.
Waits up to TIMEOUT-MS (default 0 = non-blocking) for any registered
fd to become ready, then dispatches matching callbacks in
registration order.  Returns the number of callbacks fired (0 on
timeout)."
  (let* ((per-type (nelisp-eventloop-multiplex--collect-fds-by-type))
         (rd (plist-get per-type :read))
         (wr (plist-get per-type :write))
         (ex (plist-get per-type :except)))
    (cond
     ((and (null rd) (null wr) (null ex))
      ;; No fds registered — honour the timeout and return 0.
      (when (and timeout-ms (> timeout-ms 0))
        (sleep-for (/ timeout-ms 1000.0)))
      0)
     (t
      (let* ((sel (nelisp-eventloop-select-fd rd wr ex (or timeout-ms 0)))
             (ready (plist-get sel :ready))
             (fired 0)
             (nelisp-eventloop-multiplex--in-tick t))
        (unwind-protect
            (dolist (fd ready)
              ;; Walk registrations oldest-first so registration order
              ;; = dispatch order — the regular list is newest-first
              ;; (push), so reverse once.
              (dolist (h (reverse nelisp-eventloop-multiplex--registered))
                (when (and (aref h 5)            ; alive
                           (eql (aref h 1) fd)) ; same fd
                  (let ((ty (aref h 2)))
                    (when (or (and (eq ty 'read)
                                   (memq fd rd))
                              (and (eq ty 'write)
                                   (memq fd wr))
                              (and (eq ty 'except)
                                   (memq fd ex)))
                      (let ((cb (aref h 3))
                            (ctx (aref h 4)))
                        (condition-case err
                            (progn
                              (funcall cb fd ctx)
                              (cl-incf fired))
                          (error
                           (message
                            "nelisp-eventloop-multiplex: callback error fd=%s: %S"
                            fd err)))))))))
          (nelisp-eventloop-multiplex--flush-pending))
        fired)))))

;; ---------------------------------------------------------------------
;; Run loop.
;; ---------------------------------------------------------------------

(defvar nelisp-eventloop-multiplex--running nil
  "Non-nil while `nelisp-eventloop-multiplex-run' is looping.")

(defcustom nelisp-eventloop-multiplex-run-tick-ms 50
  "Default per-tick block timeout for `nelisp-eventloop-multiplex-run'.
Matches the Doc 39 `process-eventloop-tick-ms' baseline."
  :type 'integer
  :group 'nelisp-eventloop)

(defun nelisp-eventloop-multiplex-stop ()
  "Request the run loop to exit at the end of the current tick."
  (setq nelisp-eventloop-multiplex--running nil))

(defun nelisp-eventloop-multiplex-run (&optional tick-timeout-ms)
  "Drive the multiplexer until `nelisp-eventloop-multiplex-stop' is called.
TICK-TIMEOUT-MS defaults to `nelisp-eventloop-multiplex-run-tick-ms'.
Returns the total number of callbacks fired."
  (let ((timeout (or tick-timeout-ms
                     nelisp-eventloop-multiplex-run-tick-ms))
        (total 0))
    (setq nelisp-eventloop-multiplex--running t)
    (while nelisp-eventloop-multiplex--running
      (cl-incf total (nelisp-eventloop-multiplex-tick timeout)))
    total))

;; ---------------------------------------------------------------------
;; Doc 34 §2.4 event-source pull-fn adapter.
;;
;; `EVENT_SOURCE_CONTRACT_VERSION = 1' is preserved.  The shim returns
;; a 0-arg pull-fn that consumers register via
;; `nelisp-event-source-register'.  When the source is polled, we
;; non-blocking-probe FD; if ready, we read up to one event payload
;; from the bound proxy process and emit it as a `nelisp-event' of
;; kind `fd-data'.  No new contract version is bumped — Doc 39 fd
;; polling is strictly the implementation source for an existing
;; pull-fn shape (Doc 39 §1.5 / §11.4).
;; ---------------------------------------------------------------------

(defconst nelisp-eventloop-multiplex-event-source-contract-version 1
  "The contract version this adapter implements (Doc 34 §2.4).")

(defun nelisp-eventloop--multiplex-read-fd-payload (fd)
  "Bootstrap helper — drain up to one chunk of bytes pending on FD.
Returns the string read or nil if nothing was buffered."
  (let ((proc (nelisp-eventloop-multiplex--fd-proc fd)))
    (when (and proc (process-buffer proc)
               (buffer-live-p (process-buffer proc)))
      (with-current-buffer (process-buffer proc)
        (when (> (buffer-size) 0)
          (let ((s (buffer-substring-no-properties (point-min) (point-max))))
            (erase-buffer)
            s))))))

(defun nelisp-eventloop-as-event-source-pull-fn (fd)
  "Return a Doc 34 §2.4 pull-fn for FD.

The returned function is a 0-arg closure suitable for
`nelisp-event-source-register :pull-fn' (Doc 34
`EVENT_SOURCE_CONTRACT_VERSION = 1').  Each call:

  1. Non-blocking probes FD via `nelisp-eventloop-select-fd'
     with TIMEOUT-MS = 0.
  2. If FD is read-ready, drains up to one payload chunk and
     returns a `nelisp-event' of kind `fd-data' carrying the
     payload string.
  3. Otherwise returns nil.

The adapter never blocks; the consumer's
`nelisp-event-source-pull :timeout-ms' contract is honoured by the
event-source layer itself, which calls each registered pull-fn
in FIFO order until one returns non-nil or the timeout elapses."
  (unless (integerp fd)
    (signal 'wrong-type-argument (list 'integerp fd)))
  (lambda ()
    (let ((sel (nelisp-eventloop-select-fd (list fd) nil nil 0)))
      (when (eq (plist-get sel :status) :ready)
        (let ((payload (nelisp-eventloop--multiplex-read-fd-payload fd)))
          (when payload
            (nelisp-make-event 'fd-data
                               (list :fd fd :payload payload))))))))

(provide 'nelisp-eventloop-multiplex)
;;; nelisp-eventloop-multiplex.el ends here
