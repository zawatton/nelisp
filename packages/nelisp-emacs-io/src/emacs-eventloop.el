;;; emacs-eventloop.el --- accept-process-output via libc poll(2) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 7 — Layer 2 event-loop port for NeLisp standalone.
;;
;; Builds on `emacs-network-ffi.el' (= libc FFI) and
;; `emacs-process-events.el' (= process registry + child accept) to
;; provide `accept-process-output' / `sit-for' so vendor `server.el'
;; / `jsonrpc.el' (= which dispatch via filter callbacks driven by
;; the host main loop) can run unmodified.
;;
;; Implementation:
;;   1. Build a `struct pollfd[]' for every fd in
;;      `emacs-process-events--by-fd'.
;;   2. Call libc `poll(2)' with the requested timeout.
;;   3. For each fd whose revents has POLLIN:
;;        - if it is a `network-server', call
;;          `emacs-process-events--accept-child' to spawn a child
;;          connection process and fire its sentinel.
;;        - if it is a `network-connection', call
;;          `emacs-process-events--read-and-dispatch' to recv +
;;          fire filter (or sentinel on EOF).
;;
;; A `pollfd' on Linux x86_64 / arm64 is 8 bytes:
;;   offset 0  i32 fd
;;   offset 4  i16 events
;;   offset 6  i16 revents

;;; Code:

(require 'emacs-network-ffi)
(require 'emacs-process-events)


;;;; --- POSIX poll(2) constants ----------------------------------------

(defconst emacs-eventloop-POLLIN  #x0001)
(defconst emacs-eventloop-POLLPRI #x0002)
(defconst emacs-eventloop-POLLOUT #x0004)
(defconst emacs-eventloop-POLLERR #x0008)
(defconst emacs-eventloop-POLLHUP #x0010)
(defconst emacs-eventloop-POLLNVAL #x0020)

(defconst emacs-eventloop--pollfd-size 8
  "Linux x86_64 / arm64 sizeof(struct pollfd) = 8 (i32 + i16 + i16).")


;;;; --- pollfd[] marshalling -------------------------------------------

(defun emacs-eventloop--build-pollfds (fds events)
  "Allocate a `struct pollfd[N]' on the heap; populate fd + events for each
entry in FDS (= list of int).  EVENTS is the same short value applied to
every entry (typically POLLIN).  Returns the buffer pointer + length pair
(PTR . N).  Caller must `nl-ffi-free' PTR after `poll' returns."
  (let* ((n (length fds))
         (size (* n emacs-eventloop--pollfd-size))
         (buf (nl-ffi-malloc (max size 1)))
         (i 0))
    (dolist (fd fds)
      (let ((off (* i emacs-eventloop--pollfd-size)))
        (nl-ffi-write-i32 buf off fd)            ; fd
        (nl-ffi-write-i16 buf (+ off 4) events)  ; events
        (nl-ffi-write-i16 buf (+ off 6) 0))      ; revents = 0
      (setq i (1+ i)))
    (cons buf n)))

(defun emacs-eventloop--read-revents (buf n)
  "Read revents short for each of the N pollfd entries in BUF.
Returns a list of (FD . REVENTS) cons cells in input order."
  (let ((out nil)
        (i 0))
    (while (< i n)
      (let* ((off (* i emacs-eventloop--pollfd-size))
             (fd (nl-ffi-read-i32 buf off))
             (revents (nl-ffi-read-i16 buf (+ off 6))))
        (push (cons fd revents) out))
      (setq i (1+ i)))
    (nreverse out)))


;;;; --- libc poll wrapper ----------------------------------------------

(defun emacs-eventloop--poll (fds timeout-ms)
  "Run libc `poll(2)' on FDS (list of int) with TIMEOUT-MS milliseconds.
TIMEOUT-MS = -1 → block, 0 → non-blocking, >0 → wait at most that long.
Returns a list of (FD . REVENTS) cons cells whose REVENTS is non-zero."
  (cond
   ((null fds)
    ;; No fds — emulate `select(NULL, NULL, NULL, &tv)' via usleep.
    (when (and (numberp timeout-ms) (> timeout-ms 0))
      (emacs-network-ffi--call
       "usleep" [:sint32 :sint32]
       (* 1000 timeout-ms)))
    nil)
   (t
    (let* ((pair (emacs-eventloop--build-pollfds
                  fds emacs-eventloop-POLLIN))
           (buf (car pair))
           (n (cdr pair))
           (rc (emacs-network-ffi--call
                "poll"
                [:sint32 :pointer :sint32 :sint32]
                buf n timeout-ms)))
      (let ((ready
             (cond
              ((and (integerp rc) (> rc 0))
               (let ((all (emacs-eventloop--read-revents buf n))
                     (out nil))
                 (dolist (entry all)
                   (when (and (integerp (cdr entry)) (not (zerop (cdr entry))))
                     (push entry out)))
                 (nreverse out)))
              (t nil))))
        (nl-ffi-free buf)
        ready)))))


;;;; --- public API: accept-process-output ------------------------------

(unless (fboundp 'accept-process-output)
  (defun accept-process-output (&optional process seconds millisec
                                          _just-this-one)
    "Polyfill: poll all live processes for I/O, fire filters / sentinels.
Returns t when at least one filter or sentinel fired, nil on timeout.

PROCESS is currently ignored — we always poll every registered fd.
SECONDS + MILLISEC combine into the poll timeout (each defaults to 0).
JUST-THIS-ONE is accepted for API parity but ignored.

Children that the listener fd accepts during this call are added to
the registry; `accept-process-output' is the only place new server
children become observable."
    (let* ((s (or seconds 0))
           (ms (or millisec 0))
           (timeout-ms
            (cond
             ((null process) (truncate (+ (* s 1000) ms)))
             ((and (numberp s) (zerop s) (numberp ms) (zerop ms)) 0)
             (t (truncate (+ (* s 1000) ms)))))
           (fds (emacs-process-events--all-fds))
           (events (emacs-eventloop--poll fds timeout-ms))
           (any nil))
      (dolist (entry events)
        (let* ((fd (car entry))
               (proc (emacs-process-events--lookup-by-fd fd)))
          (when proc
            (cond
             ((eq (emacs-process-events--get proc 3) 'network-server)
              ;; Listening fd — accept everything currently pending.
              (let ((child t))
                (while child
                  (setq child
                        (emacs-process-events--accept-child proc))
                  (when child (setq any t)))))
             ((eq (emacs-process-events--get proc 3) 'network-connection)
              (when (emacs-process-events--read-and-dispatch proc)
                (setq any t)))))))
      (when (and (not any) process)
        ;; PROCESS-specific wait: re-poll once more with the remaining
        ;; budget.  Phase 7 keeps this simple — host Emacs spins on
        ;; just_this_one with deadline tracking; we approximate.
        nil)
      any)))


;;;; --- public API: sit-for / sleep-for --------------------------------

(unless (fboundp 'sit-for)
  (defun sit-for (seconds &optional _nodisp)
    "Polyfill: yield for SECONDS, dispatching any I/O that arrives meanwhile.
Returns nil if input would have arrived during the wait, t otherwise.
Phase 7 only honours the timeout (= no input semantics)."
    (let ((ms (truncate (* seconds 1000))))
      (accept-process-output nil 0 ms))))

(unless (fboundp 'sleep-for)
  (defun sleep-for (seconds &optional millisec)
    "Polyfill: sleep for SECONDS + MILLISEC, ignoring I/O during the wait.
Implemented as a `usleep' through libc — does NOT dispatch process
events while sleeping.  Use `accept-process-output' or `sit-for'
instead when filter callbacks may need to run."
    (let ((total-us (truncate (+ (* (or seconds 0) 1000000)
                                  (* (or millisec 0) 1000)))))
      (when (> total-us 0)
        (emacs-network-ffi--call
         "usleep" [:sint32 :sint32] total-us))
      nil)))


(provide 'emacs-eventloop)

;;; emacs-eventloop.el ends here
