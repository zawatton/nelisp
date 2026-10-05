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

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- accept-process-output/sit-for drive the nemacs event loop.
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

(defconst emacs-eventloop--standalone-p
  (fboundp 'nl-ffi-call)
  "Non-nil when NeLisp standalone FFI is available.

Standalone starts from `emacs-stub-bulk.el', so C-level event-loop
names may already be fboundp as no-op stubs.  Runtime polyfills must
replace those stubs when this flag is non-nil.")


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


;;;; --- public compatibility surface ----------------------------------

;; This optional adapter can be loaded after the bootstrap shims.  Keep it
;; on the same shared wait instead of replacing them with separate loops.
(require 'emacs-process)
(require 'emacs-command-loop)

(when (or emacs-eventloop--standalone-p (not (fboundp 'accept-process-output)))
  (defalias 'accept-process-output #'emacs-process-accept-process-output))

(when (or emacs-eventloop--standalone-p (not (fboundp 'sit-for)))
  (defalias 'sit-for #'emacs-command-loop-sit-for))

(when (or emacs-eventloop--standalone-p (not (fboundp 'sleep-for)))
  (defalias 'sleep-for #'emacs-command-loop-sleep-for))

(provide 'emacs-eventloop)

;;; emacs-eventloop.el ends here
