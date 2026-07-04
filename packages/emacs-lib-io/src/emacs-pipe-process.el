;;; emacs-pipe-process.el --- async pipe-subprocess filter dispatch (Doc 06 C1) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 06 C1: register a pipe-subprocess stdout/stderr fd into the
;; `emacs-eventloop' poll set so non-network `make-process' gets async filters.
;; Until now only network fds were tracked (Doc 05 §5); the eventloop dispatch
;; keys on the process `kind' slot, so adding a `pipe-process' kind plus a
;; read(2)-based read path (`emacs-process-events--read-fd', since pipe fds are
;; not sockets) is all that is needed.
;;
;; `emacs-pipe-process-create' wraps an already-open read fd (typically the
;; read end of a `pipe(2)' whose write end was handed to a forked child) as a
;; process the eventloop will poll.  `emacs-pipe-process-pipe' opens a fresh
;; pipe via the syscall shim.  Verified end-to-end on the nelisp binary by
;; `test/emacs-pipe-process-binary-verify.el' (write to the pipe → poll →
;; filter fires with the data).

;;; Code:

(require 'emacs-process-events)
(require 'emacs-network-ffi)
(require 'emacs-eventloop)

(defun emacs-pipe-process-pipe ()
  "Create a pipe via the libc `pipe(2)' syscall.
Returns (READ-FD . WRITE-FD), or nil on failure (Doc 06 C1)."
  (let* ((buf (nl-ffi-malloc 8))
         (rc (emacs-network-ffi--call "pipe" [:sint32 :pointer] buf)))
    (if (and (integerp rc) (= rc 0))
        (let ((rfd (nl-ffi-read-i32 buf 0))
              (wfd (nl-ffi-read-i32 buf 4)))
          (nl-ffi-free buf)
          (cons rfd wfd))
      (nl-ffi-free buf)
      nil)))

(defun emacs-pipe-process-create (&rest args)
  "Register a pipe-subprocess process reading from a pipe READ-FD.
Keyword ARGS:
  :name NAME        diagnostic identity (default \"pipe\")
  :read-fd FD       required: the readable pipe fd to poll
  :filter FN        incoming-data callback (FN PROCESS STRING)
  :sentinel FN      status-change callback (FN PROCESS MESSAGE)
  :buffer BUFFER    process buffer passed back to the filter
  :coding CODING    (DECODE . ENCODE) coding systems (Doc 06 C4)
The READ-FD is set non-blocking and registered in the eventloop poll set, so
`accept-process-output' fires FILTER when data arrives (Doc 06 C1).
Returns the process vector."
  (let ((name (or (plist-get args :name) "pipe"))
        (read-fd (plist-get args :read-fd))
        (filter (plist-get args :filter))
        (sentinel (plist-get args :sentinel))
        (buffer (plist-get args :buffer))
        (coding (plist-get args :coding))
        (pid (plist-get args :pid)))
    (unless (and (integerp read-fd) (>= read-fd 0))
      (error "emacs-pipe-process-create: :read-fd must be a valid fd, got %S"
             read-fd))
    (emacs-network-ffi--set-nonblocking read-fd)
    (let* ((id (setq emacs-process-events--id-counter
                     (1+ emacs-process-events--id-counter)))
           ;; C2: keep the child OS pid in the plist so the SIGCHLD-fallback
           ;; reaper (`emacs-process-events--reap-children') can match it back.
           (plist (and pid (list :pid pid)))
           (proc (emacs-process-events--make-vec
                  name read-fd 'pipe-process 'run
                  filter sentinel buffer plist coding -1 id)))
      (emacs-process-events--register proc)
      proc)))

(provide 'emacs-pipe-process)
;;; emacs-pipe-process.el ends here
