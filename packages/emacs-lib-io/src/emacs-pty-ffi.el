;;; emacs-pty-ffi.el --- PTY master/slave via libc FFI (Doc 06 C3)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 06 C3: pseudo-terminal support for interactive subprocesses
;; (comint / term.el / shell-mode).  Built on the same `nl-ffi-call' libc
;; bridge as `emacs-network-ffi' — see that file for the libffi primitive and
;; the `nl-ffi-*' memory helpers.
;;
;; IMPORTANT (verification status): `nl-ffi-call' is a nelisp-runtime primitive
;; and is NOT available under host Emacs, so this module cannot be exercised in
;; a host-Emacs unit-test run (the ERT below is `skip-unless'-gated on the
;; primitive).  The code mirrors the proven `emacs-network-ffi' socket
;; sequence; runtime verification must happen on a real nelisp build with
;; libffi.  Until then this is byte-compiled + structurally reviewed only.
;;
;; Flow: posix_openpt → grantpt → unlockpt → ptsname_r → fork → child
;; (setsid + open slave + TIOCSCTTY + dup2 0/1/2 + execve) ; parent keeps the
;; non-blocking master fd, which the event loop polls for filter/sentinel
;; dispatch (reuse the C1 pipe-fd registration path).

;;; Code:

(require 'emacs-network-ffi)

(defconst emacs-pty-ffi-O_RDWR 2 "open(2) O_RDWR (Linux).")
(defconst emacs-pty-ffi-O_NOCTTY 256 "open(2) O_NOCTTY (0400 octal, Linux).")
(defconst emacs-pty-ffi-TIOCSCTTY 21518 "ioctl TIOCSCTTY (0x540E, Linux).")
(defconst emacs-pty-ffi-TIOCSPTLCK 1074025521 "ioctl TIOCSPTLCK (0x40045431, unlockpt).")
(defconst emacs-pty-ffi-TIOCGPTN 2147767344 "ioctl TIOCGPTN (0x80045430, get pty number).")

(defun emacs-pty-ffi-available-p ()
  "Return non-nil when the libc FFI needed for PTY support is present."
  (and (fboundp 'nl-ffi-call)
       (fboundp 'nl-ffi-malloc)
       (stringp emacs-network-ffi-libc-path)))

(defun emacs-pty-ffi-openpt ()
  "Open a PTY master via /dev/ptmx + unlock (ioctl TIOCSPTLCK).
Uses raw syscalls (open + ioctl) rather than glibc posix_openpt/unlockpt so it
runs through the syscall-direct shim on a real nelisp build.  Return the master
file descriptor (an integer >= 0), or nil on failure."
  (when (emacs-pty-ffi-available-p)
    (let ((master (emacs-network-ffi--call
                   "open" [:sint32 :string :sint32 :sint32]
                   "/dev/ptmx"
                   (logior emacs-pty-ffi-O_RDWR emacs-pty-ffi-O_NOCTTY) 0)))
      (when (and (integerp master) (>= master 0))
        (let* ((zero (nl-ffi-malloc 4))   ; *int = 0 to unlock
               (rc (emacs-network-ffi--call
                    "ioctl" [:sint32 :sint32 :sint64 :pointer]
                    master emacs-pty-ffi-TIOCSPTLCK zero)))
          (nl-ffi-free zero)
          (if (eql rc 0)
              master
            (emacs-network-ffi--call "close" [:sint32 :sint32] master)
            nil))))))

(defun emacs-pty-ffi-ptsname (master)
  "Return the slave device path for PTY MASTER fd via ioctl TIOCGPTN.
Reads the pty number into a caller-allocated int and formats /dev/pts/N."
  (when (emacs-pty-ffi-available-p)
    (let ((buf (nl-ffi-malloc 4)))
      (prog1
          (let ((rc (emacs-network-ffi--call
                     "ioctl" [:sint32 :sint32 :sint64 :pointer]
                     master emacs-pty-ffi-TIOCGPTN buf)))
            (when (eql rc 0)
              (format "/dev/pts/%d" (nl-ffi-read-i32 buf 0))))
        (nl-ffi-free buf)))))

(defun emacs-pty-ffi-open ()
  "Open a PTY pair.  Return (MASTER-FD . SLAVE-PATH), or nil on failure."
  (let ((master (emacs-pty-ffi-openpt)))
    (when master
      (let ((slave (emacs-pty-ffi-ptsname master)))
        (if slave
            (cons master slave)
          (emacs-network-ffi--call "close" [:sint32 :sint32] master)
          nil)))))

(defun emacs-pty-ffi-set-nonblocking (fd)
  "Set FD to non-blocking via fcntl F_SETFL O_NONBLOCK."
  (when (emacs-pty-ffi-available-p)
    (let ((flags (emacs-network-ffi--call
                  "fcntl" [:sint32 :sint32 :sint32]
                  fd emacs-network-ffi-F_GETFL)))
      (when (integerp flags)
        (emacs-network-ffi--call
         "fcntl" [:sint32 :sint32 :sint32 :sint32]
         fd emacs-network-ffi-F_SETFL
         (logior flags emacs-network-ffi-O_NONBLOCK))))))

(defun emacs-pty-ffi-spawn (program args slave-path)
  "Fork a child running PROGRAM with ARGS attached to SLAVE-PATH as its tty.
Return the child PID, or nil.  Uses `nelisp-sys' fork/exec; the child becomes
its own session leader, acquires SLAVE-PATH as its controlling terminal, and
redirects fds 0/1/2 to it.  Runtime-verification pending (see Commentary)."
  (when (and (emacs-pty-ffi-available-p)
             (fboundp 'nelisp-sys-fork)
             (fboundp 'nelisp-sys-execve))
    (let ((pid (nelisp-sys-fork)))
      (cond
       ((null pid) nil)
       ((eq pid 0)
        ;; --- child ---
        (emacs-network-ffi--call "setsid" [:sint32])
        (let ((slave (emacs-network-ffi--call
                      "open" [:sint32 :string :sint32]
                      slave-path emacs-pty-ffi-O_RDWR)))
          (when (and (integerp slave) (>= slave 0))
            (emacs-network-ffi--call
             "ioctl" [:sint32 :sint32 :sint64 :sint64]
             slave emacs-pty-ffi-TIOCSCTTY 0)
            (dolist (target '(0 1 2))
              (emacs-network-ffi--call
               "dup2" [:sint32 :sint32 :sint32] slave target))
            (nelisp-sys-execve program args)))
        ;; execve failed if we reach here.
        (emacs-network-ffi--call "_exit" [:void :sint32] 127)
        nil)
       (t
        ;; --- parent ---
        pid)))))

(provide 'emacs-pty-ffi)

;;; emacs-pty-ffi.el ends here
