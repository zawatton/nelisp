;;; nelisp-stdlib-os.el --- Doc 53 Phase 1 POSIX OS surface (Minimal-5)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 53 Phase 1 — OS-agnostic POSIX surface (Minimal-5: open / read /
;; write / close / exit) on top of:
;;
;;   - Path A (Linux/BSD): `nelisp--syscall' / `nelisp--syscall-openat'
;;     / `nelisp--syscall-read' / `nelisp--syscall-write' Rust primitives
;;     (= libc::syscall thin wrappers, Doc 50 §2.1 thin I/O pattern).
;;
;;   - Path B (Darwin/Windows): `nl-ffi-call' (Doc 51 Phase 5) for
;;     `libc.open' / `.write' / `.close' / `._exit'.  `read' is
;;     deferred to Phase 1.1 (needs mutable bytestring buffer
;;     primitive).
;;
;; Platform detect runs once at load time via
;; `nelisp--syscall-supported-p'; downstream callers see a single
;; OS-agnostic API.

;;; Code:

;; ---------------------------------------------------------------------------
;; Platform detect — set once, drives all branches below.
;; ---------------------------------------------------------------------------

(defconst nelisp-os--use-direct-syscall
  (nelisp--syscall-supported-p)
  "When t, route OS calls through `nelisp--syscall' (libc::syscall on
Linux/BSD).  When nil, fall back to `nl-ffi-call' libc bindings
(Darwin/Windows).  Set once at load time.")

;; ---------------------------------------------------------------------------
;; POSIX flag constants (Linux x86_64/arm64 values).  Phase 3+ refactor
;; should look these up from a per-OS table; for Phase 1 the Linux
;; values are sufficient since Path A is Linux-only and Path B's
;; libc.open accepts the same numeric flags on glibc-based systems.
;; ---------------------------------------------------------------------------

(defconst nelisp-os-AT-FDCWD -100)         ; openat dirfd sentinel for cwd
(defconst nelisp-os-O-RDONLY 0)
(defconst nelisp-os-O-WRONLY 1)
(defconst nelisp-os-O-RDWR   2)
(defconst nelisp-os-O-CREAT  64)           ; 0o100
(defconst nelisp-os-O-EXCL   128)          ; 0o200
(defconst nelisp-os-O-TRUNC  512)          ; 0o1000
(defconst nelisp-os-O-APPEND 1024)         ; 0o2000

;; Standard fds.
(defconst nelisp-os-STDIN  0)
(defconst nelisp-os-STDOUT 1)
(defconst nelisp-os-STDERR 2)

;; ---------------------------------------------------------------------------
;; Error helpers.  syscall return convention: negative integer = -errno.
;; ---------------------------------------------------------------------------

(defun nelisp-os--check-errno (r)
  "Return R if non-negative; signal `nelisp-os-error' with errno otherwise."
  (if (< r 0)
      (signal 'nelisp-os-error (list (- 0 r)))
    r))

;; ---------------------------------------------------------------------------
;; Minimal-5 — open / read / write / close / exit
;; ---------------------------------------------------------------------------

(defun nelisp-os-open (path flags mode)
  "POSIX open(2) — return integer fd, or signal `nelisp-os-error'.
PATH is a string, FLAGS / MODE are integers.  Routes through
`nelisp--syscall-openat' on Path A or `libc.open' via FFI on Path B."
  (if nelisp-os--use-direct-syscall
      (nelisp-os--check-errno
       (nelisp--syscall-openat nelisp-os-AT-FDCWD path flags mode))
    ;; Path B fallback — libc.open(3) sets -1 on error and we infer
    ;; -errno from the return.  Phase 1.1 will route errno through a
    ;; dedicated FFI helper; for Phase 1 the wrapper signals a generic
    ;; `nelisp-os-error' on any negative result.
    (nelisp-os--check-errno
     (nl-ffi-call "libc" "open" [:int :string :int :int]
                  path flags mode))))

(defun nelisp-os-read (fd nbytes)
  "POSIX read(2) — return string of up to NBYTES bytes or signal
`nelisp-os-error'.  Path B path is deferred to Phase 1.1 (needs
mutable bytestring buffer primitive)."
  (if nelisp-os--use-direct-syscall
      (let ((r (nelisp--syscall-read fd nbytes)))
        ;; Sum return: integer = -errno on failure, string on success.
        (if (integerp r)
            (nelisp-os--check-errno r)
          r))
    (signal 'nelisp-os-unsupported
            '(read "Path B fallback for read deferred to Phase 1.1"))))

(defun nelisp-os-write (fd str)
  "POSIX write(2) — write the bytes of STR to FD, return byte count
or signal `nelisp-os-error'."
  (if nelisp-os--use-direct-syscall
      (nelisp-os--check-errno (nelisp--syscall-write fd str))
    (nelisp-os--check-errno
     (nl-ffi-call "libc" "write" [:int :int :string :int]
                  fd str (length str)))))

(defun nelisp-os-close (fd)
  "POSIX close(2) — close FD, return nil or signal `nelisp-os-error'."
  (if nelisp-os--use-direct-syscall
      (progn (nelisp-os--check-errno (nelisp--syscall 'close fd)) nil)
    (progn
      (nelisp-os--check-errno (nl-ffi-call "libc" "close" [:int :int] fd))
      nil)))

(defun nelisp-os-exit (code)
  "POSIX exit_group(2) — terminate process with CODE.  Never returns."
  (if nelisp-os--use-direct-syscall
      (nelisp--syscall 'exit_group code)
    (nl-ffi-call "libc" "_exit" [:void :int] code)))

(provide 'nelisp-stdlib-os)

;;; nelisp-stdlib-os.el ends here
