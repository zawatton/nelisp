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

;; ---------------------------------------------------------------------------
;; Doc 76 Stage A.1 — open / read / write are now nl-ffi-call libc unified
;; (= no Path A/B branch).  libc functions return -1 on error and set
;; errno; we read errno through `nl-ffi-errno' and signal `nelisp-os-error'.
;; ---------------------------------------------------------------------------

(defun nelisp-os--ffi-errno-signal ()
  "Signal `nelisp-os-error' with the current errno."
  (signal 'nelisp-os-error (list (nl-ffi-errno))))

(defun nelisp-os-open (path flags mode)
  "POSIX open(2) — return integer fd, or signal `nelisp-os-error'.
PATH is a string, FLAGS / MODE are integers."
  ;; libc::open: int(const char *path, int flags, mode_t mode) → int.
  ;; Linux glibc: int=:sint32, mode_t=:uint32.
  (let ((r (nl-ffi-call "libc" "open" [:sint32 :string :sint32 :uint32]
                        path flags mode)))
    (if (= r -1)
        (nelisp-os--ffi-errno-signal)
      r)))

(defun nelisp-os-read (fd nbytes)
  "POSIX read(2) — return string of up to NBYTES bytes or signal
`nelisp-os-error'."
  ;; libc::read: ssize_t(int fd, void *buf, size_t count) → ssize_t.
  ;; Linux x86_64 / aarch64: ssize_t=:sint64, size_t=:uint64.
  (when (< nbytes 0)
    (signal 'nelisp-os-error (list 22)))     ; EINVAL
  (if (= nbytes 0)
      ""
    (let ((buf (nl-ffi-malloc nbytes)))
      (unwind-protect
          (let ((r (nl-ffi-call "libc" "read"
                                [:sint64 :sint32 :pointer :uint64]
                                fd buf nbytes)))
            (cond
             ((= r -1) (nelisp-os--ffi-errno-signal))
             ((= r 0)  "")
             (t        (nl-ffi-read-bytes buf r))))
        (nl-ffi-free buf)))))

(defun nelisp-os-write (fd str)
  "POSIX write(2) — write the bytes of STR to FD, return byte count
or signal `nelisp-os-error'.

Binary-safe: STR may contain interior NUL bytes and multi-byte UTF-8
sequences; we route through `nl-ffi-malloc' / `-write-bytes' so the
exact bytes (= `string-bytes' worth) reach libc.write — matching old
Path A's `as_bytes()' semantics rather than the broken Path B that
went through `:string' (= CString::new, NUL-rejecting)."
  (let* ((nbytes (string-bytes str))
         (buf    (nl-ffi-malloc nbytes)))
    (unwind-protect
        (progn
          (nl-ffi-write-bytes buf str)
          ;; libc::write: ssize_t(int fd, const void *buf, size_t count).
          (let ((r (nl-ffi-call "libc" "write"
                                [:sint64 :sint32 :pointer :uint64]
                                fd buf nbytes)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              r)))
      (nl-ffi-free buf))))

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

;; ---------------------------------------------------------------------------
;; Doc 54 Phase 3 — Core-12 additions.
;;
;; Constants below assume Linux x86_64/arm64 ABI numbers — same scope
;; restriction as Phase 1.  Path B (Darwin/Windows) lookup will land in
;; Phase 1.1 along with the `read' fallback.
;; ---------------------------------------------------------------------------

;; lseek(2) whence
(defconst nelisp-os-SEEK-SET 0)
(defconst nelisp-os-SEEK-CUR 1)
(defconst nelisp-os-SEEK-END 2)

;; fcntl(2) cmd
(defconst nelisp-os-F-DUPFD 0)
(defconst nelisp-os-F-GETFL 3)
(defconst nelisp-os-F-SETFL 4)

;; open(2) / fcntl(2) extra flag
(defconst nelisp-os-O-NONBLOCK 2048)        ; 0o4000

;; mmap(2) prot
(defconst nelisp-os-PROT-NONE  0)
(defconst nelisp-os-PROT-READ  1)
(defconst nelisp-os-PROT-WRITE 2)
(defconst nelisp-os-PROT-EXEC  4)

;; mmap(2) flags
(defconst nelisp-os-MAP-PRIVATE   2)
(defconst nelisp-os-MAP-ANONYMOUS 32)       ; 0o40
(defconst nelisp-os-MAP-FAILED    -1)       ; not strictly a flag, sentinel

;; struct stat st_mode bits
(defconst nelisp-os-S-IFMT  61440)          ; 0o170000
(defconst nelisp-os-S-IFREG 32768)          ; 0o100000
(defconst nelisp-os-S-IFDIR 16384)          ; 0o040000
(defconst nelisp-os-S-IFLNK 40960)          ; 0o120000

;; ----- Wrappers -----

(defun nelisp-os-lseek (fd offset whence)
  "POSIX lseek(2) — return new offset or signal `nelisp-os-error'.
WHENCE = `nelisp-os-SEEK-SET' / `-CUR' / `-END'."
  (nelisp-os--check-errno (nelisp--syscall 'lseek fd offset whence)))

;; ---------------------------------------------------------------------------
;; Doc 76 Stage A.3 — Linux x86_64 / aarch64 struct stat field offsets.
;; (FreeBSD / macOS / Windows have different layouts; cross-OS support is
;; deferred to a follow-up that auto-generates these consts via build.rs
;; per Doc 76 §7.1.)
;; ---------------------------------------------------------------------------

(defconst nelisp-os--stat-buflen 256
  "Allocation size for the libc.fstat output buffer (= safe upper bound
for any common arch's `struct stat'; Linux x86_64 actual = 144).")

(defconst nelisp-os--stat-offset-dev         0)
(defconst nelisp-os--stat-offset-ino         8)
(defconst nelisp-os--stat-offset-nlink      16)
(defconst nelisp-os--stat-offset-mode       24)
(defconst nelisp-os--stat-offset-uid        28)
(defconst nelisp-os--stat-offset-gid        32)
(defconst nelisp-os--stat-offset-size       48)
(defconst nelisp-os--stat-offset-atime      72)
(defconst nelisp-os--stat-offset-atime-nsec 80)
(defconst nelisp-os--stat-offset-mtime      88)
(defconst nelisp-os--stat-offset-mtime-nsec 96)
(defconst nelisp-os--stat-offset-ctime     104)
(defconst nelisp-os--stat-offset-ctime-nsec 112)

(defun nelisp-os-fstat (fd)
  "POSIX fstat(2) — return positional list of stat fields, or signal
`nelisp-os-error'.  Order matches `nelisp-os-stat-*' accessors below
and the old `nelisp--syscall-fstat' Rust primitive (Doc 54 Phase 3)."
  ;; libc::fstat: int(int fd, struct stat *buf) → 0 / -1.
  (let ((buf (nl-ffi-malloc nelisp-os--stat-buflen)))
    (unwind-protect
        (let ((r (nl-ffi-call "libc" "fstat"
                              [:sint32 :sint32 :pointer]
                              fd buf)))
          (if (= r -1)
              (nelisp-os--ffi-errno-signal)
            (list (nl-ffi-read-i64 buf nelisp-os--stat-offset-size)
                  (nl-ffi-read-i32 buf nelisp-os--stat-offset-mode)
                  (nl-ffi-read-i64 buf nelisp-os--stat-offset-mtime)
                  (nl-ffi-read-i64 buf nelisp-os--stat-offset-mtime-nsec)
                  (nl-ffi-read-i64 buf nelisp-os--stat-offset-atime)
                  (nl-ffi-read-i64 buf nelisp-os--stat-offset-atime-nsec)
                  (nl-ffi-read-i64 buf nelisp-os--stat-offset-ctime)
                  (nl-ffi-read-i64 buf nelisp-os--stat-offset-ctime-nsec)
                  (nl-ffi-read-i64 buf nelisp-os--stat-offset-nlink)
                  (nl-ffi-read-i32 buf nelisp-os--stat-offset-uid)
                  (nl-ffi-read-i32 buf nelisp-os--stat-offset-gid)
                  (nl-ffi-read-i64 buf nelisp-os--stat-offset-ino)
                  (nl-ffi-read-i64 buf nelisp-os--stat-offset-dev))))
      (nl-ffi-free buf))))

;; struct stat positional accessors (= same field order the Rust
;; primitive emits).  Add named getters as the substrate needs them.
(defun nelisp-os-stat-size       (st) (nth 0  st))
(defun nelisp-os-stat-mode       (st) (nth 1  st))
(defun nelisp-os-stat-mtime      (st) (nth 2  st))
(defun nelisp-os-stat-mtime-nsec (st) (nth 3  st))
(defun nelisp-os-stat-atime      (st) (nth 4  st))
(defun nelisp-os-stat-atime-nsec (st) (nth 5  st))
(defun nelisp-os-stat-ctime      (st) (nth 6  st))
(defun nelisp-os-stat-ctime-nsec (st) (nth 7  st))
(defun nelisp-os-stat-nlink      (st) (nth 8  st))
(defun nelisp-os-stat-uid        (st) (nth 9  st))
(defun nelisp-os-stat-gid        (st) (nth 10 st))
(defun nelisp-os-stat-ino        (st) (nth 11 st))
(defun nelisp-os-stat-dev        (st) (nth 12 st))

(defun nelisp-os-mmap (length prot flags fd offset)
  "POSIX mmap(2) — addr=0 (kernel chooses).  Returns mapping address as
integer (opaque pointer for elisp), or signals `nelisp-os-error' on
mapping failure (raw kernel returns -errno for failed mmap)."
  (nelisp-os--check-errno
   (nelisp--syscall 'mmap 0 length prot flags fd offset)))

(defun nelisp-os-mprotect (addr length prot)
  "POSIX mprotect(2) — change protection on existing mapping."
  (nelisp-os--check-errno (nelisp--syscall 'mprotect addr length prot)))

(defun nelisp-os-munmap (addr length)
  "POSIX munmap(2) — release a previously mmap'd region."
  (nelisp-os--check-errno (nelisp--syscall 'munmap addr length)))

(defun nelisp-os-dup2 (oldfd newfd)
  "POSIX dup2(2) — duplicate OLDFD onto NEWFD; returns NEWFD."
  (nelisp-os--check-errno (nelisp--syscall 'dup2 oldfd newfd)))

(defun nelisp-os-pipe ()
  "POSIX pipe(2) — return cons (READ-FD . WRITE-FD), or signal
`nelisp-os-error'."
  ;; libc::pipe: int(int pipefd[2]) → 0 / -1.  Allocate 8 bytes
  ;; (= 2 × int32) so libc.pipe can fill them, then decode via
  ;; `nl-ffi-read-i32' to dodge `nl-ffi-read-bytes''s UTF-8 lossy
  ;; conversion (= corrupts byte values 0x80-0xFF).
  (let ((buf (nl-ffi-malloc 8)))
    (unwind-protect
        (let ((r (nl-ffi-call "libc" "pipe" [:sint32 :pointer] buf)))
          (if (= r -1)
              (nelisp-os--ffi-errno-signal)
            (cons (nl-ffi-read-i32 buf 0)
                  (nl-ffi-read-i32 buf 4))))
      (nl-ffi-free buf))))

(defun nelisp-os-fcntl (fd cmd arg)
  "POSIX fcntl(2) — int-only variant (= F_GETFL / F_SETFL / F_DUPFD).
Other variadic forms (struct flock for F_SETLK etc.) need their own
primitive; not supported in Phase 3."
  (nelisp-os--check-errno (nelisp--syscall 'fcntl fd cmd arg)))

;; ---------------------------------------------------------------------------
;; Doc 55 Phase 4 — Posix-30 (subprocess + AF_INET network + poll).
;;
;; All constants below assume Linux x86_64/arm64 ABI numbers — same scope
;; restriction as Phase 1/3.  Path B (Darwin/Windows) lookup will land in
;; Phase 1.1 along with the `read' fallback.
;; ---------------------------------------------------------------------------

;; ----- Process / signals -----

(defconst nelisp-os-SIGHUP   1)
(defconst nelisp-os-SIGINT   2)
(defconst nelisp-os-SIGKILL  9)
(defconst nelisp-os-SIGTERM 15)

(defconst nelisp-os-WNOHANG    1)
(defconst nelisp-os-WUNTRACED  2)

;; ----- Network (AF_INET only in Phase 4) -----

(defconst nelisp-os-AF-INET 2)

(defconst nelisp-os-SOCK-STREAM   1)
(defconst nelisp-os-SOCK-DGRAM    2)
(defconst nelisp-os-SOCK-NONBLOCK 2048)        ; 0o4000 — OR-able into SOCK_*
(defconst nelisp-os-SOCK-CLOEXEC  524288)      ; 0o2000000 — OR-able into SOCK_*

(defconst nelisp-os-IPPROTO-IP   0)
(defconst nelisp-os-IPPROTO-TCP  6)
(defconst nelisp-os-IPPROTO-UDP 17)

;; INADDR helpers — host byte order (Rust side does htonl on the wire).
(defconst nelisp-os-INADDR-ANY      0)
(defconst nelisp-os-INADDR-LOOPBACK #x7F000001)

;; setsockopt level / option (int-valued only in Phase 4)
(defconst nelisp-os-SOL-SOCKET    1)
(defconst nelisp-os-SO-REUSEADDR  2)
(defconst nelisp-os-SO-KEEPALIVE  9)

;; ----- Poll events -----

(defconst nelisp-os-POLLIN    1)
(defconst nelisp-os-POLLPRI   2)
(defconst nelisp-os-POLLOUT   4)
(defconst nelisp-os-POLLERR   8)
(defconst nelisp-os-POLLHUP  16)
(defconst nelisp-os-POLLNVAL 32)

;; ----- Subprocess wrappers -----

(defun nelisp-os-fork ()
  "POSIX fork(2) — returns child PID in the parent, 0 in the child, or
signals `nelisp-os-error' on failure."
  (nelisp-os--check-errno (nelisp--syscall 'fork)))

(defun nelisp-os-execve (path argv envp)
  "POSIX execve(2) — replace current process image with PATH using ARGV
list and ENVP list of strings.  Only returns on failure (signals
`nelisp-os-error')."
  (nelisp-os--check-errno (nelisp--syscall-execve path argv envp)))

(defun nelisp-os-wait (pid options)
  "POSIX wait4(2) — wait for child PID with OPTIONS (= 0, WNOHANG, etc.).
Returns cons (CHILD-PID . STATUS) on success, or signals
`nelisp-os-error' on failure.  When WNOHANG and no child is ready,
returns (0 . 0)."
  (let ((r (nelisp--syscall-wait4 pid options)))
    (if (integerp r)
        (nelisp-os--check-errno r)
      r)))

(defun nelisp-os-kill (pid sig)
  "POSIX kill(2) — send SIG to PID; returns 0 on success."
  (nelisp-os--check-errno (nelisp--syscall 'kill pid sig)))

(defun nelisp-os-getpid ()
  "POSIX getpid(2) — return current process id."
  (nelisp-os--check-errno (nelisp--syscall 'getpid)))

(defun nelisp-os-getppid ()
  "POSIX getppid(2) — return parent process id."
  (nelisp-os--check-errno (nelisp--syscall 'getppid)))

;; wait status decoders.  Linux puts the exit status in bits 8-15, the
;; termination signal in bits 0-6, and 0x7F as the marker for "stopped"
;; (which we ignore in Phase 4).  These helpers mirror the C macros.
(defun nelisp-os-WIFEXITED (status)
  "Non-nil if STATUS came from a normal exit."
  (= (logand status 127) 0))

(defun nelisp-os-WEXITSTATUS (status)
  "Extract exit code from STATUS (only meaningful when WIFEXITED)."
  (logand (lsh status -8) 255))

(defun nelisp-os-WIFSIGNALED (status)
  "Non-nil if STATUS came from termination by signal."
  (let ((s (logand status 127)))
    (and (/= s 0) (/= s 127))))

(defun nelisp-os-WTERMSIG (status)
  "Extract terminating signal from STATUS (only meaningful when WIFSIGNALED)."
  (logand status 127))

;; ----- Network wrappers (AF_INET) -----

(defun nelisp-os-socket (domain type proto)
  "POSIX socket(2) — return new fd or signal `nelisp-os-error'."
  (nelisp-os--check-errno (nelisp--syscall 'socket domain type proto)))

;; ---------------------------------------------------------------------------
;; Doc 76 Stage C (2026-05-08) — sockaddr_in encode/decode + pollfd[]
;; marshaling helpers.  Used by bind-inet / connect-inet / accept-inet /
;; poll wrappers below.
;; ---------------------------------------------------------------------------

(defconst nelisp-os--AF-INET 2)
(defconst nelisp-os--sockaddr-in-len 16)
(defconst nelisp-os--pollfd-len 8)

(defun nelisp-os--encode-sockaddr-in (buf host-int port)
  "Populate BUF (= 16-byte zeroed `nl-ffi-malloc') with sockaddr_in
fields: sin_family + sin_port (BE) + sin_addr (BE).  The 8 zero pad
bytes at offset 8-15 are left as malloc'd zeros."
  (let ((port-be (nl-ffi-call "libc" "htons" [:uint16 :uint16] port))
        (addr-be (nl-ffi-call "libc" "htonl" [:uint32 :uint32] host-int)))
    (nl-ffi-write-i16 buf 0 nelisp-os--AF-INET)
    (nl-ffi-write-i16 buf 2 port-be)
    (nl-ffi-write-i32 buf 4 addr-be)))

(defun nelisp-os--decode-sockaddr-in (buf)
  "Decode 16-byte sockaddr_in BUF.  Return cons (HOST-INT . PORT) in
host byte order."
  (let* ((port-be (nl-ffi-read-u16 buf 2))
         (port    (nl-ffi-call "libc" "ntohs" [:uint16 :uint16] port-be))
         (addr-be (nl-ffi-read-u32 buf 4))
         (addr    (nl-ffi-call "libc" "ntohl" [:uint32 :uint32] addr-be)))
    (cons addr port)))

(defun nelisp-os-setsockopt-int (fd level optname value)
  "POSIX setsockopt(2) for an int-valued option (e.g. SO_REUSEADDR).
Returns 0 on success."
  ;; libc::setsockopt(int sockfd, int level, int optname,
  ;;                  const void *optval, socklen_t optlen) → int.
  (let ((buf (nl-ffi-malloc 4)))
    (unwind-protect
        (progn
          (nl-ffi-write-i32 buf 0 value)
          (let ((r (nl-ffi-call "libc" "setsockopt"
                                [:sint32 :sint32 :sint32 :sint32 :pointer :uint32]
                                fd level optname buf 4)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              r)))
      (nl-ffi-free buf))))

(defun nelisp-os-bind-inet (fd host-int port)
  "POSIX bind(2) for AF_INET.  HOST-INT is a 32-bit IPv4 address in
host byte order (e.g. `nelisp-os-INADDR-LOOPBACK').  PORT is a
16-bit host-byte-order port number.  Returns 0 on success."
  (let ((buf (nl-ffi-malloc nelisp-os--sockaddr-in-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sockaddr-in buf host-int port)
          (let ((r (nl-ffi-call "libc" "bind"
                                [:sint32 :sint32 :pointer :uint32]
                                fd buf nelisp-os--sockaddr-in-len)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              r)))
      (nl-ffi-free buf))))

(defun nelisp-os-listen (fd backlog)
  "POSIX listen(2) — mark FD as accepting connections with BACKLOG."
  (nelisp-os--check-errno (nelisp--syscall 'listen fd backlog)))

(defun nelisp-os-accept-inet (sockfd)
  "POSIX accept(2) for AF_INET.  Returns list (NEWFD CLIENT-IP CLIENT-PORT)
on success (CLIENT-IP / CLIENT-PORT in host byte order), or signals
`nelisp-os-error'."
  (let ((addr-buf (nl-ffi-malloc nelisp-os--sockaddr-in-len))
        (len-buf  (nl-ffi-malloc 4)))
    (unwind-protect
        (progn
          (nl-ffi-write-i32 len-buf 0 nelisp-os--sockaddr-in-len)
          (let ((newfd (nl-ffi-call "libc" "accept"
                                    [:sint32 :sint32 :pointer :pointer]
                                    sockfd addr-buf len-buf)))
            (if (= newfd -1)
                (nelisp-os--ffi-errno-signal)
              (let ((hp (nelisp-os--decode-sockaddr-in addr-buf)))
                (list newfd (car hp) (cdr hp))))))
      (nl-ffi-free addr-buf)
      (nl-ffi-free len-buf))))

(defun nelisp-os-connect-inet (fd host-int port)
  "POSIX connect(2) for AF_INET.  HOST-INT / PORT same convention as
`nelisp-os-bind-inet'.  Returns 0 on success."
  (let ((buf (nl-ffi-malloc nelisp-os--sockaddr-in-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sockaddr-in buf host-int port)
          (let ((r (nl-ffi-call "libc" "connect"
                                [:sint32 :sint32 :pointer :uint32]
                                fd buf nelisp-os--sockaddr-in-len)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              r)))
      (nl-ffi-free buf))))

;; ----- Multiplexing -----

(defun nelisp-os-poll (pfds timeout-ms)
  "POSIX poll(2).  PFDS is a list of (FD . EVENTS) cons cells.  Returns
the same-length list of (FD . REVENTS) cons cells.  TIMEOUT-MS = -1
blocks indefinitely; 0 polls without blocking."
  ;; libc::poll(struct pollfd *fds, nfds_t nfds, int timeout) → int.
  ;; pollfd layout: int fd (4) + short events (2) + short revents (2) = 8 bytes.
  (let* ((n (length pfds))
         (buf (nl-ffi-malloc (* n nelisp-os--pollfd-len))))
    (unwind-protect
        (progn
          (let ((idx 0))
            (dolist (entry pfds)
              (let ((off (* idx nelisp-os--pollfd-len)))
                (nl-ffi-write-i32 buf off            (car entry))
                (nl-ffi-write-i16 buf (+ off 4)      (cdr entry))
                ;; revents at offset+6 stays zero from malloc.
                )
              (setq idx (1+ idx))))
          (let ((r (nl-ffi-call "libc" "poll"
                                [:sint32 :pointer :uint64 :sint32]
                                buf n timeout-ms)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (let ((result nil)
                    (idx 0))
                (dolist (entry pfds)
                  (let ((off (* idx nelisp-os--pollfd-len)))
                    (push (cons (nl-ffi-read-i32 buf off)
                                (nl-ffi-read-i16 buf (+ off 6)))
                          result))
                  (setq idx (1+ idx)))
                (nreverse result)))))
      (nl-ffi-free buf))))

;; ---------------------------------------------------------------------------
;; Doc 56 Phase 4.1 — AF_UNIX + AF_INET6 socket family extensions.
;;
;; All other socket plumbing (`nelisp-os-socket' / -listen / -setsockopt-int
;; / -poll / -read / -write / -close) is family-independent and reused
;; verbatim from Doc 53/55.  Only bind / connect / accept differ between
;; sockaddr families.
;; ---------------------------------------------------------------------------

;; Address families
(defconst nelisp-os-AF-UNIX  1)
(defconst nelisp-os-AF-INET6 10)

;; sockaddr_un.sun_path capacity (Linux glibc).  Reference value — the
;; primitive enforces this internally and signals an error if exceeded.
(defconst nelisp-os-SUN-PATH-MAX 108)

;; IPv6 helper addresses — host-byte-order group lists.
(defconst nelisp-os-IN6ADDR-ANY      '(0 0 0 0 0 0 0 0))
(defconst nelisp-os-IN6ADDR-LOOPBACK '(0 0 0 0 0 0 0 1))

;; ----- AF_UNIX wrappers -----

(defun nelisp-os-bind-unix (fd path)
  "POSIX bind(2) for AF_UNIX (filesystem path).  Abstract namespace
sockets (= leading NUL byte) are not supported in Phase 4.1."
  (nelisp-os--check-errno (nelisp--syscall-bind-unix fd path)))

(defun nelisp-os-connect-unix (fd path)
  "POSIX connect(2) for AF_UNIX.  PATH is a filesystem path (must
already be bound by the listening server)."
  (nelisp-os--check-errno (nelisp--syscall-connect-unix fd path)))

(defun nelisp-os-accept-unix (fd)
  "POSIX accept(2) for AF_UNIX.  Returns cons (NEWFD . PEER-PATH); the
peer is typically anonymous on a listening server, in which case the
PEER-PATH is the empty string."
  (let ((r (nelisp--syscall-accept-unix fd)))
    (if (integerp r)
        (nelisp-os--check-errno r)
      r)))

;; ----- AF_INET6 wrappers -----

(defun nelisp-os-bind-inet6 (fd host6 port)
  "POSIX bind(2) for AF_INET6.  HOST6 is a list of 8 16-bit groups in
host byte order (e.g. `nelisp-os-IN6ADDR-LOOPBACK' for `::1').  PORT is
a 16-bit host-byte-order port number."
  (nelisp-os--check-errno (nelisp--syscall-bind-inet6 fd host6 port)))

(defun nelisp-os-connect-inet6 (fd host6 port)
  "POSIX connect(2) for AF_INET6.  HOST6 / PORT same convention as
`nelisp-os-bind-inet6'."
  (nelisp-os--check-errno (nelisp--syscall-connect-inet6 fd host6 port)))

(defun nelisp-os-accept-inet6 (fd)
  "POSIX accept(2) for AF_INET6.  Returns list (NEWFD CLIENT-HOST6
CLIENT-PORT); CLIENT-HOST6 is an 8-element list of 16-bit groups in
host byte order."
  (let ((r (nelisp--syscall-accept-inet6 fd)))
    (if (integerp r)
        (nelisp-os--check-errno r)
      r)))

;; ---------------------------------------------------------------------------
;; Doc 57 Phase 4.3 — Modern Linux event surface (pidfd / inotify / eventfd).
;;
;; pidfd_open / pidfd_send_signal / inotify_init1 / inotify_rm_watch /
;; eventfd2 ride the generic `nelisp--syscall' arm via `syscall_nr()'
;; symbol map.  inotify_add_watch + inotify_read need string / packed
;; binary handling and live in their own primitives.
;; ---------------------------------------------------------------------------

;; ----- pidfd flags -----

(defconst nelisp-os-PIDFD-NONBLOCK #x800)         ; O_NONBLOCK

;; ----- inotify event mask bits -----

(defconst nelisp-os-IN-ACCESS        #x1)
(defconst nelisp-os-IN-MODIFY        #x2)
(defconst nelisp-os-IN-ATTRIB        #x4)
(defconst nelisp-os-IN-CLOSE-WRITE   #x8)
(defconst nelisp-os-IN-CLOSE-NOWRITE #x10)
(defconst nelisp-os-IN-OPEN          #x20)
(defconst nelisp-os-IN-MOVED-FROM    #x40)
(defconst nelisp-os-IN-MOVED-TO      #x80)
(defconst nelisp-os-IN-CREATE        #x100)
(defconst nelisp-os-IN-DELETE        #x200)
(defconst nelisp-os-IN-DELETE-SELF   #x400)
(defconst nelisp-os-IN-MOVE-SELF     #x800)
(defconst nelisp-os-IN-ALL-EVENTS    #xFFF)

;; ----- inotify_init1 flags -----

(defconst nelisp-os-IN-NONBLOCK #x800)            ; O_NONBLOCK
(defconst nelisp-os-IN-CLOEXEC  #x80000)          ; O_CLOEXEC

;; ----- eventfd flags -----

(defconst nelisp-os-EFD-SEMAPHORE #x1)
(defconst nelisp-os-EFD-NONBLOCK  #x800)          ; O_NONBLOCK
(defconst nelisp-os-EFD-CLOEXEC   #x80000)        ; O_CLOEXEC

;; ----- pidfd wrappers -----

(defun nelisp-os-pidfd-open (pid flags)
  "Linux pidfd_open(2) — return a file descriptor referring to PID, or
signal `nelisp-os-error'.  FLAGS is currently 0 or
`nelisp-os-PIDFD-NONBLOCK'."
  (nelisp-os--check-errno (nelisp--syscall 'pidfd_open pid flags)))

(defun nelisp-os-pidfd-send-signal (pidfd sig flags)
  "Linux pidfd_send_signal(2) — send SIG to the process referenced by
PIDFD.  Phase 4.3 only supports `info = NULL', so siginfo_t is left
zero; pass FLAGS = 0 unless you know better."
  (nelisp-os--check-errno
   (nelisp--syscall 'pidfd_send_signal pidfd sig 0 flags)))

;; ----- inotify wrappers -----

(defun nelisp-os-inotify-init (flags)
  "Linux inotify_init1(2) — return a new inotify fd.  FLAGS is OR of
`nelisp-os-IN-NONBLOCK' / `nelisp-os-IN-CLOEXEC' (or 0)."
  (nelisp-os--check-errno (nelisp--syscall 'inotify_init1 flags)))

(defun nelisp-os-inotify-add-watch (fd path mask)
  "Linux inotify_add_watch(2) — return a watch descriptor (positive
integer) or signal `nelisp-os-error'.  MASK is OR of `IN-*' event bits."
  (nelisp-os--check-errno
   (nelisp--syscall-inotify-add-watch fd path mask)))

(defun nelisp-os-inotify-rm-watch (fd wd)
  "Linux inotify_rm_watch(2) — remove the watch identified by WD."
  (nelisp-os--check-errno (nelisp--syscall 'inotify_rm_watch fd wd)))

(defun nelisp-os-inotify-read (fd max-events)
  "Read up to MAX-EVENTS events off inotify FD.  Returns a list of
4-element lists `(WD MASK COOKIE NAME)'.  Empty list when no events
are ready (only possible when FD was opened `IN-NONBLOCK')."
  (let ((r (nelisp--syscall-inotify-read fd max-events)))
    (if (integerp r)
        (nelisp-os--check-errno r)
      r)))

;; ----- eventfd wrapper -----

(defun nelisp-os-eventfd (initval flags)
  "Linux eventfd2(2) — return a new eventfd with the given INITVAL
counter and FLAGS (OR of `EFD-*').  Read/write are 8-byte uint64
counters; use `nelisp-os-write' / `nelisp-os-read' on the returned fd."
  (nelisp-os--check-errno (nelisp--syscall 'eventfd2 initval flags)))

;; ---------------------------------------------------------------------------
;; Doc 58 Phase 4.1.1 + 4.1.2 — AF_UNIX abstract namespace + getsockname /
;; getpeername wrappers.
;;
;; Abstract sockets avoid the stale-file race that filesystem AF_UNIX
;; sockets suffer from (= the kernel auto-cleans on close).  Phase 4.1
;; only shipped filesystem-path bind/connect; this layer adds the
;; abstract-namespace counterparts.
;; ---------------------------------------------------------------------------

(defun nelisp-os-bind-unix-abstract (fd name)
  "Linux-specific bind(2) for an AF_UNIX abstract-namespace socket.
NAME is a NUL-free string; the kernel name is `\\0' + NAME and is
auto-cleaned on close.  Returns 0 or signals `nelisp-os-error'."
  (nelisp-os--check-errno (nelisp--syscall-bind-unix-abstract fd name)))

(defun nelisp-os-connect-unix-abstract (fd name)
  "Linux-specific connect(2) for an AF_UNIX abstract-namespace socket."
  (nelisp-os--check-errno (nelisp--syscall-connect-unix-abstract fd name)))

;; getsockname / getpeername — three families × two ops = six wrappers.
;; `_inet'  → list (HOST-INT PORT)             both host byte order
;; `_inet6' → list (HOST6-LIST PORT)            HOST6-LIST = 8 16-bit groups
;; `_unix'  → string PATH (filesystem) | (abstract . NAME) | "" (anonymous)

(defun nelisp-os-getsockname-inet (fd)
  "POSIX getsockname(2) for AF_INET — return list (HOST-INT PORT)."
  (let ((r (nelisp--syscall-getsockname-inet fd)))
    (if (integerp r) (nelisp-os--check-errno r) r)))

(defun nelisp-os-getsockname-inet6 (fd)
  "POSIX getsockname(2) for AF_INET6 — return list (HOST6 PORT)."
  (let ((r (nelisp--syscall-getsockname-inet6 fd)))
    (if (integerp r) (nelisp-os--check-errno r) r)))

(defun nelisp-os-getsockname-unix (fd)
  "POSIX getsockname(2) for AF_UNIX — return PATH string for
filesystem sockets, cons (abstract . NAME) for abstract sockets, or
\"\" for anonymous (= unbound)."
  (let ((r (nelisp--syscall-getsockname-unix fd)))
    (if (integerp r) (nelisp-os--check-errno r) r)))

(defun nelisp-os-getpeername-inet (fd)
  "POSIX getpeername(2) for AF_INET — return list (HOST-INT PORT)."
  (let ((r (nelisp--syscall-getpeername-inet fd)))
    (if (integerp r) (nelisp-os--check-errno r) r)))

(defun nelisp-os-getpeername-inet6 (fd)
  "POSIX getpeername(2) for AF_INET6 — return list (HOST6 PORT)."
  (let ((r (nelisp--syscall-getpeername-inet6 fd)))
    (if (integerp r) (nelisp-os--check-errno r) r)))

(defun nelisp-os-getpeername-unix (fd)
  "POSIX getpeername(2) for AF_UNIX — same return shape as
`nelisp-os-getsockname-unix'."
  (let ((r (nelisp--syscall-getpeername-unix fd)))
    (if (integerp r) (nelisp-os--check-errno r) r)))

;; ---------------------------------------------------------------------------
;; Doc 59 Phase 4.2 + 4.3.1 — signalfd + timerfd + sigprocmask.
;;
;; NeLisp deliberately routes all signal handling through `signalfd' (=
;; signals as fd-readable events under `poll') instead of POSIX
;; `sigaction'.  Async-callback signal handlers would force the
;; runtime to deal with re-entrancy, EINTR, and signal-safe elisp
;; dispatch — none of which is needed when the substrate already uses
;; an event loop.  See Doc 59 §1 for the rationale.
;; ---------------------------------------------------------------------------

;; ----- signalfd / sigprocmask flags / how -----

(defconst nelisp-os-SFD-NONBLOCK #x800)           ; O_NONBLOCK
(defconst nelisp-os-SFD-CLOEXEC  #x80000)         ; O_CLOEXEC

(defconst nelisp-os-SIG-BLOCK   0)
(defconst nelisp-os-SIG-UNBLOCK 1)
(defconst nelisp-os-SIG-SETMASK 2)

;; ----- timerfd clock ids / flags -----

(defconst nelisp-os-CLOCK-REALTIME            0)
(defconst nelisp-os-CLOCK-MONOTONIC           1)
(defconst nelisp-os-CLOCK-BOOTTIME            7)
(defconst nelisp-os-CLOCK-REALTIME-ALARM      8)
(defconst nelisp-os-CLOCK-BOOTTIME-ALARM      9)

(defconst nelisp-os-TFD-NONBLOCK     #x800)       ; O_NONBLOCK
(defconst nelisp-os-TFD-CLOEXEC      #x80000)     ; O_CLOEXEC
(defconst nelisp-os-TFD-TIMER-ABSTIME 1)

;; ----- common signal numbers (= same as Doc 55 SIG* but extended) -----

(defconst nelisp-os-SIGUSR1 10)
(defconst nelisp-os-SIGUSR2 12)
(defconst nelisp-os-SIGCHLD 17)
(defconst nelisp-os-SIGALRM 14)

;; ----- signalfd wrappers -----

(defun nelisp-os-signalfd (fd mask flags)
  "Linux signalfd4(2) — return (or update) a signalfd watching MASK.
FD = -1 to create a new fd, or an existing signalfd to update its
mask.  MASK is a list of signal numbers; FLAGS = OR of `SFD-*'."
  (nelisp-os--check-errno (nelisp--syscall-signalfd4 fd mask flags)))

(defun nelisp-os-signalfd-read (fd max-events)
  "Read up to MAX-EVENTS events off signalfd FD.  Returns a list of
6-element lists `(SIGNO ERRNO CODE PID UID STATUS)'.  Empty list when
no events are ready (only on FD opened `SFD-NONBLOCK')."
  (let ((r (nelisp--syscall-signalfd-read fd max-events)))
    (if (integerp r) (nelisp-os--check-errno r) r)))

(defun nelisp-os-sigprocmask (how mask)
  "POSIX pthread_sigmask(3) — apply MASK with HOW (= `SIG-BLOCK' /
`SIG-UNBLOCK' / `SIG-SETMASK').  Returns the previous mask as a
list of signal numbers."
  (let ((r (nelisp--syscall-sigprocmask how mask)))
    (if (integerp r) (nelisp-os--check-errno r) r)))

;; ----- timerfd wrappers -----

(defun nelisp-os-timerfd-create (clockid flags)
  "Linux timerfd_create(2) — return a new timer fd."
  (nelisp-os--check-errno (nelisp--syscall 'timerfd_create clockid flags)))

(defun nelisp-os-timerfd-settime (fd flags it-int-s it-int-ns it-val-s it-val-ns)
  "Linux timerfd_settime(2) — arm or disarm timer FD.  Returns the
previous itimerspec as 4-element list (PREV-INT-S PREV-INT-NS
PREV-VAL-S PREV-VAL-NS)."
  (let ((r (nelisp--syscall-timerfd-settime
            fd flags it-int-s it-int-ns it-val-s it-val-ns)))
    (if (integerp r) (nelisp-os--check-errno r) r)))

(defun nelisp-os-timerfd-gettime (fd)
  "Linux timerfd_gettime(2) — return current itimerspec as 4-element
list (INT-S INT-NS VAL-S VAL-NS)."
  (let ((r (nelisp--syscall-timerfd-gettime fd)))
    (if (integerp r) (nelisp-os--check-errno r) r)))

;; ergonomics helper — relative one-shot timer in milliseconds.
(defun nelisp-os-timerfd-set-relative-ms (fd ms)
  "Schedule a one-shot relative timer of MS milliseconds on FD.
Equivalent to `nelisp-os-timerfd-settime' with FLAGS=0, interval=0,
value=MS-as-(sec . nsec)."
  (let* ((sec  (/ ms 1000))
         (nsec (* (mod ms 1000) 1000000)))
    (nelisp-os-timerfd-settime fd 0 0 0 sec nsec)))

;; ---------------------------------------------------------------------------
;; Doc 60 Phase 4.4 — SCM_RIGHTS + SOCK_SEQPACKET + SO_PEERCRED + IPv6
;; scope_id full surface.  These wrappers close the last AF_UNIX /
;; AF_INET6 gap so daemon-style IPC (fd passing, peer auth,
;; boundary-preserving messages) and link-local IPv6 are all
;; expressible from elisp without touching anything beyond the Doc 53
;; substrate.  See docs/design/60-...org for design notes.
;; ---------------------------------------------------------------------------

;; ----- Extra socket type / cmsg / sockopt constants -----

(defconst nelisp-os-SOCK-SEQPACKET 5)              ; AF_UNIX boundary-preserving stream
(defconst nelisp-os-SCM-RIGHTS     1)              ; SOL_SOCKET cmsg type — fd passing
(defconst nelisp-os-SO-PEERCRED    17)             ; getsockopt option — struct ucred

;; ----- socketpair -----

(defun nelisp-os-socketpair (domain type protocol)
  "POSIX socketpair(2) — returns (FD1 . FD2) on success.
Typical use: (nelisp-os-socketpair AF-UNIX SOCK-STREAM 0).  Signals
`nelisp-os-error' on failure."
  (let ((r (nelisp--syscall-socketpair domain type protocol)))
    (if (and (consp r) (integerp (car r)) (integerp (cdr r)))
        r
      (nelisp-os--check-errno r))))

;; ----- SCM_RIGHTS fd passing -----

(defun nelisp-os-sendmsg-fds (fd fds payload)
  "Send PAYLOAD plus FDS (list of int file descriptors) over UDS FD
via sendmsg(2) + SCM_RIGHTS cmsg.  PAYLOAD must be a string ≥ 1 byte
(the kernel rejects cmsg-only sendmsg).  Returns bytes_sent."
  (nelisp-os--check-errno
   (nelisp--syscall-sendmsg-fds fd fds payload)))

(defun nelisp-os-recvmsg-fds (fd max-fds max-bytes)
  "Receive up to MAX-BYTES of payload + up to MAX-FDS file descriptors
over UDS FD via recvmsg(2).  Returns (PAYLOAD-STRING . FDS-LIST).
PAYLOAD-STRING is truncated to the actual bytes_received; FDS-LIST is
all descriptors collected from SCM_RIGHTS cmsgs (may be fewer than
MAX-FDS, never more)."
  (let ((r (nelisp--syscall-recvmsg-fds fd max-fds max-bytes)))
    (if (consp r)
        r
      (nelisp-os--check-errno r))))

;; ----- SO_PEERCRED -----

(defun nelisp-os-getsockopt-peercred (fd)
  "Retrieve the peer's `struct ucred' on AF_UNIX FD via getsockopt
SO_PEERCRED.  Returns (PID UID GID) on success."
  (let ((r (nelisp--syscall-getsockopt-peercred fd)))
    (if (and (listp r) (= (length r) 3))
        r
      (nelisp-os--check-errno r))))

;; ----- IPv6 scoped (full sockaddr_in6 surface) -----

(defun nelisp-os-bind-inet6-scoped (fd host6 port flowinfo scope-id)
  "Bind FD to IPv6 (HOST6 = 8-element host-byte-order group list, PORT,
FLOWINFO, SCOPE-ID).  All extra fields are wire-protocol-aware
(flowinfo gets htonl on the way out)."
  (nelisp-os--check-errno
   (nelisp--syscall-bind-inet6-scoped fd host6 port flowinfo scope-id)))

(defun nelisp-os-connect-inet6-scoped (fd host6 port flowinfo scope-id)
  "Same as `nelisp-os-bind-inet6-scoped' but for connect(2)."
  (nelisp-os--check-errno
   (nelisp--syscall-connect-inet6-scoped fd host6 port flowinfo scope-id)))

(defun nelisp-os-accept-inet6-scoped (fd)
  "Accept on listening IPv6 FD and return a 5-element list
(NEW-FD HOST6 PORT FLOWINFO SCOPE-ID)."
  (let ((r (nelisp--syscall-accept-inet6-scoped fd)))
    (if (and (listp r) (= (length r) 5))
        r
      (nelisp-os--check-errno r))))

(provide 'nelisp-stdlib-os)

;;; nelisp-stdlib-os.el ends here
