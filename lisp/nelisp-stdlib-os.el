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

;; ---------------------------------------------------------------------------
;; Doc 76 Stage B (2026-05-08) — execve argv/envp marshaling helper.
;;
;; libc.execve takes char *path, char *const argv[], char *const envp[].
;; Each `argv[]' / `envp[]' entry is a NUL-terminated C string; the
;; arrays themselves are NULL-terminated (= sentinel pointer).  We
;; allocate one buffer per string + one buffer per pointer-array.  All
;; allocations are tracked in `nl-ffi' alloc_table so the unwind-protect
;; cleanup `nl-ffi-free's them on the failure path.  On success execve
;; replaces the process so the leaks are moot.
;; ---------------------------------------------------------------------------

(defun nelisp-os--alloc-cstring (s)
  "Allocate (string-bytes S + 1) bytes, write S, return the pointer.
Trailing NUL terminator is satisfied by `nl-ffi-malloc' zeroing."
  (let* ((nbytes (string-bytes s))
         (buf    (nl-ffi-malloc (1+ nbytes))))
    (nl-ffi-write-bytes-at buf 0 s)
    buf))

(defun nelisp-os--build-cstr-array (strs)
  "Allocate per-string buffers + a NULL-terminated pointer array of
the same length.  Return (POINTER-ARRAY . LIST-OF-STRING-PTRS) so the
caller can `nl-ffi-free' both the array and each string after use."
  (let* ((string-ptrs (mapcar #'nelisp-os--alloc-cstring strs))
         (n           (length string-ptrs))
         (array       (nl-ffi-malloc (* (1+ n) 8)))
         (idx         0))
    (dolist (sp string-ptrs)
      (nl-ffi-write-i64 array (* idx 8) sp)
      (setq idx (1+ idx)))
    ;; Trailing NULL pointer at offset n*8 stays 0 from malloc.
    (cons array string-ptrs)))

(defun nelisp-os--free-cstr-array (pair)
  "Reverse `nelisp-os--build-cstr-array': free the array + each string."
  (let ((array       (car pair))
        (string-ptrs (cdr pair)))
    (dolist (sp string-ptrs) (nl-ffi-free sp))
    (nl-ffi-free array)))

(defun nelisp-os-execve (path argv envp)
  "POSIX execve(2) — replace current process image with PATH using ARGV
list and ENVP list of strings.  Only returns on failure (signals
`nelisp-os-error')."
  (let ((argv-pair (nelisp-os--build-cstr-array argv))
        (envp-pair (nelisp-os--build-cstr-array envp)))
    (unwind-protect
        (let ((r (nl-ffi-call "libc" "execve"
                              [:sint32 :string :pointer :pointer]
                              path (car argv-pair) (car envp-pair))))
          ;; libc.execve only returns on failure (= -1).
          (if (= r -1)
              (nelisp-os--ffi-errno-signal)
            r))
      (nelisp-os--free-cstr-array argv-pair)
      (nelisp-os--free-cstr-array envp-pair))))

(defun nelisp-os-wait (pid options)
  "POSIX wait4(2) — wait for child PID with OPTIONS (= 0, WNOHANG, etc.).
Returns cons (CHILD-PID . STATUS) on success, or signals
`nelisp-os-error' on failure.  When WNOHANG and no child is ready,
returns (0 . 0)."
  ;; libc.wait4(pid_t pid, int *status, int options, struct rusage *ru).
  ;; rusage = NULL (= raw 0 via :pointer).
  (let ((status-buf (nl-ffi-malloc 4)))
    (unwind-protect
        (let ((r (nl-ffi-call "libc" "wait4"
                              [:sint32 :sint32 :pointer :sint32 :pointer]
                              pid status-buf options 0)))
          (if (= r -1)
              (nelisp-os--ffi-errno-signal)
            (cons r (nl-ffi-read-i32 status-buf 0))))
      (nl-ffi-free status-buf))))

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

;; ---------------------------------------------------------------------------
;; Doc 76 Stage D (2026-05-08) — sockaddr_un / sockaddr_in6 marshaling
;; helpers.  sun_path layout (= sun_family u16 at 0 + 108 path bytes at 2,
;; total 110).  sockaddr_in6 layout (= sin6_family u16 at 0 + sin6_port
;; u16 BE at 2 + sin6_flowinfo u32 at 4 + sin6_addr u8[16] at 8 +
;; sin6_scope_id u32 at 24, total 28).
;; ---------------------------------------------------------------------------

(defconst nelisp-os--sockaddr-un-len 110)
(defconst nelisp-os--sockaddr-in6-len 28)

(defun nelisp-os--encode-sockaddr-un (buf path)
  "Populate BUF (= 110-byte zeroed `nl-ffi-malloc') with sockaddr_un for
filesystem path PATH.  Return the addrlen (= 2 + bytes + 1 NUL)."
  (nl-ffi-write-i16 buf 0 nelisp-os-AF-UNIX)
  (nl-ffi-write-bytes-at buf 2 path)
  (+ 3 (string-bytes path)))

(defun nelisp-os--encode-sockaddr-un-abstract (buf name)
  "Populate BUF with sockaddr_un for Linux abstract namespace NAME.
Leading sun_path[0] = NUL (= abstract sentinel) stays 0 from malloc.
Return the addrlen (= 2 + 1 + bytes)."
  (nl-ffi-write-i16 buf 0 nelisp-os-AF-UNIX)
  ;; sun_path[0] at offset 2 = 0 (already zeroed)
  (nl-ffi-write-bytes-at buf 3 name)
  (+ 3 (string-bytes name)))

(defun nelisp-os--decode-sockaddr-un (buf socklen)
  "Decode sockaddr_un at BUF.  SOCKLEN = total addrlen returned by
accept(2) / getsockname(2).  Return:
 - empty string for anonymous (= socklen ≤ 2);
 - cons (abstract . NAME) for abstract namespace (= sun_path[0] == NUL);
 - string PATH for filesystem path (= NUL-terminated)."
  (cond
   ((<= socklen 2) "")
   ((= (nl-ffi-read-u8 buf 2) 0)
    (let ((body-len (- socklen 3)))
      (cons 'abstract
            (if (<= body-len 0) "" (nl-ffi-read-bytes-at buf 3 body-len)))))
   (t
    (let* ((max-len (- socklen 2))
           (n (catch 'nul
                (dotimes (i max-len)
                  (when (= (nl-ffi-read-u8 buf (+ 2 i)) 0)
                    (throw 'nul i)))
                max-len)))
      (if (= n 0) "" (nl-ffi-read-bytes-at buf 2 n))))))

(defun nelisp-os--encode-sockaddr-in6 (buf groups port)
  "Populate BUF (= 28-byte zeroed) with sockaddr_in6: family + port BE +
flowinfo=0 + 8 BE u16 groups + scope_id=0."
  (nl-ffi-write-i16 buf 0 nelisp-os-AF-INET6)
  (let ((port-be (nl-ffi-call "libc" "htons" [:uint16 :uint16] port)))
    (nl-ffi-write-i16 buf 2 port-be))
  ;; flowinfo at 4 stays 0
  (let ((idx 0))
    (dolist (g groups)
      (let* ((off (+ 8 (* idx 2)))
             (g-be (nl-ffi-call "libc" "htons" [:uint16 :uint16] g)))
        (nl-ffi-write-i16 buf off g-be))
      (setq idx (1+ idx))))
  ;; scope_id at 24 stays 0
  )

(defun nelisp-os--decode-sockaddr-in6 (buf)
  "Decode 28-byte sockaddr_in6 BUF.  Return cons (GROUPS-LIST . PORT) in
host byte order."
  (let* ((port-be (nl-ffi-read-u16 buf 2))
         (port    (nl-ffi-call "libc" "ntohs" [:uint16 :uint16] port-be))
         (groups  nil))
    (dotimes (i 8)
      (let ((g-be (nl-ffi-read-u16 buf (+ 8 (* i 2)))))
        (push (nl-ffi-call "libc" "ntohs" [:uint16 :uint16] g-be) groups)))
    (cons (nreverse groups) port)))

;; ----- AF_UNIX wrappers -----

(defun nelisp-os-bind-unix (fd path)
  "POSIX bind(2) for AF_UNIX (filesystem path).  Abstract namespace
sockets use `nelisp-os-bind-unix-abstract'."
  (let ((buf (nl-ffi-malloc nelisp-os--sockaddr-un-len)))
    (unwind-protect
        (let* ((alen (nelisp-os--encode-sockaddr-un buf path))
               (r (nl-ffi-call "libc" "bind"
                               [:sint32 :sint32 :pointer :uint32]
                               fd buf alen)))
          (if (= r -1) (nelisp-os--ffi-errno-signal) r))
      (nl-ffi-free buf))))

(defun nelisp-os-connect-unix (fd path)
  "POSIX connect(2) for AF_UNIX.  PATH is a filesystem path."
  (let ((buf (nl-ffi-malloc nelisp-os--sockaddr-un-len)))
    (unwind-protect
        (let* ((alen (nelisp-os--encode-sockaddr-un buf path))
               (r (nl-ffi-call "libc" "connect"
                               [:sint32 :sint32 :pointer :uint32]
                               fd buf alen)))
          (if (= r -1) (nelisp-os--ffi-errno-signal) r))
      (nl-ffi-free buf))))

(defun nelisp-os-accept-unix (sockfd)
  "POSIX accept(2) for AF_UNIX.  Returns cons (NEWFD . PEER-PATH); the
peer is typically anonymous on a listening server."
  (let ((addr-buf (nl-ffi-malloc nelisp-os--sockaddr-un-len))
        (len-buf  (nl-ffi-malloc 4)))
    (unwind-protect
        (progn
          (nl-ffi-write-i32 len-buf 0 nelisp-os--sockaddr-un-len)
          (let ((newfd (nl-ffi-call "libc" "accept"
                                    [:sint32 :sint32 :pointer :pointer]
                                    sockfd addr-buf len-buf)))
            (if (= newfd -1)
                (nelisp-os--ffi-errno-signal)
              (let* ((socklen (nl-ffi-read-i32 len-buf 0))
                     ;; accept-unix legacy: peer string-only (= matches
                     ;; old `parse_sockaddr_un_peer' that stops at first
                     ;; NUL, so abstract peer surfaces as "").
                     (peer (cond
                            ((<= socklen 2) "")
                            ((= (nl-ffi-read-u8 addr-buf 2) 0) "")
                            (t (let* ((max-len (- socklen 2))
                                      (n (catch 'nul
                                           (dotimes (i max-len)
                                             (when (= (nl-ffi-read-u8 addr-buf (+ 2 i)) 0)
                                               (throw 'nul i)))
                                           max-len)))
                                 (if (= n 0) "" (nl-ffi-read-bytes-at addr-buf 2 n)))))))
                (cons newfd peer)))))
      (nl-ffi-free addr-buf)
      (nl-ffi-free len-buf))))

;; ----- AF_INET6 wrappers -----

(defun nelisp-os-bind-inet6 (fd host6 port)
  "POSIX bind(2) for AF_INET6.  HOST6 is a list of 8 16-bit groups in
host byte order (e.g. `nelisp-os-IN6ADDR-LOOPBACK')."
  (let ((buf (nl-ffi-malloc nelisp-os--sockaddr-in6-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sockaddr-in6 buf host6 port)
          (let ((r (nl-ffi-call "libc" "bind"
                                [:sint32 :sint32 :pointer :uint32]
                                fd buf nelisp-os--sockaddr-in6-len)))
            (if (= r -1) (nelisp-os--ffi-errno-signal) r)))
      (nl-ffi-free buf))))

(defun nelisp-os-connect-inet6 (fd host6 port)
  "POSIX connect(2) for AF_INET6.  HOST6 / PORT same convention as
`nelisp-os-bind-inet6'."
  (let ((buf (nl-ffi-malloc nelisp-os--sockaddr-in6-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sockaddr-in6 buf host6 port)
          (let ((r (nl-ffi-call "libc" "connect"
                                [:sint32 :sint32 :pointer :uint32]
                                fd buf nelisp-os--sockaddr-in6-len)))
            (if (= r -1) (nelisp-os--ffi-errno-signal) r)))
      (nl-ffi-free buf))))

(defun nelisp-os-accept-inet6 (sockfd)
  "POSIX accept(2) for AF_INET6.  Returns list (NEWFD CLIENT-HOST6
CLIENT-PORT); CLIENT-HOST6 is an 8-element list of 16-bit groups in
host byte order."
  (let ((addr-buf (nl-ffi-malloc nelisp-os--sockaddr-in6-len))
        (len-buf  (nl-ffi-malloc 4)))
    (unwind-protect
        (progn
          (nl-ffi-write-i32 len-buf 0 nelisp-os--sockaddr-in6-len)
          (let ((newfd (nl-ffi-call "libc" "accept"
                                    [:sint32 :sint32 :pointer :pointer]
                                    sockfd addr-buf len-buf)))
            (if (= newfd -1)
                (nelisp-os--ffi-errno-signal)
              (let ((gp (nelisp-os--decode-sockaddr-in6 addr-buf)))
                (list newfd (car gp) (cdr gp))))))
      (nl-ffi-free addr-buf)
      (nl-ffi-free len-buf))))

;; ---------------------------------------------------------------------------
;; Doc 57 Phase 4.3 — Modern Linux event surface (pidfd / inotify / eventfd).
;;
;; pidfd_open / pidfd_send_signal / inotify_init1 / inotify_rm_watch /
;; eventfd2 ride the generic `nelisp--syscall' arm via `syscall_nr()'
;; symbol map.  inotify_add_watch + inotify_read used to be specialized
;; Rust primitives (path string + packed `struct inotify_event' parse)
;; but were retired in Doc 76 Stage E (2026-05-09); the Stage E section
;; below now drives them elisp-side via `nl-ffi-call' libc.
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
;;
;; Doc 76 Stage E (2026-05-09): inotify_add_watch / inotify_read were
;; specialized Rust primitives (= path string + variable-length packed
;; `struct inotify_event' buffer) but are now driven elisp-side through
;; `nl-ffi-call' libc.  inotify_init1 / inotify_rm_watch keep riding
;; the generic int-only `nelisp--syscall' arm via syscall_nr() symbol map.
;;
;; struct inotify_event (Linux) layout — same on all glibc/musl arches:
;;   offset 0  i32 wd          (watch descriptor; -1 for IN_Q_OVERFLOW)
;;   offset 4  u32 mask        (event mask bits, see IN-* defconsts)
;;   offset 8  u32 cookie      (rename pair correlator)
;;   offset 12 u32 len         (length of name[], including NUL pad)
;;   offset 16 char name[len]  (NUL-padded UTF-8 file basename, len = 0
;;                              for events on the watched path itself)
;;   sizeof(struct inotify_event) without name = 16 bytes
;;   NAME_MAX (Linux) = 255, +1 NUL = 256 bytes max name field per event.
;;
;; Read pattern:
;;   1. nl-ffi-malloc (per-event-cap = 16 + 256 = 272) * MAX-EVENTS bytes
;;   2. libc.read (fd, buf, cap) → ssize_t n
;;   3. walk buf [0..n) parsing header (= read-i32/u32) + name field (=
;;      read-bytes-at + NUL-truncate via string-search), advance to next
;;      record at off + 16 + len
;; Empty result list = no events ready (only possible when FD opened
;; with IN-NONBLOCK).

(defun nelisp-os-inotify-init (flags)
  "Linux inotify_init1(2) — return a new inotify fd.  FLAGS is OR of
`nelisp-os-IN-NONBLOCK' / `nelisp-os-IN-CLOEXEC' (or 0)."
  (nelisp-os--check-errno (nelisp--syscall 'inotify_init1 flags)))

(defun nelisp-os-inotify-add-watch (fd path mask)
  "Linux inotify_add_watch(2) — return a watch descriptor (positive
integer) or signal `nelisp-os-error'.  MASK is OR of `IN-*' event bits."
  ;; libc::inotify_add_watch: int(int fd, const char *path, uint32_t mask)
  ;; → int.  Returns -1 on error and sets errno; the wd is a positive
  ;; per-fd integer otherwise.
  (let ((r (nl-ffi-call "libc" "inotify_add_watch"
                        [:sint32 :sint32 :string :uint32]
                        fd path mask)))
    (if (= r -1)
        (nelisp-os--ffi-errno-signal)
      r)))

(defun nelisp-os-inotify-rm-watch (fd wd)
  "Linux inotify_rm_watch(2) — remove the watch identified by WD."
  (nelisp-os--check-errno (nelisp--syscall 'inotify_rm_watch fd wd)))

(defun nelisp-os-inotify-read (fd max-events)
  "Read up to MAX-EVENTS events off inotify FD.  Returns a list of
4-element lists `(WD MASK COOKIE NAME)'.  Empty list when no events
are ready (only possible when FD was opened `IN-NONBLOCK')."
  ;; Doc 76 Stage E: was a Rust specialized primitive; now elisp-side
  ;; libc.read + nl-ffi-read-i32/u32/bytes-at parse loop.
  (let* ((per-event-cap (+ 16 256))             ; sizeof(header) + NAME_MAX+1
         (cap           (* max-events per-event-cap))
         (buf           (nl-ffi-malloc cap)))
    (unwind-protect
        (let ((n (nl-ffi-call "libc" "read"
                              [:sint64 :sint32 :pointer :uint64]
                              fd buf cap)))
          (cond
           ((= n -1) (nelisp-os--ffi-errno-signal))
           ((= n 0)  nil)
           (t (let ((events nil) (off 0))
                (catch 'done
                  (while (<= (+ off 16) n)
                    (let* ((wd     (nl-ffi-read-i32 buf off))
                           (mask   (nl-ffi-read-u32 buf (+ off 4)))
                           (cookie (nl-ffi-read-u32 buf (+ off 8)))
                           (nlen   (nl-ffi-read-u32 buf (+ off 12)))
                           (name-off (+ off 16)))
                      ;; Truncated tail (kernel never writes a partial
                      ;; record; this guards corruption / short read).
                      (when (> (+ name-off nlen) n)
                        (throw 'done nil))
                      (let* ((raw  (if (= nlen 0)
                                       ""
                                     (nl-ffi-read-bytes-at buf name-off nlen)))
                             (cut  (string-search "\0" raw))
                             (name (if cut (substring raw 0 cut) raw)))
                        (push (list wd mask cookie name) events))
                      (setq off (+ name-off nlen)))))
                (nreverse events)))))
      (nl-ffi-free buf))))

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
  (let ((buf (nl-ffi-malloc nelisp-os--sockaddr-un-len)))
    (unwind-protect
        (let* ((alen (nelisp-os--encode-sockaddr-un-abstract buf name))
               (r (nl-ffi-call "libc" "bind"
                               [:sint32 :sint32 :pointer :uint32]
                               fd buf alen)))
          (if (= r -1) (nelisp-os--ffi-errno-signal) r))
      (nl-ffi-free buf))))

(defun nelisp-os-connect-unix-abstract (fd name)
  "Linux-specific connect(2) for an AF_UNIX abstract-namespace socket."
  (let ((buf (nl-ffi-malloc nelisp-os--sockaddr-un-len)))
    (unwind-protect
        (let* ((alen (nelisp-os--encode-sockaddr-un-abstract buf name))
               (r (nl-ffi-call "libc" "connect"
                               [:sint32 :sint32 :pointer :uint32]
                               fd buf alen)))
          (if (= r -1) (nelisp-os--ffi-errno-signal) r))
      (nl-ffi-free buf))))

;; getsockname / getpeername — three families × two ops = six wrappers.
;; `_inet'  → list (HOST-INT PORT)             both host byte order
;; `_inet6' → list (HOST6-LIST PORT)            HOST6-LIST = 8 16-bit groups
;; `_unix'  → string PATH (filesystem) | (abstract . NAME) | "" (anonymous)

(defun nelisp-os--getname-inet (fd is-peer)
  (let ((addr-buf (nl-ffi-malloc nelisp-os--sockaddr-in-len))
        (len-buf  (nl-ffi-malloc 4)))
    (unwind-protect
        (progn
          (nl-ffi-write-i32 len-buf 0 nelisp-os--sockaddr-in-len)
          (let ((r (nl-ffi-call "libc" (if is-peer "getpeername" "getsockname")
                                [:sint32 :sint32 :pointer :pointer]
                                fd addr-buf len-buf)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (let ((hp (nelisp-os--decode-sockaddr-in addr-buf)))
                (list (car hp) (cdr hp))))))
      (nl-ffi-free addr-buf)
      (nl-ffi-free len-buf))))

(defun nelisp-os--getname-inet6 (fd is-peer)
  (let ((addr-buf (nl-ffi-malloc nelisp-os--sockaddr-in6-len))
        (len-buf  (nl-ffi-malloc 4)))
    (unwind-protect
        (progn
          (nl-ffi-write-i32 len-buf 0 nelisp-os--sockaddr-in6-len)
          (let ((r (nl-ffi-call "libc" (if is-peer "getpeername" "getsockname")
                                [:sint32 :sint32 :pointer :pointer]
                                fd addr-buf len-buf)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (let ((gp (nelisp-os--decode-sockaddr-in6 addr-buf)))
                (list (car gp) (cdr gp))))))
      (nl-ffi-free addr-buf)
      (nl-ffi-free len-buf))))

(defun nelisp-os--getname-unix (fd is-peer)
  (let ((addr-buf (nl-ffi-malloc nelisp-os--sockaddr-un-len))
        (len-buf  (nl-ffi-malloc 4)))
    (unwind-protect
        (progn
          (nl-ffi-write-i32 len-buf 0 nelisp-os--sockaddr-un-len)
          (let ((r (nl-ffi-call "libc" (if is-peer "getpeername" "getsockname")
                                [:sint32 :sint32 :pointer :pointer]
                                fd addr-buf len-buf)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (nelisp-os--decode-sockaddr-un addr-buf
                                              (nl-ffi-read-i32 len-buf 0)))))
      (nl-ffi-free addr-buf)
      (nl-ffi-free len-buf))))

(defun nelisp-os-getsockname-inet  (fd) (nelisp-os--getname-inet  fd nil))
(defun nelisp-os-getsockname-inet6 (fd) (nelisp-os--getname-inet6 fd nil))
(defun nelisp-os-getsockname-unix  (fd) (nelisp-os--getname-unix  fd nil))
(defun nelisp-os-getpeername-inet  (fd) (nelisp-os--getname-inet  fd t))
(defun nelisp-os-getpeername-inet6 (fd) (nelisp-os--getname-inet6 fd t))
(defun nelisp-os-getpeername-unix  (fd) (nelisp-os--getname-unix  fd t))

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

;; ---------------------------------------------------------------------------
;; Doc 76 Stage F (2026-05-08) — sigset_t + itimerspec + signalfd_siginfo
;; marshaling helpers.
;;
;; sigset_t = 128 bytes glibc-side; we drive it via libc.sigemptyset /
;; .sigaddset / .sigismember (= no need to know the bitmap layout).
;;
;; itimerspec = 32 bytes (= 2 × timespec, each 16 bytes: tv_sec i64 +
;; tv_nsec i64 LE).
;;
;; signalfd_siginfo = 128 bytes; offsets surfaced (matches old Rust
;; primitive's 6-tuple): signo @ 0 (u32), errno @ 4 (i32), code @ 8
;; (i32), pid @ 12 (u32), uid @ 16 (u32), status @ 40 (i32).
;; ---------------------------------------------------------------------------

(defconst nelisp-os--sigset-len 128)
(defconst nelisp-os--itimerspec-len 32)
(defconst nelisp-os--signalfd-siginfo-len 128)

(defun nelisp-os--encode-sigset (buf signals)
  "Populate BUF (= 128-byte zeroed) as sigset_t with SIGNALS list."
  (nl-ffi-call "libc" "sigemptyset" [:sint32 :pointer] buf)
  (dolist (sig signals)
    (nl-ffi-call "libc" "sigaddset" [:sint32 :pointer :sint32] buf sig)))

(defun nelisp-os--decode-sigset (buf)
  "Decode sigset_t at BUF.  Probe sigismember for signals 1..64
(= standard + Linux RT range) to recover the membership list."
  (let ((signos nil))
    (dotimes (i 64)
      (let ((sig (1+ i)))
        (when (= 1 (nl-ffi-call "libc" "sigismember"
                                [:sint32 :pointer :sint32] buf sig))
          (push sig signos))))
    (nreverse signos)))

(defun nelisp-os--encode-itimerspec (buf int-s int-ns val-s val-ns)
  "Populate BUF (= 32-byte zeroed) as itimerspec.  Layout: it_interval
(tv_sec i64 @ 0, tv_nsec i64 @ 8) + it_value (tv_sec i64 @ 16, tv_nsec
i64 @ 24)."
  (nl-ffi-write-i64 buf 0  int-s)
  (nl-ffi-write-i64 buf 8  int-ns)
  (nl-ffi-write-i64 buf 16 val-s)
  (nl-ffi-write-i64 buf 24 val-ns))

(defun nelisp-os--decode-itimerspec (buf)
  "Decode 32-byte itimerspec BUF into list (INT-S INT-NS VAL-S VAL-NS)."
  (list (nl-ffi-read-i64 buf 0)
        (nl-ffi-read-i64 buf 8)
        (nl-ffi-read-i64 buf 16)
        (nl-ffi-read-i64 buf 24)))

(defun nelisp-os--decode-signalfd-event (buf base)
  "Decode a single 128-byte signalfd_siginfo at BUF + BASE into 6-tuple
(SIGNO ERRNO CODE PID UID STATUS) matching the legacy primitive."
  (list (nl-ffi-read-u32 buf (+ base 0))
        (nl-ffi-read-i32 buf (+ base 4))
        (nl-ffi-read-i32 buf (+ base 8))
        (nl-ffi-read-u32 buf (+ base 12))
        (nl-ffi-read-u32 buf (+ base 16))
        (nl-ffi-read-i32 buf (+ base 40))))

;; ----- signalfd wrappers -----

(defun nelisp-os-signalfd (fd mask flags)
  "Linux signalfd4(2) — return (or update) a signalfd watching MASK.
FD = -1 to create a new fd, or an existing signalfd to update its
mask.  MASK is a list of signal numbers; FLAGS = OR of `SFD-*'."
  (let ((set-buf (nl-ffi-malloc nelisp-os--sigset-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sigset set-buf mask)
          (let ((r (nl-ffi-call "libc" "signalfd"
                                [:sint32 :sint32 :pointer :sint32]
                                fd set-buf flags)))
            (if (= r -1) (nelisp-os--ffi-errno-signal) r)))
      (nl-ffi-free set-buf))))

(defun nelisp-os-signalfd-read (fd max-events)
  "Read up to MAX-EVENTS events off signalfd FD.  Returns a list of
6-element lists `(SIGNO ERRNO CODE PID UID STATUS)'.  Empty list when
no events are ready (only on FD opened `SFD-NONBLOCK')."
  (let* ((cap (* nelisp-os--signalfd-siginfo-len max-events))
         (buf (nl-ffi-malloc cap)))
    (unwind-protect
        (let ((n (nl-ffi-call "libc" "read"
                              [:sint64 :sint32 :pointer :uint64]
                              fd buf cap)))
          (if (= n -1)
              (nelisp-os--ffi-errno-signal)
            (let ((events nil)
                  (off 0))
              (while (<= (+ off nelisp-os--signalfd-siginfo-len) n)
                (push (nelisp-os--decode-signalfd-event buf off) events)
                (setq off (+ off nelisp-os--signalfd-siginfo-len)))
              (nreverse events))))
      (nl-ffi-free buf))))

(defun nelisp-os-sigprocmask (how mask)
  "POSIX pthread_sigmask(3) — apply MASK with HOW (= `SIG-BLOCK' /
`SIG-UNBLOCK' / `SIG-SETMASK').  Returns the previous mask as a
list of signal numbers."
  (let ((new-buf (nl-ffi-malloc nelisp-os--sigset-len))
        (old-buf (nl-ffi-malloc nelisp-os--sigset-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sigset new-buf mask)
          (let ((r (nl-ffi-call "libc" "pthread_sigmask"
                                [:sint32 :sint32 :pointer :pointer]
                                how new-buf old-buf)))
            ;; pthread_sigmask returns errno directly (= 0 on success).
            (if (/= r 0)
                (signal 'nelisp-os-error (list r))
              (nelisp-os--decode-sigset old-buf))))
      (nl-ffi-free new-buf)
      (nl-ffi-free old-buf))))

;; ----- timerfd wrappers -----

(defun nelisp-os-timerfd-create (clockid flags)
  "Linux timerfd_create(2) — return a new timer fd."
  (nelisp-os--check-errno (nelisp--syscall 'timerfd_create clockid flags)))

(defun nelisp-os-timerfd-settime (fd flags it-int-s it-int-ns it-val-s it-val-ns)
  "Linux timerfd_settime(2) — arm or disarm timer FD.  Returns the
previous itimerspec as 4-element list (PREV-INT-S PREV-INT-NS
PREV-VAL-S PREV-VAL-NS)."
  (let ((new-buf (nl-ffi-malloc nelisp-os--itimerspec-len))
        (old-buf (nl-ffi-malloc nelisp-os--itimerspec-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-itimerspec new-buf it-int-s it-int-ns it-val-s it-val-ns)
          (let ((r (nl-ffi-call "libc" "timerfd_settime"
                                [:sint32 :sint32 :sint32 :pointer :pointer]
                                fd flags new-buf old-buf)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (nelisp-os--decode-itimerspec old-buf))))
      (nl-ffi-free new-buf)
      (nl-ffi-free old-buf))))

(defun nelisp-os-timerfd-gettime (fd)
  "Linux timerfd_gettime(2) — return current itimerspec as 4-element
list (INT-S INT-NS VAL-S VAL-NS)."
  (let ((cur-buf (nl-ffi-malloc nelisp-os--itimerspec-len)))
    (unwind-protect
        (let ((r (nl-ffi-call "libc" "timerfd_gettime"
                              [:sint32 :sint32 :pointer]
                              fd cur-buf)))
          (if (= r -1)
              (nelisp-os--ffi-errno-signal)
            (nelisp-os--decode-itimerspec cur-buf)))
      (nl-ffi-free cur-buf))))

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
