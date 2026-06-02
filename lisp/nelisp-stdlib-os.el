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
;;   - Path B (Darwin/Windows): `nelisp-os--libc-call' (Doc 51 Phase 5) for
;;     `libc.open' / `.write' / `.close' / `._exit'.  `read' is
;;     deferred to Phase 1.1 (needs mutable bytestring buffer
;;     primitive).
;;
;; Platform detect runs once at load time via
;; `nelisp--syscall-supported-p'; downstream callers see a single
;; OS-agnostic API.
;;
;; Doc 138 Stage 5 starts the Windows branch: stdout/stderr writes can route
;; through kernel32 HANDLE I/O (`GetStdHandle' + `WriteFile') instead of POSIX
;; integer fd `write'.  Stage 6 adds stdin reads through `ReadFile'.  Stage 7
;; adds a small Windows fd->HANDLE table for regular file I/O via `CreateFileW'
;; / `CloseHandle'.  Stage 9 reports Windows HANDLE failures through
;; `GetLastError'.  Stage 10 maps the anonymous mmap/mprotect/munmap surface to
;; `VirtualAlloc' / `VirtualProtect' / `VirtualFree'.  Stage 11 routes the
;; minimal process identity/exit surface through `GetCurrentProcessId' and
;; `ExitProcess'.  Stage 12 maps `pipe' to `CreatePipe' and stores both HANDLEs
;; in the Windows fd table.  Stage 13 maps `lseek' to `SetFilePointerEx'.
;; Stage 14 maps the minimal `fstat' shape to `GetFileType' / `GetFileSizeEx'.
;; Stage 15 maps `dup2' to `DuplicateHandle' / `SetStdHandle'.  Stage 16 maps
;; single-PID `kill' to `OpenProcess' / `TerminateProcess'.  Stage 17 maps
;; `execve' to `CreateProcessW' + `ExitProcess'.  Stage 18 maps `wait' to
;; `OpenProcess' / `WaitForSingleObject' / `GetExitCodeProcess'.  Stage 19
;; maps `getppid' to the Tool Help process snapshot APIs.  Stage 20 adds a
;; minimal Windows `fcntl' compatibility branch for `F_DUPFD' / `F_GETFL' /
;; `F_SETFL'.  Stage 21 rejects Linux-only event/process fd APIs on Windows
;; before they can fall through to Linux syscall or libc paths.  Stage 22 starts
;; the Winsock branch with `WSAStartup' + `socket' and socket-specific close via
;; `closesocket'.  Stage 23 maps AF_INET `bind' / `connect' / `listen' to
;; Winsock.  Stage 24 maps AF_INET `accept' to Winsock and registers accepted
;; sockets in the Windows fd table.  Stage 25 maps socket fd `read' / `write'
;; to Winsock `recv' / `send'.  Stage 26 maps socket fd `poll' to `WSAPoll'.
;; Stage 27 maps int-valued socket options to Winsock `setsockopt'.  The
;; Linux/Darwin path remains the default until a real Windows standalone runtime
;; selects `system-type' = `windows-nt'.

;;; Code:

(require 'nelisp-stdlib-os-int-helpers)

(define-error 'nelisp-os-error "NeLisp OS error")

;; ---------------------------------------------------------------------------
;; Platform detect — set once, drives all branches below.
;; ---------------------------------------------------------------------------

(defconst nelisp-os--use-direct-syscall
  (nelisp--syscall-supported-p)
  "When t, route OS calls through `nelisp--syscall' (libc::syscall on
Linux/BSD).  When nil, fall back to `nelisp-os--libc-call' libc bindings
(Darwin/Windows).  Set once at load time.")

(defun nelisp-os--windows-p ()
  "Return non-nil when running on a native Windows host."
  (and (boundp 'system-type)
       (eq system-type 'windows-nt)))

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

;; Windows standard HANDLE selectors for GetStdHandle.
(defconst nelisp-os-WIN-STD-INPUT-HANDLE  -10)
(defconst nelisp-os-WIN-STD-OUTPUT-HANDLE -11)
(defconst nelisp-os-WIN-STD-ERROR-HANDLE  -12)

;; Windows CreateFileW constants.
(defconst nelisp-os-WIN-GENERIC-READ  #x80000000)
(defconst nelisp-os-WIN-GENERIC-WRITE #x40000000)
(defconst nelisp-os-WIN-FILE-APPEND-DATA #x4)
(defconst nelisp-os-WIN-FILE-SHARE-READ   #x1)
(defconst nelisp-os-WIN-FILE-SHARE-WRITE  #x2)
(defconst nelisp-os-WIN-FILE-SHARE-DELETE #x4)
(defconst nelisp-os-WIN-CREATE-NEW        1)
(defconst nelisp-os-WIN-CREATE-ALWAYS     2)
(defconst nelisp-os-WIN-OPEN-EXISTING     3)
(defconst nelisp-os-WIN-OPEN-ALWAYS       4)
(defconst nelisp-os-WIN-TRUNCATE-EXISTING 5)
(defconst nelisp-os-WIN-FILE-ATTRIBUTE-NORMAL #x80)

;; Windows virtual memory constants.
(defconst nelisp-os-WIN-MEM-COMMIT  #x1000)
(defconst nelisp-os-WIN-MEM-RESERVE #x2000)
(defconst nelisp-os-WIN-MEM-RELEASE #x8000)
(defconst nelisp-os-WIN-PAGE-NOACCESS          #x01)
(defconst nelisp-os-WIN-PAGE-READONLY          #x02)
(defconst nelisp-os-WIN-PAGE-READWRITE         #x04)
(defconst nelisp-os-WIN-PAGE-EXECUTE           #x10)
(defconst nelisp-os-WIN-PAGE-EXECUTE-READ      #x20)
(defconst nelisp-os-WIN-PAGE-EXECUTE-READWRITE #x40)

;; Windows GetFileType constants.
(defconst nelisp-os-WIN-FILE-TYPE-UNKNOWN 0)
(defconst nelisp-os-WIN-FILE-TYPE-DISK    1)
(defconst nelisp-os-WIN-FILE-TYPE-CHAR    2)
(defconst nelisp-os-WIN-FILE-TYPE-PIPE    3)

;; Windows DuplicateHandle constants.
(defconst nelisp-os-WIN-DUPLICATE-SAME-ACCESS #x2)

;; Windows process access constants.
(defconst nelisp-os-WIN-PROCESS-TERMINATE #x0001)
(defconst nelisp-os-WIN-PROCESS-QUERY-LIMITED-INFORMATION #x1000)
(defconst nelisp-os-WIN-SYNCHRONIZE #x00100000)

;; Windows wait constants.
(defconst nelisp-os-WIN-INFINITE #xffffffff)
(defconst nelisp-os-WIN-WAIT-OBJECT-0 #x00000000)
(defconst nelisp-os-WIN-WAIT-TIMEOUT #x00000102)

;; Windows Tool Help constants and PROCESSENTRY32W layout for x86_64.
(defconst nelisp-os-WIN-TH32CS-SNAPPROCESS #x00000002)
(defconst nelisp-os-WIN-ERROR-NO-MORE-FILES 18)
(defconst nelisp-os-WIN-PROCESSENTRY32W-SIZE 568)
(defconst nelisp-os-WIN-PROCESSENTRY32W-DWSIZE-OFFSET 0)
(defconst nelisp-os-WIN-PROCESSENTRY32W-PID-OFFSET 8)
(defconst nelisp-os-WIN-PROCESSENTRY32W-PPID-OFFSET 32)

;; Windows Winsock constants.
(defconst nelisp-os-WIN-WINSOCK-VERSION-2-2 #x0202)
(defconst nelisp-os-WIN-WSADATA-SIZE 408)
(defconst nelisp-os-WIN-INVALID-SOCKET -1)
(defconst nelisp-os-WIN-SOL-SOCKET #xffff)
(defconst nelisp-os-WIN-SO-REUSEADDR #x0004)
(defconst nelisp-os-WIN-SO-KEEPALIVE #x0008)

;; Windows process-launch structure sizes/offsets (x86_64).
(defconst nelisp-os-WIN-STARTUPINFOW-SIZE 104)
(defconst nelisp-os-WIN-STARTUPINFOW-CB-OFFSET 0)
(defconst nelisp-os-WIN-PROCESS-INFORMATION-SIZE 24)
(defconst nelisp-os-WIN-PROCESS-INFORMATION-HPROCESS-OFFSET 0)
(defconst nelisp-os-WIN-PROCESS-INFORMATION-HTHREAD-OFFSET 8)

(defvar nelisp-os--windows-next-fd 3
  "Next POSIX-like fd number for Windows HANDLE table entries.")

(defvar nelisp-os--windows-fd-table nil
  "Alist mapping POSIX-like fds to Windows HANDLE values.")

(defvar nelisp-os--windows-fd-kind-table nil
  "Alist mapping POSIX-like Windows fds to resource kind symbols.")

(defvar nelisp-os--windows-winsock-started-p nil
  "Non-nil after this process successfully calls WSAStartup.")

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
;; Doc 76 Stage A.1 — open / read / write are now nelisp-os--libc-call libc unified
;; (= no Path A/B branch).  libc functions return -1 on error and set
;; errno; we read errno through `nelisp-os--errno' and signal `nelisp-os-error'.
;; ---------------------------------------------------------------------------

(defun nelisp-os--ffi-errno-signal ()
  "Signal `nelisp-os-error' with the current errno."
  (signal 'nelisp-os-error (list (nelisp-os--errno))))

(defun nelisp-os--windows-ffi-error-signal ()
  "Signal `nelisp-os-error' for a Windows FFI failure.
The payload is the raw `GetLastError' DWORD, not a POSIX errno."
  (signal 'nelisp-os-error
          (list (nelisp-os--libc-call "kernel32" "GetLastError" [:uint32]))))

(defun nelisp-os--windows-winsock-error-signal ()
  "Signal `nelisp-os-error' with the current Winsock error code."
  (signal 'nelisp-os-error
          (list (nelisp-os--libc-call "ws2_32" "WSAGetLastError" [:sint32]))))

(defun nelisp-os--windows-unsupported ()
  "Signal ENOTSUP for a POSIX/Linux API with no Windows branch yet."
  (signal 'nelisp-os-error (list 95)))

(defun nelisp-os--windows-std-handle-selector (fd)
  "Return GetStdHandle selector for POSIX-like FD, or nil if unsupported."
  (cond
   ((= fd nelisp-os-STDIN)  nelisp-os-WIN-STD-INPUT-HANDLE)
   ((= fd nelisp-os-STDOUT) nelisp-os-WIN-STD-OUTPUT-HANDLE)
   ((= fd nelisp-os-STDERR) nelisp-os-WIN-STD-ERROR-HANDLE)
   (t nil)))

(defun nelisp-os--windows-get-std-handle (fd)
  "Return the Windows HANDLE corresponding to POSIX-like FD."
  (let ((selector (nelisp-os--windows-std-handle-selector fd)))
    (unless selector
      (signal 'nelisp-os-error (list 9))) ; EBADF
    (let ((handle (nelisp-os--libc-call
                   "kernel32" "GetStdHandle" [:pointer :sint32] selector)))
      ;; GetStdHandle returns NULL on failure and INVALID_HANDLE_VALUE (-1) for
      ;; invalid selectors.  Treat both as OS errors.
      (if (or (= handle 0) (= handle -1))
          (nelisp-os--windows-ffi-error-signal)
        handle))))

(defun nelisp-os--windows-fd-alloc (handle &optional kind)
  "Allocate a POSIX-like fd for Windows HANDLE.
KIND is nil for normal HANDLE-backed fds or `socket' for Winsock sockets."
  (let ((fd nelisp-os--windows-next-fd))
    (setq nelisp-os--windows-next-fd (1+ nelisp-os--windows-next-fd))
    (push (cons fd handle) nelisp-os--windows-fd-table)
    (when kind
      (push (cons fd kind) nelisp-os--windows-fd-kind-table))
    fd))

(defun nelisp-os--windows-fd-alloc-at-least (handle min-fd)
  "Allocate a POSIX-like fd for Windows HANDLE with fd number >= MIN-FD."
  (when (< min-fd 0)
    (signal 'nelisp-os-error (list 22))) ; EINVAL
  (when (< nelisp-os--windows-next-fd min-fd)
    (setq nelisp-os--windows-next-fd min-fd))
  (nelisp-os--windows-fd-alloc handle))

(defun nelisp-os--windows-fd-kind (fd)
  "Return the Windows resource kind for FD."
  (or (cdr (assq fd nelisp-os--windows-fd-kind-table))
      'handle))

(defun nelisp-os--windows-socket-for-fd (fd)
  "Return the Winsock SOCKET for FD, or signal EBADF."
  (unless (eq (nelisp-os--windows-fd-kind fd) 'socket)
    (signal 'nelisp-os-error (list 9))) ; EBADF
  (nelisp-os--windows-fd-handle fd))

(defun nelisp-os--windows-fd-handle (fd)
  "Return the Windows HANDLE for FD, or signal EBADF."
  (let ((cell (assq fd nelisp-os--windows-fd-table)))
    (if cell
        (cdr cell)
      (signal 'nelisp-os-error (list 9)))))

(defun nelisp-os--windows-fd-remove (fd)
  "Remove FD from the Windows HANDLE table and return its HANDLE."
  (let ((cell (assq fd nelisp-os--windows-fd-table)))
    (unless cell
      (signal 'nelisp-os-error (list 9)))
    (setq nelisp-os--windows-fd-table
          (delq cell nelisp-os--windows-fd-table))
    (let ((kind-cell (assq fd nelisp-os--windows-fd-kind-table)))
      (when kind-cell
        (setq nelisp-os--windows-fd-kind-table
              (delq kind-cell nelisp-os--windows-fd-kind-table))))
    (cdr cell)))

(defun nelisp-os--windows-close-resource (handle kind)
  "Close Windows HANDLE according to KIND."
  (cond
   ((eq kind 'socket)
    (let ((ok (nelisp-os--libc-call
               "ws2_32" "closesocket" [:sint32 :pointer] handle)))
      (if (= ok -1)
          (nelisp-os--windows-winsock-error-signal)
        nil)))
   (t
    (let ((ok (nelisp-os--libc-call
               "kernel32" "CloseHandle" [:sint32 :pointer] handle)))
      (if (= ok 0)
          (nelisp-os--windows-ffi-error-signal)
        nil)))))

(defun nelisp-os--windows-handle-for-fd (fd)
  "Return a Windows HANDLE for POSIX-like FD."
  (if (nelisp-os--windows-std-handle-selector fd)
      (nelisp-os--windows-get-std-handle fd)
    (nelisp-os--windows-fd-handle fd)))

(defun nelisp-os--windows-fd-install (fd handle)
  "Install HANDLE at POSIX-like FD, replacing any existing HANDLE."
  (let ((cell (assq fd nelisp-os--windows-fd-table)))
    (when cell
      (nelisp-os--windows-close-resource
       (cdr cell)
       (nelisp-os--windows-fd-kind fd))
      (let ((kind-cell (assq fd nelisp-os--windows-fd-kind-table)))
        (when kind-cell
          (setq nelisp-os--windows-fd-kind-table
                (delq kind-cell nelisp-os--windows-fd-kind-table))))
      (setq nelisp-os--windows-fd-table
            (delq cell nelisp-os--windows-fd-table)))
    (push (cons fd handle) nelisp-os--windows-fd-table)
    (when (>= fd nelisp-os--windows-next-fd)
      (setq nelisp-os--windows-next-fd (1+ fd)))
    fd))

(defun nelisp-os--windows-open-access (flags)
  "Translate POSIX-like FLAGS to a CreateFileW desired-access mask."
  (let ((mode (logand flags 3))
        (append (not (= 0 (logand flags nelisp-os-O-APPEND)))))
    (cond
     ((and append (= mode nelisp-os-O-RDWR))
      (logior nelisp-os-WIN-GENERIC-READ nelisp-os-WIN-FILE-APPEND-DATA))
     ((and append (= mode nelisp-os-O-WRONLY))
      nelisp-os-WIN-FILE-APPEND-DATA)
     ((= mode nelisp-os-O-RDWR)
      (logior nelisp-os-WIN-GENERIC-READ nelisp-os-WIN-GENERIC-WRITE))
     ((= mode nelisp-os-O-WRONLY)
      nelisp-os-WIN-GENERIC-WRITE)
     (t
      nelisp-os-WIN-GENERIC-READ))))

(defun nelisp-os--windows-open-disposition (flags)
  "Translate POSIX-like FLAGS to a CreateFileW creation disposition."
  (let ((creat (not (= 0 (logand flags nelisp-os-O-CREAT))))
        (excl  (not (= 0 (logand flags nelisp-os-O-EXCL))))
        (trunc (not (= 0 (logand flags nelisp-os-O-TRUNC)))))
    (cond
     ((and creat excl) nelisp-os-WIN-CREATE-NEW)
     ((and creat trunc) nelisp-os-WIN-CREATE-ALWAYS)
     (creat nelisp-os-WIN-OPEN-ALWAYS)
     (trunc nelisp-os-WIN-TRUNCATE-EXISTING)
     (t nelisp-os-WIN-OPEN-EXISTING))))

(defun nelisp-os--windows-utf16-code-units (str)
  "Return UTF-16 code units for STR as a list of unsigned 16-bit integers."
  (let ((units nil))
    (dotimes (i (length str))
      (let ((ch (aref str i)))
        (cond
         ((or (< ch 0) (> ch #x10ffff)
              (and (<= #xd800 ch) (<= ch #xdfff)))
          (signal 'nelisp-os-error (list 22))) ; EINVAL
         ((<= ch #xffff)
          (push ch units))
         (t
          (let* ((n (- ch #x10000))
                 (hi (+ #xd800 (ash n -10)))
                 (lo (+ #xdc00 (logand n #x3ff))))
            (push hi units)
            (push lo units))))))
    (nreverse units)))

(defun nelisp-os--windows-write-utf16le-z (buf units)
  "Write UTF-16LE UNITS plus a terminating NUL WCHAR to BUF."
  (let ((idx 0))
    (dolist (unit units)
      (nelisp-os-write-u16 buf (* idx 2) unit)
      (setq idx (1+ idx)))
    (nelisp-os-write-u16 buf (* idx 2) 0)
    buf))

(defun nelisp-os--windows-quote-command-arg (arg)
  "Quote one Windows command-line ARG using CreateProcess-compatible rules."
  (let ((needs-quote (or (= (length arg) 0)
                         (string-match-p "[ \t\n\v\"]" arg)))
        (slashes 0)
        (out nil))
    (if (not needs-quote)
        arg
      (push ?\" out)
      (dotimes (i (length arg))
        (let ((ch (aref arg i)))
          (cond
           ((= ch ?\\)
            (setq slashes (1+ slashes)))
           ((= ch ?\")
            (dotimes (_ (* 2 slashes)) (push ?\\ out))
            (setq slashes 0)
            (push ?\\ out)
            (push ch out))
           (t
            (dotimes (_ slashes) (push ?\\ out))
            (setq slashes 0)
            (push ch out)))))
      (dotimes (_ (* 2 slashes)) (push ?\\ out))
      (push ?\" out)
      (concat (nreverse out)))))

(defun nelisp-os--windows-command-line (path argv)
  "Return a Windows command line for PATH and ARGV."
  (mapconcat #'nelisp-os--windows-quote-command-arg
             (if argv argv (list path))
             " "))

(defun nelisp-os--windows-env-block (envp)
  "Return a Windows UTF-16 environment block string from ENVP."
  (let ((block (mapconcat #'identity envp "\0")))
    (if envp
        (concat block "\0")
      "\0")))

(defun nelisp-os--windows-alloc-utf16le-z (str)
  "Allocate a UTF-16LE NUL-terminated buffer containing STR."
  (let* ((units (nelisp-os--windows-utf16-code-units str))
         (buf (nelisp-os--alloc (* 2 (1+ (length units))))))
    (nelisp-os--windows-write-utf16le-z buf units)
    buf))

(defun nelisp-os--windows-open (path flags _mode)
  "Open PATH on Windows and return a POSIX-like fd."
  (let* ((units (nelisp-os--windows-utf16-code-units path))
         (path-buf (nelisp-os--alloc (* 2 (1+ (length units)))))
         (share (logior nelisp-os-WIN-FILE-SHARE-READ
                       nelisp-os-WIN-FILE-SHARE-WRITE
                       nelisp-os-WIN-FILE-SHARE-DELETE)))
    (unwind-protect
        (progn
          (nelisp-os--windows-write-utf16le-z path-buf units)
          (let ((handle (nelisp-os--libc-call
                         "kernel32" "CreateFileW"
                         [:pointer :pointer :uint32 :uint32 :pointer
                          :uint32 :uint32 :pointer]
                         path-buf
                         (nelisp-os--windows-open-access flags)
                         share
                         0
                         (nelisp-os--windows-open-disposition flags)
                         nelisp-os-WIN-FILE-ATTRIBUTE-NORMAL
                         0)))
            (if (or (= handle 0) (= handle -1))
                (nelisp-os--windows-ffi-error-signal)
              (nelisp-os--windows-fd-alloc handle))))
      (nelisp-os--free path-buf))))

(defun nelisp-os--windows-write-handle (handle str)
  "Write STR bytes to Windows HANDLE using kernel32!WriteFile."
  (let* ((nbytes (string-bytes str))
         (buf (nelisp-os--alloc (max nbytes 1)))
         (written-buf (nelisp-os--alloc 4)))
    (unwind-protect
        (progn
          (when (> nbytes 0)
            (nelisp-os--write-bytes buf str))
          (nelisp-os-write-u32 written-buf 0 0)
          (let ((ok (nelisp-os--libc-call
                     "kernel32" "WriteFile"
                     [:sint32 :pointer :pointer :uint32 :pointer :pointer]
                     handle buf nbytes written-buf 0)))
            (if (= ok 0)
                (nelisp-os--windows-ffi-error-signal)
              (nelisp-os-read-u32 written-buf 0))))
      (nelisp-os--free written-buf)
      (nelisp-os--free buf))))

(defun nelisp-os--windows-write-std-fd (fd str)
  "Write STR to Windows standard FD using HANDLE I/O."
  (nelisp-os--windows-write-handle
   (nelisp-os--windows-get-std-handle fd)
   str))

(defun nelisp-os--windows-read-handle (handle nbytes)
  "Read up to NBYTES bytes from Windows HANDLE using kernel32!ReadFile."
  (let ((buf (nelisp-os--alloc nbytes))
        (read-buf (nelisp-os--alloc 4)))
    (unwind-protect
        (progn
          (nelisp-os-write-u32 read-buf 0 0)
          (let ((ok (nelisp-os--libc-call
                     "kernel32" "ReadFile"
                     [:sint32 :pointer :pointer :uint32 :pointer :pointer]
                     handle buf nbytes read-buf 0)))
            (if (= ok 0)
                (nelisp-os--windows-ffi-error-signal)
              (let ((nread (nelisp-os-read-u32 read-buf 0)))
                (if (= nread 0)
                    ""
                  (nelisp-os--read-bytes buf nread))))))
      (nelisp-os--free read-buf)
      (nelisp-os--free buf))))

(defun nelisp-os--windows-read-std-fd (fd nbytes)
  "Read up to NBYTES bytes from a Windows standard FD using HANDLE I/O."
  (nelisp-os--windows-read-handle
   (nelisp-os--windows-get-std-handle fd)
   nbytes))

(defun nelisp-os--windows-read-socket (fd nbytes)
  "Read up to NBYTES bytes from a Windows socket fd using Winsock recv."
  (let ((buf (nelisp-os--alloc nbytes)))
    (unwind-protect
        (let ((nread (nelisp-os--libc-call
                      "ws2_32" "recv"
                      [:sint32 :pointer :pointer :sint32 :sint32]
                      (nelisp-os--windows-socket-for-fd fd)
                      buf nbytes 0)))
          (cond
           ((= nread -1) (nelisp-os--windows-winsock-error-signal))
           ((= nread 0) "")
           (t (nelisp-os--read-bytes buf nread))))
      (nelisp-os--free buf))))

(defun nelisp-os--windows-write-socket (fd str)
  "Write STR bytes to a Windows socket fd using Winsock send."
  (let* ((nbytes (string-bytes str))
         (buf (nelisp-os--alloc (max nbytes 1))))
    (unwind-protect
        (progn
          (when (> nbytes 0)
            (nelisp-os--write-bytes buf str))
          (let ((sent (nelisp-os--libc-call
                       "ws2_32" "send"
                       [:sint32 :pointer :pointer :sint32 :sint32]
                       (nelisp-os--windows-socket-for-fd fd)
                       buf nbytes 0)))
            (if (= sent -1)
                (nelisp-os--windows-winsock-error-signal)
              sent)))
      (nelisp-os--free buf))))

(defun nelisp-os-open (path flags mode)
  "POSIX open(2) — return integer fd, or signal `nelisp-os-error'.
PATH is a string, FLAGS / MODE are integers."
  ;; libc::open: int(const char *path, int flags, mode_t mode) → int.
  ;; Linux glibc: int=:sint32, mode_t=:uint32.
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-open path flags mode)
    (let ((r (nelisp-os--libc-call "libc" "open" [:sint32 :string :sint32 :uint32]
                          path flags mode)))
      (if (= r -1)
          (nelisp-os--ffi-errno-signal)
        r))))

(defun nelisp-os-read (fd nbytes)
  "POSIX read(2) — return string of up to NBYTES bytes or signal
`nelisp-os-error'."
  ;; libc::read: ssize_t(int fd, void *buf, size_t count) → ssize_t.
  ;; Linux x86_64 / aarch64: ssize_t=:sint64, size_t=:uint64.
  (when (< nbytes 0)
    (signal 'nelisp-os-error (list 22)))     ; EINVAL
  (cond
   ((= nbytes 0) "")
   ((and (nelisp-os--windows-p)
         (= fd nelisp-os-STDIN))
    (nelisp-os--windows-read-std-fd fd nbytes))
   ((and (nelisp-os--windows-p)
         (eq (nelisp-os--windows-fd-kind fd) 'socket))
    (nelisp-os--windows-read-socket fd nbytes))
   ((nelisp-os--windows-p)
    (nelisp-os--windows-read-handle
     (nelisp-os--windows-fd-handle fd)
     nbytes))
   (t
    (let ((buf (nelisp-os--alloc nbytes)))
      (unwind-protect
          (let ((r (nelisp-os--libc-call "libc" "read"
                                [:sint64 :sint32 :pointer :uint64]
                                fd buf nbytes)))
            (cond
             ((= r -1) (nelisp-os--ffi-errno-signal))
             ((= r 0)  "")
             (t        (nelisp-os--read-bytes buf r))))
        (nelisp-os--free buf))))))

(defun nelisp-os-write (fd str)
  "POSIX write(2) — write the bytes of STR to FD, return byte count
or signal `nelisp-os-error'.

Binary-safe: STR may contain interior NUL bytes and multi-byte UTF-8
sequences; we route through `nelisp-os--alloc' / `-write-bytes' so the
exact bytes (= `string-bytes' worth) reach libc.write — matching old
Path A's `as_bytes()' semantics rather than the broken Path B that
went through `:string' (= CString::new, NUL-rejecting)."
  (cond
   ((and (nelisp-os--windows-p)
         (nelisp-os--windows-std-handle-selector fd))
    (nelisp-os--windows-write-std-fd fd str))
   ((and (nelisp-os--windows-p)
         (eq (nelisp-os--windows-fd-kind fd) 'socket))
    (nelisp-os--windows-write-socket fd str))
   ((nelisp-os--windows-p)
    (nelisp-os--windows-write-handle
     (nelisp-os--windows-fd-handle fd)
     str))
   (t
    (let* ((nbytes (string-bytes str))
           (buf    (nelisp-os--alloc nbytes)))
      (unwind-protect
          (progn
            (nelisp-os--write-bytes buf str)
            ;; libc::write: ssize_t(int fd, const void *buf, size_t count).
            (let ((r (nelisp-os--libc-call "libc" "write"
                                  [:sint64 :sint32 :pointer :uint64]
                                  fd buf nbytes)))
              (if (= r -1)
                  (nelisp-os--ffi-errno-signal)
                r)))
        (nelisp-os--free buf))))))

(defun nelisp-os-close (fd)
  "POSIX close(2) — close FD, return nil or signal `nelisp-os-error'."
  (if (nelisp-os--windows-p)
      (let* ((kind (nelisp-os--windows-fd-kind fd))
             (handle (nelisp-os--windows-fd-remove fd)))
        (nelisp-os--windows-close-resource handle kind))
    (if nelisp-os--use-direct-syscall
      (progn (nelisp-os--check-errno (nelisp--syscall 'close fd)) nil)
    (progn
      (nelisp-os--check-errno (nelisp-os--libc-call "libc" "close" [:int :int] fd))
      nil))))

(defun nelisp-os-exit (code)
  "POSIX exit_group(2) — terminate process with CODE.  Never returns."
  (cond
   ((nelisp-os--windows-p)
    (nelisp-os--libc-call "kernel32" "ExitProcess" [:void :uint32] code))
   (nelisp-os--use-direct-syscall
    (nelisp--syscall 'exit_group code))
   (t
    (nelisp-os--libc-call "libc" "_exit" [:void :int] code))))

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

(defun nelisp-os--windows-page-protect (prot)
  "Translate POSIX-like PROT bits to a Windows PAGE_* constant."
  (let ((read  (not (= 0 (logand prot nelisp-os-PROT-READ))))
        (write (not (= 0 (logand prot nelisp-os-PROT-WRITE))))
        (exec  (not (= 0 (logand prot nelisp-os-PROT-EXEC)))))
    (cond
     ((and exec write) nelisp-os-WIN-PAGE-EXECUTE-READWRITE)
     ((and exec read)  nelisp-os-WIN-PAGE-EXECUTE-READ)
     (exec             nelisp-os-WIN-PAGE-EXECUTE)
     (write            nelisp-os-WIN-PAGE-READWRITE)
     (read             nelisp-os-WIN-PAGE-READONLY)
     (t                nelisp-os-WIN-PAGE-NOACCESS))))

(defun nelisp-os--windows-anonymous-mmap-p (flags fd offset)
  "Return non-nil when mmap inputs are supported by VirtualAlloc."
  (and (= fd -1)
       (= offset 0)
       (not (= 0 (logand flags nelisp-os-MAP-ANONYMOUS)))))

(defun nelisp-os--windows-mmap (length prot flags fd offset)
  "Windows implementation of anonymous `nelisp-os-mmap' via VirtualAlloc."
  (when (or (<= length 0)
            (not (nelisp-os--windows-anonymous-mmap-p flags fd offset)))
    (signal 'nelisp-os-error (list 22))) ; EINVAL
  (let ((addr (nelisp-os--libc-call
               "kernel32" "VirtualAlloc"
               [:pointer :pointer :uint64 :uint32 :uint32]
               0 length
               (logior nelisp-os-WIN-MEM-COMMIT nelisp-os-WIN-MEM-RESERVE)
               (nelisp-os--windows-page-protect prot))))
    (if (= addr 0)
        (nelisp-os--windows-ffi-error-signal)
      addr)))

(defun nelisp-os--windows-mprotect (addr length prot)
  "Windows implementation of `nelisp-os-mprotect' via VirtualProtect."
  (when (or (= addr 0) (<= length 0))
    (signal 'nelisp-os-error (list 22))) ; EINVAL
  (let ((old-protect (nelisp-os--alloc 4)))
    (unwind-protect
        (let ((ok (nelisp-os--libc-call
                   "kernel32" "VirtualProtect"
                   [:sint32 :pointer :uint64 :uint32 :pointer]
                   addr length (nelisp-os--windows-page-protect prot)
                   old-protect)))
          (if (= ok 0)
              (nelisp-os--windows-ffi-error-signal)
            0))
      (nelisp-os--free old-protect))))

(defun nelisp-os--windows-munmap (addr length)
  "Windows implementation of `nelisp-os-munmap' via VirtualFree."
  (when (or (= addr 0) (<= length 0))
    (signal 'nelisp-os-error (list 22))) ; EINVAL
  (let ((ok (nelisp-os--libc-call
             "kernel32" "VirtualFree"
             [:sint32 :pointer :uint64 :uint32]
             addr 0 nelisp-os-WIN-MEM-RELEASE)))
    (if (= ok 0)
        (nelisp-os--windows-ffi-error-signal)
      0)))

(defun nelisp-os--windows-pipe ()
  "Windows implementation of `nelisp-os-pipe' via CreatePipe."
  (let ((read-handle-buf (nelisp-os--alloc 8))
        (write-handle-buf (nelisp-os--alloc 8)))
    (unwind-protect
        (let ((ok (nelisp-os--libc-call
                   "kernel32" "CreatePipe"
                   [:sint32 :pointer :pointer :pointer :uint32]
                   read-handle-buf write-handle-buf 0 0)))
          (if (= ok 0)
              (nelisp-os--windows-ffi-error-signal)
            (let ((read-fd (nelisp-os--windows-fd-alloc
                            (nelisp-os-read-i64 read-handle-buf 0)))
                  (write-fd (nelisp-os--windows-fd-alloc
                             (nelisp-os-read-i64 write-handle-buf 0))))
              (cons read-fd write-fd))))
      (nelisp-os--free write-handle-buf)
      (nelisp-os--free read-handle-buf))))

(defun nelisp-os--windows-lseek (fd offset whence)
  "Windows implementation of `nelisp-os-lseek' via SetFilePointerEx."
  (unless (memq whence (list nelisp-os-SEEK-SET
                             nelisp-os-SEEK-CUR
                             nelisp-os-SEEK-END))
    (signal 'nelisp-os-error (list 22))) ; EINVAL
  (let ((new-pos-buf (nelisp-os--alloc 8)))
    (unwind-protect
        (let* ((handle (nelisp-os--windows-handle-for-fd fd))
               (ok (nelisp-os--libc-call
                    "kernel32" "SetFilePointerEx"
                    [:sint32 :pointer :sint64 :pointer :uint32]
                    handle offset new-pos-buf whence)))
          (if (= ok 0)
              (nelisp-os--windows-ffi-error-signal)
            (nelisp-os-read-i64 new-pos-buf 0)))
      (nelisp-os--free new-pos-buf))))

;; struct stat st_mode bits
(defconst nelisp-os-S-IFMT  61440)          ; 0o170000
(defconst nelisp-os-S-IFREG 32768)          ; 0o100000
(defconst nelisp-os-S-IFDIR 16384)          ; 0o040000
(defconst nelisp-os-S-IFLNK 40960)          ; 0o120000

;; ----- Wrappers -----

(defun nelisp-os-lseek (fd offset whence)
  "POSIX lseek(2) — return new offset or signal `nelisp-os-error'.
WHENCE = `nelisp-os-SEEK-SET' / `-CUR' / `-END'."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-lseek fd offset whence)
    (nelisp-os--check-errno (nelisp--syscall 'lseek fd offset whence))))

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

(defun nelisp-os--windows-stat-list (size mode)
  "Return a `nelisp-os-fstat' positional stat list for Windows."
  (list size mode 0 0 0 0 0 0 1 0 0 0 0))

(defun nelisp-os--windows-fstat (fd)
  "Windows implementation of `nelisp-os-fstat' for HANDLE-backed fds."
  (let* ((handle (nelisp-os--windows-handle-for-fd fd))
         (file-type (nelisp-os--libc-call
                     "kernel32" "GetFileType" [:uint32 :pointer] handle)))
    (if (= file-type nelisp-os-WIN-FILE-TYPE-DISK)
        (let ((size-buf (nelisp-os--alloc 8)))
          (unwind-protect
              (let ((ok (nelisp-os--libc-call
                         "kernel32" "GetFileSizeEx"
                         [:sint32 :pointer :pointer]
                         handle size-buf)))
                (if (= ok 0)
                    (nelisp-os--windows-ffi-error-signal)
                  (nelisp-os--windows-stat-list
                   (nelisp-os-read-i64 size-buf 0)
                   nelisp-os-S-IFREG)))
            (nelisp-os--free size-buf)))
      (nelisp-os--windows-stat-list 0 0))))

(defun nelisp-os--windows-dup2 (oldfd newfd)
  "Windows implementation of `nelisp-os-dup2' via DuplicateHandle."
  (when (< newfd 0)
    (signal 'nelisp-os-error (list 9))) ; EBADF
  (if (= oldfd newfd)
      newfd
    (let ((target-handle-buf (nelisp-os--alloc 8)))
      (unwind-protect
          (let* ((source-handle (nelisp-os--windows-handle-for-fd oldfd))
                 (current-process
                  (nelisp-os--libc-call "kernel32" "GetCurrentProcess" [:pointer]))
                 (ok (nelisp-os--libc-call
                      "kernel32" "DuplicateHandle"
                      [:sint32 :pointer :pointer :pointer :pointer
                       :uint32 :sint32 :uint32]
                      current-process
                      source-handle
                      current-process
                      target-handle-buf
                      0
                      1
                      nelisp-os-WIN-DUPLICATE-SAME-ACCESS)))
            (if (= ok 0)
                (nelisp-os--windows-ffi-error-signal)
              (let ((target-handle (nelisp-os-read-i64 target-handle-buf 0))
                    (selector (nelisp-os--windows-std-handle-selector newfd)))
                (if selector
                    (let ((set-ok (nelisp-os--libc-call
                                   "kernel32" "SetStdHandle"
                                   [:sint32 :sint32 :pointer]
                                   selector target-handle)))
                      (if (= set-ok 0)
                          (progn
                            (nelisp-os--libc-call
                             "kernel32" "CloseHandle"
                             [:sint32 :pointer] target-handle)
                            (nelisp-os--windows-ffi-error-signal))
                        newfd))
                  (nelisp-os--windows-fd-install newfd target-handle)))))
        (nelisp-os--free target-handle-buf)))))

(defun nelisp-os--windows-duplicate-fd (oldfd min-fd)
  "Duplicate OLDFD to a new Windows fd whose number is at least MIN-FD."
  (let ((source-handle (nelisp-os--windows-handle-for-fd oldfd))
        (target-process (nelisp-os--libc-call
                         "kernel32" "GetCurrentProcess"
                         [:pointer]))
        (target-buf (nelisp-os--alloc 8)))
    (unwind-protect
        (let ((ok (nelisp-os--libc-call
                   "kernel32" "DuplicateHandle"
                   [:sint32 :pointer :pointer :pointer :pointer
                    :uint32 :sint32 :uint32]
                   target-process
                   source-handle
                   target-process
                   target-buf
                   0
                   1
                   nelisp-os-WIN-DUPLICATE-SAME-ACCESS)))
          (if (= ok 0)
              (nelisp-os--windows-ffi-error-signal)
            (nelisp-os--windows-fd-alloc-at-least
             (nelisp-os-read-i64 target-buf 0)
             min-fd)))
      (nelisp-os--free target-buf))))

(defun nelisp-os--windows-fcntl (fd cmd arg)
  "Windows implementation of the int-only `nelisp-os-fcntl' subset."
  (cond
   ((= cmd nelisp-os-F-DUPFD)
    (nelisp-os--windows-duplicate-fd fd arg))
   ((= cmd nelisp-os-F-GETFL)
    (nelisp-os--windows-handle-for-fd fd)
    0)
   ((= cmd nelisp-os-F-SETFL)
    (nelisp-os--windows-handle-for-fd fd)
    (if (/= arg 0)
        (signal 'nelisp-os-error (list 95)) ; ENOTSUP
      0))
   (t
    (signal 'nelisp-os-error (list 22))))) ; EINVAL

(defun nelisp-os-fstat (fd)
  "POSIX fstat(2) — return positional list of stat fields, or signal
`nelisp-os-error'.  Order matches `nelisp-os-stat-*' accessors below
and the old `nelisp--syscall-fstat' Rust primitive (Doc 54 Phase 3)."
  ;; libc::fstat: int(int fd, struct stat *buf) → 0 / -1.
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-fstat fd)
    (let ((buf (nelisp-os--alloc nelisp-os--stat-buflen)))
      (unwind-protect
          (let ((r (nelisp-os--libc-call "libc" "fstat"
                                [:sint32 :sint32 :pointer]
                                fd buf)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (list (nelisp-os-read-i64 buf nelisp-os--stat-offset-size)
                    (nelisp-os-read-i32 buf nelisp-os--stat-offset-mode)
                    (nelisp-os-read-i64 buf nelisp-os--stat-offset-mtime)
                    (nelisp-os-read-i64 buf nelisp-os--stat-offset-mtime-nsec)
                    (nelisp-os-read-i64 buf nelisp-os--stat-offset-atime)
                    (nelisp-os-read-i64 buf nelisp-os--stat-offset-atime-nsec)
                    (nelisp-os-read-i64 buf nelisp-os--stat-offset-ctime)
                    (nelisp-os-read-i64 buf nelisp-os--stat-offset-ctime-nsec)
                    (nelisp-os-read-i64 buf nelisp-os--stat-offset-nlink)
                    (nelisp-os-read-i32 buf nelisp-os--stat-offset-uid)
                    (nelisp-os-read-i32 buf nelisp-os--stat-offset-gid)
                    (nelisp-os-read-i64 buf nelisp-os--stat-offset-ino)
                    (nelisp-os-read-i64 buf nelisp-os--stat-offset-dev))))
        (nelisp-os--free buf)))))

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
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-mmap length prot flags fd offset)
    (nelisp-os--check-errno
     (nelisp--syscall 'mmap 0 length prot flags fd offset))))

(defun nelisp-os-mprotect (addr length prot)
  "POSIX mprotect(2) — change protection on existing mapping."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-mprotect addr length prot)
    (nelisp-os--check-errno (nelisp--syscall 'mprotect addr length prot))))

(defun nelisp-os-munmap (addr length)
  "POSIX munmap(2) — release a previously mmap'd region."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-munmap addr length)
    (nelisp-os--check-errno (nelisp--syscall 'munmap addr length))))

(defun nelisp-os-dup2 (oldfd newfd)
  "POSIX dup2(2) — duplicate OLDFD onto NEWFD; returns NEWFD."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-dup2 oldfd newfd)
    (nelisp-os--check-errno (nelisp--syscall 'dup2 oldfd newfd))))

(defun nelisp-os-pipe ()
  "POSIX pipe(2) — return cons (READ-FD . WRITE-FD), or signal
`nelisp-os-error'."
  ;; libc::pipe: int(int pipefd[2]) → 0 / -1.  Allocate 8 bytes
  ;; (= 2 × int32) so libc.pipe can fill them, then decode via
  ;; `nelisp-os-read-i32' to dodge `nelisp-os--read-bytes''s UTF-8 lossy
  ;; conversion (= corrupts byte values 0x80-0xFF).
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-pipe)
    (let ((buf (nelisp-os--alloc 8)))
      (unwind-protect
          (let ((r (nelisp-os--libc-call "libc" "pipe" [:sint32 :pointer] buf)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (cons (nelisp-os-read-i32 buf 0)
                    (nelisp-os-read-i32 buf 4))))
        (nelisp-os--free buf)))))

(defun nelisp-os-fcntl (fd cmd arg)
  "POSIX fcntl(2) — int-only variant (= F_GETFL / F_SETFL / F_DUPFD).
Other variadic forms (struct flock for F_SETLK etc.) need their own
primitive; not supported in Phase 3."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-fcntl fd cmd arg)
    (nelisp-os--check-errno (nelisp--syscall 'fcntl fd cmd arg))))

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
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (nelisp-os--check-errno (nelisp--syscall 'fork))))

;; ---------------------------------------------------------------------------
;; Doc 76 Stage B (2026-05-08) — execve argv/envp marshaling helper.
;;
;; libc.execve takes char *path, char *const argv[], char *const envp[].
;; Each `argv[]' / `envp[]' entry is a NUL-terminated C string; the
;; arrays themselves are NULL-terminated (= sentinel pointer).  We
;; allocate one buffer per string + one buffer per pointer-array.  All
;; allocations are tracked in `nl-ffi' alloc_table so the unwind-protect
;; cleanup `nelisp-os--free's them on the failure path.  On success execve
;; replaces the process so the leaks are moot.
;; ---------------------------------------------------------------------------

(defun nelisp-os--alloc-cstring (s)
  "Allocate (string-bytes S + 1) bytes, write S, return the pointer.
Trailing NUL terminator is satisfied by `nelisp-os--alloc' zeroing."
  (let* ((nbytes (string-bytes s))
         (buf    (nelisp-os--alloc (1+ nbytes))))
    (nelisp-os--write-bytes-at buf 0 s)
    buf))

(defun nelisp-os--build-cstr-array (strs)
  "Allocate per-string buffers + a NULL-terminated pointer array of
the same length.  Return (POINTER-ARRAY . LIST-OF-STRING-PTRS) so the
caller can `nelisp-os--free' both the array and each string after use."
  (let* ((string-ptrs (mapcar #'nelisp-os--alloc-cstring strs))
         (n           (length string-ptrs))
         (array       (nelisp-os--alloc (* (1+ n) 8)))
         (idx         0))
    (dolist (sp string-ptrs)
      (nelisp-os-write-i64 array (* idx 8) sp)
      (setq idx (1+ idx)))
    ;; Trailing NULL pointer at offset n*8 stays 0 from malloc.
    (cons array string-ptrs)))

(defun nelisp-os--free-cstr-array (pair)
  "Reverse `nelisp-os--build-cstr-array': free the array + each string."
  (let ((array       (car pair))
        (string-ptrs (cdr pair)))
    (dolist (sp string-ptrs) (nelisp-os--free sp))
    (nelisp-os--free array)))

(defun nelisp-os--windows-execve (path argv envp)
  "Windows implementation of `nelisp-os-execve' via CreateProcessW.
Windows cannot replace the current process image like POSIX execve.  On
successful launch, this function exits the current process so callers still see
the execve contract that success does not return."
  (let ((app-buf nil)
        (cmd-buf nil)
        (env-buf nil)
        (startup-buf nil)
        (process-info-buf nil))
    (unwind-protect
        (progn
          (setq app-buf (nelisp-os--windows-alloc-utf16le-z path))
          (setq cmd-buf (nelisp-os--windows-alloc-utf16le-z
                         (nelisp-os--windows-command-line path argv)))
          (setq env-buf (nelisp-os--windows-alloc-utf16le-z
                         (nelisp-os--windows-env-block envp)))
          (setq startup-buf (nelisp-os--alloc nelisp-os-WIN-STARTUPINFOW-SIZE))
          (setq process-info-buf
                (nelisp-os--alloc nelisp-os-WIN-PROCESS-INFORMATION-SIZE))
          (nelisp-os-write-u32 startup-buf
                               nelisp-os-WIN-STARTUPINFOW-CB-OFFSET
                               nelisp-os-WIN-STARTUPINFOW-SIZE)
          (let ((ok (nelisp-os--libc-call
                     "kernel32" "CreateProcessW"
                     [:sint32 :pointer :pointer :pointer :pointer :sint32
                      :uint32 :pointer :pointer :pointer :pointer]
                     app-buf
                     cmd-buf
                     0
                     0
                     1
                     0
                     env-buf
                     0
                     startup-buf
                     process-info-buf)))
            (if (= ok 0)
                (nelisp-os--windows-ffi-error-signal)
              (progn
                (nelisp-os--libc-call
                 "kernel32" "CloseHandle" [:sint32 :pointer]
                 (nelisp-os-read-i64 process-info-buf
                                     nelisp-os-WIN-PROCESS-INFORMATION-HTHREAD-OFFSET))
                (nelisp-os--libc-call
                 "kernel32" "CloseHandle" [:sint32 :pointer]
                 (nelisp-os-read-i64 process-info-buf
                                     nelisp-os-WIN-PROCESS-INFORMATION-HPROCESS-OFFSET))
                (nelisp-os--libc-call
                 "kernel32" "ExitProcess" [:void :uint32] 0)))))
      (when process-info-buf (nelisp-os--free process-info-buf))
      (when startup-buf (nelisp-os--free startup-buf))
      (when env-buf (nelisp-os--free env-buf))
      (when cmd-buf (nelisp-os--free cmd-buf))
      (when app-buf (nelisp-os--free app-buf)))))

(defun nelisp-os-execve (path argv envp)
  "POSIX execve(2) — replace current process image with PATH using ARGV
list and ENVP list of strings.  Only returns on failure (signals
`nelisp-os-error')."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-execve path argv envp)
    (let ((argv-pair (nelisp-os--build-cstr-array argv))
          (envp-pair (nelisp-os--build-cstr-array envp)))
      (unwind-protect
          (let ((r (nelisp-os--libc-call "libc" "execve"
                                [:sint32 :string :pointer :pointer]
                                path (car argv-pair) (car envp-pair))))
            ;; libc.execve only returns on failure (= -1).
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              r))
        (nelisp-os--free-cstr-array argv-pair)
        (nelisp-os--free-cstr-array envp-pair)))))

(defun nelisp-os--windows-wait (pid options)
  "Windows implementation of `nelisp-os-wait' for a positive PID."
  (when (or (<= pid 0)
            (not (memq options (list 0 nelisp-os-WNOHANG))))
    (signal 'nelisp-os-error (list 22))) ; EINVAL
  (let ((handle (nelisp-os--libc-call
                 "kernel32" "OpenProcess"
                 [:pointer :uint32 :sint32 :uint32]
                 (logior nelisp-os-WIN-SYNCHRONIZE
                         nelisp-os-WIN-PROCESS-QUERY-LIMITED-INFORMATION)
                 0
                 pid)))
    (if (= handle 0)
        (nelisp-os--windows-ffi-error-signal)
      (unwind-protect
          (let* ((timeout (if (= options nelisp-os-WNOHANG)
                              0
                            nelisp-os-WIN-INFINITE))
                 (wait-rc (nelisp-os--libc-call
                           "kernel32" "WaitForSingleObject"
                           [:uint32 :pointer :uint32]
                           handle timeout)))
            (cond
             ((= wait-rc nelisp-os-WIN-WAIT-TIMEOUT)
              (cons 0 0))
             ((= wait-rc nelisp-os-WIN-WAIT-OBJECT-0)
              (let ((exit-code-buf (nelisp-os--alloc 4)))
                (unwind-protect
                    (let ((ok (nelisp-os--libc-call
                               "kernel32" "GetExitCodeProcess"
                               [:sint32 :pointer :pointer]
                               handle exit-code-buf)))
                      (if (= ok 0)
                          (nelisp-os--windows-ffi-error-signal)
                          (cons pid (ash (nelisp-os-read-u32 exit-code-buf 0) 8))))
                  (nelisp-os--free exit-code-buf))))
             (t
              (nelisp-os--windows-ffi-error-signal))))
        (let ((close-ok (nelisp-os--libc-call
                         "kernel32" "CloseHandle"
                         [:sint32 :pointer]
                         handle)))
          (if (= close-ok 0)
              (nelisp-os--windows-ffi-error-signal)))))))

(defun nelisp-os-wait (pid options)
  "POSIX wait4(2) — wait for child PID with OPTIONS (= 0, WNOHANG, etc.).
Returns cons (CHILD-PID . STATUS) on success, or signals
`nelisp-os-error' on failure.  When WNOHANG and no child is ready,
returns (0 . 0)."
  ;; libc.wait4(pid_t pid, int *status, int options, struct rusage *ru).
  ;; rusage = NULL (= raw 0 via :pointer).
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-wait pid options)
    (let ((status-buf (nelisp-os--alloc 4)))
      (unwind-protect
          (let ((r (nelisp-os--libc-call "libc" "wait4"
                                [:sint32 :sint32 :pointer :sint32 :pointer]
                                pid status-buf options 0)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (cons r (nelisp-os-read-i32 status-buf 0))))
        (nelisp-os--free status-buf)))))

(defun nelisp-os--windows-kill (pid sig)
  "Windows implementation of single-PID `nelisp-os-kill'."
  (when (or (<= pid 0)
            (not (memq sig (list nelisp-os-SIGTERM nelisp-os-SIGKILL))))
    (signal 'nelisp-os-error (list 22))) ; EINVAL
  (let ((handle (nelisp-os--libc-call
                 "kernel32" "OpenProcess"
                 [:pointer :uint32 :sint32 :uint32]
                 nelisp-os-WIN-PROCESS-TERMINATE
                 0
                 pid)))
    (if (= handle 0)
        (nelisp-os--windows-ffi-error-signal)
      (unwind-protect
          (let ((ok (nelisp-os--libc-call
                     "kernel32" "TerminateProcess"
                     [:sint32 :pointer :uint32]
                     handle sig)))
            (if (= ok 0)
                (nelisp-os--windows-ffi-error-signal)
              0))
        (let ((close-ok (nelisp-os--libc-call
                         "kernel32" "CloseHandle"
                         [:sint32 :pointer]
                         handle)))
          (if (= close-ok 0)
              (nelisp-os--windows-ffi-error-signal)))))))

(defun nelisp-os-kill (pid sig)
  "POSIX kill(2) — send SIG to PID; returns 0 on success."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-kill pid sig)
    (nelisp-os--check-errno (nelisp--syscall 'kill pid sig))))

(defun nelisp-os-getpid ()
  "POSIX getpid(2) — return current process id."
  (if (nelisp-os--windows-p)
      (nelisp-os--libc-call "kernel32" "GetCurrentProcessId" [:uint32])
    (nelisp-os--check-errno (nelisp--syscall 'getpid))))

(defun nelisp-os--windows-getppid ()
  "Windows implementation of `nelisp-os-getppid' using Tool Help snapshots."
  (let ((current-pid (nelisp-os-getpid))
        (snapshot (nelisp-os--libc-call
                   "kernel32" "CreateToolhelp32Snapshot"
                   [:pointer :uint32 :uint32]
                   nelisp-os-WIN-TH32CS-SNAPPROCESS
                   0))
        (entry-buf nil))
    (if (or (= snapshot 0) (= snapshot -1))
        (nelisp-os--windows-ffi-error-signal)
      (unwind-protect
          (progn
            (setq entry-buf (nelisp-os--alloc nelisp-os-WIN-PROCESSENTRY32W-SIZE))
            (nelisp-os-write-u32 entry-buf
                                 nelisp-os-WIN-PROCESSENTRY32W-DWSIZE-OFFSET
                                 nelisp-os-WIN-PROCESSENTRY32W-SIZE)
            (let ((ok (nelisp-os--libc-call
                       "kernel32" "Process32FirstW"
                       [:sint32 :pointer :pointer]
                       snapshot entry-buf)))
              (if (= ok 0)
                  (nelisp-os--windows-ffi-error-signal)
                (let ((found nil)
                      (found-p nil)
                      (done nil))
                  (while (not done)
                    (if (= (nelisp-os-read-u32
                            entry-buf nelisp-os-WIN-PROCESSENTRY32W-PID-OFFSET)
                           current-pid)
                        (setq found
                              (nelisp-os-read-u32
                               entry-buf
                               nelisp-os-WIN-PROCESSENTRY32W-PPID-OFFSET)
                              found-p t
                              done t)
                      (let ((next-ok (nelisp-os--libc-call
                                      "kernel32" "Process32NextW"
                                      [:sint32 :pointer :pointer]
                                      snapshot entry-buf)))
                        (if (/= next-ok 0)
                            nil
                          (let ((err (nelisp-os--libc-call
                                      "kernel32" "GetLastError" [:uint32])))
                            (if (= err nelisp-os-WIN-ERROR-NO-MORE-FILES)
                                (setq done t)
                              (signal 'nelisp-os-error (list err))))))))
                  (if found-p
                      found
                    (signal 'nelisp-os-error (list 3))))))) ; ESRCH
        (when entry-buf (nelisp-os--free entry-buf))
        (let ((close-ok (nelisp-os--libc-call
                         "kernel32" "CloseHandle"
                         [:sint32 :pointer]
                         snapshot)))
          (if (= close-ok 0)
              (nelisp-os--windows-ffi-error-signal)))))))

(defun nelisp-os-getppid ()
  "POSIX getppid(2) — return parent process id."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-getppid)
    (nelisp-os--check-errno (nelisp--syscall 'getppid))))

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

(defun nelisp-os--windows-winsock-ensure ()
  "Initialize Winsock for this process if needed."
  (unless nelisp-os--windows-winsock-started-p
    (let ((wsa-data (nelisp-os--alloc nelisp-os-WIN-WSADATA-SIZE)))
      (unwind-protect
          (let ((rc (nelisp-os--libc-call
                     "ws2_32" "WSAStartup"
                     [:sint32 :uint16 :pointer]
                     nelisp-os-WIN-WINSOCK-VERSION-2-2
                     wsa-data)))
            (if (/= rc 0)
                (signal 'nelisp-os-error (list rc))
              (setq nelisp-os--windows-winsock-started-p t)))
        (nelisp-os--free wsa-data)))))

(defun nelisp-os--windows-socket (domain type proto)
  "Windows implementation of `nelisp-os-socket' via Winsock."
  (when (/= domain nelisp-os-AF-INET)
    (nelisp-os--windows-unsupported))
  (when (/= 0 (logand type (logior nelisp-os-SOCK-NONBLOCK
                                   nelisp-os-SOCK-CLOEXEC)))
    (nelisp-os--windows-unsupported))
  (unless (memq type (list nelisp-os-SOCK-STREAM nelisp-os-SOCK-DGRAM))
    (signal 'nelisp-os-error (list 22))) ; EINVAL
  (nelisp-os--windows-winsock-ensure)
  (let ((sock (nelisp-os--libc-call
               "ws2_32" "socket"
               [:pointer :sint32 :sint32 :sint32]
               domain type proto)))
    (if (= sock nelisp-os-WIN-INVALID-SOCKET)
        (nelisp-os--windows-winsock-error-signal)
      (nelisp-os--windows-fd-alloc sock 'socket))))

(defun nelisp-os-socket (domain type proto)
  "POSIX socket(2) — return new fd or signal `nelisp-os-error'."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-socket domain type proto)
    (nelisp-os--check-errno (nelisp--syscall 'socket domain type proto))))

;; ---------------------------------------------------------------------------
;; Doc 76 Stage C (2026-05-08) — sockaddr_in encode/decode + pollfd[]
;; marshaling helpers.  Used by bind-inet / connect-inet / accept-inet /
;; poll wrappers below.
;; ---------------------------------------------------------------------------

(defconst nelisp-os--AF-INET 2)
(defconst nelisp-os--sockaddr-in-len 16)
(defconst nelisp-os--pollfd-len 8)
(defconst nelisp-os--windows-wsapollfd-len 16)

(defun nelisp-os--encode-sockaddr-in (buf host-int port)
  "Populate BUF (= 16-byte zeroed `nelisp-os--alloc') with sockaddr_in
fields: sin_family + sin_port (BE) + sin_addr (BE).  The 8 zero pad
bytes at offset 8-15 are left as malloc'd zeros."
  (let ((port-be (nelisp-os--libc-call "libc" "htons" [:uint16 :uint16] port))
        (addr-be (nelisp-os--libc-call "libc" "htonl" [:uint32 :uint32] host-int)))
    (nelisp-os-write-i16 buf 0 nelisp-os--AF-INET)
    (nelisp-os-write-i16 buf 2 port-be)
    (nelisp-os-write-i32 buf 4 addr-be)))

(defun nelisp-os--decode-sockaddr-in (buf)
  "Decode 16-byte sockaddr_in BUF.  Return cons (HOST-INT . PORT) in
host byte order."
  (let* ((port-be (nelisp-os-read-u16 buf 2))
         (port    (nelisp-os--libc-call "libc" "ntohs" [:uint16 :uint16] port-be))
         (addr-be (nelisp-os-read-u32 buf 4))
         (addr    (nelisp-os--libc-call "libc" "ntohl" [:uint32 :uint32] addr-be)))
    (cons addr port)))

(defun nelisp-os--windows-sockopt-level (level)
  "Translate supported POSIX-like socket option LEVEL to Winsock."
  (cond
   ((= level nelisp-os-SOL-SOCKET) nelisp-os-WIN-SOL-SOCKET)
   (t (nelisp-os--windows-unsupported))))

(defun nelisp-os--windows-sockopt-option (level optname)
  "Translate supported POSIX-like OPTNAME at LEVEL to Winsock."
  (cond
   ((and (= level nelisp-os-SOL-SOCKET)
         (= optname nelisp-os-SO-REUSEADDR))
    nelisp-os-WIN-SO-REUSEADDR)
   ((and (= level nelisp-os-SOL-SOCKET)
         (= optname nelisp-os-SO-KEEPALIVE))
    nelisp-os-WIN-SO-KEEPALIVE)
   (t (nelisp-os--windows-unsupported))))

(defun nelisp-os--windows-setsockopt-int (fd level optname value)
  "Windows implementation of `nelisp-os-setsockopt-int'."
  (let ((sock (nelisp-os--windows-socket-for-fd fd))
        (win-level (nelisp-os--windows-sockopt-level level))
        (win-optname (nelisp-os--windows-sockopt-option level optname))
        (buf (nelisp-os--alloc 4)))
    (unwind-protect
        (progn
          (nelisp-os-write-i32 buf 0 value)
          (let ((r (nelisp-os--libc-call
                    "ws2_32" "setsockopt"
                    [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                    sock win-level win-optname buf 4)))
            (if (= r -1)
                (nelisp-os--windows-winsock-error-signal)
              r)))
      (nelisp-os--free buf))))

(defun nelisp-os-setsockopt-int (fd level optname value)
  "POSIX setsockopt(2) for an int-valued option (e.g. SO_REUSEADDR).
Returns 0 on success."
  ;; libc::setsockopt(int sockfd, int level, int optname,
  ;;                  const void *optval, socklen_t optlen) → int.
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-setsockopt-int fd level optname value)
    (let ((buf (nelisp-os--alloc 4)))
      (unwind-protect
          (progn
            (nelisp-os-write-i32 buf 0 value)
            (let ((r (nelisp-os--libc-call "libc" "setsockopt"
                                  [:sint32 :sint32 :sint32 :sint32 :pointer :uint32]
                                  fd level optname buf 4)))
              (if (= r -1)
                  (nelisp-os--ffi-errno-signal)
                r)))
        (nelisp-os--free buf)))))

(defun nelisp-os-bind-inet (fd host-int port)
  "POSIX bind(2) for AF_INET.  HOST-INT is a 32-bit IPv4 address in
host byte order (e.g. `nelisp-os-INADDR-LOOPBACK').  PORT is a
16-bit host-byte-order port number.  Returns 0 on success."
  (let ((buf (nelisp-os--alloc nelisp-os--sockaddr-in-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sockaddr-in buf host-int port)
          (let ((r (if (nelisp-os--windows-p)
                       (nelisp-os--libc-call
                        "ws2_32" "bind"
                        [:sint32 :pointer :pointer :sint32]
                        (nelisp-os--windows-socket-for-fd fd)
                        buf
                        nelisp-os--sockaddr-in-len)
                     (nelisp-os--libc-call "libc" "bind"
                                  [:sint32 :sint32 :pointer :uint32]
                                  fd buf nelisp-os--sockaddr-in-len))))
            (cond
             ((and (nelisp-os--windows-p) (= r -1))
              (nelisp-os--windows-winsock-error-signal))
             ((= r -1)
              (nelisp-os--ffi-errno-signal))
             (t r))))
      (nelisp-os--free buf))))

(defun nelisp-os-listen (fd backlog)
  "POSIX listen(2) — mark FD as accepting connections with BACKLOG."
  (if (nelisp-os--windows-p)
      (let ((r (nelisp-os--libc-call
                "ws2_32" "listen"
                [:sint32 :pointer :sint32]
                (nelisp-os--windows-socket-for-fd fd)
                backlog)))
        (if (= r -1)
            (nelisp-os--windows-winsock-error-signal)
          r))
    (nelisp-os--check-errno (nelisp--syscall 'listen fd backlog))))

(defun nelisp-os-accept-inet (sockfd)
  "POSIX accept(2) for AF_INET.  Returns list (NEWFD CLIENT-IP CLIENT-PORT)
on success (CLIENT-IP / CLIENT-PORT in host byte order), or signals
`nelisp-os-error'."
  (let ((addr-buf (nelisp-os--alloc nelisp-os--sockaddr-in-len))
        (len-buf  (nelisp-os--alloc 4)))
    (unwind-protect
        (progn
          (nelisp-os-write-i32 len-buf 0 nelisp-os--sockaddr-in-len)
          (let ((newfd (if (nelisp-os--windows-p)
                           (let ((sock (nelisp-os--libc-call
                                        "ws2_32" "accept"
                                        [:pointer :pointer :pointer :pointer]
                                        (nelisp-os--windows-socket-for-fd sockfd)
                                        addr-buf len-buf)))
                             (if (= sock nelisp-os-WIN-INVALID-SOCKET)
                                 (nelisp-os--windows-winsock-error-signal)
                               (nelisp-os--windows-fd-alloc sock 'socket)))
                         (nelisp-os--libc-call "libc" "accept"
                                      [:sint32 :sint32 :pointer :pointer]
                                      sockfd addr-buf len-buf))))
            (if (and (not (nelisp-os--windows-p)) (= newfd -1))
                (nelisp-os--ffi-errno-signal)
              (let ((hp (nelisp-os--decode-sockaddr-in addr-buf)))
                (list newfd (car hp) (cdr hp))))))
      (nelisp-os--free addr-buf)
      (nelisp-os--free len-buf))))

(defun nelisp-os-connect-inet (fd host-int port)
  "POSIX connect(2) for AF_INET.  HOST-INT / PORT same convention as
`nelisp-os-bind-inet'.  Returns 0 on success."
  (let ((buf (nelisp-os--alloc nelisp-os--sockaddr-in-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sockaddr-in buf host-int port)
          (let ((r (if (nelisp-os--windows-p)
                       (nelisp-os--libc-call
                        "ws2_32" "connect"
                        [:sint32 :pointer :pointer :sint32]
                        (nelisp-os--windows-socket-for-fd fd)
                        buf
                        nelisp-os--sockaddr-in-len)
                     (nelisp-os--libc-call "libc" "connect"
                                  [:sint32 :sint32 :pointer :uint32]
                                  fd buf nelisp-os--sockaddr-in-len))))
            (cond
             ((and (nelisp-os--windows-p) (= r -1))
              (nelisp-os--windows-winsock-error-signal))
             ((= r -1)
              (nelisp-os--ffi-errno-signal))
             (t r))))
      (nelisp-os--free buf))))

;; ----- Multiplexing -----

(defun nelisp-os--windows-poll (pfds timeout-ms)
  "Windows implementation of `nelisp-os-poll' for socket-kind fds."
  (let* ((n (length pfds))
         (buf (nelisp-os--alloc (* n nelisp-os--windows-wsapollfd-len))))
    (unwind-protect
        (progn
          (let ((idx 0))
            (dolist (entry pfds)
              (let ((off (* idx nelisp-os--windows-wsapollfd-len)))
                (nelisp-os-write-i64
                 buf off (nelisp-os--windows-socket-for-fd (car entry)))
                (nelisp-os-write-i16 buf (+ off 8) (cdr entry)))
              (setq idx (1+ idx))))
          (let ((r (nelisp-os--libc-call
                    "ws2_32" "WSAPoll"
                    [:sint32 :pointer :uint32 :sint32]
                    buf n timeout-ms)))
            (if (= r -1)
                (nelisp-os--windows-winsock-error-signal)
              (let ((result nil)
                    (idx 0))
                (dolist (entry pfds)
                  (let ((off (* idx nelisp-os--windows-wsapollfd-len)))
                    (push (cons (car entry)
                                (nelisp-os-read-i16 buf (+ off 10)))
                          result))
                  (setq idx (1+ idx)))
                (nreverse result)))))
      (nelisp-os--free buf))))

(defun nelisp-os-poll (pfds timeout-ms)
  "POSIX poll(2).  PFDS is a list of (FD . EVENTS) cons cells.  Returns
the same-length list of (FD . REVENTS) cons cells.  TIMEOUT-MS = -1
blocks indefinitely; 0 polls without blocking."
  ;; libc::poll(struct pollfd *fds, nfds_t nfds, int timeout) → int.
  ;; pollfd layout: int fd (4) + short events (2) + short revents (2) = 8 bytes.
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-poll pfds timeout-ms)
    (let* ((n (length pfds))
           (buf (nelisp-os--alloc (* n nelisp-os--pollfd-len))))
      (unwind-protect
          (progn
            (let ((idx 0))
              (dolist (entry pfds)
                (let ((off (* idx nelisp-os--pollfd-len)))
                  (nelisp-os-write-i32 buf off            (car entry))
                  (nelisp-os-write-i16 buf (+ off 4)      (cdr entry))
                  ;; revents at offset+6 stays zero from malloc.
                  )
                (setq idx (1+ idx))))
            (let ((r (nelisp-os--libc-call "libc" "poll"
                                  [:sint32 :pointer :uint64 :sint32]
                                  buf n timeout-ms)))
              (if (= r -1)
                  (nelisp-os--ffi-errno-signal)
                (let ((result nil)
                      (idx 0))
                  (dolist (_entry pfds)
                    (let ((off (* idx nelisp-os--pollfd-len)))
                      (push (cons (nelisp-os-read-i32 buf off)
                                  (nelisp-os-read-i16 buf (+ off 6)))
                            result))
                    (setq idx (1+ idx)))
                  (nreverse result)))))
        (nelisp-os--free buf)))))

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
  "Populate BUF (= 110-byte zeroed `nelisp-os--alloc') with sockaddr_un for
filesystem path PATH.  Return the addrlen (= 2 + bytes + 1 NUL)."
  (nelisp-os-write-i16 buf 0 nelisp-os-AF-UNIX)
  (nelisp-os--write-bytes-at buf 2 path)
  (+ 3 (string-bytes path)))

(defun nelisp-os--encode-sockaddr-un-abstract (buf name)
  "Populate BUF with sockaddr_un for Linux abstract namespace NAME.
Leading sun_path[0] = NUL (= abstract sentinel) stays 0 from malloc.
Return the addrlen (= 2 + 1 + bytes)."
  (nelisp-os-write-i16 buf 0 nelisp-os-AF-UNIX)
  ;; sun_path[0] at offset 2 = 0 (already zeroed)
  (nelisp-os--write-bytes-at buf 3 name)
  (+ 3 (string-bytes name)))

(defun nelisp-os--decode-sockaddr-un (buf socklen)
  "Decode sockaddr_un at BUF.  SOCKLEN = total addrlen returned by
accept(2) / getsockname(2).  Return:
 - empty string for anonymous (= socklen ≤ 2);
 - cons (abstract . NAME) for abstract namespace (= sun_path[0] == NUL);
 - string PATH for filesystem path (= NUL-terminated)."
  (cond
   ((<= socklen 2) "")
   ((= (nelisp-os-read-u8 buf 2) 0)
    (let ((body-len (- socklen 3)))
      (cons 'abstract
            (if (<= body-len 0) "" (nelisp-os--read-bytes-at buf 3 body-len)))))
   (t
    (let* ((max-len (- socklen 2))
           (n (catch 'nul
                (dotimes (i max-len)
                  (when (= (nelisp-os-read-u8 buf (+ 2 i)) 0)
                    (throw 'nul i)))
                max-len)))
      (if (= n 0) "" (nelisp-os--read-bytes-at buf 2 n))))))

(defun nelisp-os--encode-sockaddr-in6 (buf groups port)
  "Populate BUF (= 28-byte zeroed) with sockaddr_in6: family + port BE +
flowinfo=0 + 8 BE u16 groups + scope_id=0."
  (nelisp-os-write-i16 buf 0 nelisp-os-AF-INET6)
  (let ((port-be (nelisp-os--libc-call "libc" "htons" [:uint16 :uint16] port)))
    (nelisp-os-write-i16 buf 2 port-be))
  ;; flowinfo at 4 stays 0
  (let ((idx 0))
    (dolist (g groups)
      (let* ((off (+ 8 (* idx 2)))
             (g-be (nelisp-os--libc-call "libc" "htons" [:uint16 :uint16] g)))
        (nelisp-os-write-i16 buf off g-be))
      (setq idx (1+ idx))))
  ;; scope_id at 24 stays 0
  )

(defun nelisp-os--decode-sockaddr-in6 (buf)
  "Decode 28-byte sockaddr_in6 BUF.  Return cons (GROUPS-LIST . PORT) in
host byte order."
  (let* ((port-be (nelisp-os-read-u16 buf 2))
         (port    (nelisp-os--libc-call "libc" "ntohs" [:uint16 :uint16] port-be))
         (groups  nil))
    (dotimes (i 8)
      (let ((g-be (nelisp-os-read-u16 buf (+ 8 (* i 2)))))
        (push (nelisp-os--libc-call "libc" "ntohs" [:uint16 :uint16] g-be) groups)))
    (cons (nreverse groups) port)))

;; ----- AF_UNIX wrappers -----

(defun nelisp-os-bind-unix (fd path)
  "POSIX bind(2) for AF_UNIX (filesystem path).  Abstract namespace
sockets use `nelisp-os-bind-unix-abstract'."
  (let ((buf (nelisp-os--alloc nelisp-os--sockaddr-un-len)))
    (unwind-protect
        (let* ((alen (nelisp-os--encode-sockaddr-un buf path))
               (r (nelisp-os--libc-call "libc" "bind"
                               [:sint32 :sint32 :pointer :uint32]
                               fd buf alen)))
          (if (= r -1) (nelisp-os--ffi-errno-signal) r))
      (nelisp-os--free buf))))

(defun nelisp-os-connect-unix (fd path)
  "POSIX connect(2) for AF_UNIX.  PATH is a filesystem path."
  (let ((buf (nelisp-os--alloc nelisp-os--sockaddr-un-len)))
    (unwind-protect
        (let* ((alen (nelisp-os--encode-sockaddr-un buf path))
               (r (nelisp-os--libc-call "libc" "connect"
                               [:sint32 :sint32 :pointer :uint32]
                               fd buf alen)))
          (if (= r -1) (nelisp-os--ffi-errno-signal) r))
      (nelisp-os--free buf))))

(defun nelisp-os-accept-unix (sockfd)
  "POSIX accept(2) for AF_UNIX.  Returns cons (NEWFD . PEER-PATH); the
peer is typically anonymous on a listening server."
  (let ((addr-buf (nelisp-os--alloc nelisp-os--sockaddr-un-len))
        (len-buf  (nelisp-os--alloc 4)))
    (unwind-protect
        (progn
          (nelisp-os-write-i32 len-buf 0 nelisp-os--sockaddr-un-len)
          (let ((newfd (nelisp-os--libc-call "libc" "accept"
                                    [:sint32 :sint32 :pointer :pointer]
                                    sockfd addr-buf len-buf)))
            (if (= newfd -1)
                (nelisp-os--ffi-errno-signal)
              (let* ((socklen (nelisp-os-read-i32 len-buf 0))
                     ;; accept-unix legacy: peer string-only (= matches
                     ;; old `parse_sockaddr_un_peer' that stops at first
                     ;; NUL, so abstract peer surfaces as "").
                     (peer (cond
                            ((<= socklen 2) "")
                            ((= (nelisp-os-read-u8 addr-buf 2) 0) "")
                            (t (let* ((max-len (- socklen 2))
                                      (n (catch 'nul
                                           (dotimes (i max-len)
                                             (when (= (nelisp-os-read-u8 addr-buf (+ 2 i)) 0)
                                               (throw 'nul i)))
                                           max-len)))
                                 (if (= n 0) "" (nelisp-os--read-bytes-at addr-buf 2 n)))))))
                (cons newfd peer)))))
      (nelisp-os--free addr-buf)
      (nelisp-os--free len-buf))))

;; ----- AF_INET6 wrappers -----

(defun nelisp-os-bind-inet6 (fd host6 port)
  "POSIX bind(2) for AF_INET6.  HOST6 is a list of 8 16-bit groups in
host byte order (e.g. `nelisp-os-IN6ADDR-LOOPBACK')."
  (let ((buf (nelisp-os--alloc nelisp-os--sockaddr-in6-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sockaddr-in6 buf host6 port)
          (let ((r (nelisp-os--libc-call "libc" "bind"
                                [:sint32 :sint32 :pointer :uint32]
                                fd buf nelisp-os--sockaddr-in6-len)))
            (if (= r -1) (nelisp-os--ffi-errno-signal) r)))
      (nelisp-os--free buf))))

(defun nelisp-os-connect-inet6 (fd host6 port)
  "POSIX connect(2) for AF_INET6.  HOST6 / PORT same convention as
`nelisp-os-bind-inet6'."
  (let ((buf (nelisp-os--alloc nelisp-os--sockaddr-in6-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sockaddr-in6 buf host6 port)
          (let ((r (nelisp-os--libc-call "libc" "connect"
                                [:sint32 :sint32 :pointer :uint32]
                                fd buf nelisp-os--sockaddr-in6-len)))
            (if (= r -1) (nelisp-os--ffi-errno-signal) r)))
      (nelisp-os--free buf))))

(defun nelisp-os-accept-inet6 (sockfd)
  "POSIX accept(2) for AF_INET6.  Returns list (NEWFD CLIENT-HOST6
CLIENT-PORT); CLIENT-HOST6 is an 8-element list of 16-bit groups in
host byte order."
  (let ((addr-buf (nelisp-os--alloc nelisp-os--sockaddr-in6-len))
        (len-buf  (nelisp-os--alloc 4)))
    (unwind-protect
        (progn
          (nelisp-os-write-i32 len-buf 0 nelisp-os--sockaddr-in6-len)
          (let ((newfd (nelisp-os--libc-call "libc" "accept"
                                    [:sint32 :sint32 :pointer :pointer]
                                    sockfd addr-buf len-buf)))
            (if (= newfd -1)
                (nelisp-os--ffi-errno-signal)
              (let ((gp (nelisp-os--decode-sockaddr-in6 addr-buf)))
                (list newfd (car gp) (cdr gp))))))
      (nelisp-os--free addr-buf)
      (nelisp-os--free len-buf))))

;; ---------------------------------------------------------------------------
;; Doc 57 Phase 4.3 — Modern Linux event surface (pidfd / inotify / eventfd).
;;
;; pidfd_open / pidfd_send_signal / inotify_init1 / inotify_rm_watch /
;; eventfd2 ride the generic `nelisp--syscall' arm via `syscall_nr()'
;; symbol map.  inotify_add_watch + inotify_read used to be specialized
;; Rust primitives (path string + packed `struct inotify_event' parse)
;; but were retired in Doc 76 Stage E (2026-05-09); the Stage E section
;; below now drives them elisp-side via `nelisp-os--libc-call' libc.
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
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (nelisp-os--check-errno (nelisp--syscall 'pidfd_open pid flags))))

(defun nelisp-os-pidfd-send-signal (pidfd sig flags)
  "Linux pidfd_send_signal(2) — send SIG to the process referenced by
PIDFD.  Phase 4.3 only supports `info = NULL', so siginfo_t is left
zero; pass FLAGS = 0 unless you know better."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (nelisp-os--check-errno
     (nelisp--syscall 'pidfd_send_signal pidfd sig 0 flags))))

;; ----- inotify wrappers -----
;;
;; Doc 76 Stage E (2026-05-09): inotify_add_watch / inotify_read were
;; specialized Rust primitives (= path string + variable-length packed
;; `struct inotify_event' buffer) but are now driven elisp-side through
;; `nelisp-os--libc-call' libc.  inotify_init1 / inotify_rm_watch keep riding
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
;;   1. nelisp-os--alloc (per-event-cap = 16 + 256 = 272) * MAX-EVENTS bytes
;;   2. libc.read (fd, buf, cap) → ssize_t n
;;   3. walk buf [0..n) parsing header (= read-i32/u32) + name field (=
;;      read-bytes-at + NUL-truncate via string-search), advance to next
;;      record at off + 16 + len
;; Empty result list = no events ready (only possible when FD opened
;; with IN-NONBLOCK).

(defun nelisp-os-inotify-init (flags)
  "Linux inotify_init1(2) — return a new inotify fd.  FLAGS is OR of
`nelisp-os-IN-NONBLOCK' / `nelisp-os-IN-CLOEXEC' (or 0)."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (nelisp-os--check-errno (nelisp--syscall 'inotify_init1 flags))))

(defun nelisp-os-inotify-add-watch (fd path mask)
  "Linux inotify_add_watch(2) — return a watch descriptor (positive
integer) or signal `nelisp-os-error'.  MASK is OR of `IN-*' event bits."
  ;; libc::inotify_add_watch: int(int fd, const char *path, uint32_t mask)
  ;; → int.  Returns -1 on error and sets errno; the wd is a positive
  ;; per-fd integer otherwise.
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (let ((r (nelisp-os--libc-call "libc" "inotify_add_watch"
                          [:sint32 :sint32 :string :uint32]
                          fd path mask)))
      (if (= r -1)
          (nelisp-os--ffi-errno-signal)
        r))))

(defun nelisp-os-inotify-rm-watch (fd wd)
  "Linux inotify_rm_watch(2) — remove the watch identified by WD."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (nelisp-os--check-errno (nelisp--syscall 'inotify_rm_watch fd wd))))

(defun nelisp-os-inotify-read (fd max-events)
  "Read up to MAX-EVENTS events off inotify FD.  Returns a list of
4-element lists `(WD MASK COOKIE NAME)'.  Empty list when no events
are ready (only possible when FD was opened `IN-NONBLOCK')."
  ;; Doc 76 Stage E: was a Rust specialized primitive; now elisp-side
  ;; libc.read + nelisp-os-read-i32/u32/bytes-at parse loop.
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (let* ((per-event-cap (+ 16 256))             ; sizeof(header) + NAME_MAX+1
           (cap           (* max-events per-event-cap))
           (buf           (nelisp-os--alloc cap)))
      (unwind-protect
          (let ((n (nelisp-os--libc-call "libc" "read"
                                [:sint64 :sint32 :pointer :uint64]
                                fd buf cap)))
            (cond
             ((= n -1) (nelisp-os--ffi-errno-signal))
             ((= n 0)  nil)
             (t (let ((events nil) (off 0))
                  (catch 'done
                    (while (<= (+ off 16) n)
                      (let* ((wd     (nelisp-os-read-i32 buf off))
                             (mask   (nelisp-os-read-u32 buf (+ off 4)))
                             (cookie (nelisp-os-read-u32 buf (+ off 8)))
                             (nlen   (nelisp-os-read-u32 buf (+ off 12)))
                             (name-off (+ off 16)))
                        ;; Truncated tail (kernel never writes a partial
                        ;; record; this guards corruption / short read).
                        (when (> (+ name-off nlen) n)
                          (throw 'done nil))
                        (let* ((raw  (if (= nlen 0)
                                         ""
                                       (nelisp-os--read-bytes-at buf name-off nlen)))
                               (cut  (string-search "\0" raw))
                               (name (if cut (substring raw 0 cut) raw)))
                          (push (list wd mask cookie name) events))
                        (setq off (+ name-off nlen)))))
                  (nreverse events)))))
        (nelisp-os--free buf)))))

;; ----- eventfd wrapper -----

(defun nelisp-os-eventfd (initval flags)
  "Linux eventfd2(2) — return a new eventfd with the given INITVAL
counter and FLAGS (OR of `EFD-*').  Read/write are 8-byte uint64
counters; use `nelisp-os-write' / `nelisp-os-read' on the returned fd."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (nelisp-os--check-errno (nelisp--syscall 'eventfd2 initval flags))))

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
  (let ((buf (nelisp-os--alloc nelisp-os--sockaddr-un-len)))
    (unwind-protect
        (let* ((alen (nelisp-os--encode-sockaddr-un-abstract buf name))
               (r (nelisp-os--libc-call "libc" "bind"
                               [:sint32 :sint32 :pointer :uint32]
                               fd buf alen)))
          (if (= r -1) (nelisp-os--ffi-errno-signal) r))
      (nelisp-os--free buf))))

(defun nelisp-os-connect-unix-abstract (fd name)
  "Linux-specific connect(2) for an AF_UNIX abstract-namespace socket."
  (let ((buf (nelisp-os--alloc nelisp-os--sockaddr-un-len)))
    (unwind-protect
        (let* ((alen (nelisp-os--encode-sockaddr-un-abstract buf name))
               (r (nelisp-os--libc-call "libc" "connect"
                               [:sint32 :sint32 :pointer :uint32]
                               fd buf alen)))
          (if (= r -1) (nelisp-os--ffi-errno-signal) r))
      (nelisp-os--free buf))))

;; getsockname / getpeername — three families × two ops = six wrappers.
;; `_inet'  → list (HOST-INT PORT)             both host byte order
;; `_inet6' → list (HOST6-LIST PORT)            HOST6-LIST = 8 16-bit groups
;; `_unix'  → string PATH (filesystem) | (abstract . NAME) | "" (anonymous)

(defun nelisp-os--getname-inet (fd is-peer)
  (let ((addr-buf (nelisp-os--alloc nelisp-os--sockaddr-in-len))
        (len-buf  (nelisp-os--alloc 4)))
    (unwind-protect
        (progn
          (nelisp-os-write-i32 len-buf 0 nelisp-os--sockaddr-in-len)
          (let ((r (nelisp-os--libc-call "libc" (if is-peer "getpeername" "getsockname")
                                [:sint32 :sint32 :pointer :pointer]
                                fd addr-buf len-buf)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (let ((hp (nelisp-os--decode-sockaddr-in addr-buf)))
                (list (car hp) (cdr hp))))))
      (nelisp-os--free addr-buf)
      (nelisp-os--free len-buf))))

(defun nelisp-os--getname-inet6 (fd is-peer)
  (let ((addr-buf (nelisp-os--alloc nelisp-os--sockaddr-in6-len))
        (len-buf  (nelisp-os--alloc 4)))
    (unwind-protect
        (progn
          (nelisp-os-write-i32 len-buf 0 nelisp-os--sockaddr-in6-len)
          (let ((r (nelisp-os--libc-call "libc" (if is-peer "getpeername" "getsockname")
                                [:sint32 :sint32 :pointer :pointer]
                                fd addr-buf len-buf)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (let ((gp (nelisp-os--decode-sockaddr-in6 addr-buf)))
                (list (car gp) (cdr gp))))))
      (nelisp-os--free addr-buf)
      (nelisp-os--free len-buf))))

(defun nelisp-os--getname-unix (fd is-peer)
  (let ((addr-buf (nelisp-os--alloc nelisp-os--sockaddr-un-len))
        (len-buf  (nelisp-os--alloc 4)))
    (unwind-protect
        (progn
          (nelisp-os-write-i32 len-buf 0 nelisp-os--sockaddr-un-len)
          (let ((r (nelisp-os--libc-call "libc" (if is-peer "getpeername" "getsockname")
                                [:sint32 :sint32 :pointer :pointer]
                                fd addr-buf len-buf)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (nelisp-os--decode-sockaddr-un addr-buf
                                              (nelisp-os-read-i32 len-buf 0)))))
      (nelisp-os--free addr-buf)
      (nelisp-os--free len-buf))))

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
  (nelisp-os--libc-call "libc" "sigemptyset" [:sint32 :pointer] buf)
  (dolist (sig signals)
    (nelisp-os--libc-call "libc" "sigaddset" [:sint32 :pointer :sint32] buf sig)))

(defun nelisp-os--decode-sigset (buf)
  "Decode sigset_t at BUF.  Probe sigismember for signals 1..64
(= standard + Linux RT range) to recover the membership list."
  (let ((signos nil))
    (dotimes (i 64)
      (let ((sig (1+ i)))
        (when (= 1 (nelisp-os--libc-call "libc" "sigismember"
                                [:sint32 :pointer :sint32] buf sig))
          (push sig signos))))
    (nreverse signos)))

(defun nelisp-os--encode-itimerspec (buf int-s int-ns val-s val-ns)
  "Populate BUF (= 32-byte zeroed) as itimerspec.  Layout: it_interval
(tv_sec i64 @ 0, tv_nsec i64 @ 8) + it_value (tv_sec i64 @ 16, tv_nsec
i64 @ 24)."
  (nelisp-os-write-i64 buf 0  int-s)
  (nelisp-os-write-i64 buf 8  int-ns)
  (nelisp-os-write-i64 buf 16 val-s)
  (nelisp-os-write-i64 buf 24 val-ns))

(defun nelisp-os--decode-itimerspec (buf)
  "Decode 32-byte itimerspec BUF into list (INT-S INT-NS VAL-S VAL-NS)."
  (list (nelisp-os-read-i64 buf 0)
        (nelisp-os-read-i64 buf 8)
        (nelisp-os-read-i64 buf 16)
        (nelisp-os-read-i64 buf 24)))

(defun nelisp-os--decode-signalfd-event (buf base)
  "Decode a single 128-byte signalfd_siginfo at BUF + BASE into 6-tuple
(SIGNO ERRNO CODE PID UID STATUS) matching the legacy primitive."
  (list (nelisp-os-read-u32 buf (+ base 0))
        (nelisp-os-read-i32 buf (+ base 4))
        (nelisp-os-read-i32 buf (+ base 8))
        (nelisp-os-read-u32 buf (+ base 12))
        (nelisp-os-read-u32 buf (+ base 16))
        (nelisp-os-read-i32 buf (+ base 40))))

;; ----- signalfd wrappers -----

(defun nelisp-os-signalfd (fd mask flags)
  "Linux signalfd4(2) — return (or update) a signalfd watching MASK.
FD = -1 to create a new fd, or an existing signalfd to update its
mask.  MASK is a list of signal numbers; FLAGS = OR of `SFD-*'."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (let ((set-buf (nelisp-os--alloc nelisp-os--sigset-len)))
      (unwind-protect
          (progn
            (nelisp-os--encode-sigset set-buf mask)
            (let ((r (nelisp-os--libc-call "libc" "signalfd"
                                  [:sint32 :sint32 :pointer :sint32]
                                  fd set-buf flags)))
              (if (= r -1) (nelisp-os--ffi-errno-signal) r)))
        (nelisp-os--free set-buf)))))

(defun nelisp-os-signalfd-read (fd max-events)
  "Read up to MAX-EVENTS events off signalfd FD.  Returns a list of
6-element lists `(SIGNO ERRNO CODE PID UID STATUS)'.  Empty list when
no events are ready (only on FD opened `SFD-NONBLOCK')."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (let* ((cap (* nelisp-os--signalfd-siginfo-len max-events))
           (buf (nelisp-os--alloc cap)))
      (unwind-protect
          (let ((n (nelisp-os--libc-call "libc" "read"
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
        (nelisp-os--free buf)))))

(defun nelisp-os-sigprocmask (how mask)
  "POSIX pthread_sigmask(3) — apply MASK with HOW (= `SIG-BLOCK' /
`SIG-UNBLOCK' / `SIG-SETMASK').  Returns the previous mask as a
list of signal numbers."
  (let ((new-buf (nelisp-os--alloc nelisp-os--sigset-len))
        (old-buf (nelisp-os--alloc nelisp-os--sigset-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sigset new-buf mask)
          (let ((r (nelisp-os--libc-call "libc" "pthread_sigmask"
                                [:sint32 :sint32 :pointer :pointer]
                                how new-buf old-buf)))
            ;; pthread_sigmask returns errno directly (= 0 on success).
            (if (/= r 0)
                (signal 'nelisp-os-error (list r))
              (nelisp-os--decode-sigset old-buf))))
      (nelisp-os--free new-buf)
      (nelisp-os--free old-buf))))

;; ----- timerfd wrappers -----

(defun nelisp-os-timerfd-create (clockid flags)
  "Linux timerfd_create(2) — return a new timer fd."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (nelisp-os--check-errno (nelisp--syscall 'timerfd_create clockid flags))))

(defun nelisp-os-timerfd-settime (fd flags it-int-s it-int-ns it-val-s it-val-ns)
  "Linux timerfd_settime(2) — arm or disarm timer FD.  Returns the
previous itimerspec as 4-element list (PREV-INT-S PREV-INT-NS
PREV-VAL-S PREV-VAL-NS)."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (let ((new-buf (nelisp-os--alloc nelisp-os--itimerspec-len))
          (old-buf (nelisp-os--alloc nelisp-os--itimerspec-len)))
      (unwind-protect
          (progn
            (nelisp-os--encode-itimerspec new-buf it-int-s it-int-ns it-val-s it-val-ns)
            (let ((r (nelisp-os--libc-call "libc" "timerfd_settime"
                                  [:sint32 :sint32 :sint32 :pointer :pointer]
                                  fd flags new-buf old-buf)))
              (if (= r -1)
                  (nelisp-os--ffi-errno-signal)
                (nelisp-os--decode-itimerspec old-buf))))
        (nelisp-os--free new-buf)
        (nelisp-os--free old-buf)))))

(defun nelisp-os-timerfd-gettime (fd)
  "Linux timerfd_gettime(2) — return current itimerspec as 4-element
list (INT-S INT-NS VAL-S VAL-NS)."
  (if (nelisp-os--windows-p)
      (nelisp-os--windows-unsupported)
    (let ((cur-buf (nelisp-os--alloc nelisp-os--itimerspec-len)))
      (unwind-protect
          (let ((r (nelisp-os--libc-call "libc" "timerfd_gettime"
                                [:sint32 :sint32 :pointer]
                                fd cur-buf)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (nelisp-os--decode-itimerspec cur-buf)))
        (nelisp-os--free cur-buf)))))

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
;;
;; Doc 76 Stage G (2026-05-09) retired the 7 specialized Rust primitives
;; (= socketpair / sendmsg-fds / recvmsg-fds / getsockopt-peercred /
;; bind-/connect-/accept-inet6-scoped).  elisp now drives msghdr +
;; SCM_RIGHTS cmsg + struct ucred + sockaddr_in6+scope_id directly via
;; `nelisp-os--libc-call' libc + `nelisp-os--alloc' / `-write-i*' / `-read-i*' /
;; `-write-bytes-at' / `-read-bytes-at'.
;;
;; ABI assumption: 64-bit Linux (glibc / musl).  Layout constants below
;; are hardcoded to that ABI:
;;
;;   msghdr (56 bytes):
;;     msg_name@0 ptr (8B), msg_namelen@8 u32+4 pad, msg_iov@16 ptr (8B),
;;     msg_iovlen@24 size_t (8B), msg_control@32 ptr (8B),
;;     msg_controllen@40 size_t (8B), msg_flags@48 i32+4 pad
;;
;;   iovec (16 bytes):
;;     iov_base@0 ptr (8B), iov_len@8 size_t (8B)
;;
;;   cmsghdr (16 bytes, alignment 8):
;;     cmsg_len@0 size_t (8B), cmsg_level@8 i32, cmsg_type@12 i32
;;     CMSG_LEN(n)   = 16 + n
;;     CMSG_SPACE(n) = align_up(16 + n, 8)   (= round to next multiple of 8)
;;
;;   ucred (12 bytes): pid@0 i32, uid@4 u32, gid@8 u32
;;
;;   sockaddr_in6 (28 bytes):
;;     sin6_family@0 u16, sin6_port@2 u16 BE, sin6_flowinfo@4 u32 BE,
;;     sin6_addr@8 16 bytes (8 BE u16 groups), sin6_scope_id@24 u32 host order
;; ---------------------------------------------------------------------------

;; ----- Extra socket type / cmsg / sockopt constants -----

(defconst nelisp-os-SOCK-SEQPACKET 5)              ; AF_UNIX boundary-preserving stream
(defconst nelisp-os-SCM-RIGHTS     1)              ; SOL_SOCKET cmsg type — fd passing
(defconst nelisp-os-SO-PEERCRED    17)             ; getsockopt option — struct ucred

;; ----- 64-bit Linux ABI layout constants (Doc 76 Stage G) -----

(defconst nelisp-os--msghdr-len     56)            ; struct msghdr (Linux x86_64 / aarch64)
(defconst nelisp-os--iovec-len      16)            ; struct iovec
(defconst nelisp-os--cmsghdr-len    16)            ; struct cmsghdr header
(defconst nelisp-os--cmsg-align      8)            ; CMSG alignment on 64-bit Linux
(defconst nelisp-os--ucred-len      12)            ; struct ucred
(defconst nelisp-os--sockaddr-in6-scoped-len 28)   ; sockaddr_in6 with scope_id
(defconst nelisp-os--sizeof-fd       4)            ; sizeof(int) on Linux LP64

;; ----- cmsg helpers (CMSG_LEN / CMSG_SPACE re-implementation) -----

(defun nelisp-os--cmsg-align-up (n)
  "Round N up to the next multiple of `nelisp-os--cmsg-align'."
  (let ((a nelisp-os--cmsg-align))
    (* a (/ (+ n (1- a)) a))))

(defun nelisp-os--cmsg-len (datalen)
  "POSIX CMSG_LEN(DATALEN) on 64-bit Linux: cmsghdr + DATALEN, no padding."
  (+ nelisp-os--cmsghdr-len datalen))

(defun nelisp-os--cmsg-space (datalen)
  "POSIX CMSG_SPACE(DATALEN) on 64-bit Linux: align CMSG_LEN to alignment."
  (nelisp-os--cmsg-align-up (nelisp-os--cmsg-len datalen)))

(defun nelisp-os--cmsg-decode-header (buf off)
  "Decode a single cmsghdr at BUF + OFF.  Return list (LEN LEVEL TYPE).
LEN is the full cmsghdr length (= header + data), LEVEL/TYPE are the
SOL_*/SCM_* identifiers."
  (list (nelisp-os-read-i64 buf (+ off 0))
        (nelisp-os-read-i32 buf (+ off 8))
        (nelisp-os-read-i32 buf (+ off 12))))

(defun nelisp-os--cmsg-iterate (buf controllen fn)
  "Walk every cmsghdr in BUF (= MSG_CONTROL[0..CONTROLLEN]).
For each cmsg call FN with three arguments (LEN LEVEL TYPE
DATA-OFF).  DATA-OFF is the byte offset within BUF of the cmsg's
payload (= cmsghdr_off + cmsghdr_len header size).
Stops on a malformed cmsg (= len < cmsghdr_len) or buffer exhaustion."
  (let ((off 0))
    (catch 'done
      (while (<= (+ off nelisp-os--cmsghdr-len) controllen)
        (let* ((hdr   (nelisp-os--cmsg-decode-header buf off))
               (len   (nth 0 hdr))
               (level (nth 1 hdr))
               (type  (nth 2 hdr))
               (next  (+ off (nelisp-os--cmsg-align-up len))))
          (when (or (< len nelisp-os--cmsghdr-len)
                    (> next controllen))
            (throw 'done nil))
          (funcall fn len level type (+ off nelisp-os--cmsghdr-len))
          (setq off next))))))

;; ----- ucred decoder -----

(defun nelisp-os--decode-ucred (buf)
  "Decode 12-byte struct ucred at BUF into list (PID UID GID)."
  (list (nelisp-os-read-i32 buf 0)
        (nelisp-os-read-u32 buf 4)
        (nelisp-os-read-u32 buf 8)))

;; ----- scoped sockaddr_in6 encode/decode -----

(defun nelisp-os--encode-sockaddr-in6-scoped (buf groups port flowinfo scope-id)
  "Populate BUF (= 28-byte zeroed) with sockaddr_in6 carrying
flowinfo (BE) and scope_id (host order) in addition to host/port."
  (nelisp-os-write-i16 buf 0 nelisp-os-AF-INET6)
  (let ((port-be (nelisp-os--libc-call "libc" "htons" [:uint16 :uint16] port)))
    (nelisp-os-write-i16 buf 2 port-be))
  ;; flowinfo @ 4: htonl, written as i32 LE bit pattern.
  (let ((flowinfo-be (nelisp-os--libc-call "libc" "htonl" [:uint32 :uint32] flowinfo)))
    (nelisp-os-write-i32 buf 4 flowinfo-be))
  ;; sin6_addr @ 8 (8 BE u16 groups).
  (let ((idx 0))
    (dolist (g groups)
      (let* ((off  (+ 8 (* idx 2)))
             (g-be (nelisp-os--libc-call "libc" "htons" [:uint16 :uint16] g)))
        (nelisp-os-write-i16 buf off g-be))
      (setq idx (1+ idx))))
  ;; scope_id @ 24 in host byte order.
  (nelisp-os-write-i32 buf 24 scope-id))

(defun nelisp-os--decode-sockaddr-in6-scoped (buf)
  "Decode 28-byte sockaddr_in6 BUF.  Return list
(GROUPS PORT FLOWINFO SCOPE-ID) — all in host byte order."
  (let* ((port-be     (nelisp-os-read-u16 buf 2))
         (port        (nelisp-os--libc-call "libc" "ntohs" [:uint16 :uint16] port-be))
         (flowinfo-be (nelisp-os-read-u32 buf 4))
         (flowinfo    (nelisp-os--libc-call "libc" "ntohl" [:uint32 :uint32] flowinfo-be))
         (scope-id    (nelisp-os-read-u32 buf 24))
         (groups nil))
    (dotimes (i 8)
      (let ((g-be (nelisp-os-read-u16 buf (+ 8 (* i 2)))))
        (push (nelisp-os--libc-call "libc" "ntohs" [:uint16 :uint16] g-be) groups)))
    (list (nreverse groups) port flowinfo scope-id)))

;; ----- socketpair -----

(defun nelisp-os-socketpair (domain type protocol)
  "POSIX socketpair(2) — returns (FD1 . FD2) on success.
Typical use: (nelisp-os-socketpair AF-UNIX SOCK-STREAM 0).  Signals
`nelisp-os-error' on failure."
  ;; libc::socketpair(int domain, int type, int protocol, int sv[2]) → int.
  (let ((sv-buf (nelisp-os--alloc 8)))         ; 2 × sizeof(int)
    (unwind-protect
        (let ((r (nelisp-os--libc-call "libc" "socketpair"
                              [:sint32 :sint32 :sint32 :sint32 :pointer]
                              domain type protocol sv-buf)))
          (if (/= r 0)
              (nelisp-os--ffi-errno-signal)
            (cons (nelisp-os-read-i32 sv-buf 0)
                  (nelisp-os-read-i32 sv-buf 4))))
      (nelisp-os--free sv-buf))))

;; ----- SCM_RIGHTS fd passing -----

(defun nelisp-os-sendmsg-fds (fd fds payload)
  "Send PAYLOAD plus FDS (list of int file descriptors) over UDS FD
via sendmsg(2) + SCM_RIGHTS cmsg.  PAYLOAD must be a string ≥ 1 byte
(the kernel rejects cmsg-only sendmsg).  Returns bytes_sent."
  (when (= (length payload) 0)
    (signal 'nelisp-os-error (list 22)))     ; EINVAL — cmsg-only rejected
  (let* ((nfds          (length fds))
         (fds-bytes     (* nfds nelisp-os--sizeof-fd))
         (cmsg-len      (nelisp-os--cmsg-len fds-bytes))
         (cmsg-space    (nelisp-os--cmsg-space fds-bytes))
         (payload-bytes (length payload))
         (payload-buf   (nelisp-os--alloc payload-bytes))
         (iov-buf       (nelisp-os--alloc nelisp-os--iovec-len))
         (cmsg-buf      (nelisp-os--alloc cmsg-space))
         (msg-buf       (nelisp-os--alloc nelisp-os--msghdr-len)))
    (unwind-protect
        (progn
          (nelisp-os--write-bytes payload-buf payload)
          ;; iovec[0] = {payload-buf, payload-bytes}
          (nelisp-os-write-i64 iov-buf 0 payload-buf)
          (nelisp-os-write-i64 iov-buf 8 payload-bytes)
          ;; cmsg header = {cmsg_len, SOL_SOCKET, SCM_RIGHTS}
          (nelisp-os-write-i64 cmsg-buf 0 cmsg-len)
          (nelisp-os-write-i32 cmsg-buf 8  nelisp-os-SOL-SOCKET)
          (nelisp-os-write-i32 cmsg-buf 12 nelisp-os-SCM-RIGHTS)
          ;; cmsg payload = fds[] starting at offset cmsghdr_len (= 16).
          (let ((idx 0))
            (dolist (one-fd fds)
              (nelisp-os-write-i32 cmsg-buf
                                (+ nelisp-os--cmsghdr-len
                                   (* idx nelisp-os--sizeof-fd))
                                one-fd)
              (setq idx (1+ idx))))
          ;; msghdr — name=NULL, iov=&iov, controllen=cmsg-space.
          (nelisp-os-write-i64 msg-buf 0  0)              ; msg_name
          (nelisp-os-write-i32 msg-buf 8  0)              ; msg_namelen
          (nelisp-os-write-i64 msg-buf 16 iov-buf)        ; msg_iov
          (nelisp-os-write-i64 msg-buf 24 1)              ; msg_iovlen
          (nelisp-os-write-i64 msg-buf 32 cmsg-buf)       ; msg_control
          (nelisp-os-write-i64 msg-buf 40 cmsg-space)     ; msg_controllen
          (nelisp-os-write-i32 msg-buf 48 0)              ; msg_flags
          (let ((r (nelisp-os--libc-call "libc" "sendmsg"
                                [:sint64 :sint32 :pointer :sint32]
                                fd msg-buf 0)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              r)))
      (nelisp-os--free msg-buf)
      (nelisp-os--free cmsg-buf)
      (nelisp-os--free iov-buf)
      (nelisp-os--free payload-buf))))

(defun nelisp-os-recvmsg-fds (fd max-fds max-bytes)
  "Receive up to MAX-BYTES of payload + up to MAX-FDS file descriptors
over UDS FD via recvmsg(2).  Returns (PAYLOAD-STRING . FDS-LIST).
PAYLOAD-STRING is truncated to the actual bytes_received; FDS-LIST is
all descriptors collected from SCM_RIGHTS cmsgs (may be fewer than
MAX-FDS, never more)."
  (when (<= max-bytes 0)
    (signal 'nelisp-os-error (list 22)))     ; EINVAL
  (let* ((fds-bytes  (* max-fds nelisp-os--sizeof-fd))
         (cmsg-space (if (= max-fds 0)
                         0
                       (nelisp-os--cmsg-space fds-bytes)))
         (payload-buf (nelisp-os--alloc max-bytes))
         (iov-buf     (nelisp-os--alloc nelisp-os--iovec-len))
         ;; Always allocate at least 1 byte (nelisp-os--alloc 0 is undefined).
         (cmsg-buf    (nelisp-os--alloc (max cmsg-space 1)))
         (msg-buf     (nelisp-os--alloc nelisp-os--msghdr-len)))
    (unwind-protect
        (progn
          (nelisp-os-write-i64 iov-buf 0 payload-buf)
          (nelisp-os-write-i64 iov-buf 8 max-bytes)
          (nelisp-os-write-i64 msg-buf 0  0)              ; msg_name
          (nelisp-os-write-i32 msg-buf 8  0)              ; msg_namelen
          (nelisp-os-write-i64 msg-buf 16 iov-buf)        ; msg_iov
          (nelisp-os-write-i64 msg-buf 24 1)              ; msg_iovlen
          (nelisp-os-write-i64 msg-buf 32 cmsg-buf)       ; msg_control
          (nelisp-os-write-i64 msg-buf 40 cmsg-space)     ; msg_controllen
          (nelisp-os-write-i32 msg-buf 48 0)              ; msg_flags
          (let ((r (nelisp-os--libc-call "libc" "recvmsg"
                                [:sint64 :sint32 :pointer :sint32]
                                fd msg-buf 0)))
            (if (= r -1)
                (nelisp-os--ffi-errno-signal)
              (let* ((bytes-received r)
                     (payload (if (= bytes-received 0)
                                  ""
                                (nelisp-os--read-bytes-at payload-buf 0 bytes-received)))
                     ;; Kernel may shrink controllen below cmsg-space.
                     (actual-controllen (nelisp-os-read-i64 msg-buf 40))
                     (fds-out nil))
                (nelisp-os--cmsg-iterate
                 cmsg-buf actual-controllen
                 (lambda (len level type data-off)
                   (when (and (= level nelisp-os-SOL-SOCKET)
                              (= type  nelisp-os-SCM-RIGHTS))
                     (let* ((payload-len (- len nelisp-os--cmsghdr-len))
                            (n-fds (/ payload-len nelisp-os--sizeof-fd)))
                       (dotimes (i n-fds)
                         (push (nelisp-os-read-i32
                                cmsg-buf
                                (+ data-off (* i nelisp-os--sizeof-fd)))
                               fds-out))))))
                (cons payload (nreverse fds-out))))))
      (nelisp-os--free msg-buf)
      (nelisp-os--free cmsg-buf)
      (nelisp-os--free iov-buf)
      (nelisp-os--free payload-buf))))

;; ----- SO_PEERCRED -----

(defun nelisp-os-getsockopt-peercred (fd)
  "Retrieve the peer's `struct ucred' on AF_UNIX FD via getsockopt
SO_PEERCRED.  Returns (PID UID GID) on success."
  (let ((cred-buf (nelisp-os--alloc nelisp-os--ucred-len))
        (len-buf  (nelisp-os--alloc 4)))
    (unwind-protect
        (progn
          (nelisp-os-write-i32 len-buf 0 nelisp-os--ucred-len)
          (let ((r (nelisp-os--libc-call "libc" "getsockopt"
                                [:sint32 :sint32 :sint32 :sint32 :pointer :pointer]
                                fd nelisp-os-SOL-SOCKET nelisp-os-SO-PEERCRED
                                cred-buf len-buf)))
            (if (/= r 0)
                (nelisp-os--ffi-errno-signal)
              (nelisp-os--decode-ucred cred-buf))))
      (nelisp-os--free len-buf)
      (nelisp-os--free cred-buf))))

;; ----- IPv6 scoped (full sockaddr_in6 surface) -----

(defun nelisp-os-bind-inet6-scoped (fd host6 port flowinfo scope-id)
  "Bind FD to IPv6 (HOST6 = 8-element host-byte-order group list, PORT,
FLOWINFO, SCOPE-ID).  All extra fields are wire-protocol-aware
(flowinfo gets htonl on the way out)."
  (let ((buf (nelisp-os--alloc nelisp-os--sockaddr-in6-scoped-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sockaddr-in6-scoped buf host6 port flowinfo scope-id)
          (let ((r (nelisp-os--libc-call "libc" "bind"
                                [:sint32 :sint32 :pointer :uint32]
                                fd buf nelisp-os--sockaddr-in6-scoped-len)))
            (if (/= r 0) (nelisp-os--ffi-errno-signal) r)))
      (nelisp-os--free buf))))

(defun nelisp-os-connect-inet6-scoped (fd host6 port flowinfo scope-id)
  "Same as `nelisp-os-bind-inet6-scoped' but for connect(2)."
  (let ((buf (nelisp-os--alloc nelisp-os--sockaddr-in6-scoped-len)))
    (unwind-protect
        (progn
          (nelisp-os--encode-sockaddr-in6-scoped buf host6 port flowinfo scope-id)
          (let ((r (nelisp-os--libc-call "libc" "connect"
                                [:sint32 :sint32 :pointer :uint32]
                                fd buf nelisp-os--sockaddr-in6-scoped-len)))
            (if (/= r 0) (nelisp-os--ffi-errno-signal) r)))
      (nelisp-os--free buf))))

(defun nelisp-os-accept-inet6-scoped (fd)
  "Accept on listening IPv6 FD and return a 5-element list
(NEW-FD HOST6 PORT FLOWINFO SCOPE-ID)."
  (let ((addr-buf (nelisp-os--alloc nelisp-os--sockaddr-in6-scoped-len))
        (len-buf  (nelisp-os--alloc 4)))
    (unwind-protect
        (progn
          (nelisp-os-write-i32 len-buf 0 nelisp-os--sockaddr-in6-scoped-len)
          (let ((newfd (nelisp-os--libc-call "libc" "accept"
                                    [:sint32 :sint32 :pointer :pointer]
                                    fd addr-buf len-buf)))
            (if (= newfd -1)
                (nelisp-os--ffi-errno-signal)
              (let ((decoded (nelisp-os--decode-sockaddr-in6-scoped addr-buf)))
                ;; (NEW-FD GROUPS PORT FLOWINFO SCOPE-ID)
                (cons newfd decoded)))))
      (nelisp-os--free len-buf)
      (nelisp-os--free addr-buf))))

(provide 'nelisp-stdlib-os)

;;; nelisp-stdlib-os.el ends here
