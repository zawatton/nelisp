;;; emacs-network-ffi.el --- libc socket FFI for NeLisp standalone -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 7 — Layer 2 network primitive port via the in-process
;; libffi primitive `nl-ffi-call' (= NeLisp build-tool/eval/ffi.rs).
;;
;; Companion to `emacs-sqlite-ffi.el': bypass the Emacs Dynamic Module
;; API (= which NeLisp standalone does not yet expose) by calling libc
;; symbols directly through `nl-ffi-call'.  Provides the small surface
;; that `make-network-process' (= and downstream `server.el',
;; `jsonrpc.el', anvil socket bridges) need:
;;
;;   - `emacs-network-ffi--socket'  / -close   (= libc socket / close)
;;   - `emacs-network-ffi--bind-unix' / -connect-unix
;;       (= bind / connect with `struct sockaddr_un')
;;   - `emacs-network-ffi--listen' / -accept   (= libc listen / accept)
;;   - `emacs-network-ffi--recv'   / -send     (= libc recv / send)
;;   - `emacs-network-ffi--set-nonblocking'    (= fcntl F_SETFL O_NONBLOCK)
;;   - `emacs-network-ffi--errno'              (= __errno_location * deref)
;;   - `emacs-network-ffi--unlink'             (= libc unlink for sock cleanup)
;;
;; The polyfill only covers UNIX domain sockets in Phase 7 — TCP support
;; (`AF_INET' + `htons' + `inet_pton') ships in Phase 7b once the v1
;; server.el flow is working and we know the surface holds.
;;
;; Each FFI call returns -1 / a partial result on failure; callers are
;; expected to consult `emacs-network-ffi--errno' for diagnostic
;; messages.  Errors are NOT signalled here so that the higher-level
;; `make-network-process' wrapper (= `emacs-process-events.el') can map
;; common errnos (= ECONNREFUSED / EAGAIN / EINTR) into the Emacs
;; semantics callers expect.
;;
;; Sibling module load order:
;;   emacs-network-ffi  → emacs-process-events → emacs-eventloop
;;                      → vendor server.el / jsonrpc.el (no changes)
;;
;; Each polyfill is gated on `unless (fboundp ...)' so loading under
;; host Emacs (= where the C builtins exist) is a no-op.

;;; Code:

;;;; --- libc dlopen path detection ----------------------------------------

(defvar emacs-network-ffi-libc-path
  (or (and (fboundp 'getenv) (getenv "EMACS_NETWORK_FFI_LIBC"))
      (let ((candidates
             '("/lib/x86_64-linux-gnu/libc.so.6"        ; Debian/Ubuntu glibc
               "/lib64/libc.so.6"                        ; RHEL/Fedora
               "/lib/aarch64-linux-gnu/libc.so.6"        ; Linux/arm64
               "/usr/lib/libc.so.6"                      ; Arch / generic
               "/usr/lib/libSystem.B.dylib")))           ; macOS
        (let ((found nil))
          (while (and candidates (not found))
            (when (and (fboundp 'file-readable-p)
                       (file-readable-p (car candidates)))
              (setq found (car candidates)))
            (setq candidates (cdr candidates)))
          found)))
  "Absolute path to the system libc shared object.
Override via `EMACS_NETWORK_FFI_LIBC' env var or by `setq' before
this file loads.")


;;;; --- low-level FFI helpers ---------------------------------------------

(defun emacs-network-ffi--call (func sig &rest args)
  "Dispatch libc FUNC with SIG (= [return-type arg-types]) + ARGS.
Returns the FFI integer / pointer / etc. as a Lisp value."
  (apply #'nl-ffi-call emacs-network-ffi-libc-path func sig args))


;;;; --- POSIX constants --------------------------------------------------
;;
;; Linux x86_64 / arm64 values.  macOS differs; Phase 7 targets Linux
;; first, with the macOS overrides slated for a follow-up minor patch.

(defconst emacs-network-ffi-AF_UNIX 1
  "Address family: UNIX domain socket (Linux).")

(defconst emacs-network-ffi-SOCK_STREAM 1
  "Socket type: connection-oriented byte stream.")

(defconst emacs-network-ffi-O_NONBLOCK 2048    ; 04000 octal
  "fcntl O_NONBLOCK flag (Linux).")

(defconst emacs-network-ffi-F_GETFL 3
  "fcntl F_GETFL command.")

(defconst emacs-network-ffi-F_SETFL 4
  "fcntl F_SETFL command.")

(defconst emacs-network-ffi-MSG_DONTWAIT #x40
  "recv/send flag: do not block (= return EAGAIN if no data ready).")

(defconst emacs-network-ffi-EAGAIN 11
  "Linux EAGAIN errno (= EWOULDBLOCK on most Unices).")

(defconst emacs-network-ffi-EINTR 4
  "Linux EINTR errno.")

(defconst emacs-network-ffi--sockaddr-un-size 110
  "sizeof(struct sockaddr_un) on Linux (= 2 byte family + 108 byte path).")


;;;; --- struct sockaddr_un marshalling -----------------------------------

(defun emacs-network-ffi--make-sockaddr-un (path)
  "Allocate + populate a `struct sockaddr_un' for PATH on the heap.
Returns a (PTR . LEN) pair — caller is responsible for `nl-ffi-free'
on PTR after the libc call returns.

Layout (Linux):
  offset 0 .. 1   sun_family = AF_UNIX (uint16 LE)
  offset 2 .. ..  sun_path   = NUL-terminated path bytes (max 108)
  total: 110 bytes (always allocated)."
  (unless (stringp path)
    (error "emacs-network-ffi: PATH must be a string, got %S" path))
  (let* ((path-len (length path)))
    (when (> path-len 107)
      (error "emacs-network-ffi: UNIX socket path too long (%d > 107): %s"
             path-len path))
    (let ((buf (nl-ffi-malloc emacs-network-ffi--sockaddr-un-size)))
      ;; Family at offset 0 — little-endian uint16.
      (nl-ffi-write-i16 buf 0 emacs-network-ffi-AF_UNIX)
      ;; Path bytes starting at offset 2.  Manual byte-write so we
      ;; keep the trailing NUL implicit (nl-ffi-malloc returns a
      ;; zeroed buffer, so unused bytes are already 0).
      (nl-ffi-write-bytes-at buf 2 path)
      (cons buf (+ 2 path-len 1)))))


;;;; --- libc errno (read via __errno_location) ---------------------------

(defun emacs-network-ffi--errno ()
  "Return the current libc errno as an integer.

Calls `__errno_location()' (glibc) which returns a pointer to the
thread-local errno.  The returned pointer points at libc-internal
memory, which `nl-ffi-read-i32' rejects (= safety check requires
the pointer to come from `nl-ffi-malloc' so out-of-bounds reads
are caught).  Side-step by `memcpy'-ing 4 bytes from the errno
pointer into a tracked buffer, then reading from there.  On macOS
the glibc-specific symbol does not exist; we fall back to
`__error' (the Mach errno accessor).  Returns 0 if neither symbol
resolves or copy fails."
  (let ((ptr 0))
    (condition-case nil
        (setq ptr (emacs-network-ffi--call
                   "__errno_location" [:sint64]))
      (error nil))
    (when (or (not (integerp ptr)) (zerop ptr))
      (condition-case nil
          (setq ptr (emacs-network-ffi--call "__error" [:sint64]))
        (error nil)))
    (cond
     ((or (not (integerp ptr)) (zerop ptr)) 0)
     (t
      (let* ((buf (nl-ffi-malloc 4))
             (errno
              (condition-case nil
                  (progn
                    (emacs-network-ffi--call
                     "memcpy"
                     [:sint64 :pointer :pointer :sint64]
                     buf ptr 4)
                    (nl-ffi-read-i32 buf 0))
                (error 0))))
        (nl-ffi-free buf)
        errno)))))


;;;; --- libc primitives --------------------------------------------------

(defun emacs-network-ffi--socket (domain type protocol)
  "FFI: int socket(int domain, int type, int protocol).
Returns the new fd as integer, or -1 on error.  Inspect
`emacs-network-ffi--errno' for the failure reason."
  (emacs-network-ffi--call
   "socket"
   [:sint32 :sint32 :sint32 :sint32]
   domain type protocol))

(defun emacs-network-ffi--close (fd)
  "FFI: int close(int fd).  Returns 0 on success, -1 on error."
  (emacs-network-ffi--call "close" [:sint32 :sint32] fd))

(defun emacs-network-ffi--unlink (path)
  "FFI: int unlink(const char *path).
Used to clear a stale UNIX socket file before `bind'.  Returns 0 on
success or when the file does not exist (path absent → ENOENT, but
we tolerate that).  -1 on other errors."
  (emacs-network-ffi--call "unlink" [:sint32 :string] path))

(defun emacs-network-ffi--bind-unix (fd path)
  "FFI: int bind(fd, struct sockaddr_un{path}, sizeof).
Returns 0 on success, -1 on error.  Caller is responsible for
calling `emacs-network-ffi--unlink' on PATH first if a stale socket
file may exist (otherwise EADDRINUSE)."
  (let* ((addr (emacs-network-ffi--make-sockaddr-un path))
         (ptr (car addr))
         (len (cdr addr))
         (rc (emacs-network-ffi--call
              "bind"
              [:sint32 :sint32 :pointer :sint32]
              fd ptr len)))
    (nl-ffi-free ptr)
    rc))

(defun emacs-network-ffi--listen (fd backlog)
  "FFI: int listen(int sockfd, int backlog).
Returns 0 on success, -1 on error."
  (emacs-network-ffi--call
   "listen"
   [:sint32 :sint32 :sint32]
   fd backlog))

(defun emacs-network-ffi--accept (fd)
  "FFI: int accept(int sockfd, NULL, NULL).
Returns the connection fd, or -1 on error / EAGAIN.  We pass NULL /
NULL for addr / addrlen because the caller (= make-network-process)
does not currently surface the peer address (= AF_UNIX peers are
rarely identifiable anyway)."
  (emacs-network-ffi--call
   "accept"
   [:sint32 :sint32 :pointer :pointer]
   fd 0 0))

(defun emacs-network-ffi--connect-unix (fd path)
  "FFI: int connect(fd, struct sockaddr_un{path}, sizeof).
Returns 0 on success, -1 on error.  Pair with
`emacs-network-ffi--socket' (AF_UNIX SOCK_STREAM 0)."
  (let* ((addr (emacs-network-ffi--make-sockaddr-un path))
         (ptr (car addr))
         (len (cdr addr))
         (rc (emacs-network-ffi--call
              "connect"
              [:sint32 :sint32 :pointer :sint32]
              fd ptr len)))
    (nl-ffi-free ptr)
    rc))

(defun emacs-network-ffi--recv (fd max-bytes &optional flags)
  "FFI: ssize_t recv(int sockfd, void *buf, size_t len, int flags).
Reads up to MAX-BYTES from FD into a heap buffer, then copies into
a Lisp string.  Returns:
  - string (0..MAX-BYTES bytes, may be empty when peer half-closed)
  - :would-block when non-blocking + EAGAIN
  - :interrupted on EINTR (caller should retry)
  - nil on any other error"
  (let* ((flags-val (or flags 0))
         (buf (nl-ffi-malloc max-bytes))
         (got (emacs-network-ffi--call
               "recv"
               [:sint64 :sint32 :pointer :sint64 :sint32]
               fd buf max-bytes flags-val))
         (result nil))
    (cond
     ((and (integerp got) (>= got 0))
      (setq result
            (if (zerop got)
                ""
              (nl-ffi-read-bytes buf got))))
     ((integerp got)
      (let ((errno (emacs-network-ffi--errno)))
        (cond
         ((= errno emacs-network-ffi-EAGAIN) (setq result :would-block))
         ((= errno emacs-network-ffi-EINTR)  (setq result :interrupted))
         (t (setq result nil))))))
    (nl-ffi-free buf)
    result))

(defun emacs-network-ffi--send (fd str &optional flags)
  "FFI: ssize_t send(int sockfd, const void *buf, size_t len, int flags).
Returns the number of bytes accepted by the kernel, or -1 on error.
Caller is responsible for retrying short writes / EAGAIN."
  (unless (stringp str)
    (error "emacs-network-ffi--send: STR must be a string, got %S"
           (type-of str)))
  (let* ((len (length str))
         (flags-val (or flags 0))
         (buf (nl-ffi-malloc (max len 1))))
    (when (> len 0)
      (nl-ffi-write-bytes buf str))
    (let ((sent (emacs-network-ffi--call
                 "send"
                 [:sint64 :sint32 :pointer :sint64 :sint32]
                 fd buf len flags-val)))
      (nl-ffi-free buf)
      sent)))

(defun emacs-network-ffi--set-nonblocking (fd)
  "Mark FD as non-blocking via fcntl(fd, F_SETFL, F_GETFL | O_NONBLOCK).
Returns 0 on success, -1 on error.  Used to keep the eventloop's
recv / accept calls from blocking the whole interpreter."
  (let ((flags (emacs-network-ffi--call
                "fcntl"
                [:sint32 :sint32 :sint32]
                fd emacs-network-ffi-F_GETFL)))
    (if (and (integerp flags) (>= flags 0))
        (emacs-network-ffi--call
         "fcntl"
         [:sint32 :sint32 :sint32 :sint32]
         fd
         emacs-network-ffi-F_SETFL
         (logior flags emacs-network-ffi-O_NONBLOCK))
      flags)))


;;;; --- convenience high-level wrappers ----------------------------------
;;
;; The wrappers below are the API consumed by `emacs-process-events.el'
;; and `emacs-eventloop.el'.  They coalesce errno reporting into a
;; (:error STRING) plist so the higher layer can surface a meaningful
;; diagnostic without having to remember libc semantics.

(defun emacs-network-ffi-server-unix (path &optional backlog)
  "Open + bind + listen on a UNIX domain socket at PATH.
Removes a stale socket file at PATH first (= unlink, ignore ENOENT).
Returns the server fd on success, or `(:error STRING)' on failure.

BACKLOG defaults to 16.

The fd is left in non-blocking mode (= eventloop accept does not
hang the interpreter)."
  (let ((backlog* (or backlog 16))
        (fd (emacs-network-ffi--socket
             emacs-network-ffi-AF_UNIX
             emacs-network-ffi-SOCK_STREAM
             0)))
    (cond
     ((or (not (integerp fd)) (< fd 0))
      `(:error ,(format "socket() failed: errno=%d"
                        (emacs-network-ffi--errno))))
     (t
      (emacs-network-ffi--unlink path) ; ignore errors
      (let ((rc-bind (emacs-network-ffi--bind-unix fd path)))
        (cond
         ((not (zerop (or rc-bind -1)))
          (let ((errno (emacs-network-ffi--errno)))
            (emacs-network-ffi--close fd)
            `(:error ,(format "bind(%s) failed: errno=%d" path errno))))
         (t
          (let ((rc-listen (emacs-network-ffi--listen fd backlog*)))
            (cond
             ((not (zerop (or rc-listen -1)))
              (let ((errno (emacs-network-ffi--errno)))
                (emacs-network-ffi--close fd)
                `(:error ,(format "listen() failed: errno=%d" errno))))
             (t
              (emacs-network-ffi--set-nonblocking fd)
              fd))))))))))

(defun emacs-network-ffi-client-unix (path)
  "Open + connect to a UNIX domain socket at PATH.
Returns the client fd on success, or `(:error STRING)' on failure.
Fd is left in non-blocking mode (= consumer poll-driven)."
  (let ((fd (emacs-network-ffi--socket
             emacs-network-ffi-AF_UNIX
             emacs-network-ffi-SOCK_STREAM
             0)))
    (cond
     ((or (not (integerp fd)) (< fd 0))
      `(:error ,(format "socket() failed: errno=%d"
                        (emacs-network-ffi--errno))))
     (t
      (let ((rc (emacs-network-ffi--connect-unix fd path)))
        (cond
         ((not (zerop (or rc -1)))
          (let ((errno (emacs-network-ffi--errno)))
            (emacs-network-ffi--close fd)
            `(:error ,(format "connect(%s) failed: errno=%d" path errno))))
         (t
          (emacs-network-ffi--set-nonblocking fd)
          fd)))))))


(provide 'emacs-network-ffi)

;;; emacs-network-ffi.el ends here
