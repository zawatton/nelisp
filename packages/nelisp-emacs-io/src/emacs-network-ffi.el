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

(defconst emacs-network-ffi-AF_INET 2
  "Address family: IPv4 (Linux, macOS, BSD).")

(defconst emacs-network-ffi-SOCK_STREAM 1
  "Socket type: connection-oriented byte stream.")

(defconst emacs-network-ffi-INADDR_ANY 0
  "IPv4 wildcard address (= bind on every local interface).")

(defconst emacs-network-ffi-INADDR_LOOPBACK #x7F000001
  "IPv4 loopback address 127.0.0.1 in host byte order.")

(defconst emacs-network-ffi-SOL_SOCKET 1
  "setsockopt level: socket-layer options.")

(defconst emacs-network-ffi-SO_REUSEADDR 2
  "setsockopt option: allow rebinding a socket whose previous close
left it in TIME_WAIT — necessary for TCP listener restarts.")

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

(defconst emacs-network-ffi--sockaddr-in-size 16
  "sizeof(struct sockaddr_in) (= 2 family + 2 port BE + 4 addr BE + 8 zero).")


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


;;;; --- struct sockaddr_in marshalling (Phase 7b TCP) -------------------

(defun emacs-network-ffi--parse-ipv4 (host)
  "Parse HOST (= string `\"a.b.c.d\"` or symbol `'local`) into a 32-bit
integer in host byte order.  Returns nil on malformed input.  Calls
`inet_pton' via FFI to handle the conversion (= avoids hand-rolling
the dot-quad parser and matches the semantics emacsclient uses)."
  (cond
   ((eq host 'local) emacs-network-ffi-INADDR_LOOPBACK)
   ((or (null host) (eq host t) (eq host 'any)) emacs-network-ffi-INADDR_ANY)
   ((stringp host)
    ;; inet_pton(AF_INET, "a.b.c.d", &out) → 1 on success, 0 on bad input.
    ;; The 4-byte network-order address is written into `out'; we read
    ;; it back as a u32 (network = big-endian).
    (let* ((out (nl-ffi-malloc 4))
           (rc (emacs-network-ffi--call
                "inet_pton"
                [:sint32 :sint32 :string :pointer]
                emacs-network-ffi-AF_INET host out)))
      (let ((result
             (cond
              ((and (integerp rc) (= rc 1))
               ;; Read network-order bytes b0 b1 b2 b3, convert to host
               ;; (= just reverse since we are LE).
               (let ((b0 (logand 255 (nl-ffi-read-i16 out 0)))
                     (b1 (logand 255 (ash (nl-ffi-read-i16 out 0) -8)))
                     (b2 (logand 255 (nl-ffi-read-i16 out 2)))
                     (b3 (logand 255 (ash (nl-ffi-read-i16 out 2) -8))))
                 (+ (ash b0 24) (ash b1 16) (ash b2 8) b3)))
              (t nil))))
        (nl-ffi-free out)
        result)))
   (t nil)))

(defsubst emacs-network-ffi--htons (port)
  "Convert PORT (host byte order, 0..65535) to network byte order
(= big-endian).  Just byte-swap a 16-bit value."
  (let ((lo (logand port 255))
        (hi (logand 255 (ash port -8))))
    (logior (ash lo 8) hi)))

(defsubst emacs-network-ffi--htonl (addr)
  "Convert 32-bit ADDR (host byte order) to network byte order."
  (let ((b0 (logand 255 addr))
        (b1 (logand 255 (ash addr -8)))
        (b2 (logand 255 (ash addr -16)))
        (b3 (logand 255 (ash addr -24))))
    (logior (ash b0 24) (ash b1 16) (ash b2 8) b3)))

(defun emacs-network-ffi--make-sockaddr-in (host port)
  "Allocate + populate `struct sockaddr_in' for HOST + PORT.
HOST: nil / t / `any` → `INADDR_ANY`; `local` → 127.0.0.1; string →
parsed via `inet_pton'.  PORT: 1..65535 in host byte order.
Returns (PTR . LEN) — caller `nl-ffi-free's PTR.

Layout (Linux x86_64 / arm64):
  offset 0 .. 1   sin_family = AF_INET (host byte order uint16)
  offset 2 .. 3   sin_port   = port (network byte order uint16)
  offset 4 .. 7   sin_addr   = address (network byte order uint32)
  offset 8 .. 15  sin_zero[8] = padding (zeroed by nl-ffi-malloc)
  total: 16 bytes."
  (let ((addr-host (emacs-network-ffi--parse-ipv4 host)))
    (unless addr-host
      (error "emacs-network-ffi: cannot parse HOST %S as IPv4" host))
    (unless (and (integerp port) (>= port 0) (<= port 65535))
      (error "emacs-network-ffi: PORT must be 0..65535, got %S" port))
    (let* ((port-be (emacs-network-ffi--htons port))
           (addr-be (emacs-network-ffi--htonl addr-host))
           (buf (nl-ffi-malloc emacs-network-ffi--sockaddr-in-size)))
      (nl-ffi-write-i16 buf 0 emacs-network-ffi-AF_INET)
      ;; sin_port: 16-bit network byte order.  Already byte-swapped above,
      ;; so write as a u16 little-endian and the bytes land BE.
      (nl-ffi-write-i16 buf 2 port-be)
      ;; sin_addr: 32-bit network byte order.  htonl produced the
      ;; correct byte sequence for direct little-endian write.
      (nl-ffi-write-i32 buf 4 addr-be)
      (cons buf emacs-network-ffi--sockaddr-in-size))))


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

(defun emacs-network-ffi--bind-inet (fd host port)
  "FFI: int bind(fd, struct sockaddr_in{host,port}, sizeof).
Returns 0 on success, -1 on error.  Phase 7b TCP entry."
  (let* ((addr (emacs-network-ffi--make-sockaddr-in host port))
         (ptr (car addr))
         (len (cdr addr))
         (rc (emacs-network-ffi--call
              "bind"
              [:sint32 :sint32 :pointer :sint32]
              fd ptr len)))
    (nl-ffi-free ptr)
    rc))

(defun emacs-network-ffi--connect-inet (fd host port)
  "FFI: int connect(fd, struct sockaddr_in{host,port}, sizeof).
Returns 0 on success, -1 on error.  Phase 7b TCP client."
  (let* ((addr (emacs-network-ffi--make-sockaddr-in host port))
         (ptr (car addr))
         (len (cdr addr))
         (rc (emacs-network-ffi--call
              "connect"
              [:sint32 :sint32 :pointer :sint32]
              fd ptr len)))
    (nl-ffi-free ptr)
    rc))

(defun emacs-network-ffi--setsockopt-int (fd level optname value)
  "FFI: int setsockopt(fd, level, optname, &value, sizeof(int)).
Used for `SO_REUSEADDR' on TCP listeners.  Returns 0 on success."
  (let* ((buf (nl-ffi-malloc 4)))
    (nl-ffi-write-i32 buf 0 value)
    (let ((rc (emacs-network-ffi--call
               "setsockopt"
               [:sint32 :sint32 :sint32 :sint32 :pointer :sint32]
               fd level optname buf 4)))
      (nl-ffi-free buf)
      rc)))

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

(defun emacs-network-ffi--utf8-byte-length (s)
  "Return the UTF-8 byte length of S as a non-negative integer.
Pure Elisp (no FFI, no heap alloc).  NeLisp `length' returns the
character count for multibyte strings; `send(2)' and the various
length-prefixed wire formats want the raw byte count of the UTF-8
encoding.  Summing per-codepoint widths (1 / 2 / 3 / 4 bytes) is
cheap for the response sizes we deal with (<= 200 kB) and avoids
the heap-corruption / busy-loop hazard observed with a libc-malloc
+ strlen round-trip variant."
  (let ((n 0) (i 0) (len (length s)))
    (while (< i len)
      (let ((c (aref s i)))
        (cond
         ((< c #x80)    (setq n (1+ n)))
         ((< c #x800)   (setq n (+ n 2)))
         ((< c #x10000) (setq n (+ n 3)))
         (t             (setq n (+ n 4)))))
      (setq i (1+ i)))
    n))

(defun emacs-network-ffi--send (fd str &optional flags)
  "FFI: ssize_t send(int sockfd, const void *buf, size_t len, int flags).
Returns the number of bytes accepted by the kernel, or -1 on error.
Caller is responsible for retrying short writes / EAGAIN.

`nl-ffi-write-bytes' writes the raw UTF-8 byte sequence underlying
STR (= NeLisp multibyte string).  The size we malloc, the size we
hand to `send(2)' and the trace count must all be the UTF-8 byte
length — *not* `(length str)' (= character count).  Compute the
byte count up front in pure Elisp via
`emacs-network-ffi--utf8-byte-length', allocate exactly that, then
fire `send' with the same number."
  (unless (stringp str)
    (error "emacs-network-ffi--send: STR must be a string, got %S"
           (type-of str)))
  (let* ((byte-len (emacs-network-ffi--utf8-byte-length str))
         (flags-val (or flags 0))
         (buf (nl-ffi-malloc (max byte-len 1))))
    (when (> byte-len 0)
      (nl-ffi-write-bytes buf str))
    (let ((sent (emacs-network-ffi--call
                 "send"
                 [:sint64 :sint32 :pointer :sint64 :sint32]
                 fd buf byte-len flags-val)))
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


;;;; --- Phase 7b TCP wrappers -------------------------------------------

(defun emacs-network-ffi-server-tcp (host port &optional backlog)
  "Open + bind + listen on TCP HOST:PORT.

HOST: nil / t / `any` → bind on every interface (`INADDR_ANY`);
`local` → 127.0.0.1; string → parsed via `inet_pton'.
PORT: 1..65535 in host byte order.
BACKLOG defaults to 16.

Sets `SO_REUSEADDR' so listener restarts do not trip
TIME_WAIT.  Fd is left in non-blocking mode.

Returns the server fd on success, `(:error STRING)' on failure."
  (let ((backlog* (or backlog 16))
        (fd (emacs-network-ffi--socket
             emacs-network-ffi-AF_INET
             emacs-network-ffi-SOCK_STREAM
             0)))
    (cond
     ((or (not (integerp fd)) (< fd 0))
      `(:error ,(format "socket() failed: errno=%d"
                        (emacs-network-ffi--errno))))
     (t
      (emacs-network-ffi--setsockopt-int
       fd emacs-network-ffi-SOL_SOCKET emacs-network-ffi-SO_REUSEADDR 1)
      (let ((rc-bind (emacs-network-ffi--bind-inet fd host port)))
        (cond
         ((not (zerop (or rc-bind -1)))
          (let ((errno (emacs-network-ffi--errno)))
            (emacs-network-ffi--close fd)
            `(:error ,(format "bind(%s:%d) failed: errno=%d"
                              host port errno))))
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

(defun emacs-network-ffi-client-tcp (host port)
  "Open + connect to a TCP HOST:PORT.
Returns the client fd on success, `(:error STRING)' on failure."
  (let ((fd (emacs-network-ffi--socket
             emacs-network-ffi-AF_INET
             emacs-network-ffi-SOCK_STREAM
             0)))
    (cond
     ((or (not (integerp fd)) (< fd 0))
      `(:error ,(format "socket() failed: errno=%d"
                        (emacs-network-ffi--errno))))
     (t
      (let ((rc (emacs-network-ffi--connect-inet fd host port)))
        (cond
         ((not (zerop (or rc -1)))
          (let ((errno (emacs-network-ffi--errno)))
            (emacs-network-ffi--close fd)
            `(:error ,(format "connect(%s:%d) failed: errno=%d"
                              host port errno))))
         (t
          (emacs-network-ffi--set-nonblocking fd)
          fd)))))))


(provide 'emacs-network-ffi)

;;; emacs-network-ffi.el ends here
