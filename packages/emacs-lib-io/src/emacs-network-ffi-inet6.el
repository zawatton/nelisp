;;; emacs-network-ffi-inet6.el --- IPv6 + datagram FFI (Doc 06 D2) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 06 D2: extend the K1 socket FFI (`emacs-network-ffi.el', which covers
;; AF_UNIX + AF_INET/SOCK_STREAM) with:
;;
;;   - AF_INET6 (`struct sockaddr_in6' marshalling via inet_pton)
;;   - SOCK_DGRAM datagram sockets (sendto / recvfrom over IPv4 and IPv6)
;;
;; The pattern mirrors `emacs-network-ffi--make-sockaddr-in' /
;; `--bind-inet' / `--send' / `--recv' exactly, so the same shim
;; (`emacs-network-syscall-shim.el', which now maps sendto=44 / recvfrom=45 /
;; getsockname=51 and parses AF_INET6 in inet_pton) makes these verifiable on
;; the standalone nelisp binary.  Verified end-to-end via
;; `test/emacs-network-ffi-inet6-binary-verify.el' (UDP loopback round-trip on
;; both v4 and v6).
;;
;; This unblocks `make-network-process' for `:family 'ipv6' and `:type 'datagram'.

;;; Code:

(require 'emacs-network-ffi)

(defconst emacs-network-ffi-AF_INET6 10
  "AF_INET6 on Linux (x86_64 / arm64).")

(defconst emacs-network-ffi-SOCK_DGRAM 2
  "SOCK_DGRAM (connectionless datagram socket).")

(defconst emacs-network-ffi--sockaddr-in6-size 28
  "sizeof(struct sockaddr_in6) on Linux.
Layout: 2 family + 2 port(BE) + 4 flowinfo + 16 addr + 4 scope_id.")

;;;; --- struct sockaddr_in6 marshalling ---------------------------------

(defun emacs-network-ffi--make-sockaddr-in6 (host port)
  "Allocate + populate `struct sockaddr_in6' for HOST + PORT.
HOST: nil / t / `any' → \"::\" (in6addr_any); `local' → \"::1\"; string →
parsed via inet_pton(AF_INET6).  PORT: 0..65535 host byte order.
Returns (PTR . LEN); caller `nl-ffi-free's PTR.

Layout (Linux x86_64 / arm64):
  offset 0 .. 1   sin6_family   = AF_INET6 (host byte order uint16)
  offset 2 .. 3   sin6_port     = port (network byte order uint16)
  offset 4 .. 7   sin6_flowinfo = 0 (zeroed by nl-ffi-malloc)
  offset 8 .. 23  sin6_addr     = 16 bytes network order (from inet_pton)
  offset 24 .. 27 sin6_scope_id = 0 (zeroed by nl-ffi-malloc)
  total: 28 bytes."
  (unless (and (integerp port) (>= port 0) (<= port 65535))
    (error "emacs-network-ffi: PORT must be 0..65535, got %S" port))
  (let* ((h (cond
             ((or (null host) (eq host t) (eq host 'any)) "::")
             ((eq host 'local) "::1")
             ((stringp host) host)
             (t (error "emacs-network-ffi: cannot parse HOST %S as IPv6" host))))
         (port-be (emacs-network-ffi--htons port))
         (buf (nl-ffi-malloc emacs-network-ffi--sockaddr-in6-size))
         ;; inet_pton writes 16 network-order bytes at its OUT pointer's
         ;; offset 0; the sockaddr needs them at offset 8, and the shim ptr is
         ;; opaque (no pointer arithmetic), so parse into a scratch buffer and
         ;; copy the 16 bytes into place.
         (scratch (nl-ffi-malloc 16))
         (rc (emacs-network-ffi--call
              "inet_pton"
              [:sint32 :sint32 :string :pointer]
              emacs-network-ffi-AF_INET6 h scratch)))
    (unless (and (integerp rc) (= rc 1))
      (nl-ffi-free scratch)
      (nl-ffi-free buf)
      (error "emacs-network-ffi: inet_pton(AF_INET6) failed for %S (rc=%S)"
             h rc))
    (nl-ffi-write-i16 buf 0 emacs-network-ffi-AF_INET6)
    (nl-ffi-write-i16 buf 2 port-be)
    (nl-ffi-write-bytes-at buf 8 (nl-ffi-read-bytes scratch 16))
    (nl-ffi-free scratch)
    (cons buf emacs-network-ffi--sockaddr-in6-size)))

;;;; --- datagram sockets ------------------------------------------------

(defun emacs-network-ffi--socket-dgram (family)
  "FFI: int socket(FAMILY, SOCK_DGRAM, 0).
FAMILY is `emacs-network-ffi-AF_INET' or `-AF_INET6'.
Returns the new fd, or -1 on error."
  (emacs-network-ffi--socket family emacs-network-ffi-SOCK_DGRAM 0))

(defun emacs-network-ffi--bind-inet6 (fd host port)
  "FFI: int bind(FD, struct sockaddr_in6{HOST,PORT}, sizeof).
Returns 0 on success, -1 on error."
  (let* ((addr (emacs-network-ffi--make-sockaddr-in6 host port))
         (ptr (car addr))
         (len (cdr addr))
         (rc (emacs-network-ffi--call
              "bind"
              [:sint32 :sint32 :pointer :sint32]
              fd ptr len)))
    (nl-ffi-free ptr)
    rc))

(defun emacs-network-ffi--connect-inet6 (fd host port)
  "FFI: int connect(FD, struct sockaddr_in6{HOST,PORT}, sizeof).
Returns 0 on success, -1 on error."
  (let* ((addr (emacs-network-ffi--make-sockaddr-in6 host port))
         (ptr (car addr))
         (len (cdr addr))
         (rc (emacs-network-ffi--call
              "connect"
              [:sint32 :sint32 :pointer :sint32]
              fd ptr len)))
    (nl-ffi-free ptr)
    rc))

(defun emacs-network-ffi--sendto (fd str addr)
  "FFI: ssize_t sendto(FD, STR, len, 0, ADDR-PTR, ADDR-LEN).
ADDR is a (PTR . LEN) cons from `--make-sockaddr-in' / `--make-sockaddr-in6'.
Caller `nl-ffi-free's ADDR's PTR.  Returns bytes sent, or -1 on error."
  (unless (stringp str)
    (error "emacs-network-ffi--sendto: STR must be a string, got %S"
           (type-of str)))
  (let* ((ptr (car addr))
         (len (cdr addr))
         (byte-len (emacs-network-ffi--utf8-byte-length str))
         (buf (nl-ffi-malloc (max byte-len 1))))
    (when (> byte-len 0)
      (nl-ffi-write-bytes buf str))
    (let ((sent (emacs-network-ffi--call
                 "sendto"
                 [:sint64 :sint32 :pointer :sint64 :sint32 :pointer :sint32]
                 fd buf byte-len 0 ptr len)))
      (nl-ffi-free buf)
      sent)))

(defun emacs-network-ffi--sendto-inet (fd str host port)
  "Convenience: IPv4 datagram send of STR to HOST:PORT from FD."
  (let* ((addr (emacs-network-ffi--make-sockaddr-in host port))
         (rc (emacs-network-ffi--sendto fd str addr)))
    (nl-ffi-free (car addr))
    rc))

(defun emacs-network-ffi--sendto-inet6 (fd str host port)
  "Convenience: IPv6 datagram send of STR to HOST:PORT from FD."
  (let* ((addr (emacs-network-ffi--make-sockaddr-in6 host port))
         (rc (emacs-network-ffi--sendto fd str addr)))
    (nl-ffi-free (car addr))
    rc))

(defun emacs-network-ffi--recvfrom (fd max-bytes &optional flags)
  "FFI: ssize_t recvfrom(FD, buf, MAX-BYTES, FLAGS, NULL, NULL).
The peer address is discarded (NULL src) — callers that need it can be
extended later.  Returns:
  - string (0..MAX-BYTES bytes)
  - :would-block on non-blocking EAGAIN
  - :interrupted on EINTR
  - nil on any other error"
  (let* ((flags-val (or flags 0))
         (buf (nl-ffi-malloc max-bytes))
         (got (emacs-network-ffi--call
               "recvfrom"
               [:sint64 :sint32 :pointer :sint64 :sint32 :pointer :pointer]
               fd buf max-bytes flags-val 0 0))
         (result nil))
    (cond
     ((and (integerp got) (>= got 0))
      (setq result (if (zerop got) "" (nl-ffi-read-bytes buf got))))
     ((integerp got)
      (let ((errno (emacs-network-ffi--errno)))
        (cond
         ((= errno emacs-network-ffi-EAGAIN) (setq result :would-block))
         ((= errno emacs-network-ffi-EINTR)  (setq result :interrupted))
         (t (setq result nil))))))
    (nl-ffi-free buf)
    result))

(provide 'emacs-network-ffi-inet6)
;;; emacs-network-ffi-inet6.el ends here
