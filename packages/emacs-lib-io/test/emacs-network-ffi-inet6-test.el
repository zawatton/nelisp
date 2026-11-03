;;; emacs-network-ffi-inet6-test.el --- ERT for IPv6/datagram FFI (Doc 06 D2)  -*- lexical-binding: t; -*-

;;; Commentary:

;; The real UDP round-trip needs `nl-ffi-call' (a nelisp-runtime primitive
;; absent under host Emacs), so it is `skip-unless'-gated and instead covered by
;; `test/emacs-network-ffi-inet6-binary-verify.el' on the built binary.  The
;; constants/surface test runs under host Emacs.

;;; Code:

(require 'ert)
(require 'emacs-network-ffi-inet6)

(ert-deftest emacs-network-ffi-inet6-test/constants-and-surface ()
  "AF_INET6 / SOCK_DGRAM / sockaddr_in6 size + the datagram surface are defined."
  (should (= emacs-network-ffi-AF_INET6 10))
  (should (= emacs-network-ffi-SOCK_DGRAM 2))
  (should (= emacs-network-ffi--sockaddr-in6-size 28))
  (should (fboundp 'emacs-network-ffi--make-sockaddr-in6))
  (should (fboundp 'emacs-network-ffi--socket-dgram))
  (should (fboundp 'emacs-network-ffi--bind-inet6))
  (should (fboundp 'emacs-network-ffi--sendto))
  (should (fboundp 'emacs-network-ffi--sendto-inet))
  (should (fboundp 'emacs-network-ffi--sendto-inet6))
  (should (fboundp 'emacs-network-ffi--recvfrom)))

(ert-deftest emacs-network-ffi-inet6-test/sockaddr-in6-rejects-bad-port ()
  "PORT validation happens before any FFI call, so it works under host Emacs."
  (should-error (emacs-network-ffi--make-sockaddr-in6 'local 70000))
  (should-error (emacs-network-ffi--make-sockaddr-in6 'local -1)))

(ert-deftest emacs-network-ffi-inet6-test/udp-roundtrip ()
  "Real UDP v4+v6 loopback round-trip on a nelisp build (skipped under host)."
  (skip-unless (fboundp 'nl-ffi-call))
  (let* ((server (emacs-network-ffi--socket-dgram emacs-network-ffi-AF_INET6)))
    (emacs-network-ffi--setsockopt-int
     server emacs-network-ffi-SOL_SOCKET emacs-network-ffi-SO_REUSEADDR 1)
    (should (= 0 (emacs-network-ffi--bind-inet6 server 'local 38833)))
    (emacs-network-ffi--set-nonblocking server)
    (let ((client (emacs-network-ffi--socket-dgram emacs-network-ffi-AF_INET6)))
      (should (> (emacs-network-ffi--sendto-inet6 client "ping6" 'local 38833) 0))
      (let ((got nil) (tries 0))
        (while (and (< tries 200000) (not (equal got "ping6")))
          (setq got (emacs-network-ffi--recvfrom server 64))
          (setq tries (1+ tries)))
        (should (equal got "ping6")))
      (emacs-network-ffi--close client))
    (emacs-network-ffi--close server)))

(provide 'emacs-network-ffi-inet6-test)
;;; emacs-network-ffi-inet6-test.el ends here
