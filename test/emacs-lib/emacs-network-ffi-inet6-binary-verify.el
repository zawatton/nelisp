;;; emacs-network-ffi-inet6-binary-verify.el --- Real-binary IPv6/datagram verify (Doc 06 D2)  -*- lexical-binding: t; -*-

;;; Commentary:

;; IPv6 + datagram FFI cannot be exercised under host Emacs (no `nl-ffi-call').
;; This script verifies it on the *built* standalone nelisp binary, where
;; `emacs-network-syscall-shim' provides nl-ffi-call over `syscall-direct'.
;;
;; Run (from the nelisp-emacs repo root, after `make nelisp'):
;;
;;   vendor/nelisp/target/nelisp --load test/emacs-network-ffi-inet6-binary-verify.el
;;
;; Expected last line: "INET6-VERIFY: PASS v4=udp4 v6=udp6".
;; Each leg does a UDP loopback round-trip (bind a server, sendto it from a
;; client, recvfrom the server) over AF_INET (v4) and AF_INET6 (v6) — proving
;; SOCK_DGRAM + sendto/recvfrom and `struct sockaddr_in6' marshalling.

;;; Code:

;; The standalone binary leaves `load-file-name' unbound and `default-directory'
;; nil under --load; probe relative prefixes via file-exists-p (see Doc 06 §9.1).
(let* ((prefixes (list "src/" "../src/" "test/../src/"))
       (src (catch 'hit
              (dolist (p prefixes)
                (when (file-exists-p (concat p "emacs-network-ffi-inet6.el"))
                  (throw 'hit p))))))
  (unless src (error "INET6-VERIFY: cannot locate src/ from %S" prefixes))
  (load (concat src "emacs-network-syscall-shim.el") nil t)
  (load (concat src "emacs-network-ffi.el") nil t)
  (load (concat src "emacs-network-ffi-inet6.el") nil t))

(defun inet6-verify--roundtrip (family host port payload sender)
  "Bind a SOCK_DGRAM server on FAMILY HOST:PORT, SENDER sends PAYLOAD to it,
return what the server recvfrom's (spinning while non-blocking).  SENDER is a
function (client-fd payload host port)."
  (let ((server (emacs-network-ffi--socket-dgram family)))
    (emacs-network-ffi--setsockopt-int
     server emacs-network-ffi-SOL_SOCKET emacs-network-ffi-SO_REUSEADDR 1)
    (let ((brc (if (= family emacs-network-ffi-AF_INET6)
                   (emacs-network-ffi--bind-inet6 server host port)
                 (emacs-network-ffi--bind-inet server host port))))
      (unless (and (integerp brc) (= brc 0))
        (error "INET6-VERIFY: bind failed family=%S rc=%S errno=%S"
               family brc (emacs-network-ffi--errno))))
    (emacs-network-ffi--set-nonblocking server)
    (let ((client (emacs-network-ffi--socket-dgram family)))
      (funcall sender client payload host port)
      (let ((got nil) (tries 0))
        (while (and (< tries 200000)
                    (or (not (stringp got)) (string= got "")))
          (setq got (emacs-network-ffi--recvfrom server 64))
          (setq tries (1+ tries)))
        (emacs-network-ffi--close client)
        (emacs-network-ffi--close server)
        got))))

(let* ((v4 (inet6-verify--roundtrip
            emacs-network-ffi-AF_INET 'local 38821 "udp4"
            (lambda (fd p h port) (emacs-network-ffi--sendto-inet fd p h port))))
       (v6 (inet6-verify--roundtrip
            emacs-network-ffi-AF_INET6 'local 38822 "udp6"
            (lambda (fd p h port) (emacs-network-ffi--sendto-inet6 fd p h port)))))
  (princ (format "INET6-VERIFY: %s v4=%S v6=%S\n"
                 (if (and (equal v4 "udp4") (equal v6 "udp6")) "PASS" "FAIL")
                 v4 v6)))
