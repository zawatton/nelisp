;;; emacs-pty-ffi-binary-verify.el --- Real-binary PTY verification (Doc 06 C3)  -*- lexical-binding: t; -*-

;;; Commentary:

;; PTY FFI cannot be exercised under host Emacs (no `nl-ffi-call').  This script
;; verifies it on the *built* standalone nelisp binary, where the
;; `emacs-network-syscall-shim' provides nl-ffi-call over `syscall-direct'.
;;
;; Run (from the nelisp-emacs repo root, after `make nelisp'):
;;
;;   vendor/nelisp/target/nelisp --load test/emacs-pty-ffi-binary-verify.el
;;
;; Expected last line: "PTY-VERIFY: PASS ...".
;; Verified 2026-06-27: open /dev/ptmx + ioctl TIOCSPTLCK/TIOCGPTN yields a real
;; /dev/pts/N, and a master->slave write round-trips the bytes.

;;; Code:

;; Run from the repo root.  The standalone nelisp binary leaves `load-file-name'
;; *unbound* and `default-directory' nil under --load, so neither is usable for
;; path resolution; probe plain relative prefixes (file-exists-p resolves them
;; against the process cwd) instead.
(let* ((prefixes (list "src/" "../src/" "test/../src/"))
       (src (catch 'hit
              (dolist (p prefixes)
                (when (file-exists-p (concat p "emacs-pty-ffi.el"))
                  (throw 'hit p))))))
  (unless src (error "PTY-VERIFY: cannot locate src/ from %S" prefixes))
  (load (concat src "emacs-network-syscall-shim.el") nil t)
  (load (concat src "emacs-network-ffi.el") nil t)
  (load (concat src "emacs-pty-ffi.el") nil t))

(let* ((pair (emacs-pty-ffi-open))
       (master (car-safe pair))
       (path (cdr-safe pair))
       (ok-open (and (integerp master) (>= master 0)
                     (stringp path) (string-prefix-p "/dev/pts/" path))))
  (if (not ok-open)
      (princ (format "PTY-VERIFY: FAIL open=%S\n" pair))
    (let ((slave (emacs-network-ffi--call
                  "open" [:sint32 :string :sint32 :sint32] path 2 0)))
      (emacs-pty-ffi-set-nonblocking slave)
      (let ((wbuf (nl-ffi-malloc 8)) (rbuf (nl-ffi-malloc 32)) (n -1) (tries 0))
        (nl-ffi-write-bytes-at wbuf 0 "hi\n")
        (emacs-network-ffi--call
         "write" [:sint64 :sint32 :pointer :sint64] master wbuf 3)
        (while (and (< tries 100000) (or (not (integerp n)) (< n 1)))
          (setq n (emacs-network-ffi--call
                   "read" [:sint64 :sint32 :pointer :sint64] slave rbuf 32))
          (setq tries (1+ tries)))
        (let ((data (and (integerp n) (> n 0) (nl-ffi-read-bytes rbuf n))))
          (princ (format "PTY-VERIFY: %s master=%S slave=%S path=%s data=%S\n"
                         (if (and data (string-prefix-p "hi" data)) "PASS" "FAIL")
                         master slave path data)))))))
