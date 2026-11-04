;;; emacs-pty-ffi-test.el --- ERT for emacs-pty-ffi (Doc 06 C3)  -*- lexical-binding: t; -*-

;;; Commentary:

;; The actual PTY round-trip needs `nl-ffi-call' (a nelisp-runtime primitive
;; absent under host Emacs), so the round-trip test is `skip-unless'-gated.
;; The graceful-degradation test runs under host Emacs and verifies the FFI
;; guards return nil (rather than erroring) when the FFI is unavailable.

;;; Code:

(require 'ert)
(require 'emacs-pty-ffi)

(ert-deftest emacs-pty-ffi-test/graceful-without-ffi ()
  "Without the libc FFI, the PTY functions degrade gracefully (no error)."
  (skip-unless (not (emacs-pty-ffi-available-p)))
  (should-not (emacs-pty-ffi-available-p))
  (should-not (emacs-pty-ffi-openpt))
  (should-not (emacs-pty-ffi-ptsname 0))
  (should-not (emacs-pty-ffi-open)))

(ert-deftest emacs-pty-ffi-test/open-roundtrip ()
  "On a real nelisp build, posix_openpt + ptsname yield a master fd + slave
path (runtime-only; skipped under host Emacs)."
  (skip-unless (emacs-pty-ffi-available-p))
  (let ((pair (emacs-pty-ffi-open)))
    (should (consp pair))
    (should (integerp (car pair)))
    (should (>= (car pair) 0))
    (should (stringp (cdr pair)))
    (should (string-prefix-p "/dev/pts/" (cdr pair)))
    (emacs-network-ffi--call "close" [:sint32 :sint32] (car pair))))

(provide 'emacs-pty-ffi-test)
;;; emacs-pty-ffi-test.el ends here
