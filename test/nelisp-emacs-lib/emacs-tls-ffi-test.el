;;; emacs-tls-ffi-test.el --- ERT for the GnuTLS FFI (Doc 06 D1)  -*- lexical-binding: t; -*-

;;; Commentary:

;; D1 is author-only in this environment: GnuTLS is not a syscall, so the
;; syscall shim cannot reach it — full verification needs a libffi-enabled
;; nelisp build.  Here we host-test the FFI-free parts: graceful degradation
;; when the FFI is absent, the constants, and the retry/fatal classification.

;;; Code:

(require 'ert)
(require 'emacs-tls-ffi)

(ert-deftest emacs-tls-ffi-test/graceful-without-ffi ()
  "Without `nl-ffi-call' every entry point degrades to nil (no error)."
  (skip-unless (not (emacs-tls-ffi-available-p)))
  (should-not (emacs-tls-ffi-available-p))
  (should-not (emacs-tls-ffi-handshake 3))
  (should-not (emacs-tls-ffi-send '(:session 1) "hi"))
  (should-not (emacs-tls-ffi-recv '(:session 1) 16))
  (should-not (emacs-tls-ffi-close '(:session 1))))

(ert-deftest emacs-tls-ffi-test/constants ()
  "GnuTLS constants match the C headers."
  (should (= 1 emacs-tls-ffi-GNUTLS_CLIENT))
  (should (= 1 emacs-tls-ffi-GNUTLS_CRD_CERTIFICATE))
  (should (= 0 emacs-tls-ffi-GNUTLS_SHUT_RDWR))
  (should (= -28 emacs-tls-ffi-GNUTLS_E_AGAIN))
  (should (= -52 emacs-tls-ffi-GNUTLS_E_INTERRUPTED)))

(ert-deftest emacs-tls-ffi-test/fatal-classification ()
  "Only genuine negative errors are fatal; AGAIN/INTERRUPTED and success
codes are retryable / non-fatal (drives the handshake loop)."
  (should (emacs-tls-ffi--fatal-p -50))
  (should-not (emacs-tls-ffi--fatal-p emacs-tls-ffi-GNUTLS_E_AGAIN))
  (should-not (emacs-tls-ffi--fatal-p emacs-tls-ffi-GNUTLS_E_INTERRUPTED))
  (should-not (emacs-tls-ffi--fatal-p 0))
  (should-not (emacs-tls-ffi--fatal-p 5)))

(provide 'emacs-tls-ffi-test)
;;; emacs-tls-ffi-test.el ends here
