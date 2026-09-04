;;; emacs-tls-ffi.el --- GnuTLS handshake over a socket fd (Doc 06 D1) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 06 D1: TLS / `open-network-stream' by driving GnuTLS through the FFI on a
;; connected non-blocking socket fd (the same fds `emacs-network-ffi' produces).
;; Pure-Elisp TLS is out of scope (§1); this is an FFI-boundary item that
;; unblocks HTTPS / smtpmail / nnimap.
;;
;; VERIFICATION BOUNDARY (author-only here): GnuTLS lives in libgnutls.so, whose
;; symbols are NOT syscalls, so the syscall shim used to verify C3/D2/C1/C2 on
;; the standalone reader cannot reach them.  This module needs a *libffi-enabled*
;; nelisp build (a real `nl-ffi-call' that dlopens arbitrary shared objects).
;; On that build it is exercised the same way as the socket FFI: connect a TCP
;; socket, `emacs-tls-ffi-handshake' it, then `-send' / `-recv'.  Under host
;; Emacs and the syscall-shim binary `emacs-tls-ffi-available-p' is nil and every
;; entry point degrades to nil rather than erroring.
;;
;; The GnuTLS C call sequence mirrored here (client):
;;   gnutls_global_init()
;;   gnutls_certificate_allocate_credentials(&xcred)
;;   gnutls_init(&session, GNUTLS_CLIENT)
;;   gnutls_set_default_priority(session)
;;   gnutls_credentials_set(session, GNUTLS_CRD_CERTIFICATE, xcred)
;;   gnutls_transport_set_int2(session, fd, fd)
;;   do { r = gnutls_handshake(session) } while (r<0 && !gnutls_error_is_fatal(r))
;;   gnutls_record_send / gnutls_record_recv
;;   gnutls_bye(session, GNUTLS_SHUT_RDWR); gnutls_deinit; free creds

;;; Code:

(require 'emacs-network-ffi)

(defvar emacs-tls-ffi-libgnutls-path
  (or (and (fboundp 'getenv) (getenv "EMACS_TLS_FFI_LIBGNUTLS"))
      (let ((candidates
             '("/lib/x86_64-linux-gnu/libgnutls.so.30"
               "/lib64/libgnutls.so.30"
               "/lib/aarch64-linux-gnu/libgnutls.so.30"
               "/usr/lib/libgnutls.so.30"
               "/usr/lib/libgnutls.so"
               "/usr/lib/libgnutls.dylib")))
        (let ((found nil))
          (while (and candidates (not found))
            (when (and (fboundp 'file-readable-p)
                       (file-readable-p (car candidates)))
              (setq found (car candidates)))
            (setq candidates (cdr candidates)))
          found))
      "/lib/x86_64-linux-gnu/libgnutls.so.30")
  "Absolute path to the GnuTLS shared object.
Override via `EMACS_TLS_FFI_LIBGNUTLS' or by `setq' before this file loads.")

;;;; --- GnuTLS constants -------------------------------------------------

(defconst emacs-tls-ffi-GNUTLS_CLIENT 1 "gnutls_init flag: client endpoint.")
(defconst emacs-tls-ffi-GNUTLS_CRD_CERTIFICATE 1 "Certificate credentials type.")
(defconst emacs-tls-ffi-GNUTLS_SHUT_RDWR 0 "gnutls_bye: full bidirectional close.")
(defconst emacs-tls-ffi-GNUTLS_E_AGAIN -28 "Non-fatal: retry the operation.")
(defconst emacs-tls-ffi-GNUTLS_E_INTERRUPTED -52 "Non-fatal: interrupted, retry.")

;;;; --- availability + low-level call ------------------------------------

(defun emacs-tls-ffi-available-p ()
  "Non-nil when the GnuTLS FFI can be used (needs a libffi nelisp build)."
  (and (fboundp 'nl-ffi-call)
       (stringp emacs-tls-ffi-libgnutls-path)
       (or (not (fboundp 'file-readable-p))
           (file-readable-p emacs-tls-ffi-libgnutls-path))))

(defun emacs-tls-ffi--call (func sig &rest args)
  "Dispatch GnuTLS FUNC with SIG + ARGS through `nl-ffi-call'."
  (apply #'nl-ffi-call emacs-tls-ffi-libgnutls-path func sig args))

(defun emacs-tls-ffi--read-ptr (buf off)
  "Read a 64-bit pointer from BUF at OFF as two 32-bit halves.
Avoids assuming a `nl-ffi-read-i64' helper exists on every build."
  (logior (logand (nl-ffi-read-i32 buf off) #xffffffff)
          (ash (logand (nl-ffi-read-i32 buf (+ off 4)) #xffffffff) 32)))

(defun emacs-tls-ffi--fatal-p (rc)
  "Non-nil when GnuTLS return RC is a fatal (non-retryable) error."
  (and (integerp rc) (< rc 0)
       (not (= rc emacs-tls-ffi-GNUTLS_E_AGAIN))
       (not (= rc emacs-tls-ffi-GNUTLS_E_INTERRUPTED))))

;;;; --- handshake / I/O --------------------------------------------------

(defun emacs-tls-ffi-handshake (fd &optional max-retries)
  "Perform a GnuTLS client handshake over connected socket FD.
Returns a plist (:session PTR :creds PTR :fd FD) on success, or nil when the
FFI is unavailable or the handshake fails (Doc 06 D1).  MAX-RETRIES bounds the
non-blocking AGAIN/INTERRUPTED retry loop (default 10000)."
  (when (emacs-tls-ffi-available-p)
    (let ((retries (or max-retries 10000)))
      (emacs-tls-ffi--call "gnutls_global_init" [:sint32])
      (let* ((credbuf (nl-ffi-malloc 8))
             (sessbuf (nl-ffi-malloc 8)))
        (emacs-tls-ffi--call "gnutls_certificate_allocate_credentials"
                             [:sint32 :pointer] credbuf)
        (let ((xcred (emacs-tls-ffi--read-ptr credbuf 0)))
          (emacs-tls-ffi--call "gnutls_init" [:sint32 :pointer :sint32]
                               sessbuf emacs-tls-ffi-GNUTLS_CLIENT)
          (let ((session (emacs-tls-ffi--read-ptr sessbuf 0)))
            (emacs-tls-ffi--call "gnutls_set_default_priority"
                                 [:sint32 :sint64] session)
            (emacs-tls-ffi--call "gnutls_credentials_set"
                                 [:sint32 :sint64 :sint32 :sint64]
                                 session emacs-tls-ffi-GNUTLS_CRD_CERTIFICATE
                                 xcred)
            (emacs-tls-ffi--call "gnutls_transport_set_int2"
                                 [:void :sint64 :sint32 :sint32]
                                 session fd fd)
            (nl-ffi-free credbuf)
            (nl-ffi-free sessbuf)
            (let ((rc -1) (n 0))
              (while (and (< n retries)
                          (progn (setq rc (emacs-tls-ffi--call
                                           "gnutls_handshake" [:sint32 :sint64]
                                           session))
                                 (and (integerp rc) (< rc 0)
                                      (not (emacs-tls-ffi--fatal-p rc)))))
                (setq n (1+ n)))
              (if (and (integerp rc) (= rc 0))
                  (list :session session :creds xcred :fd fd)
                ;; Handshake failed: tear down and report nil.
                (ignore-errors (emacs-tls-ffi--deinit session xcred))
                nil))))))))

(defun emacs-tls-ffi--deinit (session creds)
  "Free a GnuTLS SESSION and its CREDS."
  (emacs-tls-ffi--call "gnutls_deinit" [:void :sint64] session)
  (emacs-tls-ffi--call "gnutls_certificate_free_credentials" [:void :sint64]
                       creds))

(defun emacs-tls-ffi-send (tls str)
  "Encrypt + send STR over TLS (a plist from `emacs-tls-ffi-handshake').
Returns bytes sent, or nil when unavailable."
  (when (and (emacs-tls-ffi-available-p) tls (stringp str))
    (let* ((session (plist-get tls :session))
           (byte-len (emacs-network-ffi--utf8-byte-length str))
           (buf (nl-ffi-malloc (max byte-len 1))))
      (when (> byte-len 0) (nl-ffi-write-bytes buf str))
      (let ((sent (emacs-tls-ffi--call "gnutls_record_send"
                                       [:sint64 :sint64 :pointer :sint64]
                                       session buf byte-len)))
        (nl-ffi-free buf)
        sent))))

(defun emacs-tls-ffi-recv (tls max-bytes)
  "Receive + decrypt up to MAX-BYTES from TLS.
Returns a string (\"\" at clean EOF), :would-block on AGAIN, or nil on error."
  (when (and (emacs-tls-ffi-available-p) tls)
    (let* ((session (plist-get tls :session))
           (buf (nl-ffi-malloc max-bytes))
           (got (emacs-tls-ffi--call "gnutls_record_recv"
                                     [:sint64 :sint64 :pointer :sint64]
                                     session buf max-bytes))
           (result nil))
      (cond
       ((and (integerp got) (>= got 0))
        (setq result (if (zerop got) "" (nl-ffi-read-bytes buf got))))
       ((and (integerp got)
             (or (= got emacs-tls-ffi-GNUTLS_E_AGAIN)
                 (= got emacs-tls-ffi-GNUTLS_E_INTERRUPTED)))
        (setq result :would-block)))
      (nl-ffi-free buf)
      result)))

(defun emacs-tls-ffi-close (tls)
  "Send a TLS close-notify, free the session + credentials.
Does NOT close the underlying socket fd (the caller owns it)."
  (when (and (emacs-tls-ffi-available-p) tls)
    (let ((session (plist-get tls :session)))
      (emacs-tls-ffi--call "gnutls_bye" [:sint64 :sint64 :sint32]
                           session emacs-tls-ffi-GNUTLS_SHUT_RDWR)
      (emacs-tls-ffi--deinit session (plist-get tls :creds))
      t)))

(provide 'emacs-tls-ffi)
;;; emacs-tls-ffi.el ends here
