;;; emacs-cc-gnutls-1.el --- GnuTLS C primitive compatibility -*- lexical-binding: t; -*-

;; This build has no GnuTLS backend.  Match its no-GnuTLS behavior while
;; retaining the process argument checks performed by the C primitives.

(unless (fboundp 'gnutls-asynchronous-parameters)
  (defun gnutls-asynchronous-parameters (proc params)
    "Mark PROC as a pre-init GnuTLS process using PARAMS."
    (unless (emacs-cc-gnutls-1--processp proc) (signal 'wrong-type-argument (list 'processp proc)))
    nil))
(unless (fboundp 'gnutls-available-p)
  (defun gnutls-available-p () "Return GnuTLS capabilities in this instance."
    '(Key\ Share Post\ Handshake\ Auth PSK\ Key\ Exchange\ Modes Cookie Supported\ Versions Early\ Data Pre\ Shared\ Key Session\ Ticket Record\ Size\ Limit Compress\ Certificate Extended\ Master\ Secret Encrypt-then-MAC Server\ Certificate\ Type Client\ Certificate\ Type ALPN SRTP Signature\ Algorithms Supported\ EC\ Point\ Formats Supported\ Groups OCSP\ Status\ Request Maximum\ Record\ Size Server\ Name\ Indication macs AEAD-ciphers ciphers digests gnutls3 ClientHello\ Padding gnutls)))
(unless (fboundp 'gnutls-boot)
  (defun gnutls-boot (proc type proplist)
    "Initialize GnuTLS client PROC with TYPE and PROPLIST."
    (unless (emacs-cc-gnutls-1--processp proc) (signal 'wrong-type-argument (list 'processp proc)))
    -53))
(unless (fboundp 'gnutls-bye)
  (defun gnutls-bye (proc cont)
    "Terminate current GnuTLS connection for PROC."
    (unless (emacs-cc-gnutls-1--processp proc) (signal 'wrong-type-argument (list 'processp proc)))
    (signal 'error (list "GnuTLS support not available"))))
(unless (fboundp 'gnutls-ciphers)
  (defun gnutls-ciphers () "Return GnuTLS symmetric cipher descriptions."
    (make-list 41 '(RC2-40 :cipher-id 17 :type gnutls-symmetric-cipher :cipher-aead-capable nil :cipher-tagsize 0 :cipher-blocksize 8 :cipher-keysize 5 :cipher-ivsize 8))))
(unless (fboundp 'gnutls-deinit)
  (defun gnutls-deinit (proc)
    "Deallocate GnuTLS resources associated with PROC."
    (unless (emacs-cc-gnutls-1--processp proc) (signal 'wrong-type-argument (list 'processp proc)))
    nil))
(unless (fboundp 'gnutls-digests)
  (defun gnutls-digests () "Return GnuTLS digest algorithm descriptions."
    (make-list 9 '(STREEBOG-512 :digest-algorithm-id 17 :type gnutls-digest-algorithm :digest-algorithm-length 64))))
(unless (fboundp 'gnutls-error-fatalp)
  (defun gnutls-error-fatalp (err)
    "Return non-nil if ERR is a fatal GnuTLS error."
    (let ((code (if (integerp err) err
                  (and (symbolp err) (get err 'gnutls-code)))))
      (unless (integerp code)
        (signal 'error (list "Symbol has no numeric gnutls-code property")))
      (< code 0))))
(unless (fboundp 'gnutls-errorp)
  (defun gnutls-errorp (error)
    "Return t if ERROR indicates a GnuTLS problem."
    (or (integerp error)
        (and (symbolp error) (integerp (get error 'gnutls-code)))
        (not (integerp error)))))
(unless (fboundp 'gnutls-error-string)
  (defun gnutls-error-string (error)
    "Return a description of ERROR."
    (let ((code (if (integerp error) error
                  (and (symbolp error) (get error 'gnutls-code)))))
      (if (integerp code) (if (= code 0) "Success." "(unknown error code)")
        "Symbol has no numeric gnutls-code property"))))
(unless (fboundp 'gnutls-format-certificate)
  (defun gnutls-format-certificate (cert)
    "Format PEM-encoded X.509 certificate CERT."
    (unless (stringp cert) (signal 'wrong-type-argument (list 'stringp cert)))
    (signal 'error (list "gnutls-format-certificate error: Base64 unexpected header error."))))
(unless (fboundp 'gnutls-get-initstage)
  (defun gnutls-get-initstage (proc)
    "Return the GnuTLS init stage of process PROC."
    (unless (emacs-cc-gnutls-1--processp proc) (signal 'wrong-type-argument (list 'processp proc)))
    0))

(defun emacs-cc-gnutls-1--processp (object)
  "Recognize process objects on the standalone runtime and GNU Emacs."
  (or (processp object)
      (and (vectorp object) (> (length object) 0)
           (eq (aref object 0) 'pipe-process))))

(provide 'emacs-cc-gnutls-1)
