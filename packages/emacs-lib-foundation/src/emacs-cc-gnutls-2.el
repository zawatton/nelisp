;;; emacs-cc-gnutls-2.el --- GnuTLS C primitive compatibility -*- lexical-binding: t; -*-

(unless (fboundp 'gnutls-hash-digest)
  (defun gnutls-hash-digest (digest-method input)
    "Digest INPUT with DIGEST-METHOD into a unibyte string.

Return nil on error.

The INPUT can be specified as a buffer or string or in other
ways (see Info node `(elisp)Format of GnuTLS Cryptography Inputs')."
    (unless (or (stringp input) (bufferp input))
      (signal 'wrong-type-argument (list 'consp input)))
    (unless (or (stringp digest-method) (symbolp digest-method)
                (numberp digest-method) (and (listp digest-method)
                                             (plist-member digest-method :digest-algorithm-id)))
      (signal 'wrong-type-argument (list 'consp digest-method)))
    (signal 'error (list "GnuTLS digest-method is invalid or not found" digest-method))))

(unless (fboundp 'gnutls-hash-mac)
  (defun gnutls-hash-mac (hash-method key input)
    "Hash INPUT with HASH-METHOD and KEY into a unibyte string.

Return nil on error."
    (unless (or (stringp key) (bufferp key))
      (signal 'wrong-type-argument (list 'consp key)))
    (unless (or (stringp input) (bufferp input))
      (signal 'wrong-type-argument (list 'consp input)))
    (unless (or (stringp hash-method) (symbolp hash-method) (numberp hash-method)
                (and (listp hash-method) (plist-member hash-method :mac-algorithm-id)))
      (signal 'wrong-type-argument (list 'consp hash-method)))
    (signal 'error (list "GnuTLS MAC-method is invalid or not found" hash-method))))

(unless (fboundp 'gnutls-macs)
  (defun gnutls-macs ()
    "Return alist of GnuTLS mac-algorithm method descriptions as plists."
    (and (fboundp 'gnutls-ciphers) (gnutls-ciphers))))

(unless (fboundp 'gnutls-peer-status)
  (defun gnutls-peer-status (proc)
    "Describe GnuTLS peer certificate of PROC and any warnings about it."
    (unless (and (fboundp 'processp) (processp proc))
      (signal 'wrong-type-argument (list 'processp proc)))
    nil))

(unless (fboundp 'gnutls-peer-status-warning-describe)
  (defun gnutls-peer-status-warning-describe (status-symbol)
    "Describe the warning of a GnuTLS peer status from `gnutls-peer-status'."
    (unless (symbolp status-symbol)
      (signal 'wrong-type-argument (list 'symbolp status-symbol)))
    nil))

(unless (fboundp 'gnutls-symmetric-decrypt)
  (defun gnutls-symmetric-decrypt (cipher key iv input &optional aead-auth)
    "Decrypt INPUT with symmetric CIPHER, KEY+AEAD_AUTH, and IV to a unibyte string.

Return nil on error."
    (unless (or (stringp key) (bufferp key))
      (signal 'wrong-type-argument (list 'consp key)))
    (unless (or (stringp cipher) (symbolp cipher) (numberp cipher)
                (and (listp cipher) (plist-member cipher :cipher-id)))
      (signal 'wrong-type-argument (list 'consp cipher)))
    (signal 'error (list "GnuTLS cipher is invalid or not found" cipher))))

(unless (fboundp 'gnutls-symmetric-encrypt)
  (defun gnutls-symmetric-encrypt (cipher key iv input &optional aead-auth)
    "Encrypt INPUT with symmetric CIPHER, KEY+AEAD_AUTH, and IV to a unibyte string.

Return nil on error."
    (unless (or (stringp key) (bufferp key))
      (signal 'wrong-type-argument (list 'consp key)))
    (unless (or (stringp cipher) (symbolp cipher) (numberp cipher)
                (and (listp cipher) (plist-member cipher :cipher-id)))
      (signal 'wrong-type-argument (list 'consp cipher)))
    (signal 'error (list "GnuTLS cipher is invalid or not found" cipher))))

(provide 'emacs-cc-gnutls-2)
