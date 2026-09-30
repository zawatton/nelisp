;;; gnutls-2.el --- GnuTLS C primitive probes
(gnutls-hash-digest
 (condition-case e (gnutls-hash-digest 'not-a-digest "abc") (error e))
 (condition-case e (gnutls-hash-digest 'SHA256 nil) (error e)))
(gnutls-hash-mac
 (condition-case e (gnutls-hash-mac 'not-a-mac "k" "abc") (error e))
 (condition-case e (gnutls-hash-mac 'SHA256 nil "abc") (error e)))
(gnutls-macs
 (functionp #'gnutls-macs)
 (>= (length (gnutls-macs)) 0))
(gnutls-peer-status
 (condition-case e (gnutls-peer-status nil) (error e))
 (let ((p (make-process :name "gnutls-probe" :command '("true") :noquery t)))
   (unwind-protect (if (processp p) (condition-case e (gnutls-peer-status p) (error e)) nil)
     (delete-process p))))
(gnutls-peer-status-warning-describe
 (gnutls-peer-status-warning-describe 'unknown)
 (gnutls-peer-status-warning-describe 'expired))
(gnutls-symmetric-decrypt
 (condition-case e (gnutls-symmetric-decrypt 'not-a-cipher "k" "i" "x") (error e))
 (condition-case e (gnutls-symmetric-decrypt 'AES-128-CBC nil "i" "x") (error e)))
(gnutls-symmetric-encrypt
 (condition-case e (gnutls-symmetric-encrypt 'not-a-cipher "k" "i" "x") (error e))
 (condition-case e (gnutls-symmetric-encrypt 'AES-128-CBC nil "i" "x") (error e)))
