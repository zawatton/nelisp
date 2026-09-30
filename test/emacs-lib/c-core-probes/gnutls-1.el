(gnutls-asynchronous-parameters
  (condition-case e (gnutls-asynchronous-parameters nil nil) (error e))
  (let ((p (make-pipe-process :name "gnutls-probe" :buffer nil)))
    (condition-case e (gnutls-asynchronous-parameters p '(x)) (error e))))
 (gnutls-available-p (gnutls-available-p) (memq 'gnutls3 (gnutls-available-p)))
 (gnutls-boot
  (condition-case e (gnutls-boot nil 'gnutls-anon nil) (error e))
  (let ((p (make-pipe-process :name "gnutls-probe" :buffer nil)))
    (condition-case e (gnutls-boot p 'gnutls-anon '(:hostname "example.org")) (error e))))
 (gnutls-bye
  (condition-case e (gnutls-bye nil nil) (error e))
  (condition-case e (gnutls-bye "not a process" t) (error e)))
 (gnutls-ciphers (length (gnutls-ciphers)) (car (gnutls-ciphers)))
 (gnutls-deinit
  (condition-case e (gnutls-deinit nil) (error e))
  (let ((p (make-pipe-process :name "gnutls-probe" :buffer nil))) (gnutls-deinit p)))
 (gnutls-digests (length (gnutls-digests)) (car (gnutls-digests)))
 (gnutls-error-fatalp (gnutls-error-fatalp -1)
  (condition-case e (gnutls-error-fatalp nil) (error e)))
 (gnutls-errorp (gnutls-errorp -1) (gnutls-errorp 0))
 (gnutls-error-string (gnutls-error-string -1) (gnutls-error-string 0))
 (gnutls-format-certificate
  (condition-case e (gnutls-format-certificate nil) (error e))
  (condition-case e (gnutls-format-certificate "invalid PEM") (error e)))
 (gnutls-get-initstage
  (condition-case e (gnutls-get-initstage nil) (error e))
  (let ((p (make-pipe-process :name "gnutls-probe" :buffer nil))) (gnutls-get-initstage p)))
