;;; fns-1.el --- fns.c primitive probes -*- lexical-binding: t; -*-

(base64-decode-region
 (with-temp-buffer (insert "SGk=") (list (base64-decode-region 1 5) (buffer-string)))
 (condition-case e (base64-decode-region nil 1) (error e)))
(base64-encode-region
 (with-temp-buffer (insert "Hi") (list (base64-encode-region 1 3 t) (buffer-string)))
 (with-temp-buffer (insert "a") (base64-encode-region 1 2 t)))
(base64url-encode-region
 (with-temp-buffer (insert "foo?") (list (base64url-encode-region 1 5 t) (buffer-string)))
 (condition-case e (base64url-encode-region nil 2) (error e)))
(base64url-encode-string
 (base64url-encode-string "foo?" t)
 (condition-case e (base64url-encode-string nil) (error e)))
(buffer-line-statistics
 (with-temp-buffer (insert "a\nλx") (buffer-line-statistics))
 (condition-case e (buffer-line-statistics 42) (error e)))
(define-hash-table-test
 (let ((n 'fns-1-eq-test)) (define-hash-table-test n #'eq #'sxhash) (list (symbolp n) (consp (get n 'hash-table-test))))
 (condition-case e (define-hash-table-test 1 #'eq #'sxhash) (error e)))
(hash-table-rehash-size
 (hash-table-rehash-size (make-hash-table))
 (condition-case e (hash-table-rehash-size nil) (error e)))
(hash-table-rehash-threshold
 (hash-table-rehash-threshold (make-hash-table))
 (condition-case e (hash-table-rehash-threshold nil) (error e)))
(hash-table-size
 (let ((h (make-hash-table))) (puthash 'a 1 h) (hash-table-size h))
 (condition-case e (hash-table-size nil) (error e)))
(hash-table-weakness
 (hash-table-weakness (make-hash-table :weakness 'key))
 (condition-case e (hash-table-weakness nil) (error e)))
(internal--hash-table-buckets
 (let ((h (make-hash-table))) (puthash 'a 1 h) (list (listp (internal--hash-table-buckets h)) (hash-table-count h)))
 (condition-case e (internal--hash-table-buckets nil) (error e)))
(internal--hash-table-histogram
 (let ((h (make-hash-table))) (puthash 'a 1 h) (list (listp (internal--hash-table-histogram h)) (hash-table-count h)))
 (condition-case e (internal--hash-table-histogram nil) (error e)))
