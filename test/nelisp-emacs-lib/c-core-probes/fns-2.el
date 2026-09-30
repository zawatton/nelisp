(internal--hash-table-index-size
 (internal--hash-table-index-size (make-hash-table :size 7))
 (condition-case e (internal--hash-table-index-size nil) (error e)))
(load-average
 (let ((x (load-average))) (list (length x) (and (numberp (car x)) t)))
 (let ((x (load-average t))) (list (length x) (and (numberp (car x)) t))))
(locale-info
 (and (stringp (locale-info 'codeset)) t)
 (and (vectorp (locale-info 'days)) (= (length (locale-info 'days)) 7))
 (and (vectorp (locale-info 'months)) (= (length (locale-info 'months)) 12))
 (locale-info 'not-a-locale-item)
 (locale-info nil))
(secure-hash-algorithms
 (and (memq 'sha256 (secure-hash-algorithms)) t)
 (and (listp (secure-hash-algorithms)) (> (length (secure-hash-algorithms)) 0)))
(sxhash-equal-including-properties
 (let ((a (propertize "x" 'face 'bold)) (b (propertize "x" 'face 'italic)))
   (list (integerp (sxhash-equal-including-properties a))
         (/= (sxhash-equal-including-properties a) (sxhash-equal-including-properties b))))
 (integerp (sxhash-equal-including-properties '(a 1 "b"))))
