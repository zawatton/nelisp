(clear-composition-cache
 (clear-composition-cache)
 (progn (with-temp-buffer (insert "abc") (put-text-property 1 2 'composition '((2))) (clear-composition-cache) (get-text-property 1 'composition))))
(compose-region-internal
 (with-temp-buffer (insert "abcd") (compose-region-internal 2 4) (list (buffer-substring-no-properties 1 5) (get-text-property 2 'composition)))
 (with-temp-buffer (insert "a") (condition-case e (compose-region-internal 0 2) (error (list (car e) (mapcar (lambda (x) (if (bufferp x) 'buffer x)) (cdr e)))))))
(compose-string-internal
 (let ((s (copy-sequence "abcd"))) (compose-string-internal s 1 3) (list (substring-no-properties s) (get-text-property 1 'composition s)))
 (condition-case e (compose-string-internal nil 0 1) (error (list (car e) (cdr e)))))
(composition-get-gstring
 (let ((g (composition-get-gstring 0 2 nil "abc"))) (and (vectorp g) (= (length g) 10) (equal (aref (aref g 0) 1) 97)))
 (condition-case e (composition-get-gstring 1 1 nil "abc") (error (list (car e) (car (cdr e))))))
(composition-sort-rules
 (mapcar (lambda (r) (aref r 1)) (composition-sort-rules (list ["x" 1 compose-gstring-for-graphic] [nil 3 compose-gstring-for-graphic] ["y" 2 compose-gstring-for-graphic])))
 (condition-case e (composition-sort-rules "x") (error (list (car e) (car (cdr e))))))
(find-composition-internal
 (with-temp-buffer (insert "abc") (put-text-property 1 3 'composition '((3))) (equal (find-composition-internal 1 3 nil t) '((3))))
 (with-temp-buffer (insert "abc") (condition-case e (find-composition-internal 9 0 nil nil) (error (car e))))
 (let ((s (copy-sequence "abc"))) (compose-string-internal s 0 2) (equal (find-composition-internal 0 2 s t) '((2)))))
