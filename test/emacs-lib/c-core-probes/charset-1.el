(char-charset
 (char-charset ?A)
 (char-charset ?あ '(ascii))
 (condition-case e (char-charset -1) (error (list (car e) (cdr e)))))
(charset-after
 (with-temp-buffer (insert "Aあ") (charset-after 1))
 (with-temp-buffer (insert "Aあ") (charset-after 2)))
(charset-id-internal
 (charset-id-internal 'ascii)
 (charset-id-internal 'unicode)
 (condition-case e (charset-id-internal 'charset-1-missing) (error (list (car e) (cdr e)))))
(charset-plist
 (plist-get (charset-plist 'ascii) :name)
 (condition-case e (charset-plist 'charset-1-missing) (error (list (car e) (cdr e)))))
(charset-priority-list
 (car (charset-priority-list))
 (charset-priority-list t))
(clear-charset-maps
 (progn (clear-charset-maps) t)
 (with-temp-buffer (insert "abcあ") (equal (find-charset-region (point-min) (point-max)) '(ascii unicode))))
(declare-equiv-charset
 (condition-case e (declare-equiv-charset 1 1 ?A 'ascii) (error (list (car e) (cdr e))))
 (condition-case e (declare-equiv-charset 1 95 ?A 'ascii) (error (list (car e) (cdr e))))
 (condition-case e (declare-equiv-charset 1 94 ?A 'ascii) (error (list (car e) (cdr e)))))
(define-charset-alias
 (progn (define-charset-alias 'charset-1-alias 'ascii) (eq (char-charset ?A) 'ascii))
 (condition-case e (define-charset-alias 'charset-1-bad 'charset-1-missing) (error (list (car e) (cdr e)))))
(define-charset-internal
 (condition-case e (apply #'define-charset-internal (make-list 17 0)) (error (list (car e) (cdr e))))
 (condition-case e (apply #'define-charset-internal (cons 'charset-1-invalid (make-list 16 0))) (error (list (car e) (cdr e)))))
(encode-char
 (encode-char ?A 'ascii)
 (encode-char ?あ 'ascii)
 (condition-case e (encode-char -1 'ascii) (error (list (car e) (cdr e)))))
(find-charset-region
 (with-temp-buffer (insert "abc") (find-charset-region 1 4))
 (with-temp-buffer (insert "Aあ") (find-charset-region 1 3))
 (condition-case e (find-charset-region nil 1) (error (list (car e) (cdr e)))))
(find-charset-string
 (find-charset-string "ASCII")
 (find-charset-string "Aあ")
 (condition-case e (find-charset-string nil) (error (list (car e) (cdr e)))))
