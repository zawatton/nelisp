;;; census-other-06.el --- canonical probes  -*- lexical-binding: t; -*-
(sqrt
 (sqrt 9)
 (sqrt 0)
 (condition-case e (sqrt 'bad) (error e)))
(subr-arity
 (subr-arity (symbol-function 'car))
 (subr-arity (symbol-function 'vector))
 (condition-case e (subr-arity 7) (error e)))
(subrp
 (subrp (symbol-function 'car))
 (subrp '(lambda (x) x))
 (subrp nil))
(sxhash-eq
 (let ((x (list 'a))) (= (sxhash-eq x) (sxhash-eq x)))
 (integerp (sxhash-eq nil))
 (= (sxhash-eq 42) (sxhash-eq (+ 40 2))))
(sxhash-eql
 (= (sxhash-eql 1.5) (sxhash-eql (+ 1.0 0.5)))
 (integerp (sxhash-eql nil))
 (condition-case e (sxhash-eql) (error (car e))))
(sxhash-equal
 (= (sxhash-equal (list 1 "ab")) (sxhash-equal (list 1 (concat "a" "b"))))
 (integerp (sxhash-equal []))
 (condition-case e (sxhash-equal) (error (car e))))
(symbol-name
 (symbol-name 'probe-name)
 (symbol-name (make-symbol ""))
 (condition-case e (symbol-name 7) (error e)))
(symbol-plist
 (let ((s (make-symbol "probe"))) (setplist s '(a 1 b 2)) (symbol-plist s))
 (symbol-plist (make-symbol "empty"))
 (condition-case e (symbol-plist 7) (error e)))
(symbol-value
 (let ((s (make-symbol "probe"))) (set s '(1 2)) (symbol-value s))
 (symbol-value nil)
 (condition-case e (symbol-value (make-symbol "unbound-probe")) (error e)))
(symbol-with-pos-p
 (symbol-with-pos-p (read-positioning-symbols "probe"))
 (symbol-with-pos-p 'probe)
 (symbol-with-pos-p 7))
(symbol-with-pos-pos
 (symbol-with-pos-pos (read-positioning-symbols "probe"))
 (symbol-with-pos-pos (car (read-positioning-symbols "(  probe)")))
 (condition-case e (symbol-with-pos-pos 'probe) (error e)))
(system-name
 (stringp (system-name))
 (condition-case e (system-name 1) (error (car e))))
(take
 (take 2 '(a b c))
 (list (take 0 '(a b)) (take 8 '(a b)))
 (condition-case e (take 'bad '(a b)) (error e)))
(terpri
 (with-temp-buffer (let ((r (terpri (current-buffer)))) (list r (string-to-list (buffer-string)))))
 (with-temp-buffer (list (terpri (current-buffer) t) (string-to-list (buffer-string))))
 (with-temp-buffer (insert "x") (let ((r (terpri (current-buffer) t))) (list r (string-to-list (buffer-string))))))
(time-add
 (time-convert (time-add '(2 . 3) '(1 . 3)) 'integer)
 (time-convert (time-add '(1 . 2) '(1 . 4)) 4)
 (condition-case e (time-add 'bad 0) (error e)))
(time-convert
 (time-convert '(3 . 2) 'integer)
 (time-convert '(-3 . 2) 4)
 (condition-case e (time-convert 1 0) (error e)))
(time-equal-p
 (time-equal-p '(2 . 2) '(3 . 3))
 (time-equal-p '(1 . 2) '(2 . 3))
 (condition-case e (time-equal-p 'bad 0) (error e)))
(time-less-p
 (time-less-p '(1 . 2) '(2 . 3))
 (time-less-p '(2 . 2) '(3 . 3))
 (condition-case e (time-less-p 'bad 0) (error e)))
(time-subtract
 (time-convert (time-subtract '(3 . 2) '(1 . 2)) 'integer)
 (time-convert (time-subtract '(1 . 4) '(1 . 2)) 4)
 (condition-case e (time-subtract 'bad 0) (error e)))
(type-of
 (mapcar (lambda (x) (type-of x)) '(nil 1 1.5 "ab" (a) [1 2]))
 (type-of (make-symbol "probe"))
 (condition-case e (type-of) (error (car e))))
(unintern
 (let* ((ob (make-vector 7 0)) (s (intern "probe" ob)))
   (list (unintern s ob) (intern-soft "probe" ob)))
 (let ((ob (make-vector 7 0))) (unintern "absent" ob))
 (condition-case e (unintern 7 (make-vector 7 0)) (error e)))
(user-full-name
 (stringp (user-full-name))
 (let ((user-full-name "Probe User")) (user-full-name))
 (condition-case e (stringp (user-full-name 'bad)) (error e)))
(user-login-name
 (stringp (user-login-name))
 (equal (user-login-name) (user-login-name))
 (condition-case e (stringp (user-login-name 'bad)) (error e)))
(user-real-login-name
 (stringp (user-real-login-name))
 (equal (user-real-login-name) (user-real-login-name))
 (condition-case e (user-real-login-name 1) (error (car e))))
(user-real-uid
 (integerp (user-real-uid))
 (>= (user-real-uid) 0)
 (condition-case e (user-real-uid 1) (error (car e))))
(user-uid
 (integerp (user-uid))
 (>= (user-uid) 0)
 (condition-case e (user-uid 1) (error (car e))))
(value<
 (value< 2 3)
 (list (value< [1 2] [1 3]) (value< '(1 2) '(1 2)))
 (condition-case e (value< 1 "a") (error e)))
(vector
 (vector 1 'a "b")
 (vector))
(write-char
 (with-temp-buffer (list (write-char 65 (current-buffer)) (buffer-string)))
 (with-temp-buffer (list (write-char 955 (current-buffer)) (buffer-string)))
 (condition-case e (write-char 'bad (lambda (c) c)) (error e)))
