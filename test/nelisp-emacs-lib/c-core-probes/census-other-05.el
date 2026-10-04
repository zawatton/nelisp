;;; census-other-05.el --- canonical probes  -*- lexical-binding: t; -*-
(princ
 (with-temp-buffer (let ((value (princ "hello" (current-buffer)))) (list value (buffer-string))))
 (with-temp-buffer (princ '(a 2) (current-buffer)) (buffer-string))
 (condition-case e (princ "x" nil nil) (error (car e))))
(print
 (with-temp-buffer (print '(a "b") (current-buffer)) (string-to-list (buffer-string)))
 (with-temp-buffer (print nil (current-buffer)) (string-to-list (buffer-string)))
 (condition-case e (print) (error (car e))))
(proper-list-p
 (proper-list-p '(a b c))
 (list (proper-list-p nil) (proper-list-p '(a . b)))
 (proper-list-p [a b]))
(put
 (let ((s (make-symbol "probe"))) (list (put s 'key 7) (get s 'key)))
 (let ((s (make-symbol "probe"))) (put s 'key 7) (put s 'key nil) (symbol-plist s))
 (condition-case e (put 1 'key 7) (error e)))
(puthash
 (let ((h (make-hash-table :test 'equal))) (list (puthash "a" 7 h) (gethash "a" h)))
 (let ((h (make-hash-table))) (puthash 'a 1 h) (puthash 'a 2 h) (list (gethash 'a h) (hash-table-count h)))
 (condition-case e (puthash 'a 1 nil) (error e)))
(random
 (condition-case e (random -1) (error e))
 (condition-case e (random 1 2) (error (car e))))
(rassoc
 (rassoc 2 '((a . 1) (b . 2) (c . 2)))
 (rassoc '(x) '((a x) (b y)))
 (rassoc 'absent '((a . 1))))
(rassq
 (rassq 'x '((a . y) (b . x)))
 (let ((x (list 'v))) (rassq x (list (cons 'a x))))
 (rassq 'absent nil))
(read
 (read "(a 1 [2])")
 (with-temp-buffer (insert "42 7") (goto-char (point-min)) (list (read (current-buffer)) (point)))
 (condition-case e (read "(") (error e)))
(read-from-string
 (read-from-string "(a 2) tail")
 (read-from-string "xx 42 zz" 3 5)
 (condition-case e (read-from-string "") (error e)))
(read-positioning-symbols
 (let ((symbols-with-pos-enabled t)) (let ((s (read-positioning-symbols "alpha"))) (list (symbol-with-pos-p s) (remove-pos-from-symbol s))))
 (read-positioning-symbols "[1 2]")
 (condition-case e (read-positioning-symbols "(") (error e)))
(record
 (let ((r (record 'probe 7 "x"))) (list (recordp r) (length r) (aref r 0) (aref r 1) (aref r 2)))
 (let ((r (record 'empty))) (list (recordp r) (length r) (aref r 0)))
 (let ((r (record 'probe nil [1 2]))) (list (aref r 1) (aref r 2))))
(remhash
 (let ((h (make-hash-table))) (puthash 'a 1 h) (list (remhash 'a h) (gethash 'a h 'missing) (hash-table-count h)))
 (let ((h (make-hash-table))) (list (remhash 'absent h) (hash-table-count h)))
 (condition-case e (remhash 'a nil) (error e)))
(remove-pos-from-symbol
 (let ((symbols-with-pos-enabled t)) (remove-pos-from-symbol (read-positioning-symbols "alpha")))
 (list (remove-pos-from-symbol 'alpha) (remove-pos-from-symbol 42))
 (condition-case e (remove-pos-from-symbol) (error (car e))))
(remove-variable-watcher
 (let ((s (make-symbol "watched")) (calls nil) (watcher nil))
   (setq watcher (lambda (_symbol value operation _where) (setq calls (cons (list value operation) calls))))
   (unwind-protect (progn (add-variable-watcher s watcher) (set s 1) (remove-variable-watcher s watcher) (set s 2) (reverse calls))
     (remove-variable-watcher s watcher)))
 (let ((s (make-symbol "watched"))) (remove-variable-watcher s #'ignore))
 (condition-case e (remove-variable-watcher) (error (car e))))
(reverse
 (reverse '(1 2 3))
 (list (reverse [1 2]) (reverse "abc") (reverse nil))
 (condition-case e (reverse 7) (error e)))
(run-hook-with-args
 (let ((s (make-symbol "hook")) (seen nil)) (set s (list (lambda (x y) (setq seen (list x y))))) (list (run-hook-with-args s 2 3) seen))
 (let ((s (make-symbol "hook"))) (set s nil) (run-hook-with-args s 1)))
(run-hook-with-args-until-failure
 (let ((s (make-symbol "hook")) (seen nil)) (set s (list (lambda (x) (setq seen (cons x seen)) t) (lambda (_x) nil) (lambda (_x) (setq seen 'bad)))) (list (run-hook-with-args-until-failure s 7) seen))
 (let ((s (make-symbol "hook"))) (set s nil) (run-hook-with-args-until-failure s)))
(run-hook-with-args-until-success
 (let ((s (make-symbol "hook")) (seen nil)) (set s (list (lambda (x) (setq seen (cons x seen)) nil) (lambda (_x) 'found) (lambda (_x) 'bad))) (list (run-hook-with-args-until-success s 7) seen))
 (let ((s (make-symbol "hook"))) (set s nil) (run-hook-with-args-until-success s)))
(run-hook-wrapped
 (let ((s (make-symbol "hook"))) (set s (list (lambda (x) (+ x 1)) (lambda (_x) 'bad))) (run-hook-wrapped s (lambda (fun x) (funcall fun x)) 7))
 (let ((s (make-symbol "hook"))) (set s nil) (run-hook-wrapped s (lambda (_fun) 'bad))))
(run-hooks
 (let ((a (make-symbol "hook-a")) (b (make-symbol "hook-b")) (seen nil)) (set a (list (lambda () (setq seen (cons 'a seen))))) (set b (list (lambda () (setq seen (cons 'b seen))))) (list (run-hooks a b) (reverse seen)))
 (run-hooks))
(safe-length
 (safe-length '(a b c))
 (list (safe-length '(a b . c)) (safe-length 42) (safe-length nil))
 (safe-length [1 2 3]))
(secure-hash
 (secure-hash 'sha256 "abc")
 (secure-hash 'sha1 "zabcx" 1 4)
 (condition-case e (secure-hash 'bogus "abc") (error e)))
(set
 (let ((s (make-symbol "value"))) (list (set s 7) (symbol-value s)))
 (let ((s (make-symbol "value"))) (set s nil) (list (boundp s) (symbol-value s)))
 (condition-case e (set 1 2) (error e)))
(set-default
 (let ((s (make-symbol "default"))) (list (set-default s 7) (default-value s)))
 (let ((s (make-symbol "default"))) (set-default s nil) (list (default-boundp s) (default-value s)))
 (condition-case e (set-default 1 2) (error e)))
(set-default-toplevel-value
 (let ((s (make-symbol "toplevel"))) (list (set-default-toplevel-value s 7) (default-value s)))
 (let ((s (make-symbol "toplevel"))) (set-default-toplevel-value s nil) (list (default-boundp s) (default-value s)))
 (condition-case e (set-default-toplevel-value 1 2) (error e)))
(set-time-zone-rule
 (let ((old (getenv "TZ"))) (unwind-protect (set-time-zone-rule t) (set-time-zone-rule old)))
 (let ((old (getenv "TZ"))) (unwind-protect (progn (set-time-zone-rule "UTC0") (let ((decoded (decode-time 0))) (list (nth 2 decoded) (nth 3 decoded) (nth 4 decoded)))) (set-time-zone-rule old)))
 (condition-case e (set-time-zone-rule []) (error e)))
(setcar
 (let ((x (list 1 2))) (list (setcar x 7) x))
 (let ((x (cons nil 'tail))) (setcar x nil) x)
 (condition-case e (setcar 1 2) (error e)))
(setcdr
 (let ((x (list 1 2))) (list (setcdr x '(7 8)) x))
 (let ((x (list 1 2))) (setcdr x nil) x)
 (condition-case e (setcdr 1 2) (error e)))
(setplist
 (let ((s (make-symbol "plist"))) (list (setplist s '(a 1 b 2)) (symbol-plist s)))
 (let ((s (make-symbol "plist"))) (setplist s '(a 1)) (list (setplist s nil) (symbol-plist s)))
 (condition-case e (setplist 1 nil) (error e)))
(signal
 (condition-case e (signal 'error '("probe" 7)) (error e))
 (condition-case e (signal 'wrong-type-argument '(integerp "x")) (error e)))
(sin
 (sin 0)
 (sin -0.0)
 (condition-case e (sin "x") (error e)))
(sort
 (sort (list 3 1 2 1) #'<)
 (sort (vector 3 1 2) #'>)
 (sort nil #'<))
(special-variable-p
 (special-variable-p 'case-fold-search)
 (special-variable-p (make-symbol "fresh"))
 (condition-case e (special-variable-p 1) (error e)))
