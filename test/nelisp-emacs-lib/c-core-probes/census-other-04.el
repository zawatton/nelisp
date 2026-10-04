;;; census-other-04.el --- canonical probes  -*- lexical-binding: t; -*-
(make-byte-code
 (funcall (make-byte-code 0 "\300\207" [42] 1))
 (length (make-byte-code '(x) "\300\207" [nil] 1 "probe"))
 (condition-case e (make-byte-code) (error (car e))))
(make-closure
 (funcall (make-closure (make-byte-code 0 "\300\207" [nil] 1) 23))
 (let* ((p (make-byte-code 0 "\300\207" [nil 9] 1)) (c (make-closure p 8))) (list (aref c 2) (aref p 2) (eq c p)))
 (condition-case e (make-closure 3) (error e)))
(make-hash-table
 (let ((h (make-hash-table :test 'equal))) (puthash "key" 7 h) (list (gethash "key" h) (hash-table-count h) (hash-table-test h)))
 (let ((h (make-hash-table :size 0))) (list (hash-table-count h) (gethash 'missing h 'fallback)))
 (condition-case e (make-hash-table :test 'not-a-test) (error e)))
(make-interpreted-closure
 (funcall (make-interpreted-closure '(x) '((+ x y)) '((y . 4))) 3)
 (funcall (make-interpreted-closure nil '(17) t "probe"))
 (condition-case e (make-interpreted-closure) (error (car e))))
(make-list
 (make-list 3 'x)
 (make-list 0 'x)
 (condition-case e (make-list "bad" nil) (error e)))
(make-marker
 (let ((m (make-marker))) (list (markerp m) (marker-position m) (marker-buffer m)))
 (with-temp-buffer (insert "abc") (let ((m (make-marker))) (unwind-protect (progn (set-marker m 2) (list (marker-position m) (eq (marker-buffer m) (current-buffer)))) (set-marker m nil))))
 (condition-case e (make-marker 1) (error (car e))))
(make-record
 (let ((r (make-record 'probe 2 7))) (list (type-of r) (length r) (aref r 1) (aref r 2)))
 (let ((r (make-record 'empty 0 nil))) (list (type-of r) (length r)))
 (condition-case e (make-record 'probe "bad" nil) (error e)))
(make-string
 (make-string 3 ?a)
 (make-string 0 ?a)
 (condition-case e (make-string "bad" ?a) (error e)))
(make-symbol
 (let ((s (make-symbol "probe-local"))) (list (symbol-name s) (boundp s) (fboundp s)))
 (eq (make-symbol "probe-local") (make-symbol "probe-local"))
 (condition-case e (make-symbol 3) (error e)))
(make-vector
 (make-vector 3 'x)
 (make-vector 0 nil)
 (condition-case e (make-vector "bad" nil) (error e)))
(makunbound
 (let ((s (make-symbol "probe-local"))) (set s 7) (list (eq (makunbound s) s) (boundp s)))
 (let ((s (make-symbol "probe-local"))) (list (eq (makunbound s) s) (boundp s)))
 (condition-case e (makunbound 3) (error e)))
(mapatoms
 (let ((o (obarray-make)) (names nil)) (intern "beta" o) (intern "alpha" o) (list (mapatoms (lambda (s) (push (symbol-name s) names)) o) (sort names #'string<)))
 (let ((o (obarray-make)) (n 0)) (mapatoms (lambda (_s) (setq n (1+ n))) o) n)
 (condition-case e (mapatoms #'ignore 3) (error e)))
(mapbacktrace
 (null (mapbacktrace (lambda (_evaluated _function _args _flags) nil)))
 (let ((seen nil)) (mapbacktrace (lambda (_evaluated function _args _flags) (when (eq function 'mapbacktrace) (setq seen t)))) seen)
 (condition-case e (mapbacktrace) (error (car e))))
(mapc
 (let ((seen nil)) (list (mapc (lambda (x) (push (* x 2) seen)) '(1 2 3)) (nreverse seen)))
 (let ((seen nil)) (list (mapc (lambda (x) (push x seen)) [4 5]) (nreverse seen)))
 (condition-case e (mapc #'identity 3) (error e)))
(mapcan
 (mapcan (lambda (x) (list x (* x 2))) '(1 2))
 (mapcan (lambda (x) (and (> x 1) (list x))) [1 2 3])
 (condition-case e (mapcan #'identity 3) (error e)))
(mapconcat
 (mapconcat #'symbol-name '(alpha beta) ":")
 (mapconcat #'identity [] ":")
 (condition-case e (mapconcat #'identity 3 ":") (error e)))
(maphash
 (let ((h (make-hash-table)) (sum 0)) (puthash 'a 2 h) (puthash 'b 3 h) (list (maphash (lambda (_k v) (setq sum (+ sum v))) h) sum))
 (let ((h (make-hash-table)) (n 0)) (maphash (lambda (_k _v) (setq n (1+ n))) h) n)
 (condition-case e (maphash #'ignore 3) (error e)))
(md5
 (md5 "abc" nil nil 'utf-8)
 (with-temp-buffer (insert "xabcx") (md5 (current-buffer) 2 5 'utf-8))
 (condition-case e (md5 3) (error e)))
(member
 (member '(2) '((1) (2) (3)))
 (member 'absent '(a b))
 (condition-case e (member 'a 3) (error e)))
(memory-use-counts
 (let ((counts (memory-use-counts))) (list (length counts) (listp counts) (integerp (car counts))))
 (let* ((before (car (memory-use-counts))) (allocated (make-list 20 nil)) (after (car (memory-use-counts)))) (list (= (length allocated) 20) (>= after before)))
 (condition-case e (memory-use-counts 1) (error (car e))))
(memql
 (memql 2 '(1 2 3))
 (memql 2.0 '(1.0 2.0 3.0))
 (condition-case e (memql 'a 3) (error e)))
(nconc
 (nconc (list 1 2) (list 3 4))
 (nconc nil (list 7) nil)
 (condition-case e (nconc 3 (list 4)) (error e)))
(ngettext
 (ngettext "probe-one-item" "probe-many-items" 1)
 (ngettext "probe-one-item" "probe-many-items" 0)
 (condition-case e (ngettext "probe-one-item" "probe-many-items" "bad") (error e)))
(nlistp
 (nlistp 3)
 (list (nlistp nil) (nlistp '(a . b)))
 (condition-case e (nlistp) (error (car e))))
(nreverse
 (nreverse (list 1 2 3))
 (nreverse (vector 1 2 3))
 (condition-case e (nreverse 3) (error e)))
(ntake
 (ntake 2 (list 1 2 3 4))
 (list (ntake 0 (list 1 2)) (ntake 5 (list 1 2)))
 (condition-case e (ntake "bad" (list 1 2)) (error e)))
(nthcdr
 (nthcdr 2 '(a b c d))
 (nthcdr 5 '(a b))
 (condition-case e (nthcdr "bad" '(a)) (error e)))
(obarray-make
 (let ((o (obarray-make))) (list (obarrayp o) (symbol-name (intern "local" o))))
 (let ((o (obarray-make 0))) (list (obarrayp o) (intern-soft "missing" o)))
 (condition-case e (obarray-make "bad") (error e)))
(obarrayp
 (obarrayp (obarray-make))
 (list (obarrayp nil) (obarrayp [nil nil]))
 (condition-case e (obarrayp) (error (car e))))
(plist-member
 (plist-member '(a 1 b 2) 'b)
 (plist-member '(a nil) 'missing)
 (condition-case e (plist-member 3 'a) (error e)))
(plist-put
 (plist-put (list 'a 1 'b 2) 'b 7)
 (plist-put nil 'a 3)
 (condition-case e (plist-put 3 'a 1) (error e)))
(position-symbol
 (let ((s (position-symbol 'alpha 7))) (list (symbol-with-pos-p s) (bare-symbol s) (symbol-with-pos-pos s)))
 (symbol-with-pos-pos (position-symbol 'beta (position-symbol 'alpha 0)))
 (condition-case e (position-symbol 3 7) (error e)))
(prin1
 (with-temp-buffer (let ((result (prin1 '(a "b") (current-buffer)))) (list result (buffer-string))))
 (with-temp-buffer (prin1 [1 2 3] (current-buffer) '((length . 2))) (buffer-string))
 (condition-case e (prin1) (error (car e))))
(prin1-to-string
 (prin1-to-string '(a "b"))
 (prin1-to-string "a\"b" t)
 (condition-case e (prin1-to-string) (error (car e))))
