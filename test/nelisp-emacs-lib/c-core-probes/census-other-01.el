;;; census-other-01.el --- canonical probes  -*- lexical-binding: t; -*-
(add-variable-watcher
 (let* ((s (make-symbol "probe-watched")) (events nil)
        (watcher (lambda (_symbol value operation where)
                   (push (list value operation (null where)) events))))
   (set s 0)
   (unwind-protect
       (progn (add-variable-watcher s watcher) (set s 7) events)
     (remove-variable-watcher s watcher)))
 (let* ((s (make-symbol "probe-watched")) (watcher (lambda (&rest _args) nil)))
   (unwind-protect
       (progn (add-variable-watcher s watcher)
              (add-variable-watcher s watcher)
              (length (get-variable-watchers s)))
     (remove-variable-watcher s watcher)))
 (condition-case e (add-variable-watcher) (error (car e))))
(aref
 (aref [10 20 30] 1)
 (aref "abc" 2)
 (condition-case e (aref [] 0) (error e)))
(aset
 (let ((v (vector 1 2))) (list (aset v 0 9) v))
 (let ((s (copy-sequence "abc"))) (list (aset s 2 90) s))
 (condition-case e (aset [] 0 1) (error e)))
(assoc
 (assoc 'b '((a . 1) (b . 2)))
 (assoc "a" '(("a" . 3) (b . 4)))
 (assoc 'z '((a . 1))))
(atan
 (atan 0)
 (atan 0 1)
 (condition-case e (atan 'bad) (error e)))
(backtrace--locals
 (listp (backtrace--locals 1))
 (listp (backtrace--locals 2))
 (condition-case e (backtrace--locals -1) (error e)))
(backtrace-debug
 (backtrace-debug 0 nil)
 (backtrace-debug 99999 nil)
 (condition-case e (backtrace-debug) (error (car e))))
(backtrace-eval
 (backtrace-eval '(+ 2 3) 0)
 (backtrace-eval '(list 'a 'b) 0)
 (condition-case e (backtrace-eval 7 99999) (error e)))
(backtrace-frame--internal
 (backtrace-frame--internal (lambda (&rest _args) 'found) 0 nil)
 (backtrace-frame--internal (lambda (&rest _args) 'found) 99999 nil)
 (condition-case e (backtrace-frame--internal) (error (car e))))
(bare-symbol
 (bare-symbol 'alpha)
 (bare-symbol nil)
 (condition-case e (bare-symbol 7) (error e)))
(bool-vector
 (append (bool-vector t nil t) nil)
 (append (bool-vector) nil))
(bool-vector-count-consecutive
 (bool-vector-count-consecutive (bool-vector t t nil t) t 0)
 (bool-vector-count-consecutive (bool-vector t t nil t) nil 2)
 (condition-case e (bool-vector-count-consecutive (bool-vector t) t 2) (error e)))
(bool-vector-count-population
 (bool-vector-count-population (bool-vector t nil t t))
 (bool-vector-count-population (bool-vector))
 (condition-case e (bool-vector-count-population [t]) (error e)))
(bool-vector-exclusive-or
 (append (bool-vector-exclusive-or (bool-vector t nil t nil) (bool-vector t t nil nil)) nil)
 (let ((dest (bool-vector nil nil nil nil)))
   (let ((result (bool-vector-exclusive-or (bool-vector t nil t nil) (bool-vector nil t t nil) dest)))
     (list (eq result dest) (append dest nil))))
 (condition-case e (bool-vector-exclusive-or (bool-vector t) (bool-vector)) (error e)))
(bool-vector-intersection
 (append (bool-vector-intersection (bool-vector t nil t nil) (bool-vector t t nil nil)) nil)
 (let ((dest (bool-vector nil nil nil nil)))
   (let ((result (bool-vector-intersection (bool-vector t nil t nil) (bool-vector nil t t nil) dest)))
     (list (eq result dest) (append dest nil))))
 (condition-case e (bool-vector-intersection (bool-vector t) (bool-vector)) (error e)))
(bool-vector-set-difference
 (append (bool-vector-set-difference (bool-vector t nil t nil) (bool-vector t t nil nil)) nil)
 (let ((dest (bool-vector nil nil nil nil)))
   (let ((result (bool-vector-set-difference (bool-vector t nil t nil) (bool-vector nil t t nil) dest)))
     (list (eq result dest) (append dest nil))))
 (condition-case e (bool-vector-set-difference (bool-vector t) (bool-vector)) (error e)))
(bool-vector-union
 (append (bool-vector-union (bool-vector t nil t nil) (bool-vector t t nil nil)) nil)
 (let ((dest (bool-vector nil nil nil nil)))
   (let ((result (bool-vector-union (bool-vector t nil t nil) (bool-vector nil t t nil) dest)))
     (list (eq result dest) (append dest nil))))
 (condition-case e (bool-vector-union (bool-vector t) (bool-vector)) (error e)))
(bool-vector-not
 (append (bool-vector-not (bool-vector t nil t)) nil)
 (let ((dest (bool-vector nil nil)))
   (list (eq (bool-vector-not (bool-vector t nil) dest) dest) (append dest nil)))
 (condition-case e (bool-vector-not [t]) (error e)))
(bool-vector-subsetp
 (bool-vector-subsetp (bool-vector t nil) (bool-vector t t))
 (bool-vector-subsetp (bool-vector t t) (bool-vector t nil))
 (condition-case e (bool-vector-subsetp (bool-vector t) (bool-vector)) (error e)))
(boundp
 (let ((s (make-symbol "probe-bound"))) (set s 4) (boundp s))
 (boundp (make-symbol "probe-unbound"))
 (condition-case e (boundp 4) (error e)))
(byte-code
 (byte-code "\300\207" [42] 1)
 (byte-code "\300\207" [nil] 1)
 (byte-code "\300\207" [[1 2]] 1))
(byte-code-function-p
 (byte-code-function-p (make-byte-code 0 "\300\207" [42] 1))
 (byte-code-function-p (lambda () 42))
 (byte-code-function-p [0 1]))
(car-less-than-car
 (car-less-than-car '(1 a) '(2 b))
 (car-less-than-car '(2 a) '(2 b))
 (condition-case e (car-less-than-car '(x) '(2)) (error e)))
(car-safe
 (car-safe '(a . b))
 (car-safe 7)
 (car-safe nil))
(cdr-safe
 (cdr-safe '(a . b))
 (cdr-safe 7)
 (cdr-safe nil))
(cl-type-of
 (cl-type-of 7)
 (cl-type-of [a b])
 (cl-type-of nil))
(closurep
 (closurep (lambda () 42))
 (closurep 'car)
 (closurep [0 1]))
(clrhash
 (let ((h (make-hash-table))) (puthash 'a 1 h) (clrhash h) (hash-table-count h))
 (let ((h (make-hash-table))) (eq (clrhash h) h))
 (condition-case e (clrhash 7) (error e)))
(copy-alist
 (let* ((a '((x . 1) (y . 2))) (b (copy-alist a)))
   (list b (eq a b) (eq (car a) (car b))))
 (copy-alist nil)
 (condition-case e (copy-alist 7) (error e)))
(copy-hash-table
 (let ((h (make-hash-table :test 'equal)))
   (puthash "a" 1 h)
   (let ((copy (copy-hash-table h)))
     (list (eq copy h) (hash-table-test copy) (gethash "a" copy))))
 (let* ((h (make-hash-table)) (copy (copy-hash-table h)))
   (puthash 'x 9 copy) (list (hash-table-count h) (hash-table-count copy)))
 (condition-case e (copy-hash-table 7) (error e)))
(copy-sequence
 (let* ((a (list 1 2)) (b (copy-sequence a))) (list b (eq a b)))
 (copy-sequence [1 2])
 (condition-case e (copy-sequence 7) (error e))
 (let ((c (copy-sequence (standard-case-table))))
   (list (char-table-p c) (eq c (standard-case-table)) (aref c ?A) (char-table-subtype c))))
(cos
 (cos 0)
 (cos 0.0)
 (condition-case e (cos 'bad) (error e)))
(current-time
 (let ((current-time-list t)) (length (current-time)))
 (let ((current-time-list nil))
   (let ((value (current-time)))
     (list (consp value) (integerp (car value)) (integerp (cdr value)))))
 (condition-case e (current-time 1) (error (car e))))
(current-time-string
 (current-time-string '(0 0) t)
 (current-time-string '(0 1) t)
 (condition-case e (current-time-string 'bad t) (error e)))
