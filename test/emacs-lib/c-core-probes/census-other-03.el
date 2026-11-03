;;; census-other-03.el --- canonical probes  -*- lexical-binding: t; -*-
(func-arity
 (func-arity 'cons)
 (func-arity (lambda (a &optional b &rest c) a))
 (condition-case e (func-arity 7) (error e)))
(funcall-with-delayed-message
 (funcall-with-delayed-message 60 "probe" (lambda () 17))
 (funcall-with-delayed-message 60 "probe" (lambda () '(a b)))
 (condition-case e (funcall-with-delayed-message) (error (car e))))
(garbage-collect
 (listp (garbage-collect))
 (consp (assq 'conses (garbage-collect)))
 (let ((result (condition-case e (garbage-collect 1) (error (car e)))))
   (if (symbolp result) result (list 'unexpected-result (type-of result)))))
(get
 (let ((s (make-symbol "probe"))) (put s 'key 17) (get s 'key))
 (get (make-symbol "probe") 'absent)
 (condition-case e (get 7 'key) (error e)))
(get-variable-watchers
 (get-variable-watchers (make-symbol "probe"))
 (let* ((s (make-symbol "probe")) (watcher (lambda (&rest args) nil)))
   (unwind-protect
       (progn (add-variable-watcher s watcher)
              (length (get-variable-watchers s)))
     (remove-variable-watcher s watcher)))
 (condition-case e (get-variable-watchers) (error (car e))))
(gethash
 (let ((h (make-hash-table :test 'equal))) (puthash "key" 17 h) (gethash "key" h))
 (gethash 'absent (make-hash-table) 'fallback)
 (condition-case e (gethash 'key 7) (error e)))
(group-gid
 (integerp (group-gid))
 (condition-case e (group-gid 1) (error (car e))))
(handler-bind-1
 (handler-bind-1 (lambda () 17))
 (catch 'caught
   (handler-bind-1 (lambda () (signal 'error '("probe")))
                   '(error) (lambda (e) (throw 'caught e))))
 (condition-case e (handler-bind-1) (error (car e))))
(hash-table-count
 (hash-table-count (make-hash-table))
 (let ((h (make-hash-table))) (puthash 'a 1 h) (puthash 'a 2 h)
      (puthash 'b 3 h) (hash-table-count h))
 (condition-case e (hash-table-count 7) (error e)))
(hash-table-test
 (hash-table-test (make-hash-table))
 (hash-table-test (make-hash-table :test 'equal))
 (condition-case e (hash-table-test 7) (error e)))
(help--describe-vector
 (with-temp-buffer
   (help--describe-vector [identity] "" (lambda (x) (insert (symbol-name x))) nil nil nil nil)
   (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t))
 (with-temp-buffer
   (help--describe-vector [] "" (lambda (x) (insert (symbol-name x))) nil nil nil nil)
   (buffer-string))
 (condition-case e (help--describe-vector) (error (car e))))
(identity
 (identity '(a 17))
 (identity nil)
 (condition-case e (identity) (error (car e))))
(indirect-function
 (let ((s (make-symbol "probe"))) (fset s 'identity)
      (eq (indirect-function s) (symbol-function 'identity)))
 (condition-case e (indirect-function (make-symbol "probe") t)
   (wrong-number-of-arguments (car e)) (error e))
 (condition-case e (indirect-function) (error (car e))))
(intern
 (symbol-name (intern "probe" (obarray-make 3)))
 (let ((o (obarray-make 3))) (eq (intern "probe" o) (intern "probe" o)))
 (condition-case e (intern 7) (error e)))
(intern-soft
 (let ((o (obarray-make 3))) (intern "probe" o)
      (symbol-name (intern-soft "probe" o)))
 (intern-soft "absent" (obarray-make 3))
 (condition-case e (intern-soft 7) (error e)))
(internal--define-uninitialized-variable
 (let ((s (make-symbol "probe")))
   (internal--define-uninitialized-variable s "Probe doc")
   (list (special-variable-p s) (boundp s) (get s 'variable-documentation)))
 (let ((s (make-symbol "probe"))) (set s 17)
   (internal--define-uninitialized-variable s) (symbol-value s))
 (condition-case e (internal--define-uninitialized-variable) (error (car e))))
(internal--obarray-buckets
 (let ((o (obarray-make 3)))
   (length (apply #'append (internal--obarray-buckets o))))
 (let ((o (obarray-make 3))) (intern "probe" o)
   (mapcar #'symbol-name (apply #'append (internal--obarray-buckets o))))
 (condition-case e (internal--obarray-buckets 7) (error e)))
(internal-handle-focus-in
 (condition-case e (internal-handle-focus-in 1) (error e))
 (condition-case e (internal-handle-focus-in) (error (car e))))
(internal-make-var-non-special
 (let ((s (make-symbol "probe")))
   (internal--define-uninitialized-variable s)
   (internal-make-var-non-special s) (special-variable-p s))
 (let ((s (make-symbol "probe")))
   (internal-make-var-non-special s) (special-variable-p s))
 (condition-case e (internal-make-var-non-special 7) (error e)))
(interpreted-function-p
 (interpreted-function-p (lambda (x) x))
 (interpreted-function-p 'identity)
 (interpreted-function-p 7))
(invocation-directory
 (stringp (invocation-directory))
 (condition-case e (invocation-directory 7) (error (car e))))
(invocation-name
 (stringp (invocation-name))
 (condition-case e (invocation-name 7) (error (car e))))
(isnan
 (isnan 1.0)
 (isnan 0.0e+NaN)
 (condition-case e (isnan 'bad) (error e)))
(json-parse-buffer
 (with-temp-buffer (insert "{\"a\":[1,true]}") (goto-char (point-min))
   (json-parse-buffer :object-type 'alist :array-type 'list))
 (with-temp-buffer (insert "[null,false]") (goto-char (point-min))
   (json-parse-buffer :array-type 'list :null-object 'null :false-object 'false))
 (with-temp-buffer (condition-case e (json-parse-buffer 7) (error e))))
(json-parse-string
 (json-parse-string "{\"a\":[1,true]}" :object-type 'alist :array-type 'list)
 (json-parse-string "[null,false]" :array-type 'list :null-object 'null :false-object 'false)
 (condition-case e (json-parse-string 7) (error e)))
(json-serialize
 (json-serialize '((a . 1)))
 (json-serialize [1 "two" :null :false])
 (condition-case e (json-serialize 'bad) (error e)))
(kill-emacs
 (condition-case e (kill-emacs 0 nil nil) (error (car e)))
 (condition-case e (kill-emacs 0 nil nil nil) (error (car e))))
(length<
 (length< '(a b) 3)
 (length< [a b] 2)
 (condition-case e (length< 7 2) (error e)))
(length=
 (length= '(a b) 2)
 (length= "ab" 3)
 (condition-case e (length= 7 2) (error e)))
(length>
 (length> '(a b) 1)
 (length> [] 0)
 (condition-case e (length> 7 2) (error e)))
(log
 (log 1)
 (log 8 2)
 (condition-case e (log 'bad) (error e)))
(logb
 (logb 8.0)
 (logb 0.5)
 (condition-case e (logb 'bad) (error e)))
(macroexpand
 (macroexpand '(ccore-probe-macro 7)
              '((ccore-probe-macro . (lambda (x) (list 'list x)))))
 (macroexpand '(identity 7))
 (condition-case e (macroexpand) (error (car e))))
(make-bool-vector
 (append (make-bool-vector 3 t) nil)
 (append (make-bool-vector 0 nil) nil)
 (condition-case e (make-bool-vector -1 nil) (error e)))
