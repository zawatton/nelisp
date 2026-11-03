;;; census-other-02.el --- canonical probes  -*- lexical-binding: t; -*-
(current-time-zone
 (current-time-zone 0 t)
 (current-time-zone 86400 3600)
 (condition-case e (current-time-zone 0 'invalid-zone) (error e)))
(daemonp
 (daemonp)
 (condition-case e (daemonp 1) (error (car e))))
(debugger-trap
 (debugger-trap)
 (condition-case e (debugger-trap 1) (error (car e))))
(decode-time
 (decode-time 0 t)
 (decode-time 86400 3600)
 (condition-case e (decode-time 0 'invalid-zone) (error e)))
(defalias
 (let ((s (make-symbol "probe-alias"))) (defalias s 'car) (funcall s '(7 8)))
 (let ((s (make-symbol "probe-alias"))) (defalias s 'car) (defalias s 'cdr) (funcall s '(7 8)))
 (condition-case e (defalias 42 'car) (error e)))
(default-boundp
 (default-boundp (make-symbol "probe-unbound"))
 (let ((s (make-symbol "probe-bound"))) (set s 17) (default-boundp s))
 (condition-case e (default-boundp 17) (error e)))
(default-toplevel-value
 (let ((s (make-symbol "probe-top"))) (set s 17) (default-toplevel-value s))
 (let ((s (make-symbol "probe-top"))) (set s nil) (default-toplevel-value s))
 (condition-case e (default-toplevel-value (make-symbol "probe-unbound")) (error e)))
(defconst-1
 (let ((s (make-symbol "probe-constant"))) (defconst-1 s 17 "Constant doc.") (list (symbol-value s) (get s 'variable-documentation)))
 (let ((s (make-symbol "probe-constant"))) (set s 3) (defconst-1 s 9) (symbol-value s))
 (condition-case e (defconst-1 17 9) (error e)))
(defvar-1
 (let ((s (make-symbol "probe-variable"))) (defvar-1 s 17 "Variable doc.") (list (symbol-value s) (get s 'variable-documentation)))
 (let ((s (make-symbol "probe-variable"))) (set s 3) (defvar-1 s 9) (symbol-value s))
 (condition-case e (defvar-1 17 9) (error e)))
(delete
 (delete 2 (list 1 2 3 2))
 (delete "a" (vector "a" "b" "a"))
 (condition-case e (delete 1 42) (error e)))
(delq
 (delq 2 (list 1 2 3 2))
 (let ((a (list 1)) (b (list 1))) (length (delq a (list a b a))))
 (condition-case e (delq 1 42) (error e)))
(describe-vector
 (with-temp-buffer (let ((standard-output (current-buffer))) (describe-vector [a b])) (buffer-string))
 (with-temp-buffer (let ((standard-output (current-buffer))) (describe-vector [])) (buffer-string))
 (condition-case e (describe-vector 42) (error e)))
(documentation
 (let ((s (make-symbol "probe-doc"))) (fset s '(lambda () "Function doc." 17)) (documentation s t))
 (let ((s (make-symbol "probe-doc"))) (fset s '(lambda () 17)) (documentation s t))
 (condition-case e (documentation 42) (error e)))
(documentation-property
 (let ((s (make-symbol "probe-doc"))) (put s 'probe-doc "Property doc.") (documentation-property s 'probe-doc t))
 (documentation-property (make-symbol "probe-doc") 'probe-doc t)
 (condition-case e (documentation-property 42 'probe-doc) (error e)))
(documentation-stringp
 (documentation-stringp "Doc.")
 (documentation-stringp 42)
 (documentation-stringp '("Doc.")))
(elt
 (elt '(a b c) 1)
 (elt [a b] 0)
 (condition-case e (elt [a] 2) (error e)))
(encode-time
 (encode-time 0 0 0 1 1 1970 t)
 (encode-time '(0 0 1 1 1 1970 nil nil 3600))
 (condition-case e (encode-time 0 0 0 1 1 1970 'invalid-zone) (error e)))
(eql
 (eql 7 7)
 (eql 7 7.0)
 (eql 1.5 1.5))
(equal-including-properties
 (equal-including-properties "abc" "abc")
 (equal-including-properties (propertize "abc" 'probe 1) "abc")
 (equal-including-properties (propertize "abc" 'probe 1) (propertize "abc" 'probe 1)))
(error-message-string
 (error-message-string '(error "Probe failure"))
 (error-message-string '(void-variable probe-missing))
 (error-message-string '(wrong-type-argument integerp "x")))
(eval
 (eval '(+ 2 3) t)
 (eval '(+ probe-x 4) '((probe-x . 7)))
 (condition-case e (eval '(car 42) t) (error e)))
(exp
 (exp 0)
 (exp -1.0e+300)
 (condition-case e (exp 'bad) (error e)))
(expt
 (expt 2 10)
 (expt 2 -3)
 (condition-case e (expt 2 'bad) (error e)))
(fboundp
 (fboundp 'car)
 (fboundp (make-symbol "probe-unbound"))
 (condition-case e (fboundp 42) (error e)))
(fceiling
 (fceiling 1.2)
 (fceiling -1.2)
 (condition-case e (fceiling 'bad) (error e)))
(ffloor
 (ffloor 1.8)
 (ffloor -1.2)
 (condition-case e (ffloor 'bad) (error e)))
(fillarray
 (fillarray (vector 1 2 3) 7)
 (fillarray (copy-sequence "abc") 120)
 (condition-case e (fillarray 42 7) (error e)))
(float
 (float 7)
 (float -1.25)
 (condition-case e (float 'bad) (error e)))
(float-time
 (float-time 0)
 (float-time '(0 1 500000 0))
 (condition-case e (float-time 'bad) (error e)))
(fmakunbound
 (let ((s (make-symbol "probe-function"))) (fset s 'car) (fmakunbound s) (fboundp s))
 (let ((s (make-symbol "probe-function"))) (eq s (fmakunbound s)))
 (condition-case e (fmakunbound 42) (error e)))
(format-time-string
 (format-time-string "%Y-%m-%d %H:%M:%S" 0 t)
 (format-time-string "%Y-%m-%d %H:%M:%S" 0 3600)
 (condition-case e (format-time-string 42 0 t) (error e)))
(fround
 (fround 2.5)
 (fround -3.5)
 (condition-case e (fround 'bad) (error e)))
(fset
 (let ((s (make-symbol "probe-function"))) (fset s 'car) (funcall s '(7 8)))
 (let ((s (make-symbol "probe-function"))) (fset s '(lambda (x) (+ x 2))) (funcall s 3))
 (condition-case e (fset 42 'car) (error e)))
(ftruncate
 (ftruncate 1.8)
 (ftruncate -1.8)
 (condition-case e (ftruncate 'bad) (error e)))
