;;; nelisp-n2-reference.el --- GNU 31.1 N2 storage/arity reference -*- lexical-binding: t; -*-
;; GNU clear-string is the storage oracle: GNU aset rejects byte erasure of a multibyte string.
(defun n2-error (form name)
  (condition-case e (eval form t)
    (wrong-number-of-arguments
     (let ((fn (cadr e)))
       (list (car e)
	     (if (symbolp fn) fn
	       (if (eq fn (symbol-function name)) (list 'subr name)
		 fn))
	     (car (cddr e)))))
    (error e)))
(defun n2-row (name value)
  (princ "N2| ") (prin1 name) (princ " | ") (prin1 value) (terpri))
(condition-case n2-error
    (n2-row 'gc-arity (func-arity 'garbage-collect))
  (error (n2-row 'gc-arity n2-error)))
(dolist (name '(garbage-collect equal cons car))
  (let ((args (if (eq name 'garbage-collect) '(1) nil)))
    (n2-row (list name 'direct) (n2-error (cons name args) name))
    (n2-row (list name 'funcall)
	    (n2-error (cons 'funcall (cons (list 'quote name) args))
		      name))
    (n2-row (list name 'apply)
	    (n2-error
	     (list 'apply (list 'quote name) (list 'quote args)) name))))
(condition-case n2-error
    (n2-row 'gc-row-shape
	    (mapcar
	     (lambda (r)
	       (list (car r) (length r)
		     (and (integerp (nth 1 r)) (>= (nth 1 r) 0))
		     (and (integerp (nth 2 r)) (>= (nth 2 r) 0))))
	     (garbage-collect)))
  (error (n2-row 'gc-row-shape n2-error)))
(condition-case n2-error
    (n2-row 'gc-cons-movement
	    (let*
		((before (nth 2 (assq 'conses (garbage-collect))))
		 (live (make-list 2000 17))
		 (after (nth 2 (assq 'conses (garbage-collect)))))
	      (list (>= (- after before) 2000) (length live)
		    (car live))))
  (error (n2-row 'gc-cons-movement n2-error)))
(condition-case n2-error
    (n2-row 'clear-multibyte
	    (let* ((s (copy-sequence "é")) (alias s))
	      (list (clear-string s) (eq s alias) (length alias)
		    (string-bytes alias) (multibyte-string-p alias)
		    (append alias nil))))
  (error (n2-row 'clear-multibyte n2-error)))
(condition-case n2-error
    (n2-row 'clear-forced-aliases
	    (let*
		((s (string-as-multibyte (copy-sequence "abc")))
		 (alias s) (v (vector s)) (c (list s))
		 (h (make-hash-table :test 'eq))
		 (closure (lambda nil s)))
	      (puthash 's s h) (put-text-property 0 1 'secret t s)
	      (clear-string s)
	      (mapcar
	       (lambda (x)
		 (list (eq x s) (multibyte-string-p x) (length x)
		       (append x nil) (get-text-property 0 'secret x)))
	       (list alias (aref v 0) (car c) (gethash 's h)
		     (funcall closure)))))
  (error (n2-row 'clear-forced-aliases n2-error)))
(condition-case n2-error
    (n2-row 'clear-isolation
	    (let*
		((s (copy-sequence "éabc")) (c (copy-sequence s))
		 (sub (substring s 0)) (bytes (string-as-unibyte s)))
	      (clear-string s)
	      (list c sub (append bytes nil) (append s nil))))
  (error (n2-row 'clear-isolation n2-error)))
(condition-case n2-error
    (n2-row 'clear-cache
	    (let*
		((s
		  (concat (make-string 70 120) "é"
			  (make-string 70 121)))
		 (alias s))
	      (length s) (aref s 90) (substring s 80 100)
	      (clear-string s)
	      (list (length alias) (string-bytes alias)
		    (multibyte-string-p alias) (aref alias 90)
		    (substring alias 80 100))))
  (error (n2-row 'clear-cache n2-error)))
(condition-case n2-error
    (n2-row 'clear-repeated
	    (mapcar
	     (lambda (s) (clear-string s)
	       (clear-string s)
	       (list (length s) (multibyte-string-p s) (append s nil)))
	     (list (copy-sequence "") (unibyte-string 200 255)
		   (copy-sequence "abc"))))
  (error (n2-row 'clear-repeated n2-error)))
(condition-case n2-error
    (n2-row 'clear-type (condition-case e (clear-string 42) (error e)))
  (error (n2-row 'clear-type n2-error)))
(condition-case n2-error
    (n2-row 'aset-restriction
	    (condition-case e (aset (copy-sequence "é") 0 0)
	      (error (car e))))
  (error (n2-row 'aset-restriction n2-error)))
(condition-case n2-error
    (n2-row 'equal-semantics
	    (list (equal '(1 (2)) '(1 (2))) (equal [1 [2]] [1 [2]])
		  (equal [1 2] [1 3])
		  (let ((x (list 1))) (setcdr x x) (equal x x))))
  (error (n2-row 'equal-semantics n2-error)))
(princ "N2-DONE\n")
(condition-case n2-error
    (n2-row 'direct-alias
            (progn (defalias 'n2-gc-alias 'garbage-collect)
                   (n2-error '(n2-gc-alias 1) 'n2-gc-alias)))
  (error (n2-row 'direct-alias n2-error)))
(condition-case n2-error
    (n2-row 'clear-argument-alias
            (let ((s (string-as-multibyte "abc")))
              (funcall (lambda (a b) (list (eq a b) (multibyte-string-p a)
                                         (multibyte-string-p b)))
                       s (progn (clear-string s) s))))
  (error (n2-row 'clear-argument-alias n2-error)))
(condition-case n2-error
    (n2-row 'clear-gc-survival
            (let* ((s (string-as-multibyte "abc")) (saved (list s)))
              (clear-string s) (garbage-collect) (aset s 1 200)
              (list (eq s (car saved)) (multibyte-string-p (car saved))
                    (append (car saved) nil))))
  (error (n2-row 'clear-gc-survival n2-error)))
(princ "N2-EXTRA-DONE\n")

t

(dolist (source '("abc" ""))
  (condition-case n2-error
      (let* ((s (string-as-multibyte source))
             (eq-table (make-hash-table :test 'eq))
             (eql-table (make-hash-table :test 'eql))
             (hash (sxhash-eq s)))
        (puthash s 17 eq-table) (puthash s 19 eql-table)
        (clear-string s)
        (n2-row (list 'clear-string-key source)
                (list (gethash s eq-table 'missing)
                      (gethash s eql-table 'missing) (= hash (sxhash-eq s)))))
    (error (n2-row (list 'clear-string-key source) n2-error))))
(princ "N2-HASH-DONE\n")
t
