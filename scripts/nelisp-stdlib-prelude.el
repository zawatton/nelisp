;;; nelisp-stdlib-prelude.el --- stdlib prelude for the standalone NeLisp reader  -*- lexical-binding: nil; -*-
;;
;; A loadable .el that bootstraps `defmacro' + the core macros (when/unless/cond/
;; and/or/prog1/prog2/push/pop/dolist/defun), the list library (nth/reverse/
;; append/cXXr/...), search (memq/member/assq/assoc), HOF (mapcar/mapc), plist
;; (plist-get/-put/-member), copy-sequence and the backquote machinery
;; (nelisp--bq-* + the `backquote' macro).  Every form here LOADS AS-IS on the
;; standalone NeLisp reader binary once the Wave-1 (B) breadth primitives exist
;; (consp/eq/car/cdr/setcar/setcdr/symbol-name/vector ops/equal/...).
;;
;; USAGE (the binary loads the prelude then user code via file-load):
;;   cat scripts/nelisp-stdlib-prelude.el yourfile.el > /tmp/prog.el
;;   target/nelisp-standalone-reader /tmp/prog.el       # exit = last form's value
;; or use the `standalone-reader-prelude-test' Makefile target as a worked example.
;;
;; Assembled from the repo stdlib sources (lisp/nelisp-stdlib-{eval-special,list,
;; search,hof,misc,plist-str}.el + lisp/nelisp-cl-macros.el for backquote).
;; Regenerate with /tmp/make-prelude.el (or re-assemble those sources) -- 48 forms.

(fset 'defmacro
      (cons 'macro
	    (cons
	     (lambda (name args &rest body)
	       (let*
		   ((lambda-form (cons 'lambda (cons args body)))
		    (qname (cons 'quote (cons name nil)))
		    (inner-cons
		     (cons 'cons (cons lambda-form (cons nil nil))))
		    (outer-cons
		     (cons 'cons
			   (cons (cons 'quote (cons 'macro nil))
				 (cons inner-cons nil)))))
		 (cons 'progn
		       (cons
			(cons 'fset (cons qname (cons outer-cons nil)))
			(cons qname nil)))))
	     nil)))

(defmacro when (cond &rest body)
  "If COND yields non-nil, eval BODY forms sequentially and return last value."
  (cons 'if (cons cond (cons (cons 'progn body) (cons nil nil)))))

(defmacro unless (cond &rest body)
  "If COND yields nil, eval BODY forms sequentially and return last value."
  (cons 'if (cons cond (cons nil (cons (cons 'progn body) nil)))))

(defmacro cond (&rest clauses)
  "Try each clause until one succeeds.\nEach clause is `(TEST BODY...)'.  If TEST evaluates non-nil, BODY is\nevaluated and its last value returned.  When BODY is empty the value\nof TEST itself is returned."
  (if (null clauses) nil
    (let*
	((clause (car clauses)) (rest (cdr clauses))
	 (test (car clause)) (body (cdr clause)))
      (if (null body)
	  (cons 'let
		(cons (cons (cons '--nl-cond-tmp (cons test nil)) nil)
		      (cons
		       (cons 'if
			     (cons '--nl-cond-tmp
				   (cons '--nl-cond-tmp
					 (cons (cons 'cond rest) nil))))
		       nil)))
	(cons 'if
	      (cons test
		    (cons (cons 'progn body)
			  (cons (cons 'cond rest) nil))))))))

(defmacro and (&rest forms)
  "Eval FORMS left-to-right, short-circuiting on nil.  Empty form list = t."
  (if (null forms) t
    (if (null (cdr forms)) (car forms)
      (cons 'if
	    (cons (car forms)
		  (cons (cons 'and (cdr forms)) (cons nil nil)))))))

(defmacro or (&rest forms)
  "Eval FORMS left-to-right, returning the first non-nil value (or nil)."
  (if (null forms) nil
    (if (null (cdr forms)) (car forms)
      (cons 'let
	    (cons
	     (cons (cons '--nl-or-tmp (cons (car forms) nil)) nil)
	     (cons
	      (cons 'if
		    (cons '--nl-or-tmp
			  (cons '--nl-or-tmp
				(cons (cons 'or (cdr forms)) nil))))
	      nil))))))

(defmacro prog1 (first &rest rest)
  "Eval FIRST, then REST forms in order; return value of FIRST."
  (cons 'let
	(cons (cons (cons '--nl-prog1-tmp (cons first nil)) nil)
	      (append rest (cons '--nl-prog1-tmp nil)))))

(defmacro prog2 (form1 form2 &rest rest)
  "Eval FORM1, FORM2, then REST forms; return value of FORM2."
  (cons 'progn (cons form1 (cons (cons 'prog1 (cons form2 rest)) nil))))

(defmacro push (newelt place)
  "(setq PLACE (cons NEWELT PLACE))' for symbol PLACE; otherwise\ndelegate to `setf' so cl-defstruct accessor places and `car' / `cdr'\n/ `aref' / `nth' places work via Wave A21-fix's generalised `setf'."
  (if (symbolp place)
      (cons 'setq
	    (cons place
		  (cons (cons 'cons (cons newelt (cons place nil)))
			nil)))
    (list 'setf place (list 'cons newelt place))))

(defmacro pop (place)
  "(prog1 (car PLACE) (setq PLACE (cdr PLACE)))' for symbol PLACE;\ngeneralised PLACE delegates to `setf'."
  (if (symbolp place)
      (cons 'prog1
	    (cons (cons 'car (cons place nil))
		  (cons
		   (cons 'setq
			 (cons place
			       (cons (cons 'cdr (cons place nil)) nil)))
		   nil)))
    (list 'prog1 (list 'car place)
	  (list 'setf place (list 'cdr place)))))

(defmacro dolist (spec &rest body)
  "(dolist (VAR LIST [RESULT]) BODY...) — iterate VAR over LIST.\nBindings:  --nl-dolist-list = LIST cursor."
  (let*
      ((var (car spec)) (list-form (car (cdr spec)))
       (result-form (car (cdr (cdr spec)))))
    (cons 'let*
	  (cons
	   (cons (cons '--nl-dolist-list (cons list-form nil))
		 (cons (cons var (cons nil nil)) nil))
	   (cons
	    (cons 'while
		  (cons '--nl-dolist-list
			(cons
			 (cons 'setq
			       (cons var
				     (cons
				      (cons 'car
					    (cons '--nl-dolist-list
						  nil))
				      nil)))
			 (append body
				 (cons
				  (cons 'setq
					(cons '--nl-dolist-list
					      (cons
					       (cons 'cdr
						     (cons
						      '--nl-dolist-list
						      nil))
					       nil)))
				  nil)))))
	    (cons result-form nil))))))

(defmacro defun (name args &rest body)
  "(defun NAME ARGS BODY...) → (progn (fset 'NAME (lambda ARGS BODY...)) 'NAME).\nUnlike Rust `sf_defun' which stores the raw `(lambda ...)' form\nunmodified, the elisp expansion goes through evaluation of\n`(lambda ARGS BODY...)' = produces a closure with the current lexical\nenv captured.  For top-level defun the captured env is empty so\nsemantics match Rust; defuns nested inside `let' would receive a\nnon-empty captured env in elisp but the bare form in Rust — this is\nan intentional improvement, not a regression."
  (let
      ((lambda-form (cons 'lambda (cons args body)))
       (qname (cons 'quote (cons name nil))))
    (cons 'progn
	  (cons (cons 'fset (cons qname (cons lambda-form nil)))
		(cons qname nil)))))

(defun nthcdr (n list)
  (if (= n 0) list (if (null list) nil (nthcdr (1- n) (cdr list)))))

(defun nth (n list) (car (nthcdr n list)))

(defun reverse (list)
  (let ((acc nil))
    (while list
      (setq acc (cons (car list) acc)) (setq list (cdr list)))
    acc))

(defun nreverse (list) (reverse list))

(unless (fboundp 'last)
  (defun last (list &optional n)
    "Return the last link of LIST.  Its `car' is the last element.\nIf LIST is nil, return nil.  If N is non-nil, return the Nth-to-last\nlink of LIST."
    (let* ((m (or n 1)) (cur list) (lead list))
      (let ((i 0))
	(while (and (consp lead) (< i m))
	  (setq lead (cdr lead)) (setq i (1+ i))))
      (while (consp lead) (setq cur (cdr cur)) (setq lead (cdr lead)))
      cur)))

(unless (fboundp 'butlast)
  (defun butlast (list &optional n)
    "Return a copy of LIST with the last N elements removed.\nIf N is omitted or nil, the last element is removed.  If N is zero\nor negative, return a full copy of LIST."
    (let ((m (or n 1)))
      (if (<= m 0) (copy-sequence list)
	(let* ((len 0) (cur list))
	  (while (consp cur) (setq len (1+ len)) (setq cur (cdr cur)))
	  (let ((keep (- len m)))
	    (if (<= keep 0) nil
	      (let ((acc nil) (i 0) (src list))
		(while (and (< i keep) (consp src))
		  (setq acc (cons (car src) acc)) (setq src (cdr src))
		  (setq i (1+ i)))
		(reverse acc)))))))))

(defun nelisp--append-collect (acc seq)
  "Walk SEQ and `cons' each element onto ACC (= reverse-order\naccumulator).  SEQ may be nil / cons / vector / string.  Returns\nthe new ACC.  Signals `wrong-type-argument' for improper-list cons\nor non-sequence atom."
  (cond ((null seq) acc)
	((consp seq)
	 (let ((cur seq))
	   (while (consp cur)
	     (setq acc (cons (car cur) acc)) (setq cur (cdr cur)))
	   (when cur (signal 'wrong-type-argument (list 'listp seq)))
	   acc))
	((vectorp seq)
	 (let ((i 0) (n (length seq)))
	   (while (< i n)
	     (setq acc (cons (aref seq i) acc)) (setq i (1+ i)))
	   acc))
	((stringp seq)
	 (let ((i 0) (n (length seq)))
	   (while (< i n)
	     (setq acc (cons (aref seq i) acc)) (setq i (1+ i)))
	   acc))
	(t (signal 'wrong-type-argument (list 'sequencep seq)))))

(defun append (&rest args)
  "Concatenate sequences ARGS into a fresh proper-list spine.\nNon-final args may be list / vector / string / nil.  The FINAL arg\nis used as the tail (= unchanged, can be any value).  Single-arg\ncall returns the arg unchanged (= no copy)."
  (cond ((null args) nil) ((null (cdr args)) (car args))
	(t
	 (let ((cur args) (acc nil) (tail nil))
	   (while (cdr cur)
	     (setq acc (nelisp--append-collect acc (car cur)))
	     (setq cur (cdr cur)))
	   (setq tail (car cur))
	   (let ((result tail))
	     (while acc
	       (setq result (cons (car acc) result))
	       (setq acc (cdr acc)))
	     result)))))

(defun caar (x) (car (car x)))

(defun cadr (x) (car (cdr x)))

(defun cdar (x) (cdr (car x)))

(defun cddr (x) (cdr (cdr x)))

(defun caaar (x) (car (car (car x))))

(defun caadr (x) (car (car (cdr x))))

(defun cadar (x) (car (cdr (car x))))

(defun caddr (x) (car (cdr (cdr x))))

(defun cdaar (x) (cdr (car (car x))))

(defun cdadr (x) (cdr (car (cdr x))))

(defun cddar (x) (cdr (cdr (car x))))

(defun cdddr (x) (cdr (cdr (cdr x))))

(defun cadddr (x) (car (cdr (cdr (cdr x)))))

(defun copy-sequence (seq)
  (cond ((null seq) nil)
	((consp seq)
	 (let ((acc nil) (cur seq))
	   (while (consp cur)
	     (setq acc (cons (car cur) acc)) (setq cur (cdr cur)))
	   (when cur (signal 'wrong-type-argument (list 'list seq)))
	   (nreverse acc)))
	(t seq)))

(defun memq (elt list)
  (let ((found nil))
    (while (and list (not found))
      (if (eq elt (car list)) (setq found list)
	(setq list (cdr list))))
    found))

(defun member (elt list)
  (let ((found nil))
    (while (and list (not found))
      (if (equal elt (car list)) (setq found list)
	(setq list (cdr list))))
    found))

(defun assq (key alist)
  (let ((found nil))
    (while (and alist (not found))
      (let ((pair (car alist)))
	(if (and (consp pair) (eq (car pair) key)) (setq found pair)
	  (setq alist (cdr alist)))))
    found))

(defun assoc (key alist)
  (let ((found nil))
    (while (and alist (not found))
      (let ((pair (car alist)))
	(if (and (consp pair) (equal (car pair) key))
	    (setq found pair)
	  (setq alist (cdr alist)))))
    found))

(defun mapcar (fn list)
  (let ((acc nil))
    (while list
      (setq acc (cons (funcall fn (car list)) acc))
      (setq list (cdr list)))
    (let ((out nil))
      (while acc (setq out (cons (car acc) out)) (setq acc (cdr acc)))
      out)))

(defun mapc (fn list)
  (let ((orig list))
    (while list (funcall fn (car list)) (setq list (cdr list))) orig))

(defun plist-member (plist key)
  (let ((cur plist) (found nil))
    (while (and cur (not found))
      (if (eq (car cur) key) (setq found cur)
	(setq cur (cdr (cdr cur)))))
    found))

(defun plist-get (plist key)
  (let ((tail (plist-member plist key)))
    (if tail (car (cdr tail)) nil)))

(defun plist-put (plist key value)
  (let ((tail (plist-member plist key)))
    (if tail (progn (setcar (cdr tail) value) plist)
      (if (null plist) (cons key (cons value nil))
	(let ((cur plist))
	  (while (cdr (cdr cur)) (setq cur (cdr (cdr cur))))
	  (setcdr (cdr cur) (cons key (cons value nil))) plist)))))

(defun string-empty-p (s) (= (length s) 0))

(defun nelisp--bq-expand (form)
  "Return the expansion of FORM under `backquote'."
  (cond
   ((vectorp form)
    (signal 'error (list "nelisp-bq: vector quasi not supported")))
   ((not (consp form)) (list 'quote form))
   ((eq (car form) 'comma) (cadr form))
   ((eq (car form) 'comma-at)
    (signal 'error (list "nelisp-bq: top-level ,@ not allowed")))
   ((eq (car form) 'backquote)
    (signal 'error (list "nelisp-bq: nested backquote not supported")))
   (t (nelisp--bq-expand-list form))))

(defun nelisp--bq-expand-list (form)
  "Walk list FORM, producing the expansion.\nRecognises both (... ,X ...) interior unquote and (... . ,X) dotted\nunquote / (... . ,@X) dotted splice patterns."
  (let
      ((parts nil) (cur form) (tail-expr nil) (done nil)
       (has-splice nil))
    (while (and (not done) (consp cur))
      (let ((head (car cur)))
	(cond
	 ((eq head 'comma) (setq tail-expr (cadr cur)) (setq done t))
	 ((eq head 'comma-at) (setq tail-expr (cadr cur))
	  (setq has-splice t) (setq done t))
	 (t
	  (let ((elem head))
	    (cond
	     ((and (consp elem) (eq (car elem) 'comma-at))
	      (setq has-splice t)
	      (push (cons 'splice (cadr elem)) parts))
	     ((and (consp elem) (eq (car elem) 'comma))
	      (push (cons 'list (cadr elem)) parts))
	     (t (push (cons 'list (nelisp--bq-expand elem)) parts))))
	  (setq cur (cdr cur))))))
    (when (and (not done) (not (null cur)) (not (consp cur)))
      (setq tail-expr (list 'quote cur)))
    (nelisp--bq-build (nreverse parts) tail-expr has-splice)))

(defun nelisp--bq-build (parts tail has-splice)
  "Build the final form from PARTS list, TAIL expression, HAS-SPLICE flag."
  (cond ((and (null parts) (null tail)) (list 'quote nil))
	((null parts) tail)
	((and (not has-splice) (null tail))
	 (cons 'list (mapcar 'cdr parts)))
	((not has-splice)
	 (let ((acc tail) (rp (reverse parts)))
	   (while rp
	     (setq acc (list 'cons (cdr (car rp)) acc))
	     (setq rp (cdr rp)))
	   acc))
	(t
	 (let ((args nil) (p parts))
	   (while p
	     (let ((kind (car (car p))) (val (cdr (car p))))
	       (cond ((eq kind 'list) (push (list 'list val) args))
		     ((eq kind 'splice) (push val args))))
	     (setq p (cdr p)))
	   (setq args (nreverse args))
	   (when tail (setq args (append args (list tail))))
	   (cons 'append args)))))

(defmacro backquote (form)
  "Expand FORM as a quasiquoted template (NeLisp minimal subset).\nSee `nelisp--bq-expand' for the supported shapes."
  (nelisp--bq-expand form))

