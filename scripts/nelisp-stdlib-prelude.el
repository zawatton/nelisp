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
;;   target/nelisp /tmp/prog.el       # exit = last form's value
;; or use the `standalone-reader-prelude-test' Makefile target as a worked example.
;;
;; Assembled from the repo stdlib sources (lisp/nelisp-stdlib-{eval-special,list,
;; search,hof,misc,plist-str}.el + lisp/nelisp-cl-macros.el for backquote).
;; WAVE-2 BREADTH 2026-05-31: the core above is followed in this file by:
;;   cl-macros.el AS-IS, pcase.el AS-IS, 7 fboundp-gated prims, final cl-loop redef.
;;   Assembled by nelisp-standalone-build.el reader-units; lisp/ stays pristine.
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

(defmacro dotimes (spec &rest body)
  "(dotimes (VAR COUNT [RESULT]) BODY...) - iterate VAR from 0 below COUNT."
  (let* ((var (car spec))
         (count-form (car (cdr spec)))
         (result-form (car (cdr (cdr spec)))))
    (cons 'let*
          (cons
           (cons (cons '--nl-dotimes-limit (cons count-form nil))
                 (cons (cons var (cons 0 nil)) nil))
           (cons
            (cons 'while
                  (cons (cons '< (cons var (cons '--nl-dotimes-limit nil)))
                        (append body
                                (cons
                                 (cons 'setq
                                       (cons var
                                             (cons (cons '1+ (cons var nil))
                                                   nil)))
                                 nil))))
            (cons result-form nil))))))

(defmacro defun (name args &rest body)
  "(defun NAME ARGS BODY...) → (progn (fset 'NAME (lambda ARGS BODY...)) 'NAME).\nUnlike Rust `sf_defun' which stores the raw `(lambda ...)' form\nunmodified, the elisp expansion goes through evaluation of\n`(lambda ARGS BODY...)' = produces a closure with the current lexical\nenv captured.  For top-level defun the captured env is empty so\nsemantics match Rust; defuns nested inside `let' would receive a\nnon-empty captured env in elisp but the bare form in Rust — this is\nan intentional improvement, not a regression."
  (let*
      ((real-body
	(if (and body (cdr body) (stringp (car body)))
	    (cdr body)
	  body))
       (lambda-form (cons 'lambda (cons args real-body)))
       (qname (cons 'quote (cons name nil))))
    (cons 'progn
	  (cons (cons 'fset (cons qname (cons lambda-form nil)))
		(cons qname nil)))))

(defmacro declare-function (_fn _file &rest _args)
  "No-op byte-compiler hint stub for standalone loads."
  nil)

(defmacro eval-when-compile (&rest body)
  "Interpreter-mode stub: run BODY immediately."
  (cons 'progn body))

(defmacro eval-and-compile (&rest body)
  "Interpreter-mode stub: run BODY immediately."
  (cons 'progn body))

(defmacro with-no-warnings (&rest body)
  "Standalone stub: just run BODY."
  (cons 'progn body))

(defmacro with-suppressed-warnings (_warnings &rest body)
  "Standalone stub: just run BODY."
  (cons 'progn body))

(defmacro setq-default (&rest pairs)
  "NeLisp has no buffer-local distinction; alias to `setq'."
  (cons 'setq pairs))

(defmacro setq-local (&rest pairs)
  "NeLisp has no buffer-local distinction; alias to `setq'."
  (cons 'setq pairs))

(defmacro defvar (name &rest args)
  "Define NAME as a global variable, setting VALUE if unbound.
With NO value form (`(defvar NAME)' forward declaration) NAME is only
declared, NOT bound — matching Emacs `defvar' so a later
`(defvar NAME VALUE)' still initializes it.  Detecting the zero-value
form needs `&rest' (arity); `&optional value' cannot tell `(defvar X)'
from `(defvar X nil)'."
  (if args
      ;; (defvar NAME VALUE [DOC]):
      ;;   (progn (if (boundp 'NAME) nil (set 'NAME VALUE)) 'NAME)
      (cons 'progn
            (cons (cons 'if
                        (cons (cons 'boundp
                                    (cons (cons 'quote (cons name nil)) nil))
                              (cons nil
                                    (cons (cons 'set
                                                (cons (cons 'quote (cons name nil))
                                                      (cons (car args) nil)))
                                          nil))))
                  (cons (cons 'quote (cons name nil)) nil)))
    ;; (defvar NAME): forward declaration — return 'NAME, leave it UNBOUND.
    (cons 'quote (cons name nil))))

(defmacro defvar-local (name &optional value docstring)
  "Alias for `defvar' in the standalone."
  (cons 'defvar (cons name (cons value (cons docstring nil)))))

(defvar lexical-binding t
  "Standalone default: evaluated source is treated as lexical.")

(defmacro defconst (name value &optional _docstring)
  "Define NAME as a constant with VALUE in the standalone."
  (cons 'progn
        (cons (cons 'set
                    (cons (cons 'quote (cons name nil))
                          (cons value nil)))
              (cons (cons 'nelisp--env-globals-set-constant
                          (cons (cons 'quote (cons name nil))
                                (cons t nil)))
                    (cons (cons 'quote (cons name nil)) nil)))))

(defmacro defcustom (name value docstring &rest _options)
  "Standalone stub: behave like `defvar'."
  (cons 'defvar (cons name (cons value (cons docstring nil)))))

(defmacro defgroup (name _parent _docstring &rest _options)
  "Standalone stub: return NAME."
  (cons 'quote (cons name nil)))

(defun nthcdr (n list)
  (if (= n 0) list (if (null list) nil (nthcdr (1- n) (cdr list)))))

(defun car-safe (object)
  (if (consp object) (car object) nil))

(defun cdr-safe (object)
  (if (consp object) (cdr object) nil))

;; Doc 143 worklist A (WRITE): delq/delete were void in the reader runtime
;; (not in source).  List forms (the dominant use); rebuild semantics.
(defun delq (elt list)
  (let ((acc nil))
    (while list
      (if (not (eq (car list) elt))
          (setq acc (cons (car list) acc)))
      (setq list (cdr list)))
    (nreverse acc)))

(defun delete (elt seq)
  (let ((acc nil))
    (while seq
      (if (not (equal (car seq) elt))
          (setq acc (cons (car seq) acc)))
      (setq seq (cdr seq)))
    (nreverse acc)))

(defun delete-dups (list)
  "Destructively remove duplicate elements from LIST using `equal'."
  (let ((seen nil)
        (tail list)
        (prev nil))
    (while tail
      (if (member (car tail) seen)
          (if prev
              (setcdr prev (cdr tail))
            (setq list (cdr tail)))
        (setq seen (cons (car tail) seen))
        (setq prev tail))
      (setq tail (cdr tail)))
    list))

;; Doc 143 (WIRE from lisp/nelisp-stdlib-plist-str.el): high-frequency string
;; primitives that were void in the reader runtime.  Low-dependency forms only.
(defun string-equal (a b)
  (let ((sa (cond ((stringp a) a) ((symbolp a) (symbol-name a)) (t a)))
        (sb (cond ((stringp b) b) ((symbolp b) (symbol-name b)) (t b))))
    (equal sa sb)))

(defun string= (a b) (string-equal a b))

(defun regexp-quote (s)
  ;; Build via substring + string concat: the reader's `concat' does not
  ;; accept a char-list argument, so accumulate 1-char substrings instead.
  (let ((out "") (i 0) (n (length s)))
    (while (< i n)
      (let ((ch (aref s i)) (cs (substring s i (1+ i))))
        (when (or (eq ch ?.) (eq ch ?*) (eq ch ?+) (eq ch ??)
                  (eq ch ?\[) (eq ch ?\]) (eq ch ?^) (eq ch ?$)
                  (eq ch ?\\) (eq ch ?\() (eq ch ?\))
                  (eq ch ?\{) (eq ch ?\}) (eq ch ?|))
          (setq out (concat out "\\")))
        (setq out (concat out cs)))
      (setq i (1+ i)))
    out))

;; Doc 143 (pure, no-helper primitives): high-frequency, dependency-free.
(defun natnump (x) (and (integerp x) (>= x 0)))
(defun int-to-string (n) (number-to-string n))
(defun prefix-numeric-value (arg)
  (cond ((null arg) 1) ((eq arg '-) -1) ((consp arg) (car arg)) (t arg)))

;; Doc 143 arithmetic (helper-free, via >/</- which are reader primitives).
(defun max (x &rest rest)
  (let ((acc x)) (while rest (if (> (car rest) acc) (setq acc (car rest))) (setq rest (cdr rest))) acc))
(defun min (x &rest rest)
  (let ((acc x)) (while rest (if (< (car rest) acc) (setq acc (car rest))) (setq rest (cdr rest))) acc))
(defun abs (x) (if (< x 0) (- 0 x) x))

;; Doc 143 string ops (self-contained: funcall/aref/char-to-string/concat).
(defun mapconcat (fn seq &optional sep)
  (let ((out "") (first t) (tail seq) (s (or sep "")))
    (while tail
      (unless first (setq out (concat out s)))
      (setq out (concat out (funcall fn (car tail))))
      (setq first nil)
      (setq tail (cdr tail)))
    out))

(defun downcase (obj)
  (cond ((stringp obj)
         (let ((out "") (i 0) (n (length obj)))
           (while (< i n)
             (let ((c (aref obj i)))
               (setq out (concat out (char-to-string (if (and (>= c 65) (<= c 90)) (+ c 32) c)))))
             (setq i (1+ i)))
           out))
        ((integerp obj) (if (and (>= obj 65) (<= obj 90)) (+ obj 32) obj))
        (t obj)))

(defun upcase (obj)
  (cond ((stringp obj)
         (let ((out "") (i 0) (n (length obj)))
           (while (< i n)
             (let ((c (aref obj i)))
               (setq out (concat out (char-to-string (if (and (>= c 97) (<= c 122)) (- c 32) c)))))
             (setq i (1+ i)))
           out))
        ((integerp obj) (if (and (>= obj 97) (<= obj 122)) (- obj 32) obj))
        (t obj)))

;; Doc 143 file-name path ops (pure string slicing, from nelisp-stdlib-plist-str.el).
(defun file-name-directory (path)
  (let ((idx -1) (i 0) (n (length path)))
    (while (< i n) (when (eq (aref path i) ?/) (setq idx i)) (setq i (1+ i)))
    (if (< idx 0) nil (substring path 0 (1+ idx)))))
(defun file-name-nondirectory (path)
  (let ((idx -1) (i 0) (n (length path)))
    (while (< i n) (when (eq (aref path i) ?/) (setq idx i)) (setq i (1+ i)))
    (if (< idx 0) path (substring path (1+ idx)))))
(defun file-name-as-directory (path)
  (cond ((= (length path) 0) "/")
        ((eq (aref path (1- (length path))) ?/) path)
        (t (concat path "/"))))
(defun directory-file-name (path)
  (let ((n (length path)))
    (cond ((<= n 1) path)
          ((eq (aref path (1- n)) ?/) (substring path 0 (1- n)))
          (t path))))
(defun expand-file-name (path &optional base)
  ;; MVP (from nelisp-stdlib-misc.el): absolute paths pass through; relative
  ;; paths anchor on BASE or default-directory.  No ~ / . / .. resolution.
  (cond
   ((or (null path) (= (length path) 0)) path)
   ((eq (aref path 0) ?/) path)
   (t (let ((b (or base (and (boundp 'default-directory) default-directory))))
        (if (and (stringp b) (> (length b) 0))
            (concat (file-name-as-directory b) path)
          path)))))
(defun file-name-extension (path &optional period)
  (let* ((non (file-name-nondirectory path)) (n (length non)) (idx -1) (i 0))
    (while (< i n) (when (eq (aref non i) ?.) (setq idx i)) (setq i (1+ i)))
    (if (or (< idx 0) (= idx 0))
        (if period "" nil)
      (substring non (if period idx (1+ idx))))))
(defun file-name-sans-extension (path)
  (let* ((non (file-name-nondirectory path))
         (dir-len (- (length path) (length non)))
         (n (length non)) (idx -1) (i 0))
    (while (< i n) (when (eq (aref non i) ?.) (setq idx i)) (setq i (1+ i)))
    (if (or (< idx 0) (= idx 0)) path (substring path 0 (+ dir-len idx)))))

;; Doc 143 common pure string/seq predicates + builders.
(unless (fboundp 'string-prefix-p)
  (defun string-prefix-p (prefix string &optional _ignore-case)
    (let ((pl (length prefix)))
      (and (<= pl (length string)) (string= prefix (substring string 0 pl))))))
(unless (fboundp 'string-suffix-p)
  (defun string-suffix-p (suffix string &optional _ignore-case)
    (let ((sl (length suffix)) (stl (length string)))
      (and (<= sl stl) (string= suffix (substring string (- stl sl)))))))
(unless (fboundp 'char-equal)
  (defun char-equal (a b) (eq a b)))
(unless (fboundp 'string-to-list)
  (defun string-to-list (s)
    (let ((l nil) (i (1- (length s))))
      (while (>= i 0) (setq l (cons (aref s i) l)) (setq i (1- i)))
      l)))
(unless (fboundp 'number-sequence)
  (defun number-sequence (from &optional to inc)
    (if (null to) (list from)
      (let ((step (or inc 1)) (acc nil) (x from))
        (if (> step 0)
            (while (<= x to) (setq acc (cons x acc)) (setq x (+ x step)))
          (while (>= x to) (setq acc (cons x acc)) (setq x (+ x step))))
        (nreverse acc)))))
(unless (fboundp 'string-trim)
  (defun string-trim (s &optional _trim-left _trim-right)
    (let ((n (length s)) (a 0) (b 0))
      (setq b n)
      (while (and (< a b) (let ((c (aref s a))) (or (= c 32) (= c 9) (= c 10) (= c 13) (= c 12))))
        (setq a (1+ a)))
      (while (and (> b a) (let ((c (aref s (1- b)))) (or (= c 32) (= c 9) (= c 10) (= c 13) (= c 12))))
        (setq b (1- b)))
      (substring s a b))))

;; Doc 143 common modern-elisp pure utilities.
(unless (fboundp 'alist-get)
  (defun alist-get (key alist &optional default _remove _testfn)
    (let ((e (assq key alist))) (if e (cdr e) default))))
(unless (fboundp 'take)
  (defun take (n list)
    (let ((acc nil) (i 0))
      (while (and (< i n) list)
        (setq acc (cons (car list) acc)) (setq list (cdr list)) (setq i (1+ i)))
      (nreverse acc))))
(unless (fboundp 'ensure-list)
  (defun ensure-list (x) (if (listp x) x (list x))))
(unless (fboundp 'flatten-tree)
  (defun flatten-tree (tree)
    (cond ((null tree) nil)
          ((consp tree) (append (flatten-tree (car tree)) (flatten-tree (cdr tree))))
          (t (list tree)))))
(unless (fboundp 'string-join)
  (defun string-join (strings &optional separator)
    (mapconcat (lambda (x) x) strings (or separator ""))))

;; Doc 143 seq.el core (list/vector/string via a to-list coercion; funcall-based,
;; no closures -- safe under the dynamic-binding prelude).
(defun nelisp-seq--to-list (seq)
  (cond ((listp seq) seq)
        (t (let ((n (length seq)) (i 0) (acc nil))
             (while (< i n) (setq acc (cons (aref seq i) acc)) (setq i (1+ i)))
             (nreverse acc)))))
(unless (fboundp 'seq-filter)
  (defun seq-filter (pred seq)
    (let ((l (nelisp-seq--to-list seq)) (acc nil))
      (while l (when (funcall pred (car l)) (setq acc (cons (car l) acc))) (setq l (cdr l)))
      (nreverse acc))))
(unless (fboundp 'seq-remove)
  (defun seq-remove (pred seq)
    (let ((l (nelisp-seq--to-list seq)) (acc nil))
      (while l (unless (funcall pred (car l)) (setq acc (cons (car l) acc))) (setq l (cdr l)))
      (nreverse acc))))
(unless (fboundp 'seq-map)
  (defun seq-map (fn seq)
    (let ((l (nelisp-seq--to-list seq)) (acc nil))
      (while l (setq acc (cons (funcall fn (car l)) acc)) (setq l (cdr l)))
      (nreverse acc))))
(unless (fboundp 'seq-find)
  (defun seq-find (pred seq &optional default)
    (let ((l (nelisp-seq--to-list seq)) (found nil) (got nil))
      (while (and l (not got))
        (when (funcall pred (car l)) (setq found (car l) got t))
        (setq l (cdr l)))
      (if got found default))))
(unless (fboundp 'seq-reduce)
  (defun seq-reduce (fn seq initial)
    (let ((l (nelisp-seq--to-list seq)) (acc initial))
      (while l (setq acc (funcall fn acc (car l))) (setq l (cdr l)))
      acc)))
(unless (fboundp 'seq-some)
  (defun seq-some (pred seq)
    (let ((l (nelisp-seq--to-list seq)) (res nil))
      (while (and l (not res)) (setq res (funcall pred (car l))) (setq l (cdr l)))
      res)))
(unless (fboundp 'seq-every-p)
  (defun seq-every-p (pred seq)
    (let ((l (nelisp-seq--to-list seq)) (ok t))
      (while (and l ok) (unless (funcall pred (car l)) (setq ok nil)) (setq l (cdr l)))
      ok)))
(unless (fboundp 'seq-empty-p)
  (defun seq-empty-p (seq) (= (length seq) 0)))
(unless (fboundp 'seq-length)
  (defun seq-length (seq) (length seq)))
(unless (fboundp 'seq-elt)
  (defun seq-elt (seq n) (if (listp seq) (nth n seq) (aref seq n))))
(unless (fboundp 'seq-contains-p)
  (defun seq-contains-p (seq elt &optional testfn)
    (let ((l (nelisp-seq--to-list seq)) (found nil))
      (while (and l (not found))
        (when (if testfn (funcall testfn elt (car l)) (equal elt (car l))) (setq found t))
        (setq l (cdr l)))
      found)))
(unless (fboundp 'seq-do)
  (defun seq-do (fn seq)
    (let ((l (nelisp-seq--to-list seq)))
      (while l (funcall fn (car l)) (setq l (cdr l)))
      nil)))
(unless (fboundp 'cl-remove-if)
  (defun cl-remove-if (pred seq) (seq-remove pred seq)))
(unless (fboundp 'cl-remove-if-not)
  (defun cl-remove-if-not (pred seq) (seq-filter pred seq)))
(unless (fboundp 'cl-find-if)
  (defun cl-find-if (pred seq)
    (let ((cur (nelisp-seq--to-list seq))
          (found nil)
          (value nil))
      (while (and cur (not found))
        (when (funcall pred (car cur))
          (setq found t)
          (setq value (car cur)))
        (setq cur (cdr cur)))
      value)))
(unless (fboundp 'cl-find-if-not)
  (defun cl-find-if-not (pred seq)
    (cl-find-if (lambda (x) (not (funcall pred x))) seq)))
(unless (fboundp 'seq-take)
  (defun seq-take (seq n) (take n (nelisp-seq--to-list seq))))
(unless (fboundp 'seq-drop)
  (defun seq-drop (seq n) (nthcdr n (nelisp-seq--to-list seq))))
(unless (fboundp 'seq-count)
  (defun seq-count (pred seq)
    (let ((l (nelisp-seq--to-list seq)) (c 0))
      (while l (when (funcall pred (car l)) (setq c (1+ c))) (setq l (cdr l)))
      c)))
(unless (fboundp 'seq-position)
  (defun seq-position (seq elt &optional testfn)
    (let ((l (nelisp-seq--to-list seq)) (i 0) (found nil) (idx nil))
      (while (and l (not found))
        (when (if testfn (funcall testfn elt (car l)) (equal elt (car l)))
          (setq found t idx i))
        (setq l (cdr l) i (1+ i)))
      idx)))
(unless (fboundp 'seq-uniq)
  (defun seq-uniq (seq &optional testfn)
    (let ((l (nelisp-seq--to-list seq)) (acc nil))
      (while l
        (let ((x (car l)))
          (unless (let ((a acc) (hit nil))
                    (while (and a (not hit))
                      (when (if testfn (funcall testfn x (car a)) (equal x (car a))) (setq hit t))
                      (setq a (cdr a)))
                    hit)
            (setq acc (cons x acc))))
        (setq l (cdr l)))
      (nreverse acc))))
(unless (fboundp 'seq-into)
  (defun seq-into (seq type)
    (let ((l (nelisp-seq--to-list seq)))
      (cond ((eq type 'list) l)
            ((eq type 'vector) (apply #'vector l))
            ((eq type 'string) (apply #'string l))
            (t l)))))
(unless (fboundp 'string-replace)
  (defun string-replace (from to in)
    ;; literal (non-regexp) replace-all of FROM with TO in IN
    (if (= (length from) 0) in
      (let ((out "") (i 0) (n (length in)) (fl (length from)))
        (while (< i n)
          (if (and (<= (+ i fl) n) (string= from (substring in i (+ i fl))))
              (progn (setq out (concat out to)) (setq i (+ i fl)))
            (setq out (concat out (substring in i (1+ i)))) (setq i (1+ i))))
        out))))

;; Doc 143: purecopy (no pure space -> identity), destructive nconc (setcdr),
;; princ/terpri (via the wired printer + nelisp--write-stdout-bytes).
(defun purecopy (x) x)
(defun nconc (&rest lists)
  (let ((result nil) (tail nil))
    (while lists
      (let ((l (car lists)))
        (when (consp l)
          (if tail (setcdr tail l) (setq result l))
          (setq tail l)
          (while (consp (cdr tail)) (setq tail (cdr tail)))))
      (setq lists (cdr lists)))
    result))
(defun princ (object &optional _stream)
  (nelisp--write-stdout-bytes (nelisp--prn-to-string object nil))
  object)
(defun terpri (&optional _stream)
  (nelisp--write-stdout-bytes "\n")
  nil)

;; Doc 143 more pure primitives.
(defun string (&rest chars)
  (apply #'concat (mapcar #'char-to-string chars)))
(defun prin1 (object &optional _stream)
  (nelisp--write-stdout-bytes (nelisp--prn-to-string object t))
  object)
(defun format-message (fmt &rest args)
  (apply #'format (cons fmt args)))
(defun assoc-string (key alist &optional _case-fold)
  (let ((k (if (symbolp key) (symbol-name key) key)) (found nil))
    (while (and alist (not found))
      (let* ((entry (car alist))
             (ek (if (consp entry) (car entry) entry))
             (eks (if (symbolp ek) (symbol-name ek) ek)))
        (if (string= k eks) (setq found entry) (setq alist (cdr alist)))))
    found))

;; Doc 143 pure list utilities.
(unless (fboundp 'rassq)
  (defun rassq (value alist)
    (let ((found nil))
      (while (and alist (not found))
        (if (and (consp (car alist)) (eq (cdr (car alist)) value))
            (setq found (car alist))
          (setq alist (cdr alist))))
      found)))
(unless (fboundp 'rassoc)
  (defun rassoc (value alist)
    (let ((found nil))
      (while (and alist (not found))
        (if (and (consp (car alist)) (equal (cdr (car alist)) value))
            (setq found (car alist))
          (setq alist (cdr alist))))
      found)))
(unless (fboundp 'last)
  (defun last (list &optional n)
    (let ((len (length list)) (m (or n 1)))
      (nthcdr (if (> len m) (- len m) 0) list))))
(unless (fboundp 'butlast)
  (defun butlast (list &optional n)
    (let* ((len (length list)) (m (or n 1)) (keep (- len m)) (acc nil) (i 0))
      (while (and (< i keep) list)
        (setq acc (cons (car list) acc))
        (setq list (cdr list))
        (setq i (1+ i)))
      (nreverse acc))))
(unless (fboundp 'copy-tree)
  (defun copy-tree (tree &optional _vecp)
    (if (consp tree)
        (cons (copy-tree (car tree)) (copy-tree (cdr tree)))
      tree)))

(defun nth (n list) (car (nthcdr n list)))

(defun make-list (length object)
  (let ((acc nil))
    (while (> length 0)
      (setq acc (cons object acc))
      (setq length (1- length)))
    acc))

(defun reverse (list)
  (let ((acc nil))
    (while list
      (setq acc (cons (car list) acc)) (setq list (cdr list)))
    acc))

(defun nreverse (list)
  (let ((prev nil) (cur list) next)
    (while cur
      (setq next (cdr cur))
      (setcdr cur prev)
      (setq prev cur)
      (setq cur next))
    prev))

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
    (cond
     ((stringp elt)
      (while (and list (not found))
        (let ((x (car list)))
          (if (and (stringp x) (string= elt x)) (setq found list)
            (setq list (cdr list))))))
     ((symbolp elt)
      (while (and list (not found))
        (if (eq elt (car list)) (setq found list)
          (setq list (cdr list)))))
     ((numberp elt)
      (while (and list (not found))
        (let ((x (car list)))
          (if (and (numberp x) (= elt x)) (setq found list)
            (setq list (cdr list))))))
     (t
      (while (and list (not found))
        (if (equal elt (car list)) (setq found list)
          (setq list (cdr list))))))
    found))

(defun assq (key alist)
  (let ((found nil))
    (while (and alist (not found))
      (let ((pair (car alist)))
	(if (and (consp pair) (eq (car pair) key)) (setq found pair)
	  (setq alist (cdr alist)))))
    found))

(defun assoc (key alist &optional testfn)
  (let ((found nil))
    (cond
     (testfn
      (while (and alist (not found))
        (let ((pair (car alist)))
          (if (and (consp pair) (funcall testfn (car pair) key))
              (setq found pair)
            (setq alist (cdr alist))))))
     ((stringp key)
      (while (and alist (not found))
        (let ((pair (car alist)))
          (if (and (consp pair) (stringp (car pair)) (string= (car pair) key))
              (setq found pair)
            (setq alist (cdr alist))))))
     ((symbolp key)
      (while (and alist (not found))
        (let ((pair (car alist)))
          (if (and (consp pair) (eq (car pair) key))
              (setq found pair)
            (setq alist (cdr alist))))))
     ((numberp key)
      (while (and alist (not found))
        (let ((pair (car alist)))
          (if (and (consp pair) (numberp (car pair)) (= (car pair) key))
              (setq found pair)
            (setq alist (cdr alist))))))
     (t
      (while (and alist (not found))
        (let ((pair (car alist)))
          (if (and (consp pair) (equal (car pair) key))
              (setq found pair)
            (setq alist (cdr alist)))))))
    found))

(defun mapcar (fn seq)
  "Apply FN to each element of SEQ (list, vector, or string); collect results.
Doc 22 A6: arrays are iterated by index via `aref'/`length' (cons-cell
walking only works for lists)."
  (if (or (vectorp seq) (stringp seq))
      (let ((n (length seq)) (i 0) (acc nil))
        (while (< i n)
          (setq acc (cons (funcall fn (aref seq i)) acc))
          (setq i (1+ i)))
        (nreverse acc))
    (let ((acc nil))
      (while seq
        (setq acc (cons (funcall fn (car seq)) acc))
        (setq seq (cdr seq)))
      (nreverse acc))))

(defun mapc (fn seq)
  "Apply FN to each element of SEQ for side effect; return SEQ.
Doc 22 A6: arrays are iterated by index."
  (if (or (vectorp seq) (stringp seq))
      (let ((n (length seq)) (i 0))
        (while (< i n) (funcall fn (aref seq i)) (setq i (1+ i)))
        seq)
    (let ((orig seq))
      (while seq (funcall fn (car seq)) (setq seq (cdr seq))) orig)))

(defun plist-member (plist key &optional predicate)
  (let ((cur plist) (found nil))
    (if predicate
        (while (and cur (not found))
          (if (funcall predicate (car cur) key) (setq found cur)
            (setq cur (cdr (cdr cur)))))
      (while (and cur (not found))
        (if (eq (car cur) key) (setq found cur)
          (setq cur (cdr (cdr cur))))))
    found))

(defun plist-get (plist key &optional predicate)
  (let ((cur plist) (found nil) (value nil))
    (if predicate
        (while (and cur (not found))
          (if (funcall predicate (car cur) key)
              (progn (setq found t) (setq value (car (cdr cur))))
            (setq cur (cdr (cdr cur)))))
      (while (and cur (not found))
        (if (eq (car cur) key)
            (progn (setq found t) (setq value (car (cdr cur))))
          (setq cur (cdr (cdr cur))))))
    value))

(defun plist-put (plist key value &optional predicate)
  (let ((cur plist) (tail nil))
    (if predicate
        (while (and cur (not tail))
          (if (funcall predicate (car cur) key) (setq tail cur)
            (setq cur (cdr (cdr cur)))))
      (while (and cur (not tail))
        (if (eq (car cur) key) (setq tail cur)
          (setq cur (cdr (cdr cur))))))
    (if tail (progn (setcar (cdr tail) value) plist)
      (if (null plist) (cons key (cons value nil))
	(let ((end plist))
	  (while (cdr (cdr end)) (setq end (cdr (cdr end))))
	  (setcdr (cdr end) (cons key (cons value nil))) plist)))))

(defun string-empty-p (s) (= (length s) 0))

;; ---- macroexpand (Doc 47 self-host / compiler frontend) ----
;;
;; `defmacro' stores a macro as the function value `(macro CLOSURE)' (= a
;; two-element list: car `macro', cadr the macro CLOSURE).  `nelisp-aot-
;; compiler--preprocess-source' calls `(macroexpand FORM)' on every form it
;; does not structurally recognise, relying on (equal expanded form) to detect
;; "no expansion happened".  These reproduce host Emacs's contract:
;;   macroexpand-1  expands at most ONE level.
;;   macroexpand    expands repeatedly until the head is no longer a macro.
;; The macro CLOSURE is applied to FORM's UNEVALUATED args (= (cdr FORM)); the
;; result is the expansion, which is NOT evaluated.
(defun nelisp--macro-function (head)
  "If symbol HEAD names a macro, return its CLOSURE; else nil.
Guards `symbol-function' behind `fboundp' (calling it on an unbound symbol
traps), and only recognises the `(macro CLOSURE)' shape."
  (if (and (symbolp head) (fboundp head))
      (let ((f (symbol-function head)))
        (if (and (consp f) (eq (car f) 'macro))
            (car (cdr f))
          nil))
    nil))

(defun macroexpand-1 (form &optional env)
  "Expand FORM by one macro step if its head is a macro; else return FORM.
ENV is a macro environment alist; an entry (SYMBOL . EXPANDER) shadows the
global binding (Emacs semantics): a non-nil EXPANDER is applied to the arg
forms, a nil EXPANDER marks the head as locally not-a-macro.  Honoring ENV
fixes Doc 22 A12 (env-driven local macros, e.g. generator.el iter-yield)."
  (if (consp form)
      (let ((cell (and env (symbolp (car form)) (assq (car form) env))))
        (if cell
            (if (cdr cell) (apply (cdr cell) (cdr form)) form)
          (let ((mfn (nelisp--macro-function (car form))))
            (if mfn (apply mfn (cdr form)) form))))
    form))

(defun macroexpand (form &optional environment)
  "Repeatedly macroexpand FORM until its head is no longer a macro.
ENVIRONMENT is the macro environment alist threaded to `macroexpand-1'
(Doc 22 A12); local macros in ENVIRONMENT shadow the global mirror."
  (let ((cur form) (again t))
    (while again
      (let ((next (macroexpand-1 cur environment)))
        (if (eq next cur) (setq again nil) (setq cur next))))
    cur))

(defun nelisp--macroexpand-all-map (forms environment)
  "Apply `macroexpand-all' (threading ENVIRONMENT) to each of FORMS."
  (let ((out nil))
    (while forms
      (setq out (cons (macroexpand-all (car forms) environment) out)
            forms (cdr forms)))
    (nreverse out)))

(defun macroexpand-all (form &optional environment)
  "Expand every macro call reachable from FORM, honoring ENVIRONMENT.
Doc 22 A12: ENVIRONMENT is threaded to `macroexpand' at every node, so
env-driven local macros (e.g. the macro-environment generator.el passes
to intercept `iter-yield') expand.  Non-evaluated positions are kept
literal: `quote' datums, lambda/`function' parameter lists, `let'/`let*'
binding variables, and `condition-case' VAR pass through unchanged; every
other special form has only evaluated args (or atomic name/doc slots that
pass through), so the default recursion into the cdr is correct for them."
  (if (not (consp form))
      form
    (let ((expanded (macroexpand form environment)))
      (if (not (consp expanded))
          expanded
        (let ((head (car expanded)))
          (cond
           ((eq head 'quote) expanded)
           ((eq head 'function)
            (let ((arg (cadr expanded)))
              (if (and (consp arg) (eq (car arg) 'lambda))
                  (list 'function
                        (cons 'lambda
                              (cons (cadr arg)
                                    (nelisp--macroexpand-all-map (cddr arg)
                                                                 environment))))
                expanded)))
           ((eq head 'lambda)
            (cons 'lambda
                  (cons (cadr expanded)
                        (nelisp--macroexpand-all-map (cddr expanded)
                                                     environment))))
           ((or (eq head 'let) (eq head 'let*))
            (cons head
                  (cons (mapcar (lambda (b)
                                  (if (and (consp b) (consp (cdr b)))
                                      (list (car b)
                                            (macroexpand-all (cadr b) environment))
                                    b))
                                (cadr expanded))
                        (nelisp--macroexpand-all-map (cddr expanded)
                                                     environment))))
           ((or (eq head 'defun) (eq head 'defmacro))
            (cons head
                  (cons (cadr expanded)
                        (cons (caddr expanded)
                              (nelisp--macroexpand-all-map (cdddr expanded)
                                                           environment)))))
           ((eq head 'setq)
            (let ((rest (cdr expanded)) (out nil))
              (while rest
                (setq out (cons (car rest) out) rest (cdr rest))
                (when rest
                  (setq out (cons (macroexpand-all (car rest) environment) out)
                        rest (cdr rest))))
              (cons 'setq (nreverse out))))
           ((eq head 'cond)
            (cons 'cond
                  (mapcar (lambda (clause)
                            (if (consp clause)
                                (nelisp--macroexpand-all-map clause environment)
                              clause))
                          (cdr expanded))))
           ((eq head 'condition-case)
            (cons 'condition-case
                  (cons (cadr expanded)
                        (cons (macroexpand-all (caddr expanded) environment)
                              (mapcar (lambda (h)
                                        (if (consp h)
                                            (cons (car h)
                                                  (nelisp--macroexpand-all-map
                                                   (cdr h) environment))
                                          h))
                                      (cdddr expanded))))))
           (t
            (cons head (nelisp--macroexpand-all-map (cdr expanded)
                                                    environment)))))))))

(unless (fboundp 'macroexp-parse-body)
  (defun macroexp-parse-body (body)
    "Split BODY into declarations and remaining forms.
Return (DECLARATIONS . BODY-FORMS), matching the shape used by Emacs
macro helpers such as `iter-defun'.  A leading docstring and any
following `(declare ...)' forms are treated as declarations."
    (let ((declarations nil)
          (cur body))
      (when (and cur (stringp (car cur)))
        (setq declarations (cons (car cur) declarations))
        (setq cur (cdr cur)))
      (while (and cur (consp (car cur)) (eq (car (car cur)) 'declare))
        (setq declarations (cons (car cur) declarations))
        (setq cur (cdr cur)))
      (cons (nreverse declarations) cur))))

(defun nelisp--parse-cl-formals (formals)
  "Parse the subset of `cl-defun' FORMALS used by standalone runtime code."
  (let ((mode 'pos)
        (positional nil)
        (optionals nil)
        (rest-sym nil)
        (keys nil)
        (cursor formals))
    (while cursor
      (let ((f (car cursor)))
        (cond
         ((eq f '&optional)
          (setq mode 'opt))
         ((eq f '&rest)
          (setq mode 'rest))
         ((eq f '&key)
          (setq mode 'key))
         ((eq f '&aux)
          (setq mode 'aux))
         ((eq mode 'pos)
          (setq positional (cons f positional)))
         ((eq mode 'opt)
          (setq optionals (cons f optionals)))
         ((eq mode 'rest)
          (unless rest-sym
            (setq rest-sym f)))
         ((eq mode 'key)
          (let (param default keyword)
            (if (consp f)
                (progn
                  (setq param (car f))
                  (setq default (car (cdr f))))
              (setq param f)
              (setq default nil))
            (setq keyword (intern (concat ":" (symbol-name param))))
            (setq keys (cons (list keyword param default) keys))))))
      (setq cursor (cdr cursor)))
    (list (nreverse positional)
          (nreverse optionals)
          rest-sym
          (nreverse keys))))

(defmacro cl-defun (name formals &rest body)
  "Standalone `cl-defun' subset with positional, optional, rest and key args."
  (let* ((parsed (nelisp--parse-cl-formals formals))
         (positional (car parsed))
         (optionals (car (cdr parsed)))
         (rest-sym (car (cdr (cdr parsed))))
         (keys (car (cdr (cdr (cdr parsed))))))
    (if (null keys)
        (cons 'defun (cons name (cons formals body)))
      (let* ((rest-name (or rest-sym '--cl-keys))
             (new-formals
              (append positional
                      (if optionals (cons '&optional optionals) nil)
                      (cons '&rest (cons rest-name nil))))
             (bindings
              (mapcar
               (lambda (key-spec)
                 (let ((keyword (car key-spec))
                       (param (car (cdr key-spec)))
                       (default (car (cdr (cdr key-spec)))))
                   (list param
                         (list 'or
                               (list 'car
                                     (list 'cdr
                                           (list 'memq
                                                 (list 'quote keyword)
                                                 rest-name)))
                               default))))
               keys))
             (let-form (cons 'let* (cons bindings body))))
        (cons 'defun
              (cons name
                    (cons new-formals
                          (cons let-form nil))))))))

(unless (fboundp 'nelisp-cc-runtime-aot-module-init-plan)
  (defun nelisp-cc-runtime-aot-module-init-plan
      (init-helpers &optional custom-metadata root-descriptors closure-descriptors)
    "Standalone compile-time subset of `nelisp-cc-runtime-aot-module-init-plan'."
    (let ((helpers (mapcar (lambda (d) (plist-get d :helper)) init-helpers))
          (custom (or custom-metadata nil))
          (roots (or root-descriptors nil))
          (closures (or closure-descriptors nil)))
      (list :init-helpers init-helpers
            :helper-order helpers
            :custom-metadata custom
            :custom-by-helper
            (mapcar (lambda (d) (cons (plist-get d :helper) d)) custom)
            :root-descriptors roots
            :closure-descriptors closures))))

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
    ;; Preserve nested backquote forms for the inner macro expansion
    ;; pass.  This is enough for local macros such as generator.el's
    ;; `(cl-macrolet ... `(cps-internal-yield ,value))' body.
    (list 'quote form))
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

;;; nelisp-cl-macros.el --- cl-loop / cl-block / cl-return elisp impl  -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust-min: cl-loop family を elisp 実装として stdlib に集約。
;; (= 各 consumer (nelisp-emacs / nelisp-cc / etc.) が独自の stub を
;; defun し合うのを止めて NeLisp 上流で共通実装を持つ。)
;;
;; 提供:
;;   cl-block NAME BODY...        catch+throw 経由の名前付き block
;;   cl-return-from NAME &optional VAL   block NAME を VAL で抜ける
;;   cl-return &optional VAL      最近接の unnamed block を VAL で抜ける
;;   cl-loop CLAUSES...           Common Lisp loop の subset
;;
;; cl-loop の対応 clause:
;;   for VAR in LIST                      list iterator
;;   for VAR from N to M                  numeric inclusive
;;   for VAR from N below M               numeric exclusive
;;   for VAR = INIT then UPDATE          accumulator (deferred)
;;   with VAR = VAL                       binding
;;   do FORM …                            unconditional side-effect
;;   collect FORM                         accumulate into list
;;   sum FORM                             accumulate sum
;;   count FORM                           count truthy
;;   when COND return FORM                early-exit with FORM
;;   when COND do FORM                    conditional side-effect
;;   while COND                           continue while COND non-nil
;;   until COND                           continue until COND non-nil
;;   bodyless (= no for/with/do keyword)  infinite loop with cl-return
;;
;; cl-loop は最終的に `cl-block nil (... while ...)' に展開され、
;; `cl-return' で抜ける。

;;; Code:

;;;; --- block / return ----------------------------------------------------

(defun nelisp-cl-macros--block-tag (name)
  "Catch tag symbol used by `cl-block' NAME (default NAME = anon)."
  (intern (format "cl-block-%s" (or name "anon"))))

(defmacro cl-block (name &rest body)
  "Establish a named BLOCK, returning a value or via `cl-return-from'.
NAME is captured as a catch tag; `(cl-return-from NAME VAL)' inside
BODY immediately exits the block with VAL.  A bare `(cl-return VAL)'
targets the nearest *unnamed* block (= NAME = nil), matching CL."
  (declare (indent 1) (debug (symbolp body)))
  (let ((tag (nelisp-cl-macros--block-tag name)))
    (list 'catch (list 'quote tag) (cons 'progn body))))

(defmacro cl-return-from (name &optional val)
  "Throw VAL out of the cl-block named NAME."
  (declare (indent 1) (debug (symbolp &optional form)))
  (list 'throw (list 'quote (nelisp-cl-macros--block-tag name)) val))

(defmacro cl-return (&optional val)
  "Throw VAL out of the nearest unnamed cl-block."
  (declare (debug (&optional form)))
  (list 'cl-return-from nil val))

;;;; --- loop builder ------------------------------------------------------

(defun nelisp-cl-macros--loop-destructure-bindings (pattern source)
  "Return `let' bindings destructuring PATTERN from SOURCE."
  (let ((bindings nil)
        (cur pattern)
        (access source))
    (while (consp cur)
      (when (car cur)
        (setq bindings
              (cons (list (car cur) (list 'car access)) bindings)))
      (setq access (list 'cdr access))
      (setq cur (cdr cur)))
    (when cur
      (setq bindings (cons (list cur access) bindings)))
    (nreverse bindings)))

(defun nelisp-cl-macros--loop-wrap-body (pattern item forms)
  "Return one loop body form for PATTERN bound from ITEM and FORMS."
  (if (symbolp pattern)
      (cons 'progn forms)
    (cons 'let
          (cons (nelisp-cl-macros--loop-destructure-bindings pattern item)
                forms))))

(defun nelisp-cl-macros--loop-build (clauses)
  "Build expansion for `cl-loop' CLAUSES.

See header for supported shapes.  Returns a form that, when the
shape is unrecognised, expands to nil (= caller gets a no-op
expansion rather than a runtime error)."
  (let ((var nil) (list-form nil) (do-forms nil) (collect-form nil)
        (sum-form nil) (count-form nil) (with-bindings nil)
        (when-return-cond nil) (when-return-form nil)
        (when-do-cond nil) (when-do-forms nil)
        (when-collect-cond nil) (when-collect-form nil)
        (numeric-from nil) (numeric-to nil) (numeric-below nil)
        (while-cond nil) (until-cond nil)
        (bodyless-forms nil)
        (cur clauses) (recognised t))
    ;; Detect bodyless form: first clause is NOT a known keyword.
    (when (and clauses
               (not (memq (car clauses)
                          '(for with do collect sum count when
                                while until repeat finally return
                                named))))
      (setq bodyless-forms clauses
            cur nil
            recognised t))
    (while (and cur recognised)
      (let ((kw (car cur)))
        (cond
         ((eq kw 'for)
          (setq var (car (cdr cur)))
          (cond
           ((eq (car (cdr (cdr cur))) 'in)
            (setq list-form (car (cdr (cdr (cdr cur)))))
            (setq cur (cdr (cdr (cdr (cdr cur))))))
           ((eq (car (cdr (cdr cur))) 'from)
            (setq numeric-from (car (cdr (cdr (cdr cur)))))
            (let ((kw2 (car (cdr (cdr (cdr (cdr cur))))))
                  (val2 (car (cdr (cdr (cdr (cdr (cdr cur))))))))
              (cond
               ((eq kw2 'to)
                (setq numeric-to val2)
                (setq cur (cdr (cdr (cdr (cdr (cdr (cdr cur))))))))
               ((eq kw2 'below)
                (setq numeric-below val2)
                (setq cur (cdr (cdr (cdr (cdr (cdr (cdr cur))))))))
               (t (setq recognised nil)))))
           (t (setq recognised nil))))
         ((eq kw 'do)
          (setq do-forms (cons (car (cdr cur)) do-forms))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'collect)
          (setq collect-form (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'sum)
          (setq sum-form (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'count)
          (setq count-form (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'with)
          (let ((wname (car (cdr cur))))
            (when (eq (car (cdr (cdr cur))) '=)
              (setq with-bindings
                    (append with-bindings
                            (list (list wname (car (cdr (cdr (cdr cur))))))))
              (setq cur (cdr (cdr (cdr (cdr cur))))))))
         ((eq kw 'while)
          (setq while-cond (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'until)
          (setq until-cond (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'when)
          (let ((cond-form (car (cdr cur)))
                (next-kw (car (cdr (cdr cur))))
                (next-form (car (cdr (cdr (cdr cur))))))
            (cond
             ((eq next-kw 'return)
              (setq when-return-cond cond-form
                    when-return-form next-form
                    cur (cdr (cdr (cdr (cdr cur))))))
             ((eq next-kw 'do)
              (setq when-do-cond cond-form
                    when-do-forms (cons next-form when-do-forms)
                    cur (cdr (cdr (cdr (cdr cur)))))
              (while (and cur (eq (car cur) 'and))
                (let ((and-kw (car (cdr cur)))
                      (and-form (car (cdr (cdr cur)))))
                  (cond
                   ((eq and-kw 'do)
                    (setq when-do-forms (cons and-form when-do-forms)
                          cur (cdr (cdr (cdr cur)))))
                   ((eq and-kw 'collect)
                    (setq when-collect-cond cond-form
                          when-collect-form and-form
                          cur (cdr (cdr (cdr cur)))))
                   (t (setq recognised nil
                            cur nil))))))
             ((eq next-kw 'collect)
              (setq when-collect-cond cond-form
                    when-collect-form next-form
                    cur (cdr (cdr (cdr (cdr cur))))))
             (t (setq recognised nil)))))
         (t (setq recognised nil)))))
    (cond
     ((not recognised) nil)
     ;; Bodyless infinite loop wrapped in cl-block nil.
     (bodyless-forms
      (list 'cl-block nil
            (cons 'while
                  (cons t bodyless-forms))))
     ;; Numeric `for VAR from N {to,below} M' [sum FORM | do FORM ...]
     ;; (Task 2: thread the sum accumulator the numeric branch used to drop).
     ((and numeric-from (or numeric-to numeric-below))
      (let ((cmp (if numeric-to '<= '<))
            (limit (or numeric-to numeric-below))
            (rev nil)
            (acc (and sum-form (make-symbol "--loop-sum--"))))
        (while do-forms (setq rev (cons (car do-forms) rev))
               (setq do-forms (cdr do-forms)))
        (when acc
          (setq rev (cons (list 'setq acc (list '+ acc sum-form)) rev)))
        (list 'let (cons (list var numeric-from)
                         (if acc (cons (list acc 0) with-bindings) with-bindings))
              (list 'while (list cmp var limit)
                    (cons 'progn rev)
                    (list 'setq var (list '1+ var)))
              (if acc acc var))))
     ;; While / until plain loops (= no iterator).
     (while-cond
      (let ((rev nil))
        (while do-forms (setq rev (cons (car do-forms) rev))
               (setq do-forms (cdr do-forms)))
        (list 'let with-bindings
              (cons 'while (cons while-cond rev)))))
     (until-cond
      (let ((rev nil))
        (while do-forms (setq rev (cons (car do-forms) rev))
               (setq do-forms (cdr do-forms)))
        (list 'let with-bindings
              (cons 'while (cons (list 'not until-cond) rev)))))
     ;; `for VAR in LIST when COND return FORM' — early exit pattern.
     (when-return-cond
      (let ((tag-sym (make-symbol "--loop-tag--"))
            (result-sym (make-symbol "--loop-r--"))
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
        (list 'let (cons (list result-sym nil) with-bindings)
              (list 'catch (list 'quote tag-sym)
                    (list 'dolist (list loop-var list-form)
                          (nelisp-cl-macros--loop-wrap-body
                           var loop-var
                           (list (list 'when when-return-cond
                                       (list 'setq result-sym when-return-form)
                                       (list 'throw (list 'quote tag-sym) nil))))))
              result-sym)))
     ;; `for VAR in LIST collect FORM'
     ((or collect-form when-collect-cond)
      (let ((acc-sym (make-symbol "--loop-acc--"))
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--")))
            (body nil)
            (rev nil))
        (when collect-form
          (setq body
                (append body
                        (list (list 'setq acc-sym
                                    (list 'cons collect-form acc-sym))))))
        (when when-do-cond
          (while when-do-forms
            (setq rev (cons (car when-do-forms) rev))
            (setq when-do-forms (cdr when-do-forms)))
          (setq body
                (append body
                        (list (cons 'when
                                    (cons when-do-cond rev))))))
        (when when-collect-cond
          (setq body
                (append body
                        (list (list 'when when-collect-cond
                                    (list 'setq acc-sym
                                          (list 'cons when-collect-form acc-sym)))))))
        (list 'let (cons (list acc-sym nil) with-bindings)
              (list 'dolist (list loop-var list-form)
                    (nelisp-cl-macros--loop-wrap-body var loop-var body))
              (list 'nreverse acc-sym))))
     ;; `for VAR in LIST sum FORM'
     (sum-form
      (let ((acc-sym (make-symbol "--loop-sum--"))
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
        (list 'let (cons (list acc-sym 0) with-bindings)
              (list 'dolist (list loop-var list-form)
                    (nelisp-cl-macros--loop-wrap-body
                     var loop-var
                     (list (list 'setq acc-sym (list '+ acc-sym sum-form)))))
              acc-sym)))
     ;; `for VAR in LIST count FORM'
     (count-form
      (let ((acc-sym (make-symbol "--loop-count--"))
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
        (list 'let (cons (list acc-sym 0) with-bindings)
              (list 'dolist (list loop-var list-form)
                    (nelisp-cl-macros--loop-wrap-body
                     var loop-var
                     (list (list 'when count-form
                                 (list 'setq acc-sym (list '+ acc-sym 1))))))
              acc-sym)))
     ;; `for VAR in LIST when COND do FORM …'
     (when-do-cond
      (let ((rev nil)
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
        (while when-do-forms
          (setq rev (cons (car when-do-forms) rev))
          (setq when-do-forms (cdr when-do-forms)))
        (list 'let with-bindings
              (list 'dolist (list loop-var list-form)
                    (nelisp-cl-macros--loop-wrap-body
                     var loop-var
                     (list (cons 'when (cons when-do-cond rev))))))))
     ;; `for VAR in LIST do FORM …'
     (do-forms
      (let ((rev nil)
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
        (while do-forms (setq rev (cons (car do-forms) rev))
               (setq do-forms (cdr do-forms)))
        (list 'let with-bindings
              (list 'dolist (list loop-var list-form)
                    (nelisp-cl-macros--loop-wrap-body var loop-var rev)))))
     (t (list 'let with-bindings nil)))))

(defmacro cl-loop (&rest clauses)
  "Loop CLAUSES — minimal CL-style iteration macro.

See `nelisp-cl-macros--loop-build' commentary for supported shapes.
Patterns this stub does not recognise expand to nil."
  (declare (debug (&rest sexp)))
  (nelisp-cl-macros--loop-build clauses))

;;;; --- defstruct ---------------------------------------------------------
;;
;; Doc 50 stage 4e — `cl-defstruct' macro built on the Stage 4c
;; record primitives (`nelisp--make-record' / -ref / -set / -length /
;; -type / `recordp').  Minimal CL surface: positional + keyword
;; constructor, predicate, accessors.  Intentionally does NOT yet
;; implement: `:include' / `:type' / `:print-function' / `:copier'
;; auto-name / setf integration.  Those land alongside Stage 4d
;; (equality / setf gv) in a follow-up.
;;
;; Expansion shape for `(cl-defstruct point x y)':
;;   (progn
;;     (defun point-p (obj)
;;       (and (recordp obj) (eq (nelisp--record-type obj) 'point)))
;;     (defun make-point (&rest cl-defstruct--args)
;;       (apply 'nelisp--make-record 'point
;;              (list (nelisp-cl-macros--struct-arg :x cl-defstruct--args nil)
;;                    (nelisp-cl-macros--struct-arg :y cl-defstruct--args nil))))
;;     (defun point-x (cl-defstruct--rec) (nelisp--record-ref cl-defstruct--rec 0))
;;     (defun point-y (cl-defstruct--rec) (nelisp--record-ref cl-defstruct--rec 1))
;;     'point)
;;
;; The slot-spec helper `nelisp-cl-macros--struct-arg' is plain elisp
;; so it can be byte-compiled and reused; the macro itself just
;; assembles `defun' forms.

(defun nelisp-cl-macros--struct-arg (kw args default)
  "Look up KW (a keyword like :x) in plist ARGS, returning DEFAULT
when absent.  Used by the constructor expanded from `cl-defstruct'."
  (let ((cell (memq kw args)))
    (if cell (car (cdr cell)) default)))

(defun nelisp-cl-macros--struct-slot-name (slot-spec)
  "Return the slot symbol for SLOT-SPEC (a symbol or `(NAME DEFAULT)')."
  (if (consp slot-spec) (car slot-spec) slot-spec))

(defun nelisp-cl-macros--struct-slot-default (slot-spec)
  "Return the default-value form for SLOT-SPEC (nil if symbol-only)."
  (if (consp slot-spec) (car (cdr slot-spec)) nil))

(defun nelisp-cl-macros--defstruct-ctor-parts (arglist)
  "Return (FORMALS AUX-BINDINGS VALUE-SYMS) for constructor ARGLIST."
  (let ((cur arglist)
        (formals nil)
        (aux-bindings nil)
        (value-syms nil)
        (in-aux nil))
    (while cur
      (let ((item (car cur)))
        (cond
         ((eq item '&aux)
          (setq in-aux t))
         (in-aux
          (let ((var (if (consp item) (car item) item))
                (init (and (consp item) (consp (cdr item)) (cadr item))))
            (push (list var init) aux-bindings)
            (push var value-syms)))
         ((memq item '(&optional &rest))
          (push item formals))
         ((consp item)
          (let ((var (car item)))
            (push var formals)
            (push var value-syms)))
         (t
          (push item formals)
          (push item value-syms))))
      (setq cur (cdr cur)))
    (list (nreverse formals)
          (nreverse aux-bindings)
          (nreverse value-syms))))

(defun nelisp-cl-macros--struct-name-or-options (head)
  "Return the type symbol from HEAD (a symbol or `(NAME OPTION ...)').
OPTIONS are parsed by `nelisp-cl-macros--struct-options' separately."
  (if (consp head) (car head) head))

(defun nelisp-cl-macros--struct-options (head)
  "Return the option list from HEAD: nil for symbol, cdr for cons.
Each option is `(KEY VALUE)' (e.g. `(:copier my-copy)' or
`(:constructor nil)')."
  (if (consp head) (cdr head) nil))

(defvar nelisp-cl-macros--struct-absent
  (make-symbol "nelisp-cl-macros--struct-absent")
  "Sentinel returned by `--struct-opt' when an option key is absent.
Distinct from any user-supplied value — used to differentiate
`(:copier nil)' (= explicit disable) from no `:copier' clause at
all (= use default name `copy-NAME').")

(defun nelisp-cl-macros--struct-opt (key options)
  "Look up KEY in OPTIONS plist-of-cells.
Return the (cadr cell) when found, or
`nelisp-cl-macros--struct-absent' when no `(KEY ...)' cell exists."
  (let ((cell (assq key options)))
    (if cell (car (cdr cell)) nelisp-cl-macros--struct-absent)))

(defun nelisp-cl-macros--struct-resolve-name (name-form default-sym)
  "Resolve a `:copier'/`:constructor'-style NAME-FORM.
Returns:
  - `nelisp-cl-macros--struct-absent' → use DEFAULT-SYM (auto-generate)
  - nil (= explicit disable in option) → return nil (skip generation)
  - any other symbol → use that symbol verbatim."
  (cond
   ((eq name-form nelisp-cl-macros--struct-absent) default-sym)
   ((null name-form) nil)
   (t name-form)))

;;;; --- defstruct registry (Stage 4f-4 :include) -------------------------

(defvar nelisp-cl-macros--struct-info nil
  "Alist of (NAME . PLIST) describing every defined cl-defstruct.
PLIST has keys :slot-names (list of symbols, parent-first when
:included) and :parent (symbol or nil).  Populated both at
macro expansion time (so that `:include' can resolve parent
slots while expanding the child) and at runtime evaluation
of the macro's expansion (so that AOT-compiled callers and
predicates see the same data).  `assq' takes the most-recent
push, which keeps re-loading idempotent.")

(defvar nelisp-cl-macros--accessor-info nil
  "Alist of (ACCESSOR-SYM . INDEX) for every cl-defstruct slot accessor.
`setf' consults this at expansion time to rewrite
`(setf (ACCESSOR REC) VAL)' into `(nelisp--record-set REC INDEX VAL)'.")

(defun nelisp-cl-macros--struct-record (name parent slot-names)
  "Push (NAME . (:slot-names SLOT-NAMES :parent PARENT)) into the
runtime struct registry.  Re-pushes shadow earlier entries — the
front-of-list wins on lookup.  Also (re-)registers every accessor's
slot index in `nelisp-cl-macros--accessor-info' so `setf' can find it."
  (setq nelisp-cl-macros--struct-info
        (cons (cons name (list :slot-names slot-names :parent parent))
              nelisp-cl-macros--struct-info))
  (let ((i 0))
    (dolist (s slot-names)
      (let ((acc (intern (format "%s-%s" name s))))
        (setq nelisp-cl-macros--accessor-info
              (cons (cons acc i) nelisp-cl-macros--accessor-info)))
      (setq i (1+ i)))))

(defun nelisp-cl-macros--struct-isa (tag target)
  "Return non-nil iff TAG = TARGET or one of TAG's :include ancestors
is TARGET.  Walks `nelisp-cl-macros--struct-info' chain.  Used by
predicates of structs that have been `:included' as a parent so a
descendant record satisfies the parent predicate."
  (cond
   ((eq tag target) t)
   ((null tag) nil)
   (t
    (let ((info (cdr (assq tag nelisp-cl-macros--struct-info))))
      (let ((parent (and info (car (cdr (memq :parent info))))))
        (and parent (nelisp-cl-macros--struct-isa parent target)))))))

(defun nelisp-cl-macros--struct-lookup-slots (name)
  "Return the :slot-names list for struct NAME, or nil if unknown."
  (let ((info (cdr (assq name nelisp-cl-macros--struct-info))))
    (and info (car (cdr (memq :slot-names info))))))

(defmacro cl-defstruct (name-or-options &rest slots)
  "Define a record type and its predicate / constructor / accessors.

NAME-OR-OPTIONS is either NAME (symbol) or `(NAME OPTION ...)'.
Each SLOT is `SLOT-NAME' or `(SLOT-NAME DEFAULT)'.  Generated:
  - `NAME-p OBJECT'        predicate
  - `make-NAME &rest ARGS' constructor (keyword form: `:slot value')
  - `copy-NAME REC'        shallow copier (option `:copier')
  - `NAME-SLOT REC'        accessor (one per slot)

Supported options:
  - `(:constructor nil)'    → suppress make-NAME generation
  - `(:constructor NAME)'   → rename make-NAME
  - `(:copier nil)'         → suppress copy-NAME generation
  - `(:copier NAME)'        → rename copy-NAME
  - `(:include PARENT)'     → inherit PARENT's slots (parent-first)

Slot index assignment: positional, in declaration order.  The
record's `type_tag' is NAME (a symbol); accessors call
`nelisp--record-ref' which is 0-based and excludes the tag — the
type tag is reachable via `nelisp--record-type'.

`:include' semantics: child slots come AFTER parent slots, so the
parent's accessor indices remain valid for the child record.  The
parent's predicate continues to satisfy child records via the
runtime chain walk in `nelisp-cl-macros--struct-isa'.

Limitations: no `:type', no `setf' integration, no docstring slot
form.

Note: `(declare ...)' metadata is intentionally omitted because the
NeLisp Rust evaluator does not yet strip declare forms from macro
bodies (= Stage 4 follow-up).  Indent / edebug specs come back when
`defmacro' grows declare-handling parity with host Emacs."
  (let* ((name (nelisp-cl-macros--struct-name-or-options name-or-options))
         (options (nelisp-cl-macros--struct-options name-or-options))
         (parent-form (nelisp-cl-macros--struct-opt :include options))
         (parent (if (eq parent-form nelisp-cl-macros--struct-absent)
                     nil parent-form))
         (own-slot-names (mapcar #'nelisp-cl-macros--struct-slot-name slots))
         (own-slot-defaults
          (mapcar #'nelisp-cl-macros--struct-slot-default slots))
         (parent-slot-names
          (and parent (nelisp-cl-macros--struct-lookup-slots parent)))
         (slot-names (append parent-slot-names own-slot-names))
         (slot-defaults
          (let ((pads nil) (rem parent-slot-names))
            (while rem (setq pads (cons nil pads)) (setq rem (cdr rem)))
            (append pads own-slot-defaults)))
         (slot-default-alist
          (let ((names slot-names)
                (defaults slot-defaults)
                (out nil))
            (while names
              (push (cons (car names) (car defaults)) out)
              (setq names (cdr names)
                    defaults (cdr defaults)))
            (nreverse out)))
         (predicate (intern (format "%s-p" name)))
         (constructor-cell (assq :constructor options))
         (constructor-arglist
          (and constructor-cell (consp (cdr (cdr constructor-cell)))
               (car (cdr (cdr constructor-cell)))))
         (constructor
          (nelisp-cl-macros--struct-resolve-name
           (if constructor-cell
               (car (cdr constructor-cell))
             nelisp-cl-macros--struct-absent)
           (intern (format "make-%s" name))))
         (copier
          (nelisp-cl-macros--struct-resolve-name
           (nelisp-cl-macros--struct-opt :copier options)
           (intern (format "copy-%s" name)))))
    (when (and parent (null parent-slot-names))
      ;; Either parent doesn't exist or parent has zero slots.  The
      ;; latter is rare but legal — distinguish via registry presence.
      (unless (assq parent nelisp-cl-macros--struct-info)
        (error "cl-defstruct :include — parent struct `%s' not defined"
               parent)))
    ;; Expansion-time registry update so subsequent (cl-defstruct
    ;; (CHILD (:include NAME)) ...)  macros expanded in this same
    ;; pass can resolve our slot list.
    (nelisp-cl-macros--struct-record name parent slot-names)
    (let ((forms nil)
          (args-sym (make-symbol "cl-defstruct--args"))
          (rec-sym (make-symbol "cl-defstruct--rec"))
          (src-sym (make-symbol "cl-defstruct--src"))
          (slot-arg-forms nil)
          (copy-arg-forms nil)
          (i 0))
      ;; Build slot value-extraction forms for the constructor.
      (dolist (s slot-names)
        (let ((kw (intern (format ":%s" s)))
              (def (nth i slot-defaults)))
          (push (list 'nelisp-cl-macros--struct-arg
                      (list 'quote kw) args-sym def)
                slot-arg-forms))
        (setq i (1+ i)))
      (setq slot-arg-forms (nreverse slot-arg-forms))
      ;; Build per-slot ref forms for the copier.
      (setq i 0)
      (dolist (_s slot-names)
        (push (list 'nelisp--record-ref src-sym i) copy-arg-forms)
        (setq i (1+ i)))
      (setq copy-arg-forms (nreverse copy-arg-forms))
      ;; Runtime registry update — keeps the registry in sync with
      ;; the runtime form (matters for AOT-compiled code where the
      ;; expansion-time setq above no longer runs in fresh processes).
      (push (list 'nelisp-cl-macros--struct-record
                  (list 'quote name)
                  (list 'quote parent)
                  (list 'quote slot-names))
            forms)
      ;; Predicate form — uses --struct-isa for chain matching so
      ;; descendant records still satisfy the parent predicate when
      ;; this struct is later used as someone else's `:include'.
      (push (list 'defun predicate (list 'obj)
                  (list 'and
                        (list 'recordp 'obj)
                        (list 'nelisp-cl-macros--struct-isa
                              (list 'nelisp--record-type 'obj)
                              (list 'quote name))))
            forms)
      ;; Constructor form (keyword args by default; positional
      ;; `(:constructor NAME ARGLIST)' when requested).
      (when constructor
        (if constructor-arglist
            (let* ((ctor-parts
                    (nelisp-cl-macros--defstruct-ctor-parts
                     constructor-arglist))
                   (ctor-formals (car ctor-parts))
                   (ctor-aux-bindings (cadr ctor-parts))
                   (ctor-value-syms (caddr ctor-parts))
                   (body
                    (cons 'apply
                          (cons (list 'quote 'nelisp--make-record)
                                (cons (list 'quote name)
                                      (list
                                       (cons
                                        'list
                                        (mapcar
                                         (lambda (slot)
                                           (if (memq slot ctor-value-syms)
                                               slot
                                             (cdr (assq slot slot-default-alist))))
                                         slot-names))))))))
              (push (list 'defun constructor ctor-formals
                          (if ctor-aux-bindings
                              (list 'let ctor-aux-bindings body)
                            body))
                    forms))
          (push (list 'defun constructor (list '&rest args-sym)
                      (cons 'apply
                            (cons (list 'quote 'nelisp--make-record)
                                  (cons (list 'quote name)
                                        (list (cons 'list slot-arg-forms))))))
                forms)))
      ;; Copier form (shallow copy via record-ref / make-record).
      (when copier
        (push (list 'defun copier (list src-sym)
                    (cons 'apply
                          (cons (list 'quote 'nelisp--make-record)
                                (cons (list 'quote name)
                                      (list (cons 'list copy-arg-forms))))))
              forms))
      ;; Accessor forms — one per slot, indexed positionally.
      (setq i 0)
      (dolist (s slot-names)
        (let ((acc (intern (format "%s-%s" name s))))
          (push (list 'defun acc (list rec-sym)
                      (list 'nelisp--record-ref rec-sym i))
                forms))
        (setq i (1+ i)))
      ;; Result form: (progn DEFUN ... 'NAME).
      (cons 'progn
            (append (nreverse forms)
                    (list (list 'quote name)))))))

;; ---------------------------------------------------------------------------
;; Doc 49 Wave 7 follow-up (2026-05-22): minimal cl-lib subset wired into
;; the same module so `(require 'cl-lib)' resolves via featurep without
;; needing a separate `lisp/cl-lib.el' bake entry.  Coverage = what
;; `nelisp-aot-compiler.el' and `scripts/compile-elisp-objects.el'
;; need to run end-to-end under `nelisp --batch'.
;;
;; Already provided elsewhere (kept here for reference):
;;   `cl-defun'   — lisp/nelisp-stdlib-eval-special.el:432 (full &key)
;;   `cl-loop'    — line 230 above
;;   `cl-block' / `cl-return' / `cl-return-from' — lines 42-56
;;   `cl-defstruct' — line 352
;; ---------------------------------------------------------------------------

(defun cl-mapcar (fn seq &rest more-seqs)
  "Apply FN to corresponding elements of SEQ and MORE-SEQS, returning a list.
The walk stops at the shortest sequence.  Like Emacs `cl-mapcar'."
  (let ((all (cons seq more-seqs))
        (result nil)
        (done nil))
    (while (not done)
      (let ((heads nil) (tails nil) (any-empty nil) (cur all))
        (while (and cur (not any-empty))
          (let ((s (car cur)))
            (if (null s)
                (setq any-empty t)
              (setq heads (cons (car s) heads))
              (setq tails (cons (cdr s) tails))))
          (setq cur (cdr cur)))
        (if any-empty
            (setq done t)
          (setq result (cons (apply fn (nreverse heads)) result))
          (setq all (nreverse tails)))))
    (nreverse result)))

(defun cl-mapc (fn seq &rest more-seqs)
  "Apply FN to corresponding elements of SEQ and MORE-SEQS for side effect.
Returns SEQ (= first sequence) like Emacs `cl-mapc'."
  (let ((all (cons seq more-seqs))
        (done nil))
    (while (not done)
      (let ((heads nil) (tails nil) (any-empty nil) (cur all))
        (while (and cur (not any-empty))
          (let ((s (car cur)))
            (if (null s)
                (setq any-empty t)
              (setq heads (cons (car s) heads))
              (setq tails (cons (cdr s) tails))))
          (setq cur (cdr cur)))
        (if any-empty
            (setq done t)
          (apply fn (nreverse heads))
          (setq all (nreverse tails)))))
    seq))

(defun cl-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START up to END (default end of SEQ).
Supports proper lists only (= what `nelisp-aot-compiler.el' uses)."
  (let ((tail (nthcdr start seq))
        (n (if end (- end start) nil))
        (acc nil))
    (if (null n)
        (copy-sequence tail)
      (let ((i 0))
        (while (and (< i n) tail)
          (setq acc (cons (car tail) acc))
          (setq tail (cdr tail))
          (setq i (1+ i)))
        (nreverse acc)))))

(defun cl-remove-if-not (pred seq)
  "Return a list of SEQ elements where (PRED ELT) is non-nil.
Linear, allocates a fresh list; preserves order."
  (let ((acc nil) (cur seq))
    (while cur
      (when (funcall pred (car cur))
        (setq acc (cons (car cur) acc)))
      (setq cur (cdr cur)))
    (nreverse acc)))

(defmacro cl-labels (bindings &rest body)
  "Bind locally-recursive functions BINDINGS and run BODY.
BINDINGS = ((NAME (ARGS...) BODY...) ...).  Expands to a `let'-bound
funarg + `flet'-style cl-flet substitution so each binding can call
itself by NAME.  This is the minimal shape used by
`nelisp-aot-compiler.el' (single-binding walk-helper recursion);
sibling cross-calls within a single `cl-labels' block are NOT
supported (= would need a forward-declared placeholder set, deferred)."
  (let ((let-bindings nil)
        (defalias-forms nil)
        (unalias-forms nil))
    (dolist (b bindings)
      (let* ((name (car b))
             (fn-formals (car (cdr b)))
             (fn-body (cdr (cdr b)))
             (saved (intern (format "--cl-labels-saved-%s" name))))
        (setq let-bindings
              (cons (list saved (list 'and (list 'fboundp (list 'quote name))
                                      (list 'symbol-function (list 'quote name))))
                    let-bindings))
        (setq defalias-forms
              (cons (list 'defalias (list 'quote name)
                          (cons 'lambda (cons fn-formals fn-body)))
                    defalias-forms))
        (setq unalias-forms
              (cons (list 'if saved
                          (list 'defalias (list 'quote name) saved)
                          (list 'fmakunbound (list 'quote name)))
                    unalias-forms))))
    (list 'let (nreverse let-bindings)
          (cons 'unwind-protect
                (cons (cons 'progn (append (nreverse defalias-forms) body))
                      (nreverse unalias-forms))))))

(defmacro cl-incf (place &optional delta)
  "Increment PLACE by DELTA (default 1).
Symbol PLACE expands to `(setq PLACE (+ PLACE DELTA))'; generalised
PLACE (= cl-defstruct accessor, `car' / `cdr' / `aref' / `nth')
delegates to `setf' so the same call works on records and lists.

Note: PLACE is read TWICE in the generalised path because that
matches what `setf' supports; if PLACE has side-effects, hoist
it into a `let' first."
  (if (symbolp place)
      (list 'setq place (list '+ place (or delta 1)))
    (list 'setf place (list '+ place (or delta 1)))))

(defmacro defsubst (name args &rest body)
  "Define NAME as an inline function.  Standalone NeLisp has no
byte-compiler so defsubst is a strict synonym for `defun'."
  (cons 'defun (cons name (cons args body))))

(defun cl-every (pred seq)
  "Return non-nil iff (PRED ELT) is non-nil for every ELT in SEQ."
  (let ((all t) (cur seq))
    (while (and all cur)
      (unless (funcall pred (car cur)) (setq all nil))
      (setq cur (cdr cur)))
    all))

(defun cl-some (pred seq)
  "Return the first non-nil (PRED ELT) over SEQ, else nil.
Used by the AOT compiler's `--emit-defun' gp prologue gate
\(`(cl-some #'consp param-regs)').  Its absence made every defun-emit on
standalone NeLisp hit a void-function — which, under the void-function-
miss bug, returns garbage instead of signalling and corrupts the compile."
  (let ((res nil) (cur seq))
    (while (and cur (not res))
      (setq res (funcall pred (car cur)))
      (setq cur (cdr cur)))
    res))

;; ---------------------------------------------------------------------------
;; Doc 49 Wave 7 R6c (2026-05-22) — minimal `backquote' macro.
;;
;; The reader (`nelisp-stdlib-reader.el') desugars source-level `\`'
;; and `,' / `,@' into `(backquote FORM)' / `(comma X)' / `(comma-at X)'
;; cons forms.  Without a `backquote' macro, evaluating these dies with
;; `(void-function backquote)' — observed when loading
;; `nelisp-sexp-layout.el' whose final `defconst' uses `((NAME . ,V) ...)'.
;;
;; Scope (Minimal):
;;   `atom              =>  'atom
;;   `,X                =>  X
;;   `(A B C)           =>  (list 'A 'B 'C)
;;   `(A ,X B)          =>  (list 'A X 'B)
;;   `(A ,@X B)         =>  (append (list 'A) X (list 'B))
;;   `(A . ,X)          =>  (cons 'A X)
;;   `(A . X)           =>  (cons 'A 'X)
;; Unsupported (signal):  nested ``X, vector quasi `[A ,X B].
;; ---------------------------------------------------------------------------

(defun nelisp--bq-expand (form)
  "Return the expansion of FORM under `backquote'."
  (cond
   ((vectorp form)
    (signal 'error (list "nelisp-bq: vector quasi not supported")))
   ((not (consp form))
    (list 'quote form))
   ((eq (car form) 'comma) (cadr form))
   ((eq (car form) 'comma-at)
    (signal 'error (list "nelisp-bq: top-level ,@ not allowed")))
   ((eq (car form) 'backquote)
    ;; Preserve nested backquote forms for the inner macro expansion
    ;; pass.  This is enough for local macros such as generator.el's
    ;; `(cl-macrolet ... `(cps-internal-yield ,value))' body.
    (list 'quote form))
   (t (nelisp--bq-expand-list form))))

(defun nelisp--bq-expand-list (form)
  "Walk list FORM, producing the expansion.
Recognises both (... ,X ...) interior unquote and (... . ,X) dotted
unquote / (... . ,@X) dotted splice patterns."
  (let ((parts nil)        ; alist entries (KIND . EXPR) where KIND = list|splice
        (cur form)
        (tail-expr nil)
        (done nil)
        (has-splice nil))
    (while (and (not done) (consp cur))
      (let ((head (car cur)))
        (cond
         ;; cdr-position bare `comma' → source had `. ,X'.
         ((eq head 'comma)
          (setq tail-expr (cadr cur))
          (setq done t))
         ;; cdr-position bare `comma-at' → source had `. ,@X'.
         ((eq head 'comma-at)
          (setq tail-expr (cadr cur))
          (setq has-splice t)
          (setq done t))
         (t
          (let ((elem head))
            (cond
             ((and (consp elem) (eq (car elem) 'comma-at))
              (setq has-splice t)
              (push (cons 'splice (cadr elem)) parts))
             ((and (consp elem) (eq (car elem) 'comma))
              (push (cons 'list (cadr elem)) parts))
             (t
              (push (cons 'list (nelisp--bq-expand elem)) parts))))
          (setq cur (cdr cur))))))
    (when (and (not done) (not (null cur)) (not (consp cur)))
      (setq tail-expr (list 'quote cur)))
    (nelisp--bq-build (nreverse parts) tail-expr has-splice)))

(defun nelisp--bq-build (parts tail has-splice)
  "Build the final form from PARTS list, TAIL expression, HAS-SPLICE flag."
  (cond
   ((and (null parts) (null tail))
    (list 'quote nil))
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
          (cond
           ((eq kind 'list) (push (list 'list val) args))
           ((eq kind 'splice) (push val args))))
        (setq p (cdr p)))
      (setq args (nreverse args))
      (when tail (setq args (append args (list tail))))
      (cons 'append args)))))

(defmacro backquote (form)
  "Expand FORM as a quasiquoted template (NeLisp minimal subset).
See `nelisp--bq-expand' for the supported shapes."
  (nelisp--bq-expand form))

(unless (fboundp 'zerop) (defun zerop (n) "Return t if N is zero." (= n 0)))

;; ---------------------------------------------------------------------------
;; Wave A21-fix (2026-05-24) — cl-case / cl-position / cl-set-difference /
;; cl-gensym / cl-macrolet.  Standalone NeLisp loads `nelisp-bytecode.el'
;; which uses these five `cl-' helpers — host Emacs provides them through
;; `cl-lib.el', NeLisp ships them here so the same source compiles + runs
;; identically on both substrates (= byte-identity, Rust LOC delta = 0).
;; ---------------------------------------------------------------------------

(defmacro cl-case (expr &rest clauses)
  "Common Lisp `case' macro: dispatch on EXPR equality.
Each CLAUSE = (KEYS BODY...).  KEYS is either a single literal
(matched with `eql' = NeLisp `equal') or a list of literals
(matched with `memq'/`member'); `t' or `otherwise' matches any.
Expands to a `let' + `cond'."
  (let* ((sym (intern (format "--cl-case-%s"
                              (if (fboundp 'cl-gensym)
                                  (symbol-name (cl-gensym "v"))
                                "v"))))
         (cond-clauses
          (mapcar (lambda (clause)
                    (let ((keys (car clause)) (body (cdr clause)))
                      (cond
                       ((or (eq keys t) (eq keys 'otherwise))
                        (cons t body))
                       ((and (consp keys) (consp (cdr keys)))
                        ;; List of keys.
                        (cons (list 'or
                                    (cons 'and
                                          (mapcar (lambda (k)
                                                    (list 'eql sym
                                                          (list 'quote k)))
                                                  (list (car keys))))
                                    (list 'member sym (list 'quote keys)))
                              body))
                       ((consp keys)
                        ;; Single-element list (KEY).
                        (cons (list 'eql sym (list 'quote (car keys)))
                              body))
                       (t
                        ;; Bare symbol/atom = single key.
                        (cons (list 'eql sym (list 'quote keys))
                              body)))))
                  clauses)))
    (list 'let (list (list sym expr))
          (cons 'cond cond-clauses))))

(defun cl-position (item seq &rest keys)
  "Return the 0-based index of ITEM in SEQ (list), or nil if absent.
NeLisp minimal: list-only.  Recognised KEYS:
  :test FN   — predicate to use (default `equal').
Unknown keys are silently ignored."
  (let* ((test (or (let ((p keys) (v nil))
                     (while p
                       (when (eq (car p) :test)
                         (setq v (car (cdr p))))
                       (setq p (cdr (cdr p))))
                     v)
                   #'equal))
         (i 0) (cur seq) (found nil))
    (while (and cur (not found))
      (if (funcall test (car cur) item)
          (setq found i)
        (setq i (1+ i))
        (setq cur (cdr cur))))
    found))

(defun cl-set-difference (list1 list2)
  "Return elements of LIST1 not present in LIST2, preserving order.
NeLisp minimal: no `:test' / `:key' keywords; uses `equal'."
  (let ((acc nil) (cur list1))
    (while cur
      (unless (member (car cur) list2)
        (setq acc (cons (car cur) acc)))
      (setq cur (cdr cur)))
    (nreverse acc)))

(defvar nelisp-cl-macros--gensym-counter 0
  "Monotone counter used by `cl-gensym' for unique symbol names.")

(defun cl-gensym (&optional prefix)
  "Return a fresh uninterned symbol named PREFIX (default \"G\") + counter.
NeLisp uses `intern' (= the standalone runtime has no
`make-symbol' equivalent that yields a usable callable name)."
  (setq nelisp-cl-macros--gensym-counter
        (1+ nelisp-cl-macros--gensym-counter))
  (intern (format "%s%d"
                  (or prefix "G")
                  nelisp-cl-macros--gensym-counter)))

;; ---------------------------------------------------------------------------
;; cl-macrolet — lexical macro bindings.
;;
;; Strategy: at expansion time, walk BODY and replace each call to a
;; bound macro NAME with its expansion.  The macro body is evaluated
;; under a `let' that binds the macro's formal parameters to the raw
;; (unevaluated) argument forms — same contract as host `defmacro' /
;; `cl-macrolet'.  Result is spliced back into BODY in place of the
;; call.  Sub-forms of the call's args are walked first so nested
;; cl-macrolet calls expand inside-out.
;;
;; Walker honours common special forms: `quote' / `function' / `lambda'
;; pass their inert subforms through unchanged; other lists recurse.
;; ---------------------------------------------------------------------------

(defun nelisp-cl-macros--macrolet-expand-one (entry args)
  "Expand a single cl-macrolet call.
ENTRY = (NAME FORMALS BODY...).  ARGS = the raw (unevaluated)
argument forms from the call site.  Returns the expansion form."
  (let* ((formals (car (cdr entry)))
         (body    (cdr (cdr entry)))
         (bindings nil)
         (rest-args args)
         (rest-mode nil)
         (rest-var nil))
    (while formals
      (let ((f (car formals)))
        (cond
         ((eq f '&rest)
          (setq rest-mode t)
          (setq rest-var (car (cdr formals)))
          (setq formals nil))
         (t
          (setq bindings (cons (list f (list 'quote (car rest-args)))
                               bindings))
          (setq rest-args (cdr rest-args))
          (setq formals (cdr formals))))))
    (when rest-mode
      (setq bindings (cons (list rest-var (list 'quote rest-args))
                           bindings)))
    (eval (list 'let (nreverse bindings)
                (cons 'progn body)))))

(defun nelisp-cl-macros--macrolet-walk-bindings (bindings env)
  "Walk BINDINGS (a list of (VAR INIT) pairs or bare symbols) for `let'."
  (mapcar (lambda (b)
            (cond
             ((symbolp b) b)
             ((and (consp b) (consp (cdr b)))
              (list (car b)
                    (nelisp-cl-macros--macrolet-walk (car (cdr b)) env)))
             (t b)))
          bindings))

(defun nelisp-cl-macros--macrolet-walk-list (forms env)
  "Walk a list of FORMS."
  (mapcar (lambda (s) (nelisp-cl-macros--macrolet-walk s env)) forms))

(defun nelisp-cl-macros--macrolet-walk (form env)
  "Walk FORM, replacing calls to macros bound in ENV with their expansions.
ENV is an alist (NAME . (FORMALS BODY...))."
  (cond
   ((not (consp form)) form)
   ((eq (car form) 'quote) form)
   ((eq (car form) 'function)
    ;; Recur into the body of a lambda inside #'(lambda ...), but
    ;; leave the wrapper intact.
    (let ((arg (car (cdr form))))
      (if (and (consp arg) (eq (car arg) 'lambda))
          (list 'function
                (cons 'lambda
                      (cons (car (cdr arg))
                            (nelisp-cl-macros--macrolet-walk-list
                             (cdr (cdr arg)) env))))
        form)))
   ((eq (car form) 'lambda)
    (cons 'lambda
          (cons (car (cdr form))
                (nelisp-cl-macros--macrolet-walk-list (cdr (cdr form)) env))))
   ((or (eq (car form) 'let) (eq (car form) 'let*))
    (cons (car form)
          (cons (nelisp-cl-macros--macrolet-walk-bindings (cadr form) env)
                (nelisp-cl-macros--macrolet-walk-list (cddr form) env))))
   ((eq (car form) 'condition-case)
    (let ((var (cadr form))
          (protected (car (cddr form)))
          (handlers (cdr (cddr form))))
      (cons 'condition-case
            (cons var
                  (cons (nelisp-cl-macros--macrolet-walk protected env)
                        (mapcar (lambda (h)
                                  (if (consp h)
                                      (cons (car h)
                                            (nelisp-cl-macros--macrolet-walk-list
                                             (cdr h) env))
                                    h))
                                handlers))))))
   ((eq (car form) 'cond)
    (cons 'cond
          (mapcar (lambda (clause)
                    (if (consp clause)
                        (nelisp-cl-macros--macrolet-walk-list clause env)
                      clause))
                  (cdr form))))
   ((eq (car form) 'pcase)
    ;; (pcase EXPR (PAT BODY...)...) — EXPR is a form, PAT is literal,
    ;; each clause BODY is walked.
    (cons 'pcase
          (cons (nelisp-cl-macros--macrolet-walk (cadr form) env)
                (mapcar (lambda (clause)
                          (if (consp clause)
                              (cons (car clause)
                                    (nelisp-cl-macros--macrolet-walk-list
                                     (cdr clause) env))
                            clause))
                        (cddr form)))))
   ((eq (car form) 'setq)
    ;; Even-position elements are forms; odd-position are symbol names.
    (let ((rest (cdr form)) (out nil))
      (while rest
        (push (car rest) out)
        (setq rest (cdr rest))
        (when rest
          (push (nelisp-cl-macros--macrolet-walk (car rest) env) out)
          (setq rest (cdr rest))))
      (cons 'setq (nreverse out))))
   (t
    (let* ((head (car form))
           (entry (and (symbolp head) (assq head env))))
      (cond
       (entry
        ;; Recur into the (raw) args first so inner cl-macrolet calls
        ;; expand inside-out, then expand this call.
        (let ((walked-args (nelisp-cl-macros--macrolet-walk-list
                            (cdr form) env)))
          (nelisp-cl-macros--macrolet-walk
           (nelisp-cl-macros--macrolet-expand-one entry walked-args)
           env)))
       ((symbolp head)
        ;; Ordinary function-like call: leave head, walk args.
        (cons head
              (nelisp-cl-macros--macrolet-walk-list (cdr form) env)))
       (t
        ;; Head is itself a list (= sub-form, e.g. a binding pair).
        ;; Recurse into both head and cdr so nested macro calls expand.
        (cons (nelisp-cl-macros--macrolet-walk head env)
              (nelisp-cl-macros--macrolet-walk-list (cdr form) env))))))))

(defmacro setf (&rest pairs)
  "Generalised assignment macro (NeLisp minimal).
Each pair PLACE VAL assigns VAL to PLACE.  Supported PLACE shapes:
  - SYMBOL                 → `(setq SYMBOL VAL)'
  - (ACCESSOR REC)         where ACCESSOR is a registered cl-defstruct
                            slot accessor → `(nelisp--record-set REC I VAL)'
  - (car X)  / (cdr X)     → `(setcar X VAL)' / `(setcdr X VAL)'
  - (aref V I) / (nth I L) → `(aset V I VAL)' / `(setcar (nthcdr I L) VAL)'
  - registered simple setter → calls setter with PLACE args + VAL
  - registered struct setter → calls setter with REC + VAL
Other shapes signal a host `error' at expand time."
  (when (null pairs) (signal 'error (list "setf: empty body")))
  (let ((forms nil))
    (while pairs
      (let ((place (car pairs))
            (val (cadr pairs)))
        (setq pairs (cdr (cdr pairs)))
        (push
         (cond
          ((symbolp place)
           (list 'setq place val))
          ((and (consp place) (eq (car place) 'car))
           (list 'setcar (cadr place) val))
          ((and (consp place) (eq (car place) 'cdr))
           (list 'setcdr (cadr place) val))
          ((and (consp place) (eq (car place) 'aref))
           (list 'aset (cadr place) (caddr place) val))
          ((and (consp place) (eq (car place) 'nth))
           (list 'setcar (list 'nthcdr (cadr place) (caddr place)) val))
          ((and (consp place) (symbolp (car place))
                (get (car place) 'cl-simple-setter))
           (cons 'funcall
                 (cons (list 'quote (get (car place) 'cl-simple-setter))
                       (append (cdr place) (list val)))))
          ((and (consp place) (symbolp (car place))
                (get (car place) 'cl-struct-setter))
           (list 'funcall
                 (list 'quote (get (car place) 'cl-struct-setter))
                 (cadr place)
                 val))
          ((and (consp place) (symbolp (car place))
                (assq (car place) nelisp-cl-macros--accessor-info))
           (let ((idx (cdr (assq (car place)
                                 nelisp-cl-macros--accessor-info))))
             (list 'nelisp--record-set (cadr place) idx val)))
          (t
           (signal 'error
                   (list "setf: unsupported place"
                         (and (consp place) (car place))))))
         forms)))
    (if (cdr forms)
        (cons 'progn (nreverse forms))
      (car forms))))

(defmacro cl-macrolet (bindings &rest body)
  "Locally bind macros for the lexical scope of BODY.
BINDINGS = ((NAME (ARGS...) BODY...) ...).  Each NAME is treated as
a macro: calls `(NAME a1 a2 ...)' inside BODY are replaced at
expansion time with the result of evaluating the macro BODY with
ARGS bound to the raw (unevaluated) call-site forms.

NeLisp minimal implementation: a code walker substitutes calls
in BODY.  Macros may use backquote; expansion produces code that
naturally lexically captures whatever the surrounding `let'
bindings provide.  &rest is honoured."
  (let ((env (mapcar (lambda (b)
                       (cons (car b) (cdr b)))
                     bindings)))
    (cons 'progn
          (mapcar (lambda (s)
                    (nelisp-cl-macros--macrolet-walk s env))
                  body))))

(defun nelisp-cl-macros--symbol-macrolet-walk (form env)
  "Replace symbol references in FORM according to ENV."
  (cond
   ((symbolp form)
    (let ((cell (assq form env)))
      (if cell (cdr cell) form)))
   ((not (consp form)) form)
   ((memq (car form) '(quote function)) form)
   ((eq (car form) 'setq)
    (let ((pairs (cdr form))
          (out nil))
      (while pairs
        (let* ((place (car pairs))
               (value (cadr pairs))
               (cell (and (symbolp place) (assq place env))))
          (setq out
                (append out
                        (list (if cell (cdr cell) place)
                              (nelisp-cl-macros--symbol-macrolet-walk
                               value env)))))
        (setq pairs (cddr pairs)))
      (cons 'setq out)))
   ((memq (car form) '(let let*))
    (let ((bindings (cadr form))
          (body (cddr form))
          (shadowed nil)
          (new-bindings nil)
          new-env)
      (dolist (binding bindings)
        (let ((var (if (symbolp binding) binding (car binding))))
          (push var shadowed)
          (push (if (symbolp binding)
                    binding
                  (list var
                        (nelisp-cl-macros--symbol-macrolet-walk
                         (cadr binding) env)))
                new-bindings)))
      (setq new-env
            (let ((cur env) (acc nil))
              (while cur
                (unless (memq (caar cur) shadowed)
                  (push (car cur) acc))
                (setq cur (cdr cur)))
              (nreverse acc)))
      (cons (car form)
            (cons (nreverse new-bindings)
                  (mapcar (lambda (body-form)
                            (nelisp-cl-macros--symbol-macrolet-walk
                             body-form new-env))
                          body)))))
   (t
    (mapcar (lambda (item)
              (nelisp-cl-macros--symbol-macrolet-walk item env))
            form))))

(defmacro cl-symbol-macrolet (bindings &rest body)
  "Minimal symbol macro substitution used by generator.el CPS rewrites."
  (let ((env (mapcar (lambda (binding)
                       (cons (car binding) (cadr binding)))
                     bindings)))
    (cons 'progn
          (mapcar (lambda (body-form)
                    (nelisp-cl-macros--symbol-macrolet-walk body-form env))
                  body))))

(provide 'cl-lib)
(provide 'nelisp-cl-macros)

;; nelisp-cl-macros.el ends here
;;; nelisp-pcase.el --- pcase macro elisp implementation  -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust-min: pcase の Elisp 実装 (= Rust special form 削除に伴う migrate)。
;;
;; 対応 pattern shape:
;;   _, t, nil          ワイルドカード / 真偽リテラル
;;   :keyword           keyword 自己評価リテラル (eq 比較)
;;   integer / string   数値・文字列リテラル (equal 比較)
;;   symbol             変数 binding (常に match)
;;   (quote SYM)        symbol 等価
;;   (cons P1 P2)       cons cell 分解
;;   (or P1 P2 ...)     どれか match
;;   (and P1 P2 ...)    全部 match
;;   (pred FN)          (FN value) → 非 nil
;;   (guard EXPR)       EXPR → 非 nil
;;   (let PAT EXPR)     PAT を EXPR に対し test
;;   `(...)             backquote pattern (cons 分解 + ,SYM binding)
;;
;; pcase 本体は (let ((--v-- EXPR)) (cond (TEST1 BODY1) ...)) に展開。

;;; Code:

(defun nelisp-pcase--test (pattern value-form)
  "Build (TEST-FORM . BINDINGS) for matching PATTERN against VALUE-FORM."
  (cond
   ((eq pattern '_) (cons t nil))
   ((keywordp pattern)
    (cons (list 'eq value-form pattern) nil))
   ((or (null pattern) (eq pattern t))
    (cons (list 'eq value-form (list 'quote pattern)) nil))
   ((symbolp pattern)
    (cons t (list (list pattern value-form))))
   ((or (integerp pattern) (stringp pattern))
    (cons (list 'equal value-form pattern) nil))
   ((consp pattern)
    (let ((head (car pattern))
          (rest (cdr pattern)))
      (cond
       ((eq head 'quote)
        (cons (list 'eq value-form (list 'quote (car rest))) nil))
       ((eq head 'pred)
        (let ((fn (car rest)))
          (cons (list 'funcall (list 'function fn) value-form) nil)))
       ((eq head 'guard)
        (cons (car rest) nil))
       ((eq head 'let)
        (let* ((sub-pat (car rest))
               (sub-expr (car (cdr rest)))
               (built (nelisp-pcase--test sub-pat sub-expr)))
          (cons (car built) (cdr built))))
       ((eq head 'and)
        (nelisp-pcase--and rest value-form))
       ((eq head 'or)
        (nelisp-pcase--or rest value-form))
       ((eq head 'cons)
        (nelisp-pcase--cons rest value-form))
       ((eq head 'backquote)
        (nelisp-pcase--backquote (car rest) value-form))
       (t (cons t nil)))))
   (t (cons (list 'equal value-form (list 'quote pattern)) nil))))

(defun nelisp-pcase--and (patterns value-form)
  "Build (TEST . BINDINGS) for an `and' pattern."
  (let ((tests nil)
        (bindings nil)
        (cur patterns))
    (while cur
      (let* ((built (nelisp-pcase--test (car cur) value-form))
             (t1 (car built))
             (b1 (cdr built)))
        (setq tests (cons t1 tests))
        (setq bindings (append bindings b1)))
      (setq cur (cdr cur)))
    (cons (cons 'and (let ((rev nil))
                       (while tests
                         (setq rev (cons (car tests) rev))
                         (setq tests (cdr tests)))
                       rev))
          bindings)))

(defun nelisp-pcase--or (patterns value-form)
  "Build (TEST . BINDINGS) for an `or' pattern (no bindings)."
  (let ((tests nil)
        (cur patterns))
    (while cur
      (let* ((built (nelisp-pcase--test (car cur) value-form))
             (t1 (car built)))
        (setq tests (cons t1 tests)))
      (setq cur (cdr cur)))
    (cons (cons 'or (let ((rev nil))
                      (while tests
                        (setq rev (cons (car tests) rev))
                        (setq tests (cdr tests)))
                      rev))
          nil)))

(defun nelisp-pcase--cons (rest value-form)
  "Build (TEST . BINDINGS) for a `(cons P1 P2)' pattern."
  (let* ((p1 (car rest))
         (p2 (car (cdr rest)))
         (b1 (nelisp-pcase--test p1 (list 'car value-form)))
         (b2 (nelisp-pcase--test p2 (list 'cdr value-form))))
    (cons (list 'and
                (list 'consp value-form)
                (car b1)
                (car b2))
          (append (cdr b1) (cdr b2)))))

(defun nelisp-pcase--backquote (pat value-form)
  "Build (TEST . BINDINGS) for a backquote pattern."
  (cond
   ((and (consp pat) (eq (car pat) 'comma))
    (let ((sym (car (cdr pat))))
      (cond
       ((eq sym '_) (cons t nil))
       ((symbolp sym) (cons t (list (list sym value-form))))
       (t (nelisp-pcase--test sym value-form)))))
   ((and (consp pat) (eq (car pat) 'comma-at))
    (let ((sym (car (cdr pat))))
      (cons t (list (list sym value-form)))))
   ((consp pat)
    (let* ((head-build (nelisp-pcase--backquote
                        (car pat) (list 'car value-form)))
           (tail-build (nelisp-pcase--backquote
                        (cdr pat) (list 'cdr value-form))))
      (cons (list 'and
                  (list 'consp value-form)
                  (car head-build)
                  (car tail-build))
            (append (cdr head-build) (cdr tail-build)))))
   ((null pat)
    (cons (list 'null value-form) nil))
   (t
    (cons (list 'equal value-form (list 'quote pat)) nil))))

(defmacro pcase (expr &rest cases)
  "Dispatch EXPR through CASES.
See `nelisp-pcase--test' for supported pattern shapes.

Rust-min migration (= moved out of build-tool/src/eval/special_forms.rs)."
  (let ((value-sym (make-symbol "--pcase-value--"))
        (cond-clauses nil))
    (dolist (case cases)
      (let* ((pat (car case))
             (body (cdr case))
             (built (nelisp-pcase--test pat value-sym))
             (test (car built))
             (bindings (cdr built)))
        (push (list test
                    (if bindings
                        (cons 'let (cons bindings body))
                      (cons 'progn body)))
              cond-clauses)))
    (let ((forward nil))
      (while cond-clauses
        (setq forward (cons (car cond-clauses) forward))
        (setq cond-clauses (cdr cond-clauses)))
      (list 'let (list (list value-sym expr))
            (cons 'cond forward)))))

;; nelisp-pcase.el ends here
(unless (fboundp 'keywordp)
  (defun keywordp (x) (and (symbolp x) (let ((n (symbol-name x))) (and (> (length n) 0) (= (aref n 0) 58))))))
(unless (fboundp 'nelisp--env-globals-get-value)
  (defun nelisp--env-globals-get-value (sym)
    (nelisp--env-globals-op 'get-value sym)))
(unless (fboundp 'nelisp--env-globals-set-value)
  (defun nelisp--env-globals-set-value (sym val)
    (nelisp--env-globals-op 'set-value sym val)))
(unless (fboundp 'nelisp--env-globals-is-bound)
  (defun nelisp--env-globals-is-bound (sym)
    (nelisp--env-globals-op 'is-bound sym)))
(unless (fboundp 'nelisp--env-globals-set-constant)
  (defun nelisp--env-globals-set-constant (sym flag)
    (nelisp--env-globals-op 'set-constant sym flag)))
(unless (fboundp 'symbol-value)
  (defun symbol-value (sym)
    (nelisp--env-globals-get-value sym)))
(unless (fboundp 'boundp)
  (defun boundp (sym)
    (nelisp--env-globals-is-bound sym)))
(unless (fboundp 'set)
  (defun set (sym val)
    (nelisp--env-globals-set-value sym val)
    val))
(unless (fboundp 'defalias)
  (defun defalias (sym def &rest _)
    (if (and (symbolp def) (not (fboundp def)))
        (eval (list 'defun sym '(&rest args)
                    (list 'apply (list 'quote def) 'args)))
      (fset sym def))
    sym))
(unless (fboundp 'fmakunbound) (defun fmakunbound (sym) (fset sym nil) sym))
(unless (fboundp 'functionp) (defun functionp (x) (or (and (consp x) (eq (car x) 'lambda)) (and (symbolp x) (fboundp x)))))
(unless (fboundp 'recordp) (defun recordp (x) nil))
(unless (fboundp 'nlistp) (defun nlistp (x) (not (listp x))))
(unless (fboundp 'eql) (defun eql (a b) (if (and (numberp a) (numberp b)) (= a b) (eq a b))))
(unless (fboundp 'encode-coding-string)
  (defun encode-coding-string (str coding &optional _nocopy)
    (when (and coding (not (eq coding 'utf-8)))
      (signal 'error
              (list (format "encode-coding-string stub: only utf-8 supported, got %S"
                            coding))))
    str))
(unless (fboundp 'decode-coding-string)
  (defun decode-coding-string (str coding &optional _nocopy)
    (when (and coding (not (eq coding 'utf-8)))
      (signal 'error
              (list (format "decode-coding-string stub: only utf-8 supported, got %S"
                            coding))))
    str))
(unless (fboundp 'bufferp)
  (defun bufferp (_obj) nil))
(unless (fboundp 'set-buffer-multibyte)
  (defun set-buffer-multibyte (_flag) nil))
(unless (fboundp 'multibyte-string-p)
  (defun multibyte-string-p (obj) (stringp obj)))
(unless (fboundp 'unibyte-string-p)
  (defun unibyte-string-p (_obj) nil))
(unless (fboundp 'string-as-multibyte)
  (defun string-as-multibyte (s) s))
(unless (fboundp 'string-as-unibyte)
  (defun string-as-unibyte (s) s))
(unless (fboundp 'string-make-multibyte)
  (defun string-make-multibyte (s) s))
(unless (fboundp 'string-make-unibyte)
  (defun string-make-unibyte (s) s))
(unless (fboundp 'write-region)
  (defun write-region (start end filename &optional append _visit _lockname _mustbenew)
    (unless (stringp start)
      (signal 'wrong-type-argument (list 'stringp start)))
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (when append
      (signal 'error (list "write-region stub: APPEND not supported")))
    (let* ((bytes (cond
                   ((null end) start)
                   ((integerp end) (substring start 0 end))
                   (t (signal 'wrong-type-argument
                              (list '(or null integerp) end)))))
           (rc (wrf filename bytes)))
      (unless (= rc (length bytes))
        (signal 'error
                (list (format "write-region stub: wrf returned %S (expected %S) path=%s"
                              rc (length bytes) filename)))))
	    nil))
(defvar nelisp--with-temp-file-contents nil)
(unless (fboundp 'insert)
  (defun insert (&rest strings)
    (dolist (s strings)
      (setq nelisp--with-temp-file-contents
            (concat (or nelisp--with-temp-file-contents "") s)))
    nil))
(unless (fboundp 'with-temp-file)
  (defmacro with-temp-file (file &rest body)
    `(let ((nelisp--with-temp-file-contents ""))
       ,@body
       (write-region nelisp--with-temp-file-contents nil ,file))))
(unless (fboundp 'ignore-errors)
  (defmacro ignore-errors (&rest body)
    `(condition-case nil (progn ,@body) (error nil))))
(defvar nelisp--current-buffer nil)
(defvar nelisp--pending-processes nil)
(defvar nelisp--process-props nil)
(defvar coding-system-for-write nil)
(defvar coding-system-for-read nil)
(defvar system-type 'gnu/linux)
(defvar system-configuration "x86_64-pc-linux-gnu")
(unless (fboundp 'generate-new-buffer)
  (defun generate-new-buffer (name)
    (vector 'buffer name "" t)))
(unless (fboundp 'buffer-live-p)
  (defun buffer-live-p (buffer)
    (and (vectorp buffer)
         (eq (aref buffer 0) 'buffer)
         (aref buffer 3))))
(unless (fboundp 'kill-buffer)
  (defun kill-buffer (buffer)
    (when (and (vectorp buffer) (eq (aref buffer 0) 'buffer))
      (aset buffer 3 nil))
    nil))
(unless (fboundp 'with-current-buffer)
  (defmacro with-current-buffer (buffer &rest body)
    `(let ((nelisp--current-buffer ,buffer))
       ,@body)))
(unless (fboundp 'with-temp-buffer)
  (defmacro with-temp-buffer (&rest body)
    `(let ((nelisp--current-buffer (generate-new-buffer " *temp*")))
       ,@body)))
(when (fboundp 'rdf)
  ;; Keep the public compatibility name on `rdf' so callers get the same
  ;; value-returning read path used by the short builtin.
  (fset 'nelisp--syscall-read-file (symbol-function 'rdf)))
(unless (fboundp 'buffer-string)
  (defun buffer-string ()
    (if (and (vectorp nelisp--current-buffer)
             (eq (aref nelisp--current-buffer 0) 'buffer))
        (aref nelisp--current-buffer 2)
      "")))
(unless (fboundp 'insert-file-contents)
  (defun insert-file-contents (filename &rest _args)
    (let ((contents (or (nelisp--syscall-read-file filename) "")))
      (when (and (vectorp nelisp--current-buffer)
                 (eq (aref nelisp--current-buffer 0) 'buffer))
        (aset nelisp--current-buffer 2
              (concat (aref nelisp--current-buffer 2) contents)))
      (list filename (length contents)))))
(unless (fboundp 'insert-file-contents-literally)
  (defun insert-file-contents-literally (filename &rest args)
    (apply #'insert-file-contents filename args)))
(unless (fboundp 'processp)
  (defun processp (process)
    (or (and (vectorp process) (eq (aref process 0) 'process))
        (and (fboundp 'nelisp-process-object-p)
             (nelisp-process-object-p process)))))
(unless (fboundp 'process-get)
  (defun process-get (process key)
    (cond
     ((and (vectorp process) (eq (aref process 0) 'process))
      (cdr (assq key (aref process 4))))
     ((and (fboundp 'nelisp-process-object-p)
           (nelisp-process-object-p process))
      (cdr (assq key (cdr (assq process nelisp--process-props)))))
     (t nil))))
(unless (fboundp 'process-put)
  (defun process-put (process key value)
    (cond
     ((and (vectorp process) (eq (aref process 0) 'process))
      (let ((cell (assq key (aref process 4))))
        (if cell
            (setcdr cell value)
          (aset process 4 (cons (cons key value) (aref process 4))))))
     ((and (fboundp 'nelisp-process-object-p)
           (nelisp-process-object-p process))
      (let ((entry (assq process nelisp--process-props)))
        (unless entry
          (setq entry (cons process nil))
          (setq nelisp--process-props
                (cons entry nelisp--process-props)))
        (let ((cell (assq key (cdr entry))))
          (if cell
              (setcdr cell value)
            (setcdr entry (cons (cons key value) (cdr entry))))))))
    value))
(unless (fboundp 'nelisp--process-status-symbol)
  (defun nelisp--process-status-symbol (status)
    (cond
     ((eq status 'run) 'run)
     ((eq status 'exit) 'exit)
     ((and (integerp status) (= status 0)) 'run)
     ((and (integerp status) (= status 1)) 'exit)
     (t 'exit))))
(unless (fboundp 'process-status)
  (defun process-status (process)
    (cond
     ((and (fboundp 'nelisp-process-object-p)
           (nelisp-process-object-p process))
      (nelisp--process-status-symbol (nelisp-process-status process)))
     ((and (vectorp process) (eq (aref process 0) 'process))
      (aref process 2))
     (t (signal 'wrong-type-argument (list 'processp process))))))
(unless (fboundp 'process-exit-status)
  (defun process-exit-status (process)
    (cond
     ((and (fboundp 'nelisp-process-object-p)
           (nelisp-process-object-p process))
      (nelisp-process-exit-status process))
     ((and (vectorp process) (eq (aref process 0) 'process))
      (aref process 3))
     (t (signal 'wrong-type-argument (list 'processp process))))))
(unless (fboundp 'process-live-p)
  (defun process-live-p (process)
    (eq (process-status process) 'run)))
(unless (fboundp 'delete-process)
  (defun delete-process (process)
    (when (and (fboundp 'nelisp-process-object-p)
               (nelisp-process-object-p process)
               (fboundp 'nelisp-process-delete))
      (nelisp-process-delete process))
    nil))
(unless (fboundp 'kill-process)
  (defun kill-process (process &optional _current-group)
    (delete-process process)))
(unless (fboundp 'executable-find)
  (defun executable-find (command &optional _remote)
    (unless (and (stringp command) (> (length command) 0))
      (signal 'wrong-type-argument (list 'stringp command)))
    (if (string-match-p "/" command)
        (and (file-exists-p command) command)
      (let* ((separator (if (boundp 'path-separator) path-separator ":"))
             (dirs (split-string
                    (or (getenv "PATH") "/usr/local/bin:/usr/bin:/bin")
                    separator))
             found)
        (while (and dirs (not found))
          (let ((path (expand-file-name command
                                        (if (equal (car dirs) "")
                                            "."
                                          (car dirs)))))
            (when (file-exists-p path)
              (setq found path)))
          (setq dirs (cdr dirs)))
        found))))
(unless (fboundp 'make-process)
  (defun make-process (&rest plist)
    (let* ((name (or (plist-get plist :name) "process"))
           (command (plist-get plist :command))
           (stderr-buffer (plist-get plist :stderr))
           (sentinel (plist-get plist :sentinel))
           (resolved (and command
                          (cons (or (executable-find (car command))
                                    (car command))
                                (cdr command)))))
      (unless command
        (signal 'wrong-type-argument (list 'listp command)))
      (if (fboundp 'nelisp-process-start)
          (let ((proc (apply #'nelisp-process-start resolved)))
            (process-put proc :name name)
            (process-put proc :sentinel sentinel)
            (process-put proc :stderr stderr-buffer)
            (setq nelisp--pending-processes
                  (cons proc nelisp--pending-processes))
            proc)
        (let ((proc (vector 'process name 'run -1 nil sentinel "")))
          (setq nelisp--pending-processes
                (cons proc nelisp--pending-processes))
          proc)))))
(unless (fboundp 'nelisp-make-process)
  (defun nelisp-make-process (&rest plist)
    (apply #'make-process plist)))
(unless (fboundp 'call-process)
  (defun call-process (program &optional infile destination display &rest args)
    (let ((resolved (or (executable-find program) program)))
      (if (eq destination t)
          (let ((out-file (make-temp-file "nelisp-call-process-out-")))
            (unwind-protect
                (let ((rc (apply #'nelisp-process-call-process
                                 resolved infile out-file display args)))
                  (when nelisp--current-buffer
                    (insert-file-contents out-file))
                  rc)
              (ignore-errors (delete-file out-file))))
        (apply #'nelisp-process-call-process
               resolved infile destination display args)))))
(unless (fboundp 'start-process)
  (defun start-process (name buffer program &rest args)
    (make-process :name name :buffer buffer :command (cons program args))))
(unless (fboundp 'accept-process-output)
  (defun accept-process-output (&rest _args)
    (let ((pending nelisp--pending-processes))
      (setq nelisp--pending-processes nil)
      (dolist (proc pending)
        (when (and (fboundp 'nelisp-process-object-p)
                   (nelisp-process-object-p proc)
                   (fboundp 'nelisp-process-wait))
          (nelisp-process-wait proc))
        (when (and (vectorp proc) (eq (aref proc 0) 'process))
          (aset proc 2 'exit)
          (aset proc 3 0))
        (let ((sentinel (process-get proc :sentinel)))
          (when sentinel
            (funcall sentinel proc "finished\n")))))
    t))
(unless (fboundp 'set-file-modes)
  (defun set-file-modes (filename mode &optional _flag)
    "Apply MODE to FILENAME via chmod(2) when a syscall primitive exists.
No-ops on substrates without `nelisp--syscall-path-int' (the historic stub)."
    (when (fboundp 'nelisp--syscall-path-int)
      (let ((rc (nelisp--syscall-path-int 90 filename mode)))   ; chmod
        (unless (= rc 0)
          (error "set-file-modes: rc=%S %s" rc filename))))
    nil))

;; --- Wave-2 (C): sort (stable merge sort, 2-arg PREDICATE form) ----------
;; (sort LIST PREDICATE) -> a new list ordered by PREDICATE (a < b).  Stable.
;; Non-destructive (builds fresh cons cells) to avoid setcar/setcdr churn on
;; the caller's data under the standalone GC.  Only the LIST + 2-arg form is
;; supported (the static linker calls `(sort (copy-sequence units) #'pred)').
(unless (fboundp 'sort)
  (progn
    (defun nelisp-stdlib--merge (a b pred)
      (let ((acc nil))
        (while (and a b)
          (if (funcall pred (car b) (car a))
              (progn (setq acc (cons (car b) acc)) (setq b (cdr b)))
            (setq acc (cons (car a) acc)) (setq a (cdr a))))
        (while a (setq acc (cons (car a) acc)) (setq a (cdr a)))
        (while b (setq acc (cons (car b) acc)) (setq b (cdr b)))
        (nreverse acc)))
    (defun nelisp-stdlib--msort (list pred)
      (if (or (null list) (null (cdr list)))
          list
        ;; split into halves via slow/fast pointer
        (let ((slow list) (fast (cdr list)) (left nil))
          (while (and fast (cdr fast))
            (setq fast (cdr (cdr fast)))
            (setq left (cons (car slow) left))
            (setq slow (cdr slow)))
          ;; `left' now holds the reversed first half (excludes slow); take
          ;; slow's car too, then the rest is the right half.
          (setq left (nreverse (cons (car slow) left)))
          (let ((right (cdr slow)))
            (nelisp-stdlib--merge
             (nelisp-stdlib--msort left pred)
             (nelisp-stdlib--msort right pred)
             pred)))))
    (defun sort (seq pred)
      (nelisp-stdlib--msort seq pred))))

;; --- Wave-2 (C): symbol plists (put/get) + define-error -----------------
;; The standalone reader has no per-symbol plist slot, so model the global
;; symbol-plist store as one hash-table keyed by symbol (gethash/puthash use
;; symbol-eq on the name).  Each value is a property plist (NAME VAL NAME VAL...).
(unless (boundp 'nelisp-stdlib--symbol-plists)
  (setq nelisp-stdlib--symbol-plists (make-hash-table)))
(unless (fboundp 'symbol-plist)
  (defun symbol-plist (sym) (gethash sym nelisp-stdlib--symbol-plists)))
(unless (fboundp 'setplist)
  (defun setplist (sym plist) (puthash sym plist nelisp-stdlib--symbol-plists) plist))
(unless (fboundp 'get)
  (defun get (sym prop) (plist-get (gethash sym nelisp-stdlib--symbol-plists) prop)))
(unless (fboundp 'put)
  (defun put (sym prop val)
    (puthash sym
             (plist-put (gethash sym nelisp-stdlib--symbol-plists) prop val)
             nelisp-stdlib--symbol-plists)
    val))
;; define-error NAME MESSAGE &optional PARENT: register an error symbol.  In
;; real elisp this sets `error-conditions'/`error-message' on NAME's plist so
;; condition-case can match the hierarchy.  Minimal LOAD-correct version: store
;; the message + the parent's conditions (PARENT defaults to `error') under the
;; conventional plist props.  No-op-safe if the matcher never consults them.
(unless (fboundp 'define-error)
  (defun define-error (name message &optional parent)
    (let* ((parent (or parent 'error))
           (conditions
            (if (consp parent)
                (apply #'append
                       (mapcar (lambda (p) (get p 'error-conditions)) parent))
              (get parent 'error-conditions))))
      (put name 'error-conditions (cons name conditions))
      (put name 'error-message message)
      name)))
;; Seed the root `error' condition so derived errors inherit it.
(unless (get 'error 'error-conditions)
  (put 'error 'error-conditions (list 'error))
  (put 'error 'error-message "error"))
(defmacro cl-loop (&rest clauses) (nelisp-cl-macros--loop-build clauses))

;; --- Doc 143: wire the elisp Sexp printer into the reader runtime ---------
;; prin1-to-string / prin1 were void in the reader (lisp/nelisp-stdlib-prn.el
;; was never bootstrapped here), so all print-then-read roundtrips failed with
;; "Wrong type argument".  Functions below are copied verbatim from
;; lisp/nelisp-stdlib-prn.el; deps char-to-string/aref/length/concat/nreverse/
;; number-to-string/substring/symbol-name are reader primitives, string-search
;; + the record accessors are added here.

(unless (fboundp 'string-search)
  (defun string-search (needle haystack &optional start)
    (let ((nl (length needle)) (hl (length haystack)) (i (or start 0)) (found nil))
      (if (= nl 0) i
        (while (and (not found) (<= (+ i nl) hl))
          (if (string= needle (substring haystack i (+ i nl)))
              (setq found i)
            (setq i (1+ i))))
        found))))

;; Record primitives backing cl-defstruct.  Representation = a plain
;; vector `[TAG slot0 slot1 ...]': index 0 is the type tag, slots are
;; 1-based in the vector but 0-based / tag-excluded through the
;; -record-ref/-set API (the contract cl-defstruct accessors assume,
;; per the macro docstring "0-based and excludes the tag").
(unless (fboundp 'nelisp--make-record)
  (defun nelisp--make-record (type-tag &rest slots)
    "Build a record vector [TYPE-TAG . SLOTS] for cl-defstruct."
    (apply 'vector type-tag slots)))
(unless (fboundp 'nelisp--record-type)
  (defun nelisp--record-type (rec) (aref rec 0)))
(unless (fboundp 'nelisp--record-length)
  ;; Total vector length = 1 tag + N slots (matches nelisp--prn-record).
  (defun nelisp--record-length (rec) (length rec)))
;; Override the older tag-inclusive -ref with the tag-excluded 0-based
;; form the cl-defstruct accessors expect; (defun is unconditional so a
;; previously-bound buggy version is replaced.)
(defun nelisp--record-ref (rec i) (aref rec (1+ i)))
(unless (fboundp 'nelisp--record-set)
  (defun nelisp--record-set (rec i val) (aset rec (1+ i) val) val))
;; A record is represented as a vector; the real discrimination is the
;; type-tag check inside cl-defstruct predicates, so a permissive
;; vectorp-based recordp is sufficient here (replaces the nil stub).
(defun recordp (x) (vectorp x))

;; Hash-table predicate + iteration for the reader's builtin hash table.
;; The builtin `make-hash-table' returns the cons pair (MARKER . ALIST) where
;; MARKER is the integer 0 and ALIST is ((KEY . VALUE) ...).  `make-hash-table'
;; / `gethash' / `puthash' / `hash-table-count' ship as native builtins, but
;; `maphash' ships only as a no-op stub and `hash-table-p' is absent -- an
;; incomplete substrate, not a minimal one.  Complete it here in the core
;; stdlib (these are the ops over the core-owned representation): the elisp
;; `maphash' overrides the stub, and `hash-table-p' keys off the integer-0 car
;; (an alist has a cons car, a plist a keyword car, so the discrimination is
;; clean for the shapes Elisp passes to `hash-table-p').
(defun hash-table-p (x)
  (and (consp x) (integerp (car x)) (eq (car x) 0)
       (let ((c (cdr x))) (or (null c) (and (consp c) (consp (car c)))))))
(defun maphash (fn table)
  (let ((node (cdr table)))
    (while (consp node)
      (let ((entry (car node)))
        (when (consp entry) (funcall fn (car entry) (cdr entry))))
      (setq node (cdr node)))
    nil))

(defun nelisp--prn-chunks-add (state chunk)
  (let ((cell (cons chunk nil)))
    (if (car state)
        (setcdr (cdr state) cell)
      (setcar state cell))
    (setcdr state cell)
    state))

(defun nelisp--prn-chunks-string (state)
  (apply 'concat (car state)))

(defun nelisp--prn-string-escaped (s)
  (let ((chunks (cons nil nil)) (i 0) (n (length s)))
    (while (< i n)
      (let ((c (aref s i)))
        (cond
         ((= c 34) (nelisp--prn-chunks-add chunks "\\\""))
         ((= c 92) (nelisp--prn-chunks-add chunks "\\\\"))
         ((= c 10) (nelisp--prn-chunks-add chunks "\\n"))
         ((= c 13) (nelisp--prn-chunks-add chunks "\\r"))
         ((= c 9)  (nelisp--prn-chunks-add chunks "\\t"))
         (t        (nelisp--prn-chunks-add chunks (char-to-string c)))))
      (setq i (1+ i)))
    (nelisp--prn-chunks-string chunks)))

(defun nelisp--prn-float (x)
  (let ((s (number-to-string x)))
    (cond
     ((string= s "inf") s)
     ((string= s "-inf") s)
     ((string= s "NaN") s)
     (t
      (let ((dot (string-search "." s))
            (eee (or (string-search "e" s) (string-search "E" s))))
        (cond
         (eee s)
         ((null dot) (concat s ".0"))
         (t
          (let ((i (1- (length s))))
            (while (and (> i (1+ dot)) (eq (aref s i) ?0))
              (setq i (1- i)))
            (substring s 0 (1+ i))))))))))

(defun nelisp--prn-reader-macro-abbrev (lst escape)
  (when (and (consp lst) (symbolp (car lst))
             (consp (cdr lst)) (null (cdr (cdr lst))))
    (let* ((tag-name (symbol-name (car lst)))
           (arg (car (cdr lst)))
           (prefix (cond ((string= tag-name "quote")     "'")
                         ((string= tag-name "function")  "#'")
                         ((string= tag-name "backquote") "`")
                         ((string= tag-name "comma")     ",")
                         ((string= tag-name "comma-at")  ",@")
                         (t nil))))
      (when prefix
        (concat prefix (nelisp--prn-to-string arg escape))))))

(defun nelisp--prn-list-body (lst escape)
  (let ((chunks (cons nil nil)) (cur lst) (first t))
    (while (consp cur)
      (unless first (nelisp--prn-chunks-add chunks " "))
      (nelisp--prn-chunks-add chunks
                              (nelisp--prn-to-string (car cur) escape))
      (setq first nil)
      (setq cur (cdr cur)))
    (unless (null cur)
      (nelisp--prn-chunks-add chunks " . ")
      (nelisp--prn-chunks-add chunks (nelisp--prn-to-string cur escape)))
    (nelisp--prn-chunks-string chunks)))

(defun nelisp--prn-vector (vec escape)
  (let ((n (length vec)) (chunks (cons nil nil)))
    (nelisp--prn-chunks-add chunks "[")
    (let ((i 0))
      (while (< i n)
        (when (> i 0) (nelisp--prn-chunks-add chunks " "))
        (nelisp--prn-chunks-add chunks
                                (nelisp--prn-to-string (aref vec i) escape))
        (setq i (1+ i))))
    (nelisp--prn-chunks-add chunks "]")
    (nelisp--prn-chunks-string chunks)))

(defun nelisp--prn-record (rec escape)
  (let ((tag (nelisp--record-type rec)) (n (nelisp--record-length rec))
        (chunks (cons nil nil)))
    (nelisp--prn-chunks-add chunks "#s(")
    (nelisp--prn-chunks-add chunks (nelisp--prn-to-string tag escape))
    (let ((i 0))
      (while (< i (1- n))
        (nelisp--prn-chunks-add chunks " ")
        (nelisp--prn-chunks-add
         chunks (nelisp--prn-to-string (nelisp--record-ref rec i) escape))
        (setq i (1+ i))))
    (nelisp--prn-chunks-add chunks ")")
    (nelisp--prn-chunks-string chunks)))

(defun nelisp--prn-to-string (obj escape)
  (cond
   ((null obj) "nil")
   ((eq obj t) "t")
   ((integerp obj) (number-to-string obj))
   ((floatp obj)   (nelisp--prn-float obj))
   ((symbolp obj)  (symbol-name obj))
   ((stringp obj)
    (if escape (concat "\"" (nelisp--prn-string-escaped obj) "\"") obj))
   ((consp obj)
    (or (nelisp--prn-reader-macro-abbrev obj escape)
        (concat "(" (nelisp--prn-list-body obj escape) ")")))
   ((vectorp obj) (nelisp--prn-vector obj escape))
   ((recordp obj) (nelisp--prn-record obj escape))
   (t (format "#<unprintable %S>" obj))))

(unless (fboundp 'prin1-to-string)
  (defun prin1-to-string (object) (nelisp--prn-to-string object t)))

;; --- Doc 143: minimal read-from-string for the reader runtime -------------
;; Recursive-descent parser for the core sexp grammar (int/float/symbol/string/
;; list/dotted/vector/quote forms).  Records (#s) are out of scope (no record
;; constructor primitive yet).  Deps: aref/length/substring/intern/
;; string-to-number/vector/cons/setcdr/char-to-string -- all reader primitives.
(defun nelisp--rd-skip-ws (s i n)
  (let ((go t))
    (while go
      (setq go nil)
      (while (and (< i n)
                  (let ((c (aref s i)))
                    (or (= c 32) (= c 9) (= c 10) (= c 13) (= c 12))))
        (setq i (1+ i)))
      (when (and (< i n) (= (aref s i) 59)) ; ;
        (while (and (< i n) (not (= (aref s i) 10))) (setq i (1+ i)))
        (setq go t)))
    i))

(defun nelisp--rd-atom-end (s i n)
  (let ((stop nil))
    (while (and (< i n) (not stop))
      (let ((c (aref s i)))
        (if (or (= c 32) (= c 9) (= c 10) (= c 13) (= c 12)
                (= c 40) (= c 41) (= c 91) (= c 93)
                (= c 34) (= c 39) (= c 96) (= c 44) (= c 59))
            (setq stop t)
          (setq i (1+ i)))))
    i))

(defun nelisp--rd-numeric-token-p (tok)
  (let ((n (length tok)) (i 0) (seen-digit nil) (ok t))
    (when (and (> n 0) (let ((c (aref tok 0))) (or (= c 43) (= c 45))))
      (setq i 1))
    (when (= i n) (setq ok nil))
    (while (and ok (< i n))
      (let ((c (aref tok i)))
        (cond ((and (>= c 48) (<= c 57)) (setq seen-digit t))
              ((or (= c 46) (= c 101) (= c 69) (= c 43) (= c 45)) nil)
              (t (setq ok nil))))
      (setq i (1+ i)))
    (and ok seen-digit)))

(defun nelisp--rd-unescape (body)
  (let ((out "") (i 0) (n (length body)))
    (while (< i n)
      (let ((c (aref body i)))
        (if (and (= c 92) (< (1+ i) n))
            (let ((d (aref body (1+ i))))
              (setq out (concat out (cond ((= d 110) "\n") ((= d 116) "\t")
                                          ((= d 114) "\r") (t (char-to-string d)))))
              (setq i (+ i 2)))
          (setq out (concat out (char-to-string c)))
          (setq i (1+ i)))))
    out))

(defun nelisp--rd-one (s i n)
  (setq i (nelisp--rd-skip-ws s i n))
  (if (>= i n) (cons nil i)
    (let ((c (aref s i)))
      (cond
       ((= c 34) ; "
        (let ((j (1+ i)) (started (1+ i)))
          (while (and (< j n) (not (= (aref s j) 34)))
            (if (= (aref s j) 92) (setq j (+ j 2)) (setq j (1+ j))))
          (cons (nelisp--rd-unescape (substring s started j)) (1+ j))))
       ((= c 40) ; (
        (let ((items nil) (k (1+ i)) (done nil) (tail nil) (has-tail nil))
          (while (not done)
            (setq k (nelisp--rd-skip-ws s k n))
            (cond
             ((>= k n) (setq done t))
             ((= (aref s k) 41) (setq k (1+ k)) (setq done t))
             ((and (= (aref s k) 46) (< (1+ k) n)
                   (let ((nc (aref s (1+ k)))) (or (= nc 32) (= nc 9) (= nc 10) (= nc 13))))
              (let ((r (nelisp--rd-one s (1+ k) n)))
                (setq tail (car r) has-tail t)
                (setq k (nelisp--rd-skip-ws s (cdr r) n))
                (when (and (< k n) (= (aref s k) 41)) (setq k (1+ k)))
                (setq done t)))
             (t (let ((r (nelisp--rd-one s k n)))
                  (setq items (cons (car r) items) k (cdr r))))))
          (let ((lst (nreverse items)))
            (when has-tail
              (if (null lst) (setq lst tail)
                (let ((cur lst)) (while (cdr cur) (setq cur (cdr cur))) (setcdr cur tail))))
            (cons lst k))))
       ((= c 91) ; [
        (let ((items nil) (k (1+ i)) (done nil))
          (while (not done)
            (setq k (nelisp--rd-skip-ws s k n))
            (cond ((>= k n) (setq done t))
                  ((= (aref s k) 93) (setq k (1+ k)) (setq done t))
                  (t (let ((r (nelisp--rd-one s k n)))
                       (setq items (cons (car r) items) k (cdr r))))))
          (cons (apply #'vector (nreverse items)) k)))
       ((= c 39) (let ((r (nelisp--rd-one s (1+ i) n))) (cons (list 'quote (car r)) (cdr r))))
       ((= c 96) (let ((r (nelisp--rd-one s (1+ i) n))) (cons (list 'backquote (car r)) (cdr r))))
       ((= c 44)
        (if (and (< (1+ i) n) (= (aref s (1+ i)) 64))
            (let ((r (nelisp--rd-one s (+ i 2) n))) (cons (list 'comma-at (car r)) (cdr r)))
          (let ((r (nelisp--rd-one s (1+ i) n))) (cons (list 'comma (car r)) (cdr r)))))
       ((= c 35) ; #
        (if (and (< (1+ i) n) (= (aref s (1+ i)) 39))
            (let ((r (nelisp--rd-one s (+ i 2) n))) (cons (list 'function (car r)) (cdr r)))
          (let* ((e (nelisp--rd-atom-end s i n))) (cons (intern (substring s i e)) e))))
       (t
        (let* ((e (nelisp--rd-atom-end s i n)) (tok (substring s i e)))
          (cons (if (nelisp--rd-numeric-token-p tok) (string-to-number tok) (intern tok)) e)))))))

(unless (fboundp 'read-from-string)
  (defun read-from-string (string &optional start end)
    (let* ((base (or start 0))
           (s (if (or start end) (substring string base (or end (length string))) string))
           (r (nelisp--rd-one s 0 (length s))))
      (cons (car r) (+ base (cdr r))))))

(unless (fboundp 'read)
  (defun read (&optional stream)
    (if (stringp stream) (car (read-from-string stream))
      (signal 'error (list "read: only string streams supported")))))

;; Doc 152 gate-G: give the standard built-in error symbols their
;; `error-conditions' so a `(condition-case ... (error H))' handler matches
;; them.  The standalone reader's condition-case matcher checks membership of
;; the handler condition in the signalled symbol's `error-conditions'; without
;; this, a `void-function' (undefined-function call), `wrong-type-argument',
;; etc. is trapped only by an exact-symbol clause, never the catch-all `error'
;; clause that ERT and most code rely on -- so one such signal aborts an
;; otherwise-trappable run (it blocked the anvil-pkg ERT suite at test #0).
;; Mirrors Emacs subr.el's define-error chain (symbol first, `error' last).
(put 'error 'error-conditions '(error))
(put 'quit 'error-conditions '(quit))
(put 'void-function 'error-conditions '(void-function error))
(put 'void-variable 'error-conditions '(void-variable error))
(put 'wrong-type-argument 'error-conditions '(wrong-type-argument error))
(put 'args-out-of-range 'error-conditions '(args-out-of-range error))
(put 'wrong-number-of-arguments 'error-conditions '(wrong-number-of-arguments error))
(put 'invalid-function 'error-conditions '(invalid-function error))
(put 'arith-error 'error-conditions '(arith-error error))
(put 'end-of-file 'error-conditions '(end-of-file error))
(put 'file-error 'error-conditions '(file-error error))
(put 'file-missing 'error-conditions '(file-missing file-error error))
(put 'setting-constant 'error-conditions '(setting-constant error))
(put 'user-error 'error-conditions '(user-error error))

;; Doc 152 gate-G: polyfill standard Emacs builtins missing from standalone
;; NeLisp so the anvil-pkg ERT suite's helpers (with-mock, registry-clear, ...)
;; run for real instead of signalling void-function.  All guarded so a real
;; builtin (host Emacs / future NeLisp primitive) always wins.
(defvar load-path nil)
(defvar features nil)
(defvar nelisp--environment nil)
(unless (fboundp 'getenv)
  (defun getenv (variable)
    (cdr (assoc variable nelisp--environment))))
(unless (fboundp 'setenv)
  (defun setenv (variable value &optional _substitute)
    (let ((cell (assoc variable nelisp--environment)))
      (if value
          (if cell
              (setcdr cell value)
            (setq nelisp--environment
                  (cons (cons variable value) nelisp--environment)))
        (let ((out nil)
              (tail nelisp--environment))
          (while tail
            (unless (equal (car (car tail)) variable)
              (setq out (cons (car tail) out)))
            (setq tail (cdr tail)))
          (setq nelisp--environment (nreverse out)))))
    value))
(defvar nelisp--temp-name-counter 0)
(unless (fboundp 'make-temp-name)
  (defun make-temp-name (prefix)
    (setq nelisp--temp-name-counter (1+ nelisp--temp-name-counter))
    (format "%s%d-%d" prefix nelisp--temp-name-counter (length prefix))))
;; File-system ops via the reader's path-syscall builtins (nelisp--syscall-path
;; = syscall(NR, cpath); -path-int = syscall(NR, cpath, INT)).  x86_64 NRs:
;; access=21, unlink=87, mkdir=83.  Returns 0 on success / -errno.
;;
;; nelisp--syscall-stat: pure-elisp reimplementation on top of
;; nelisp--syscall-path-int (access(2), NR=21).  The Rust bi_syscall_stat shim
;; uses extern-call stat (libc symbol), which hangs on the standalone reader
;; (no libc link; the combiner stashes a WTA and aborts the caller).  This
;; fallback uses the access(2) trichotomy instead:
;;   - access(path, F_OK) != 0              -> 'absent
;;   - access(concat(path, "/"), F_OK) == 0 -> 'directory  (trailing slash is
;;                                              accepted by a dir but returns
;;                                              ENOTDIR for a regular file)
;;   - else                                 -> 'file
;; Installed unconditionally when nelisp--syscall-path-int is fboundp (=
;; standalone reader binary); uses fset to shadow the broken deferred builtin
;; registration that makes fboundp return t but causes the combiner to abort.
;; On host Emacs, nelisp--syscall-path-int is not fboundp, so no shadowing.
(when (fboundp 'nelisp--syscall-path-int)
  (fset 'nelisp--syscall-stat
        (lambda (path)
          (if (not (= 0 (nelisp--syscall-path-int 21 path 0)))
              'absent
            (if (= 0 (nelisp--syscall-path-int 21 (concat path "/") 0))
                'directory
              'file)))))
;; nelisp--syscall-readdir: pure-elisp reimplementation on top of the
;; working `nelisp--syscall-readdir-names' builtin.  The Rust-side
;; `nelisp--syscall-readdir' is a CLASS-2 deferred builtin on the standalone
;; reader (the combiner stashes a WTA signal and aborts the caller), making
;; `directory-files' (which calls it) hang indefinitely.
;;
;; `nelisp--syscall-readdir-names' IS a proper dispatch-armed builtin that
;; already returns a newline-joined string of all entry names (including
;; "." and "..").  We split that string, prepend the canonical absolute
;; directory path as the first element, and return `(ABS-DIR NAME ...)',
;; exactly matching the contract expected by `directory-files' in
;; nelisp-stdlib-misc.el.
;;
;; Guarded by `(fboundp 'nelisp--syscall-readdir-names)': true only on the
;; standalone reader binary, so host Emacs is unaffected.  Uses `fset'
;; (not `unless fboundp') because the deferred CLASS-2 registration makes
;; `fboundp' return t before the prelude runs.
;;
;; `directory-files' from nelisp-stdlib-misc.el is also fset here so that
;; the prelude's version (built on the working `nelisp--syscall-readdir-names'
;; directly, without the sort/count/full-path complexity of the misc.el one)
;; takes effect.  The misc.el version would also work once readdir is fixed,
;; but the fset here is a belt-and-suspenders override that avoids any
;; dependency on the misc.el load order.
;; nelisp--readdir-scan-raw: helper that walks a newline-terminated name
;; string from nelisp--syscall-readdir-names and builds a list of strings,
;; one per entry.  The loop uses `if' (not the `or' macro) to avoid the
;; let-frame overflow that `or' causes inside tight while loops on the
;; standalone reader when the string exceeds ~32KB.
;;
;; Callers pass a SKIP-DOTDOT argument: when non-nil, "." and ".." are
;; excluded (needed by directory-files).
(defun nelisp--readdir-scan-raw (raw skip-dotdot)
  (let ((len (length raw))
        (idx 0)
        (start 0)
        (result nil))
    (while (< idx len)
      (if (= (aref raw idx) 10)
          (let ((name (substring raw start idx)))
            (if skip-dotdot
                (if (= (length name) 1)
                    (if (= (aref name 0) 46)
                        nil
                      (setq result (cons name result)))
                  (if (= (length name) 2)
                      (if (= (aref name 0) 46)
                          (if (= (aref name 1) 46)
                              nil
                            (setq result (cons name result)))
                        (setq result (cons name result)))
                    (setq result (cons name result))))
              (setq result (cons name result)))
            (setq start (1+ idx))))
      (setq idx (1+ idx)))
    (nreverse result)))
(when (fboundp 'nelisp--syscall-readdir-names)
  ;; fset nelisp--syscall-readdir: pure-elisp replacement for the CLASS-2
  ;; deferred builtin.  Returns (ABS-DIR NAME ...) or nil, matching the Rust
  ;; bi_syscall_readdir contract expected by directory-files in misc.el.
  (fset 'nelisp--syscall-readdir
        (lambda (dir)
          (let ((raw (nelisp--syscall-readdir-names dir)))
            (if raw
                (cons (expand-file-name dir)
                      (nelisp--readdir-scan-raw raw nil))
              nil))))
  ;; fset directory-files: override the misc.el version (which calls
  ;; nelisp--syscall-readdir, itself deferred) with one that calls
  ;; nelisp--syscall-readdir-names directly and scans via the
  ;; nelisp--readdir-scan-raw helper (no or-macro loops).
  (fset 'directory-files
        (lambda (directory &optional full match _nosort)
          (let ((raw (nelisp--syscall-readdir-names directory)))
            (if raw
                (let ((names (nelisp--readdir-scan-raw raw t))
                      (out nil))
                  (while names
                    (let ((name (car names)))
                      (if match
                          (if (string-match-p match name)
                              (setq out (cons (if full
                                                  (expand-file-name name directory)
                                                name)
                                              out)))
                        (setq out (cons (if full
                                            (expand-file-name name directory)
                                          name)
                                        out))))
                    (setq names (cdr names)))
                  (nreverse out))
              nil)))))
(unless (fboundp 'file-exists-p)
  (defun file-exists-p (filename)
    (let ((s (nelisp--syscall-stat filename)))
      (or (eq s 'file) (eq s 'directory)))))
(unless (fboundp 'file-directory-p)
  (defun file-directory-p (filename)
    (eq (nelisp--syscall-stat filename) 'directory)))
(unless (fboundp 'file-regular-p)
  (defun file-regular-p (filename)
    (eq (nelisp--syscall-stat filename) 'file)))
(unless (fboundp 'file-attributes)
  (defun file-attributes (filename &optional _id-format)
    (if (not (file-exists-p filename))
        nil
      (let ((size (nelisp--syscall-stat-field filename 48))
            (mtime (nelisp--syscall-stat-field filename 88)))
        (list nil 1 0 0 0 mtime 0 size "" nil nil nil)))))
(unless (fboundp 'file-attribute-size)
  (defun file-attribute-size (attrs) (nth 7 attrs)))
(unless (fboundp 'file-attribute-modification-time)
  (defun file-attribute-modification-time (attrs) (nth 5 attrs)))
(defun nelisp--split-on-char (string char omit-empty)
  (let ((start 0)
        (idx 0)
        (len (length string))
        (parts nil))
    (while (<= idx len)
      (if (or (= idx len) (= (aref string idx) char))
          (let ((part (substring string start idx)))
            (unless (and omit-empty (= (length part) 0))
              (setq parts (cons part parts)))
            (setq start (1+ idx))))
      (setq idx (1+ idx)))
    (nreverse parts)))
(unless (fboundp 'split-string)
  (defun split-string (string &optional separators omit-nulls _trim)
    (nelisp--split-on-char
     string
     (if (and separators (> (length separators) 0))
         (aref separators 0)
       32)
     omit-nulls)))
;; nelisp-ec-write-region (Layer-2 fileio) is the write backend anvil-pkg's
;; compat layer prefers on NeLisp; it is in nelisp-emacs-compat-fileio (not
;; loaded by the suite).  Map it onto the reader's `write-region' builtin --
;; the compat call shape is (CONTENT nil PATH nil silent), and write-region
;; accepts a STRING as its START arg.
(unless (fboundp 'nelisp-ec-write-region)
  (defun nelisp-ec-write-region (string _end filename &rest _ignore)
    (write-region string nil filename)))
(unless (fboundp 'file-readable-p)
  ;; Returns t only for regular files with R_OK (nil for directories, by design).
  ;; Uses nelisp--syscall-stat when available (checks file type first); falls back
  ;; to access(R_OK) only on non-reader environments where syscall-stat is absent.
  (if (fboundp 'nelisp--syscall-stat)
      (defun file-readable-p (filename) (eq (nelisp--syscall-stat filename) 'file))
    (defun file-readable-p (filename) (= 0 (nelisp--syscall-path-int 21 filename 4)))))
(unless (fboundp 'file-writable-p)
  (defun file-writable-p (filename) (= 0 (nelisp--syscall-path-int 21 filename 2))))
(unless (fboundp 'delete-file)
  (defun delete-file (filename &optional _trash) (nelisp--syscall-path 87 filename) nil))
(defun nelisp--delete-directory-recursive (path)
  (let ((names (nelisp--split-on-char
                (or (nelisp--syscall-readdir-names path) "") 10 t)))
    (dolist (name names)
      (unless (or (equal name ".") (equal name ".."))
        (nelisp--delete-directory-recursive
         (expand-file-name name path))))
    (let ((rc (nelisp--syscall-path 84 path)))
      (unless (= rc 0)
        (nelisp--syscall-path 87 path)))))
(unless (fboundp 'delete-directory)
  (defun delete-directory (directory &optional recursive _trash)
    (if recursive
        (nelisp--delete-directory-recursive directory)
      (nelisp--syscall-path 84 directory))
    nil))
(unless (fboundp 'directory-files)
  (progn
    (defun nelisp--directory-files-match-p (name match)
      (if (not match)
          t
        (let ((prefix (if (and (>= (length match) 2)
                               (equal (substring match 0 2) "\\`"))
                          (substring match 2)
                        match)))
          (and (>= (length name) (length prefix))
               (equal (substring name 0 (length prefix)) prefix)))))
    (defun directory-files (directory &optional full match _nosort)
      (let ((names (nelisp--split-on-char
                    (or (nelisp--syscall-readdir-names directory) "") 10 t))
            (out nil))
        (dolist (name names)
          (unless (or (equal name ".") (equal name ".."))
            (when (nelisp--directory-files-match-p name match)
              (setq out (cons (if full
                                  (expand-file-name name directory)
                                name)
                              out)))))
        (nreverse out)))))
(unless (fboundp 'make-directory)
  (defun make-directory (dir &optional parents)
    (if parents
        (let ((acc ""))
          (dolist (component (nelisp--split-on-char dir 47 t))
            (setq acc (concat acc "/" component))
            (nelisp--syscall-path-int 83 acc 511)))
      (nelisp--syscall-path-int 83 dir 511))
    dir))
;; Native-store file builtins via direct syscalls (pure elisp, no Rust).
;; x86_64 Linux numbers, matching the access=21/unlink=87/mkdir=83/rmdir=84
;; convention above: rename=82, symlink=88, chmod=90, access(X_OK)=21.
(unless (fboundp 'file-name-absolute-p)
  (defun file-name-absolute-p (filename)
    (and (stringp filename)
         (> (length filename) 0)
         (let ((c (aref filename 0)))
           (or (= c 47) (= c 126))))))      ; "/" or "~"
(unless (fboundp 'rename-file)
  (defun rename-file (file newname &optional ok-if-already-exists)
    (when (and (not ok-if-already-exists) (file-exists-p newname))
      (error "rename-file: target exists: %s" newname))
    (let ((rc (nelisp--syscall-path2 82 file newname)))
      (unless (= rc 0)
        (error "rename-file: rc=%S %s -> %s" rc file newname)))
    nil))
(unless (fboundp 'make-symbolic-link)
  (defun make-symbolic-link (target linkname &optional ok-if-already-exists)
    (when (and ok-if-already-exists (file-exists-p linkname))
      (nelisp--syscall-path 87 linkname))     ; unlink existing
    (let ((rc (nelisp--syscall-path2 88 target linkname)))
      (unless (= rc 0)
        (error "make-symbolic-link: rc=%S %s -> %s" rc target linkname)))
    nil))
(unless (fboundp 'file-executable-p)
  (defun file-executable-p (filename)
    (= 0 (nelisp--syscall-path-int 21 filename 1))))  ; access X_OK
(unless (fboundp 'make-temp-file)
  (defun make-temp-file (prefix &optional dir-flag suffix text)
    (let ((path (concat "/tmp/" (make-temp-name prefix) (or suffix ""))))
      (if dir-flag (make-directory path t) (write-region (or text "") nil path))
      path)))
(unless (fboundp 'clrhash)
  (defun clrhash (table)
    (let (ks)
      (maphash (lambda (k _v) (setq ks (cons k ks))) table)
      (while ks (remhash (car ks) table) (setq ks (cdr ks))))
    table))
(unless (fboundp 'assoc-delete-all)
  (defun assoc-delete-all (key alist &optional test)
    (let ((tt (or test (function equal))))
      (while (and (consp alist) (consp (car alist)) (funcall tt (car (car alist)) key))
        (setq alist (cdr alist)))
      (let ((tail alist))
        (while (cdr tail)
          (if (and (consp (car (cdr tail))) (funcall tt (car (car (cdr tail))) key))
              (setcdr tail (cdr (cdr tail)))
            (setq tail (cdr tail)))))
      alist)))
(unless (fboundp 'add-to-list)
  (defun add-to-list (list-var element &optional append compare-fn)
    (let* ((lst (symbol-value list-var))
           (test (or compare-fn (function equal)))
           (cur lst)
           (found nil))
      (while (and cur (not found))
        (when (funcall test element (car cur))
          (setq found t))
        (setq cur (cdr cur)))
      (unless found
        (setq lst (if append
                      (append lst (list element))
                    (cons element lst)))
        (set list-var lst))
      lst)))
;; `declare' must be a no-op MACRO (not a function): NeLisp's defmacro does not
;; strip a `(declare (indent N) ...)' form from a macro/defun body, so it is
;; evaluated at runtime; as a macro it expands to nil without evaluating the
;; specs (a function would try to eval `(indent 1)' -> another void-function).
(unless (fboundp 'declare)
  (defmacro declare (&rest _specs) nil))
(unless (fboundp 'lwarn)
  (defun lwarn (&rest _args) nil))
(unless (fboundp 'identity)
  (defun identity (x) x))
(unless (fboundp 'booleanp)
  (defun booleanp (x)
    (or (eq x t) (eq x nil))))
(unless (fboundp 'error-message-string)
  (defun error-message-string (error-descriptor)
    (cond
     ((and (consp error-descriptor)
           (stringp (cadr error-descriptor)))
      (cadr error-descriptor))
     ((stringp error-descriptor) error-descriptor)
     (t (format "%S" error-descriptor)))))
(unless (fboundp 'format-time-string)
  (defun format-time-string (&rest _args)
    "1970-01-01"))
(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp replacement string
                                          &optional _fixedcase _literal
                                          _subexp _start)
    (if (equal regexp "[^A-Za-z0-9_]")
        (let ((i 0)
              (len (length string))
              (out ""))
          (while (< i len)
            (let ((ch (aref string i)))
              (setq out
                    (concat out
                            (if (or (and (>= ch ?A) (<= ch ?Z))
                                    (and (>= ch ?a) (<= ch ?z))
                                    (and (>= ch ?0) (<= ch ?9))
                                    (= ch ?_))
                                (char-to-string ch)
                              replacement))))
            (setq i (1+ i)))
          out)
      string)))
(unless (fboundp 'nelisp--base64-value)
  (defun nelisp--base64-value (char)
    (cond
     ((and (>= char ?A) (<= char ?Z)) (- char ?A))
     ((and (>= char ?a) (<= char ?z)) (+ 26 (- char ?a)))
     ((and (>= char ?0) (<= char ?9)) (+ 52 (- char ?0)))
     ((= char ?+) 62)
     ((= char ?/) 63)
     (t -1))))
(unless (fboundp 'nelisp--base64-flush-chunk)
  (defun nelisp--base64-flush-chunk (bytes chunks)
    (if bytes
        (cons (apply 'concat (nreverse bytes)) chunks)
      chunks)))
(unless (fboundp 'base64-encode-string)
  (defun base64-encode-string (string &optional _no-line-break)
    (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
          (i 0)
          (len (length string))
          (chunks nil)
          (parts nil)
          (part-count 0))
      (while (< i len)
        (let* ((a (aref string i))
               (have-b (< (1+ i) len))
               (have-c (< (+ i 2) len))
               (b (if have-b (aref string (1+ i)) 0))
               (c (if have-c (aref string (+ i 2)) 0))
               (triple (logior (ash a 16) (ash b 8) c))
               (c1 (aref alphabet (logand (ash triple -18) 63)))
               (c2 (aref alphabet (logand (ash triple -12) 63)))
               (c3 (if have-b (aref alphabet (logand (ash triple -6) 63)) ?=))
               (c4 (if have-c (aref alphabet (logand triple 63)) ?=)))
          (setq parts
                (cons (concat (char-to-string c1)
                              (char-to-string c2)
                              (char-to-string c3)
                              (char-to-string c4))
                      parts))
          (setq part-count (1+ part-count))
          (when (>= part-count 128)
            (setq chunks (cons (apply 'concat (nreverse parts)) chunks))
            (setq parts nil)
            (setq part-count 0)))
        (setq i (+ i 3)))
      (when parts
        (setq chunks (cons (apply 'concat (nreverse parts)) chunks)))
      (apply 'concat (nreverse chunks)))))
(unless (fboundp 'base64-decode-string)
  (defun base64-decode-string (string)
    (let ((i 0)
          (len (length string))
          (vals-count 0)
          (a 0)
          (b 0)
          (c 0)
          (d 0)
          (bytes nil)
          (byte-count 0)
          (chunks nil))
      (while (< i len)
        (let ((ch (aref string i)))
          (cond
           ((= ch ?=)
            (cond
             ((= vals-count 0) (setq a -2))
             ((= vals-count 1) (setq b -2))
             ((= vals-count 2) (setq c -2))
             (t (setq d -2)))
            (setq vals-count (1+ vals-count)))
           ((or (= ch 10) (= ch 13) (= ch 9) (= ch 32))
            nil)
           (t
            (let ((v (nelisp--base64-value ch)))
              (when (>= v 0)
                (cond
                 ((= vals-count 0) (setq a v))
                 ((= vals-count 1) (setq b v))
                 ((= vals-count 2) (setq c v))
                 (t (setq d v)))
                (setq vals-count (1+ vals-count))))))
          (when (= vals-count 4)
            (let ((triple (logior (ash a 18)
                                  (ash b 12)
                                  (if (= c -2) 0 (ash c 6))
                                  (if (= d -2) 0 d))))
              (setq bytes
                    (cons (char-to-string (logand (ash triple -16) 255))
                          bytes))
              (setq byte-count (1+ byte-count))
              (unless (= c -2)
                (setq bytes
                      (cons (char-to-string (logand (ash triple -8) 255))
                            bytes))
                (setq byte-count (1+ byte-count)))
              (unless (= d -2)
                (setq bytes
                      (cons (char-to-string (logand triple 255))
                            bytes))
                (setq byte-count (1+ byte-count)))
              (when (>= byte-count 128)
                (setq chunks (nelisp--base64-flush-chunk bytes chunks))
                (setq bytes nil)
                (setq byte-count 0))
              (setq vals-count 0))))
        (setq i (1+ i)))
      (setq chunks (nelisp--base64-flush-chunk bytes chunks))
      (apply 'concat (nreverse chunks)))))
;; nelisp stdlib lowering helpers (referenced by the dotimes/loop macro
;; expansions in nelisp-stdlib-eval-special; their definitions live in
;; nelisp-stdlib.el which aborts on load standalone).  They are trivial
;; numeric ops, so define them directly.
(unless (fboundp 'nelisp--num-lt2) (defun nelisp--num-lt2 (a b) (< a b)))
(unless (fboundp 'nelisp--num-gt2) (defun nelisp--num-gt2 (a b) (> a b)))
(unless (fboundp 'nelisp--num-le2) (defun nelisp--num-le2 (a b) (<= a b)))
(unless (fboundp 'nelisp--num-ge2) (defun nelisp--num-ge2 (a b) (>= a b)))
(unless (fboundp 'nelisp--num-eq2) (defun nelisp--num-eq2 (a b) (= a b)))
(unless (fboundp 'nelisp--add2) (defun nelisp--add2 (a b) (+ a b)))
(unless (fboundp 'nelisp--sub2) (defun nelisp--sub2 (a b) (- a b)))
(unless (fboundp 'nelisp--mul2) (defun nelisp--mul2 (a b) (* a b)))
;; json-serialize: anvil-pkg's compat layer prefers the native `json-serialize'
;; when fbound (passing :null-object/:false-object).  The NeLisp json backend
;; (nelisp-json-serialize) is not loaded by the suite AND aborts on a nil hash
;; value (expires-at:nil -> JSON null).  Provide a self-contained encoder that
;; maps hash-table->object, list->array, nil->null, t->true, with minimal string
;; escaping -- enough for anvil-pkg-state's JSON shape.
(unless (fboundp 'nelisp--json-escape)
  (defun nelisp--json-escape (s)
    (let ((out "") (i 0) (n (length s)))
      (while (< i n)
        (let ((c (aref s i)))
          (setq out (concat out (cond ((= c 34) "\\\"") ((= c 92) "\\\\")
                                      ((= c 10) "\\n") ((= c 9) "\\t") ((= c 13) "\\r")
                                      (t (char-to-string c))))))
        (setq i (1+ i)))
      out)))
(unless (fboundp 'json-serialize)
  (defun json-serialize (obj &rest _keys)
    (cond
     ((null obj) "null")
     ((eq obj t) "true")
     ((eq obj :null) "null")
     ((eq obj :json-false) "false")
     ((integerp obj) (number-to-string obj))
     ((floatp obj) (number-to-string obj))
     ((stringp obj) (concat "\"" (nelisp--json-escape obj) "\""))
     ((hash-table-p obj)
      (let ((parts "") (first t))
        (maphash (lambda (k v)
                   (setq parts (concat parts (if first "" ",")
                                       "\"" (nelisp--json-escape (if (stringp k) k (format "%s" k))) "\":"
                                       (json-serialize v))
                         first nil))
                 obj)
        (concat "{" parts "}")))
     ((listp obj)
      (let ((parts "") (first t))
        (while obj (setq parts (concat parts (if first "" ",") (json-serialize (car obj)))
                         first nil obj (cdr obj)))
        (concat "[" parts "]")))
     ((symbolp obj) (concat "\"" (nelisp--json-escape (symbol-name obj)) "\""))
     (t (concat "\"" (nelisp--json-escape (format "%s" obj)) "\"")))))

;; ---- Doc 22 reader-core gap fixes (A1/A2/A3/A5/A10/A12) ----
;;
;; The bare standalone reader ships native primitives whose contract diverges
;; from host Emacs for several core functions.  Because the prelude loads AFTER
;; the native builtins and the runtime resolves these through the global
;; function cell, we capture the native implementation and install a corrected
;; pure-elisp wrapper here (verified: user-level redefinition shadows the native
;; primitive).  No Rust change is involved.

;; A10: `arrayp' is VOID on the bare reader (silently returns nil).
(unless (fboundp 'arrayp)
  (defun arrayp (x)
    "Return t if X is an array (= a string or a vector)."
    (if (or (vectorp x) (stringp x)) t nil)))

;; A1: 2-arg `floor'/`ceiling'/`truncate' ignored the divisor.  Capture the
;; native 1-arg implementation, fix the 2-arg integer path with a toward-zero
;; quotient (`/') plus a floor/ceil sign adjustment (the reader has no `%').
(fset 'nelisp--native-floor (symbol-function 'floor))
(fset 'nelisp--native-ceiling (symbol-function 'ceiling))
(fset 'nelisp--native-truncate (symbol-function 'truncate))

(defun nelisp--int-floor-div (x div)
  "Integer floor division X/DIV toward negative infinity (DIV /= 0)."
  (let* ((q (/ x div))
         (r (- x (* q div))))
    (if (and (not (= r 0)) (if (< div 0) (> r 0) (< r 0)))
        (- q 1)
      q)))

(defun floor (x &optional div)
  "Return the largest integer <= X (1-arg) or <= X/DIV (2-arg)."
  (cond
   ((null div) (nelisp--native-floor x))
   ((and (integerp x) (integerp div)) (nelisp--int-floor-div x div))
   (t (nelisp--native-floor (/ x div)))))

(defun ceiling (x &optional div)
  "Return the smallest integer >= X (1-arg) or >= X/DIV (2-arg)."
  (cond
   ((null div) (nelisp--native-ceiling x))
   ((and (integerp x) (integerp div))
    (- (nelisp--int-floor-div (- x) div)))
   (t (nelisp--native-ceiling (/ x div)))))

(defun truncate (x &optional div)
  "Truncate X (1-arg) or X/DIV (2-arg) toward zero."
  (cond
   ((null div) (nelisp--native-truncate x))
   ((and (integerp x) (integerp div)) (/ x div))
   (t (nelisp--native-truncate (/ x div)))))

;; A2: `mod' used truncate-remainder semantics (sign followed the dividend).
;; Reinstall host floor-mod: the result carries the sign of the divisor.
(defun mod (a b)
  "Return A modulo B with the sign of B (host floor-mod, Doc 22 A2)."
  (if (= b 0)
      (error "Arithmetic error")
    (let ((r (- a (* (/ a b) b))))
      (if (and (not (= r 0)) (if (< b 0) (> r 0) (< r 0)))
          (+ r b)
        r))))

;; A3: native `equal' never compared vectors element-wise.  Capture native
;; `equal' for the atom/string/number leaves and recurse over cons + vector.
(fset 'nelisp--native-equal (symbol-function 'equal))
(defun equal (a b)
  "Structural equality with vector support (Doc 22 A3).
Only `cons' and `vector' are walked in elisp; every atom (number, string,
symbol, nil, t) is delegated to the native `equal', which compares them
correctly.  We deliberately avoid an `(eq a b)' fast path: on the bare
reader `eq' returns t for distinct strings, which would make any two
strings compare equal."
  (cond
   ((and (consp a) (consp b))
    (and (equal (car a) (car b)) (equal (cdr a) (cdr b))))
   ((and (vectorp a) (vectorp b))
    (let ((n (length a)))
      (if (= n (length b))
          (let ((i 0) (ok t))
            (while (and ok (< i n))
              (if (equal (aref a i) (aref b i))
                  (setq i (1+ i))
                (setq ok nil)))
            ok)
        nil)))
   (t (nelisp--native-equal a b))))

;; A5: native `substring' returned garbage for vectors.  Slice vectors in
;; elisp via `aref'/`aset'; defer strings to the (correct) native path.
(fset 'nelisp--native-substring (symbol-function 'substring))
(defun substring (seq from &optional to)
  "Return the SEQ slice [FROM, TO); vector support added (Doc 22 A5)."
  (if (vectorp seq)
      (let* ((n (length seq))
             (s (if (< from 0) (+ n from) from))
             (e (if to (if (< to 0) (+ n to) to) n))
             (out (make-vector (- e s) nil))
             (i 0))
        (while (< (+ s i) e)
          (aset out i (aref seq (+ s i)))
          (setq i (1+ i)))
        out)
    ;; String path: native `substring' returns "" when TO is passed as an
    ;; explicit nil, so only forward the 3rd argument when it was supplied.
    (if to
        (nelisp--native-substring seq from to)
      (nelisp--native-substring seq from))))

;; ---- Doc 22 reader-core gap fixes, iteration 2 (A7/A13) ----

;; A7: native `format' ignores field width / flags / precision (e.g. "%-5s"
;; / "%05d" pass through literally).  The underlying conversion (s S d x X o c
;; e f g %) is correct, so we delegate each directive's value to native format
;; and add the field-width/justify/zero-pad/precision layer in elisp.
(fset 'nelisp--native-format (symbol-function 'format))

(defun nelisp--digit-char-p (ch) (and (>= ch 48) (<= ch 57)))

(defun format (template &rest args)
  "Format TEMPLATE with ARGS honoring %[flags][width][.prec]conv (Doc 22 A7).
Width, left-justify (-), zero-pad (0), sign (+/space) and string precision
(.N) are applied in elisp; the conversion itself is delegated to native
`format', which lacks only the field-width layer."
  (let ((n (length template)) (i 0) (out "") (argp args))
    (while (< i n)
      (let ((ch (aref template i)))
        (if (= ch 37)                   ; ?%
            (let ((j (1+ i))
                  (f- nil) (f0 nil) (fplus nil) (fspace nil)
                  (width nil) (prec nil) (scan t))
              (while (and scan (< j n))
                (let ((c (aref template j)))
                  (cond
                   ((= c 45) (setq f- t j (1+ j)))        ; -
                   ((= c 48) (setq f0 t j (1+ j)))        ; 0
                   ((= c 43) (setq fplus t j (1+ j)))     ; +
                   ((= c 32) (setq fspace t j (1+ j)))    ; space
                   ((= c 35) (setq j (1+ j)))             ; # (accept, ignore)
                   (t (setq scan nil)))))
              (let ((w 0) (have nil))
                (while (and (< j n) (nelisp--digit-char-p (aref template j)))
                  (setq w (+ (* w 10) (- (aref template j) 48)) have t j (1+ j)))
                (when have (setq width w)))
              (when (and (< j n) (= (aref template j) 46))   ; ?.
                (setq j (1+ j))
                (let ((p 0))
                  (while (and (< j n) (nelisp--digit-char-p (aref template j)))
                    (setq p (+ (* p 10) (- (aref template j) 48)) j (1+ j)))
                  (setq prec p)))
              (if (>= j n)
                  (setq out (concat out "%") i (1+ i))
                (let ((conv (aref template j)))
                  (setq i (1+ j))
                  (if (= conv 37)        ; ?%
                      (setq out (concat out "%"))
                    (let* ((arg (car argp))
                           (body (nelisp--native-format
                                  (concat "%" (char-to-string conv)) arg)))
                      (setq argp (cdr argp))
                      (when (and prec (or (= conv 115) (= conv 83))   ; s S
                                 (> (length body) prec))
                        (setq body (substring body 0 prec)))
                      (when (and (or (= conv 100) (= conv 102) (= conv 101) (= conv 103)) ; d f e g
                                 (> (length body) 0) (not (= (aref body 0) 45)))
                        (cond (fplus (setq body (concat "+" body)))
                              (fspace (setq body (concat " " body)))))
                      (when (and width (< (length body) width))
                        (let ((pad (- width (length body))))
                          (cond
                           (f- (setq body (concat body (make-string pad 32))))
                           ((and f0 (or (= conv 100) (= conv 120) (= conv 88)
                                        (= conv 111) (= conv 102) (= conv 101) (= conv 103)))
                            (if (and (> (length body) 0)
                                     (or (= (aref body 0) 45) (= (aref body 0) 43)))
                                (setq body (concat (substring body 0 1)
                                                   (make-string pad 48)
                                                   (substring body 1)))
                              (setq body (concat (make-string pad 48) body))))
                           (t (setq body (concat (make-string pad 32) body))))))
                      (setq out (concat out body)))))))
          (setq out (concat out (char-to-string ch)) i (1+ i)))))
    out))

;; A13: `type-of' is VOID on the bare reader (returns nil for everything), and
;; native `functionp' fails to recognise a `(lambda ...)' / `(closure ...)' /
;; `(builtin ...)' cons (returns nil).  Provide a predicate-composed `type-of'
;; and a corrected `functionp' so function type-dispatch works.
(fset 'nelisp--native-functionp (symbol-function 'functionp))
(defun functionp (x)
  "Return t if X is callable (lambda / closure / builtin cons, or native)."
  (if (and (consp x) (memq (car x) '(lambda closure builtin)))
      t
    (if (nelisp--native-functionp x) t nil)))

(unless (fboundp 'type-of)
  (defun type-of (x)
    "Return a symbol naming the primitive type of X (Doc 22 A13).
Callable conses report `function'/`subr'; otherwise composed from the
native predicates."
    (cond
     ((null x) 'symbol)
     ((and (consp x) (memq (car x) '(lambda closure))) 'function)
     ((and (consp x) (eq (car x) 'builtin)) 'subr)
     ((consp x) 'cons)
     ((symbolp x) 'symbol)
     ((stringp x) 'string)
     ((integerp x) 'integer)
     ((floatp x) 'float)
     ((vectorp x) 'vector)
     (t 'cons))))
