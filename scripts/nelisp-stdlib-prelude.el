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
	(if (and body (stringp (car body)))
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

(defmacro defvar (name &optional value _docstring)
  "Define NAME as a global variable, setting VALUE if unbound."
  (cons 'progn
        (cons (cons 'if
                    (cons (cons 'boundp
                                (cons (cons 'quote (cons name nil)) nil))
                          (cons nil
                                (cons (cons 'set
                                            (cons (cons 'quote (cons name nil))
                                                  (cons value nil)))
                                      nil))))
              (cons (cons 'quote (cons name nil)) nil))))

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

;; ---- macroexpand (Doc 47 self-host / compiler frontend) ----
;;
;; `defmacro' stores a macro as the function value `(macro CLOSURE)' (= a
;; two-element list: car `macro', cadr the macro CLOSURE).  `nelisp-phase47-
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

(defun macroexpand-1 (form)
  "Expand FORM by one macro step if its head is a macro; else return FORM."
  (if (consp form)
      (let ((mfn (nelisp--macro-function (car form))))
        (if mfn (apply mfn (cdr form)) form))
    form))

(defun macroexpand (form &optional _environment)
  "Repeatedly macroexpand FORM until its head is no longer a macro.
ENVIRONMENT is accepted for host compatibility and ignored (the standalone
reader resolves macros through the global function mirror)."
  (let ((cur form) (again t))
    (while again
      (if (consp cur)
          (let ((mfn (nelisp--macro-function (car cur))))
            (if mfn
                (setq cur (apply mfn (cdr cur)))
              (setq again nil)))
        (setq again nil)))
    cur))

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
;; `nelisp-phase47-compiler.el' and `scripts/compile-elisp-objects.el'
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
Supports proper lists only (= what `nelisp-phase47-compiler.el' uses)."
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
`nelisp-phase47-compiler.el' (single-binding walk-helper recursion);
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
Used by the Phase 47 compiler's `--emit-defun' gp prologue gate
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
(unless (fboundp 'set-file-modes)
  (defun set-file-modes (_filename _mode) nil))

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
