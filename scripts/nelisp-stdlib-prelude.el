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

;; A leading string is a docstring only when something follows it.  When it
;; is the whole body it IS the body, and Emacs returns it: (defun f ()
;; "hello") answers "hello" there and answered nil here, because this
;; stripped every leading string unconditionally.  Measured 2026-08-19
;; against Emacs 30.1, which also confirms the other three edges this has
;; to get right: (defun f () "doc" "body") returns "body", so the first
;; string still goes; (defun f () (declare (indent 1))) returns nil, so a
;; `declare' goes even when it is the whole body; and `documentation' of a
;; string-only function is nil, i.e. the string never became a docstring.
;;
;; `defun', `defmacro' and `defsubst' all reach this, so every function
;; whose body was a bare string literal -- the accessor-returning-a-constant
;; shape -- silently returned nil.
(fset 'nelisp--strip-body-declarations
      (lambda (body)
        (let ((cur body))
          (while (and cur
                      (or (and (stringp (car cur)) (cdr cur))
                          (and (consp (car cur))
                               (eq (car (car cur)) 'declare))))
            (setq cur (cdr cur)))
          cur)))

(fset 'defmacro
      (cons 'macro
	    (cons
	     (lambda (name args &rest body)
	       (let*
		   ((real-body
                     (nelisp--strip-body-declarations body))
                    (lambda-form (cons 'lambda (cons args real-body)))
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

;; The expansions carried a trailing nil arm (`when') and wrapped the body
;; in `progn' (`unless') where Emacs does neither.  The value is the same
;; either way, but a macro's expansion is compared for byte-identity in this
;; tree, and anything that walks expanded code -- a compiler pass, a test
;; that reads `macroexpand-1' -- sees a shape Emacs never produces.
;; Argument-type checks used throughout this file.  They live HERE, before
;; the first caller, because the prelude calls its own functions while it
;; loads -- defining them further down made `file-name-directory' hit
;; void-function during boot.
(unless (fboundp 'nelisp--check-float-arg)
  ;; `fceiling' and its siblings name `floatp' -- the result is a float and
  ;; that is the requirement Emacs states -- even though an integer argument
  ;; is accepted.  Naming `numberp' here would be a different claim.
  (defun nelisp--check-float-arg (x)
    (unless (numberp x) (signal 'wrong-type-argument (list 'floatp x)))
    x))
(unless (fboundp 'nelisp--check-number)
  (defun nelisp--check-number (x)
    (unless (numberp x) (signal 'wrong-type-argument (list 'numberp x)))
    x))
(unless (fboundp 'nelisp--check-integer)
  (defun nelisp--check-integer (x)
    (unless (integerp x) (signal 'wrong-type-argument (list 'integerp x)))
    x))
(unless (fboundp 'nelisp--check-natnum)
  (defun nelisp--check-natnum (x)
    (unless (and (integerp x) (>= x 0))
      (signal 'wrong-type-argument (list 'natnump x)))
    x))
(unless (fboundp 'nelisp--check-character)
  (defun nelisp--check-character (x)
    (unless (and (integerp x) (>= x 0) (<= x 4194303))
      (signal 'wrong-type-argument (list 'characterp x)))
    x))
(unless (fboundp 'nelisp--check-list)
  (defun nelisp--check-list (x)
    (unless (listp x) (signal 'wrong-type-argument (list 'listp x)))
    x))
;; A sequence function walking (1 2 . 3) names the TAIL, not the whole list:
;; Emacs answers (wrong-type-argument listp 3).  Naming the argument instead
;; sent the caller looking at the wrong value.
(unless (fboundp 'nelisp--check-seq-list)
  (defun nelisp--check-seq-list (seq)
    (unless (sequencep seq) (signal 'wrong-type-argument (list 'sequencep seq)))
    (when (consp seq)
      (let ((l seq))
        (while (consp l) (setq l (cdr l)))
        (when l (signal 'wrong-type-argument (list 'listp l)))))
    seq))
(unless (fboundp 'nelisp--check-symbol)
  (defun nelisp--check-symbol (x)
    (unless (symbolp x) (signal 'wrong-type-argument (list 'symbolp x)))
    x))
;; Byte-identical to the prelude copy so `make ns-gate' polices the two.
(unless (fboundp 'nelisp--check-string)
  (defun nelisp--check-string (x)
    (unless (stringp x) (signal 'wrong-type-argument (list 'stringp x)))
    x))
;; Byte-identical to the lisp/nelisp-stdlib.el copy, so `make ns-gate'
;; polices the two rather than letting them drift.  It was void in the
;; standalone, so (sequencep 0) was `void-function' where Emacs answers nil.
;; This runtime has one global intern table and no first-class obarray
;; object, so nothing here can BE an obarray -- but Emacs still type-checks
;; the argument, and answering for a vector hid the fact that OBARRAY is
;; ignored (recorded in tools/partial-accepted.txt).
(unless (fboundp 'obarrayp)
  (defun obarrayp (_x) nil))
;; `intern' takes an OBARRAY it cannot honour -- one global table here --
;; but Emacs type-checks the argument, and accepting anything hid the fact
;; that it is ignored (recorded in tools/partial-accepted.txt).
(unless (fboundp 'nelisp--native-intern)
  (defalias 'nelisp--native-intern (symbol-function 'intern))
  (defun intern (name &optional obarray)
    (when (and obarray (not (obarrayp obarray)))
      (signal 'wrong-type-argument (list 'obarrayp obarray)))
    (nelisp--native-intern name)))
;; Absent, so a caller got `void-function' -- which reads as "the runtime
;; cannot do this" rather than "nobody wrote it yet".  There are no buffers
;; or byte-compiler here, so these answer nil and signal on a wrong type,
;; which is the honest half of what Emacs does (recorded in
;; tools/partial-accepted.txt).
(unless (fboundp 'file-truename)
  (defun file-truename (path &optional counter prev-dirs)
    ;; Two predicates, by what the argument is: a SYMBOL (nil included) gets
    ;; `arrayp', anything else `stringp'.  Measured across nil / 1 / a
    ;; symbol / a vector / a float -- guessing one name gets three of the
    ;; five wrong.
    (unless (stringp path)
      (signal 'wrong-type-argument
              (list (if (symbolp path) 'arrayp 'stringp) path)))
    ;; COUNTER is a symlink-depth list in Emacs, and it names `listp'.
    ;; COUNTER is a CONS whose car counts the links left to chase -- Emacs
    ;; defaults it to (list 100).  So a non-cons names `listp', a cons with a
    ;; non-number car names `number-or-marker-p' about the CAR, and a car at
    ;; or below zero is the cycle report.
    (when counter
      (unless (consp counter)
        (signal 'wrong-type-argument (list 'listp counter)))
      (unless (numberp (car counter))
        (signal 'wrong-type-argument
                (list 'number-or-marker-p (car counter))))
      (when (<= (car counter) 0)
        (signal 'error (list (format "Apparent cycle of symbolic links for %s"
                                     path)))))
    ;; PREV-DIRS is a list of (DIR . TRUENAME) pairs, and the walk asks each
    ;; entry for its car -- so a non-list entry names itself, not the list.
    (unless (listp prev-dirs)
      (signal 'wrong-type-argument (list 'listp prev-dirs)))
    (dolist (e prev-dirs)
      (unless (listp e) (signal 'wrong-type-argument (list 'listp e))))
    (expand-file-name path)))
(unless (fboundp 'buffer-file-name)
  (defun buffer-file-name (&optional buffer)
    (when buffer (signal 'wrong-type-argument (list 'bufferp buffer)))
    nil))
(unless (fboundp 'make-bool-vector)
  (defun make-bool-vector (length init)
    (unless (and (integerp length) (>= length 0))
      (signal 'wrong-type-argument (list 'wholenump length)))
    (make-vector length (and init t))))
(unless (fboundp 'log)
  (defun nelisp--dd-div (x y)
    "Double-double X / Y, each a (HI . LO) pair."
    (let* ((q (/ (car x) (car y)))
           (p (nelisp--dd-mul (cons q 0.0) y))
           (rem (nelisp--dd-add x (cons (- 0.0 (car p)) (- 0.0 (cdr p))))))
      (nelisp--two-sum q (/ (car rem) (car y)))))
  (defun log (x &optional base)
    "The natural log of X, or its log to BASE.
Correctly rounded: x is reduced to 2^k * m with m in [sqrt(2)/2, sqrt(2)),
and the atanh series for log(m) is summed in DOUBLE-DOUBLE arithmetic, as
is the k*ln2 that is added back.  A plain double series was about 1e-15
out, which is a wrong last bit on most arguments; measured against the host
over 17 arguments, none differ now.

A non-positive X is not an error: Emacs answers -inf for zero and NaN for
a negative, and signalling there turned a limit into a failure."
    (nelisp--check-number x)
    (when base (nelisp--check-number base))
    (let ((ln (cond
               ((< x 0) (/ 0.0 0.0))
               ((= x 0) (/ -1.0 0.0))
               (t (let ((k 0) (m (float x)))
                    (while (>= m 1.4142135623730951) (setq m (/ m 2.0)) (setq k (1+ k)))
                    (while (< m 0.7071067811865476) (setq m (* m 2.0)) (setq k (1- k)))
                    (let* ((num (nelisp--two-sum m -1.0))
                           (den (nelisp--two-sum m 1.0))
                           (z (nelisp--dd-div num den))
                           (z2 (nelisp--dd-mul z z))
                           (sum z)
                           (term z)
                           (n 1))
                      (while (< n 30)
                        (setq term (nelisp--dd-mul term z2))
                        (setq sum (nelisp--dd-add sum (nelisp--dd-div-int term (+ (* 2 n) 1))))
                        (setq n (1+ n)))
                      (setq sum (nelisp--dd-mul sum (cons 2.0 0.0)))
                      (let ((kk (nelisp--dd-mul (cons (float k) 0.0)
                                                (cons 6.93147180369123816490e-01
                                                      1.90821492927058770002e-10))))
                        (setq sum (nelisp--dd-add sum kk))
                        (+ (car sum) (cdr sum)))))))))
      (if base (/ ln (log base)) ln))))
(unless (fboundp 'bool-vector)
  (defun bool-vector (&rest args)
    "Bool vectors are plain vectors of t/nil in this runtime (Doc 22)."
    (apply #'vector (mapcar (lambda (x) (and x t)) args))))
;; `path-separator' moved below `system-type' (Doc: it needs to know the
;; target OS to pick ";" on windows-nt vs ":" elsewhere -- see the
;; defconst there for the fix and why this used to be a fixed ":").
(unless (fboundp 'bool-vector-p)
  (defun bool-vector-p (_x)
    "Always nil: bool vectors are plain vectors here (Doc 22)."
    nil))
;; `point-min'/`point-max' used to be hardcoded here (`(defun point-min ()
;; 1)', unconditionally, regardless of buffer state -- Doc 188 §1.3).  The
;; real, buffer-backed definitions now live in the Doc 188 P1 buffer
;; section below (search for "Doc 188 P1"), after the buffer object model
;; they read from is defined.  Moving the hardcoded stub out from under
;; its own `unless (fboundp ...)' guard, rather than leaving it here and
;; overriding it unconditionally later, keeps that guard meaningful: the
;; real definition is the ONLY one that ever runs, so `unless (fboundp
;; ...)' still means "only if nothing has defined this yet" everywhere in
;; this file, not "only if my own earlier stub hasn't already claimed it".
(unless (fboundp 'locate-library)
  (defun locate-library (library &optional _nosuffix _path _interactive-call)
    "Find LIBRARY on `load-path', trying .el; nil when not found."
    (nelisp--check-string library)
    (locate-file library load-path '(".el" ""))))
(unless (fboundp 'exp)
  (defun nelisp--two-sum (a b)
    "A+B as an exact pair (SUM . ERR)."
    (let* ((s (+ a b)) (bb (- s a)))
      (cons s (+ (- a (- s bb)) (- b bb)))))
  (defun nelisp--two-prod (a b)
    "A*B as an exact pair (PRODUCT . ERR)."
    (let ((p (* a b)))
      (cons p (nelisp--two-prod-err a b p))))
  (defun nelisp--dd-add (x y)
    "Double-double X + Y, each a (HI . LO) pair."
    (let* ((se (nelisp--two-sum (car x) (car y)))
           (e (+ (cdr se) (+ (cdr x) (cdr y)))))
      (nelisp--two-sum (car se) e)))
  (defun nelisp--dd-mul (x y)
    "Double-double X * Y, each a (HI . LO) pair."
    (let* ((pe (nelisp--two-prod (car x) (car y)))
           (e (+ (cdr pe) (+ (* (car x) (cdr y)) (* (cdr x) (car y))))))
      (nelisp--two-sum (car pe) e)))
  (defun nelisp--dd-div-int (x n)
    "Double-double X divided by the small integer N."
    (let* ((q (/ (car x) (float n)))
           (pe (nelisp--two-prod q (float n)))
           (rem (+ (- (- (car x) (car pe)) (cdr pe)) (cdr x))))
      (nelisp--two-sum q (/ rem (float n)))))
  (defun exp (x)
    "e raised to X.
Newton-free and correctly rounded: x is reduced to k*ln2 + r with ln2 split
hi/lo so the subtraction is exact, and exp(r) is then summed term by term in
DOUBLE-DOUBLE arithmetic.  A plain double series left the last bit wrong --
(exp 1) came back one ULP above the host's answer -- because the residual
that decides the rounding is smaller than the sum can represent.  Measured
against the host over 28 arguments: none differ."
    (nelisp--check-number x)
    (let ((xf (float x)))
      (if (= xf 0.0)
          1.0
        (let* ((k (if (> xf 0)
                      (truncate (+ (* 1.44269504088896338700e+00 xf) 0.5))
                    (truncate (- (* 1.44269504088896338700e+00 xf) 0.5))))
               (hi (- xf (* k 6.93147180369123816490e-01)))
               (lo (- 0.0 (* k 1.90821492927058770002e-10)))
               (r (nelisp--two-sum hi lo))
               (term (cons 1.0 0.0))
               (sum (cons 1.0 0.0))
               (n 1))
          (while (< n 22)
            (setq term (nelisp--dd-mul term r))
            (setq term (nelisp--dd-div-int term n))
            (setq sum (nelisp--dd-add sum term))
            (setq n (1+ n)))
          (let ((acc (+ (car sum) (cdr sum))) (j (abs k)))
            (while (> j 0)
              (setq acc (if (< k 0) (/ acc 2.0) (* acc 2.0)))
              (setq j (1- j)))
            acc))))))
;; There are no buffers here, so this can only report the type error --
;; which is the half Emacs reaches first anyway for a bad position.
(unless (fboundp 'buffer-substring-no-properties)
  (defun buffer-substring-no-properties (start end)
    (unless (integerp start)
      (signal 'wrong-type-argument (list 'integer-or-marker-p start)))
    (unless (integerp end)
      (signal 'wrong-type-argument (list 'integer-or-marker-p end)))
    ""))
;; No text properties in this runtime, so this is `equal' -- and the same
;; `defalias' the lisp/ mirror uses, so `make ns-gate' sees one definition.
(unless (fboundp 'equal-including-properties)
  (defalias 'equal-including-properties 'equal))
(unless (fboundp 'locate-file)
  (defun locate-file (filename path &optional suffixes _predicate)
    "Find FILENAME in PATH, trying each of SUFFIXES; nil when not found."
    (nelisp--check-string filename)
    (when (= (length filename) 0) (setq path nil))
    ;; SUFFIXES is the checked one, not PATH: a non-string PATH entry is
    ;; skipped on the way to answering nil, while a non-string SUFFIX is
    ;; what Emacs names.  Measured both ways round.
    (unless (listp suffixes) (setq suffixes nil))
    (let ((sl suffixes))
      (while (consp sl)
        (unless (stringp (car sl))
          (signal 'wrong-type-argument (list 'stringp (car sl))))
        (setq sl (cdr sl))))
    (let ((dirs (and (proper-list-p path) path)) (hit nil))
      (while (and dirs (not hit))
        (let ((dir (and (stringp (car dirs)) (car dirs))))
          (dolist (suf (or suffixes '("")))
            (let ((cand (and (stringp suf)
                             (concat (file-name-as-directory (or dir ".")) filename suf))))
              (when (and cand (not hit) (file-exists-p cand)) (setq hit cand)))))
        (setq dirs (cdr dirs)))
      hit)))
(unless (fboundp 'sequencep)
  (defun sequencep (x)
    "Return t if X is a sequence (= nil, cons, string, or vector)."
    (or (null x) (consp x) (stringp x) (vectorp x))))
(unless (fboundp 'string-search)
  (defun string-search (needle haystack &optional start)
    (nelisp--check-string needle)
    (nelisp--check-string haystack)
    (when start
      (unless (integerp start) (signal 'wrong-type-argument (list 'fixnump start)))
      (when (or (< start 0) (> start (length haystack)))
        (signal 'args-out-of-range (list start))))
    (let ((nl (length needle)) (hl (length haystack)) (i (or start 0)) (found nil))
      (if (= nl 0) i
        (while (and (not found) (<= (+ i nl) hl))
          (if (string= needle (substring haystack i (+ i nl)))
              (setq found i)
            (setq i (1+ i))))
        found))))

(defmacro when (cond &rest body)
  "If COND yields non-nil, eval BODY forms sequentially and return last value."
  (cons 'if (cons cond (cons (cons 'progn body) nil))))

(defmacro unless (cond &rest body)
  "If COND yields nil, eval BODY forms sequentially and return last value."
  (cons 'if (cons cond (cons nil body))))

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
        (nelisp--strip-body-declarations body))
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

;; A negative N behaves as zero in Emacs, which is why `(nth -1 '(1 2 3))'
;; is 1 and not nil.  The `(= n 0)' test walked past it and recursed
;; downwards forever -- except `(null list)' stopped it, so the answer came
;; back nil for every negative index instead.
;; An improper list names the WHOLE list, not the tail it stopped on:
;; (nthcdr 5 '(1 2 . 3)) is (wrong-type-argument listp (1 2 . 3)) in Emacs,
;; and naming 3 sent the caller looking at a value it never passed in.
;; Stopping exactly ON the non-list tail is not an error at all --
;; (nthcdr 1 '(1 . 2)) is 2 -- so the check belongs after the walk.
(defun nthcdr (n list)
  (unless (integerp n) (signal 'wrong-type-argument (list 'integerp n)))
  (let ((i n) (l list))
    (while (and (> i 0) (consp l)) (setq l (cdr l)) (setq i (1- i)))
    (cond ((<= i 0) l)
          ((null l) nil)
          (t (signal 'wrong-type-argument (list 'listp list))))))

;;; nelisp-stdlib-list.el --- Sweep 9 G1 list operations  -*- lexical-binding: t; -*-

(defun car-safe (object)
  "Return the car of OBJECT if it is a cons cell, otherwise nil."
  (if (consp object) (car object) nil))

(defun cdr-safe (object)
  "Return the cdr of OBJECT if it is a cons cell, otherwise nil."
  (if (consp object) (cdr object) nil))

;; Doc 143 worklist A (WRITE): delq/delete were void in the reader runtime
;; (not in source).  List forms (the dominant use); rebuild semantics.
(defun delq (elt list)
  (unless (listp list) (signal 'wrong-type-argument (list 'listp list)))
  (unless (proper-list-p list) (signal 'wrong-type-argument (list 'listp list)))
  (let ((acc nil))
    (while list
      (if (not (eq (car list) elt))
          (setq acc (cons (car list) acc)))
      (setq list (cdr list)))
    (nreverse acc)))

;; `delete' is destructive in Emacs: it rewrites the list in place and the
;; caller's variable sees the removal.  This built a fresh list, so
;; `(let ((l (list 1 2 3))) (delete 2 l) l)' still answered (1 2 3) -- the
;; element the caller asked to remove was still there.  Emacs also takes
;; vectors and strings, returning the same type.
(defun delete (elt seq)
  (cond
   ((null seq) nil)
   ((consp seq)
    ;; Drop leading matches, then unlink the rest through `setcdr'.  The
    ;; end-of-list check comes AFTER the leading drop and names what is left:
    ;; (delete 1 '(1 . 2)) is `listp 2' while (delete nil '(1 . 2)) is
    ;; `listp (1 . 2)', because the first call has already walked past the 1.
    (let ((head seq))
      (while (and (consp head) (equal (car head) elt))
        (setq head (cdr head)))
      (unless (proper-list-p head)
        (signal 'wrong-type-argument (list 'listp head)))
      (let ((cur head))
        (while (consp cur)
          (let ((nxt (cdr cur)))
            (if (and (consp nxt) (equal (car nxt) elt))
                (setcdr cur (cdr nxt))
              (setq cur nxt)))))
      head))
   ((or (vectorp seq) (stringp seq))
    (let ((keep nil) (i 0) (n (length seq)))
      (while (< i n)
        (unless (equal (aref seq i) elt)
          (setq keep (cons (aref seq i) keep)))
        (setq i (1+ i)))
      (setq keep (nreverse keep))
      (if (stringp seq)
          (apply 'string keep)
        (apply 'vector keep))))
   (t (signal 'wrong-type-argument (list 'sequencep seq)))))

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
;; The `t' arms fell through to `equal', so `(string-equal 5 5)' answered t
;; -- a type bug turned into a plausible-looking yes.  Emacs takes strings
;; and symbols and signals for anything else.
(defun string-equal (a b)
  (let ((sa (cond ((stringp a) a)
                  ((symbolp a) (symbol-name a))
                  (t (signal 'wrong-type-argument (list 'stringp a)))))
        (sb (cond ((stringp b) b)
                  ((symbolp b) (symbol-name b))
                  (t (signal 'wrong-type-argument (list 'stringp b))))))
    (equal sa sb)))

(defun string= (a b) (string-equal a b))

;; `lsh' was missing entirely (void-function).  It is NOT `ash': a right
;; shift of a negative number fills with zeros rather than the sign bit, so
;; `(lsh -1 -1)' is a large positive number where `(ash -1 -1)' is -1.
;; Aliasing the two -- which is what lisp/nelisp-stdlib-misc.el does -- gets
;; every bit-packing routine that shifts a sign-bit-set value wrong.
(unless (fboundp 'lsh)
  (defun lsh (value count)
    ;; Measured: only a NON-NUMBER in argument one names
    ;; `number-or-marker-p'.  Everything else -- a float anywhere, or a
    ;; non-number in argument two -- names `integerp'.
    ;;   (lsh "a" 1) -> number-or-marker-p    (lsh 48 "a") -> integerp
    ;;   (lsh 1.5 1) -> integerp              (lsh 1 1.5)  -> integerp
    (unless (numberp value) (signal 'wrong-type-argument (list 'number-or-marker-p value)))
    ;; COUNT's predicate depends on the SIGN of VALUE, and so does the ORDER:
    ;; a negative value takes the marker-coercing path, which asks about
    ;; COUNT before it asks VALUE to be an integer.  (lsh -1 t) names
    ;; `number-or-marker-p', (lsh 1 t) names `integerp', and
    ;; (lsh -1.5 [1 2 3]) names the vector rather than -1.5.
    (when (< value 0)
      (unless (numberp count)
        (signal 'wrong-type-argument (list 'number-or-marker-p count))))
    (unless (integerp value) (signal 'wrong-type-argument (list 'integerp value)))
    (unless (integerp count) (signal 'wrong-type-argument (list 'integerp count)))
    (if (>= count 0)
        (ash value count)
      (if (>= value 0)
          (ash value count)
        ;; A right shift of a negative value fills with zeros, so the answer
        ;; is the UNSIGNED 62-bit pattern shifted.  One masked step does the
        ;; conversion: shift right once arithmetically, then clear the sign
        ;; bits the shift copied in.  The remaining places are an ordinary
        ;; `ash' on a value that is now positive.
        ;;
        ;; The mask is written `(1- (ash 1 61))' because integers here are
        ;; 62-bit and `(ash 1 61)' wraps negative -- `most-positive-fixnum'
        ;; and `integer-length' do not exist in this runtime to ask with.
        (ash (logand (ash value -1) (1- (ash 1 61))) (+ count 1))))))

(defun regexp-quote (s)
  (nelisp--check-string s)
  ;; Emacs escapes exactly these eight: $ * + . ? [ \\ ^ -- the characters
  ;; that ARE special in its regexp syntax.  This escaped six more, and five
  ;; of them made things worse rather than merely noisier: in Emacs regexps
  ;; ( ) { } | are LITERAL, and the constructs are the backslashed forms, so
  ;; escaping a literal `(' turns it into a group opener.  `(regexp-quote
  ;; "(a)")' produced "\\(a\\)", which is a capturing group matching "a" --
  ;; the opposite of quoting.  Same for | becoming alternation and {} becoming
  ;; a repetition count.  The sixth, ], Emacs leaves alone; \\] is harmless but
  ;; is not what Emacs returns, so a caller comparing quoted strings misses.
  ;; Measured 2026-08-19 against Emacs 30.1 over all 128 ASCII codes.
  ;; Build via substring + string concat: the reader's `concat' does not
  ;; accept a char-list argument, so accumulate 1-char substrings instead.
  (let ((out "") (i 0) (n (length s)))
    (while (< i n)
      (let ((ch (aref s i)) (cs (substring s i (1+ i))))
        (when (or (eq ch ?.) (eq ch ?*) (eq ch ?+) (eq ch ??)
                  (eq ch ?\[) (eq ch ?^) (eq ch ?$) (eq ch ?\\))
          (setq out (concat out "\\")))
        (setq out (concat out cs)))
      (setq i (1+ i)))
    out))

;; Doc 143 (pure, no-helper primitives): high-frequency, dependency-free.
(defun natnump (x) (and (integerp x) (>= x 0)))
(defun int-to-string (n) (number-to-string (nelisp--check-number n)))
(defun prefix-numeric-value (arg)
  ;; Emacs answers 1 for anything it does not recognise as a prefix -- a
  ;; STRING included -- rather than handing the argument back.
  (cond ((null arg) 1) ((eq arg '-) -1)
        ((consp arg) (if (numberp (car arg)) (car arg) 1))
        ((integerp arg) arg)
        ;; Anything else is not a prefix argument, and Emacs answers 1 --
        ;; handing the argument back made a string flow on as a "count".
        (t 1)))

;; Doc 143 arithmetic (helper-free, via >/</- which are reader primitives).
(defun max (&rest args)
  ;; Name the FIRST bad argument.  The fold reported whichever one it was
  ;; holding when the comparison failed, which for (min '(1 2) '(1)) is the
  ;; second -- so the reader was pointed at the wrong one.
  ;;
  ;; `&rest' rather than (X &rest REST) so the no-argument call reports what
  ;; Emacs reports.  Emacs has TWO shapes here and only running both shows
  ;; it: a direct (max) names the SYMBOL, while (apply #'max '()) names the
  ;; subr OBJECT.  This is the direct-call shape; `seq-max' carries the other
  ;; one itself, because from here the two calls look the same.
  (unless args
    (signal 'wrong-number-of-arguments (list 'max 0)))
  (let ((x (car args)) (rest (cdr args)))
    (unless (numberp x) (signal 'wrong-type-argument (list 'number-or-marker-p x)))
    (dolist (a rest) (unless (numberp a)
                       (signal 'wrong-type-argument (list 'number-or-marker-p a))))
    (let ((acc x)) (while rest (if (> (car rest) acc) (setq acc (car rest))) (setq rest (cdr rest))) acc)))
(defun min (&rest args)
  ;; Name the FIRST bad argument.  The fold reported whichever one it was
  ;; holding when the comparison failed, which for (min '(1 2) '(1)) is the
  ;; second -- so the reader was pointed at the wrong one.
  ;;
  ;; `&rest' rather than (X &rest REST) so the no-argument call reports what
  ;; Emacs reports.  Emacs has TWO shapes here and only running both shows
  ;; it: a direct (min) names the SYMBOL, while (apply #'min '()) names the
  ;; subr OBJECT.  This is the direct-call shape; `seq-min' carries the other
  ;; one itself, because from here the two calls look the same.
  (unless args
    (signal 'wrong-number-of-arguments (list 'min 0)))
  (let ((x (car args)) (rest (cdr args)))
    (unless (numberp x) (signal 'wrong-type-argument (list 'number-or-marker-p x)))
    (dolist (a rest) (unless (numberp a)
                       (signal 'wrong-type-argument (list 'number-or-marker-p a))))
    (let ((acc x)) (while rest (if (< (car rest) acc) (setq acc (car rest))) (setq rest (cdr rest))) acc)))
(defun abs (x)
  (nelisp--check-number x)
  (if (< x 0) (- 0 x) x))

;; SEQ may be a list, a vector or a string, as in Emacs.  The vector and
;; string forms used to be documented as out of scope, and out of scope
;; meant a vector answered the empty string: a vector is not nil, so the
;; list walk never entered and the empty accumulator came back.  No error,
;; no content.  `append' normalises the sequence, and for a string it
;; yields characters, which is what Emacs passes to FN.
;;
;; The explanation lives here rather than in a docstring because the
;; example needs quotation marks, and an unescaped one ends the string --
;; which is exactly what happened on the first attempt at this fix: the
;; docstring closed early, the rest of it parsed as forms, and `mapconcat'
;; answered void-variable.
(defun mapconcat (fn seq &optional sep)
  (nelisp--check-seq-list seq)
  (let* ((items (if (if (null seq) 1 (consp seq)) seq (append seq nil)))
         (out "")
         (first t)
         (joiner (or sep ""))
         (tail items))
    (while tail
      (unless first
        (setq out (concat out joiner)))
      (setq out (concat out (funcall fn (car tail))))
      (setq first nil)
      (setq tail (cdr tail)))
    out))

(defun nelisp--case-pair-p (c lo hi even-is-upper)
  "Non-nil when C sits in the LO..HI block of alternating case pairs."
  (and (>= c lo) (<= c hi)
       (if even-is-upper (= 0 (logand c 1)) (= 1 (logand c 1)))))

;; Simple case mapping over ASCII, Latin-1, Latin Extended-A, Greek and
;; Cyrillic.  Verified character by character against Emacs 30.1 across every
;; code point in those ranges -- the three that no rule covers (#xDF, #x4C0,
;; #x4CF) are written out.  OUTSIDE those ranges a character passes through
;; unchanged, which is a real limit and is stated rather than hidden: Latin
;; Extended-B and the rest of the BMP need a generated table, and a table is
;; the honest way to get them, not more rules.
;;
;; Before this the mapping was ASCII only, so (upcase "ae\u0301") -- any accented
;; letter -- came back unchanged.  A case operation cannot report a character
;; it did not know how to map, so this failed by answering.
(defun nelisp--case-up-char (c)
  (cond
   ((and (>= c ?a) (<= c ?z)) (- c 32))
   ((= c #xDF) #x1E9E)
   ((and (>= c #xE0) (<= c #xFE) (/= c #xF7)) (- c 32))
   ((= c #xFF) #x178)
   ((nelisp--case-pair-p c #x100 #x12F nil) (1- c))
   ((nelisp--case-pair-p c #x132 #x137 nil) (1- c))
   ((nelisp--case-pair-p c #x139 #x148 t) (1- c))
   ((nelisp--case-pair-p c #x14A #x177 nil) (1- c))
   ((nelisp--case-pair-p c #x179 #x17E t) (1- c))
   ((= c #x3AC) #x386)
   ((and (>= c #x3AD) (<= c #x3AF)) (- c 37))
   ((and (>= c #x3B1) (<= c #x3C1)) (- c 32))
   ((= c #x3C2) #x3A3)
   ((and (>= c #x3C3) (<= c #x3CB)) (- c 32))
   ((= c #x3CC) #x38C)
   ((and (>= c #x3CD) (<= c #x3CE)) (- c 63))
   ((and (>= c #x430) (<= c #x44F)) (- c 32))
   ((and (>= c #x450) (<= c #x45F)) (- c 80))
   ((nelisp--case-pair-p c #x460 #x481 nil) (1- c))
   ((nelisp--case-pair-p c #x48A #x4BF nil) (1- c))
   ((= c #x4CF) #x4C0)
   ((nelisp--case-pair-p c #x4C1 #x4CE t) (1- c))
   ((nelisp--case-pair-p c #x4D0 #x4FF nil) (1- c))
   (t c)))

(defun nelisp--case-down-char (c)
  (cond
   ((and (>= c ?A) (<= c ?Z)) (+ c 32))
   ((and (>= c #xC0) (<= c #xDE) (/= c #xD7)) (+ c 32))
   ((= c #x178) #xFF)
   ((nelisp--case-pair-p c #x100 #x12F t) (1+ c))
   ((nelisp--case-pair-p c #x132 #x137 t) (1+ c))
   ((nelisp--case-pair-p c #x139 #x148 nil) (1+ c))
   ((nelisp--case-pair-p c #x14A #x177 t) (1+ c))
   ((nelisp--case-pair-p c #x179 #x17E nil) (1+ c))
   ((= c #x386) #x3AC)
   ((and (>= c #x388) (<= c #x38A)) (+ c 37))
   ((= c #x38C) #x3CC)
   ((and (>= c #x38E) (<= c #x38F)) (+ c 63))
   ((and (>= c #x391) (<= c #x3A1)) (+ c 32))
   ((and (>= c #x3A3) (<= c #x3AB)) (+ c 32))
   ((and (>= c #x400) (<= c #x40F)) (+ c 80))
   ((and (>= c #x410) (<= c #x42F)) (+ c 32))
   ((nelisp--case-pair-p c #x460 #x481 t) (1+ c))
   ((nelisp--case-pair-p c #x48A #x4BF t) (1+ c))
   ((= c #x4C0) #x4CF)
   ((nelisp--case-pair-p c #x4C1 #x4CE nil) (1+ c))
   ((nelisp--case-pair-p c #x4D0 #x4FF t) (1+ c))
   (t c)))

(defun nelisp--case-letter-p (c)
  (not (and (eq (nelisp--case-up-char c) c) (eq (nelisp--case-down-char c) c))))

(defun nelisp--case-unibyte-string (string upcasep)
  "Fold ASCII letters in raw-byte STRING and preserve every other byte."
  (let ((i 0) (n (length string)) (bytes nil))
    (while (< i n)
      (let ((c (aref string i)))
        (setq bytes
              (cons (if upcasep
                        (if (and (>= c ?a) (<= c ?z)) (- c 32) c)
                      (if (and (>= c ?A) (<= c ?Z)) (+ c 32) c))
                    bytes)))
      (setq i (1+ i)))
    (apply #'unibyte-string (nreverse bytes))))

(defun downcase (obj)
  (unless (or (stringp obj)
              (and (integerp obj) (>= obj 0) (<= obj 4194303)))
    (signal 'wrong-type-argument (list 'char-or-string-p obj)))
  (cond ((and (stringp obj) (unibyte-string-p obj))
         (nelisp--case-unibyte-string obj nil))
        ((stringp obj)
         (let ((out "") (i 0) (n (length obj)))
           (while (< i n)
             (setq out (concat out (char-to-string (nelisp--case-down-char (aref obj i)))))
             (setq i (1+ i)))
           out))
        ((integerp obj) (nelisp--case-down-char obj))
        (t obj)))

;; Upcase the first character of each word and leave the rest of the word
;; as it is -- so "hello wORLD" becomes "Hello WORLD", not "Hello World".
;; `capitalize' is the one that downcases the tail; conflating them is the
;; easy mistake here.
(unless (fboundp 'upcase-initials)
 (defun upcase-initials (obj)
  (if (integerp obj) (upcase obj)
    (let ((n (length obj)) (i 0) (out "") (prev nil))
      (while (< i n)
        (let* ((c (aref obj i))
               (w (or (nelisp--case-letter-p c) (and (>= c ?0) (<= c ?9)))))
          (setq out (concat out (char-to-string (if (and w (not prev))
                                                    (nelisp--case-up-char c)
                                                  c))))
          (setq prev w))
        (setq i (1+ i)))
      out))))

;; The regexp engine honours this (lisp/nelisp-stdlib-regexp.el); stock Emacs
;; already binds it, so the guard makes this a declaration for the standalone
;; only.  t is Emacs's default value, and matching that default is the point:
;; an unqualified (string-match "A" "a") answers 0 in Emacs.
(unless (boundp 'case-fold-search)
  (defvar case-fold-search t
    "Non-nil means string/regexp matching ignores case, as in Emacs."))

(defun upcase (obj)
  ;; A STRING gets the FULL mapping and a CHARACTER the simple one, which is
  ;; Emacs's rule and is not a distinction worth guessing at: (upcase ?\N{U+00DF})
  ;; is U+1E9E, while (upcase "\N{U+00DF}") is "SS", because a full mapping may
  ;; change the length and a character cannot.  U+00DF is the only expansion
  ;; inside the ranges mapped here.
  (unless (or (stringp obj)
              (and (integerp obj) (>= obj 0) (<= obj 4194303)))
    (signal 'wrong-type-argument (list 'char-or-string-p obj)))
  (cond ((and (stringp obj) (unibyte-string-p obj))
         (nelisp--case-unibyte-string obj t))
        ((stringp obj)
         (let ((out "") (i 0) (n (length obj)))
           (while (< i n)
             (let ((c (aref obj i)))
               (setq out (concat out (if (= c #xDF) "SS"
                                       (char-to-string (nelisp--case-up-char c))))))
             (setq i (1+ i)))
           out))
        ((integerp obj) (nelisp--case-up-char obj))
        (t obj)))

;; Doc 143 file-name path ops (pure string slicing, from nelisp-stdlib-plist-str.el).
(defun file-name-directory (path)
  "Return the directory part of PATH, or nil if PATH has no slash.
Result keeps the trailing slash."
  (nelisp--check-string path)
  (let ((idx -1)
        (i 0)
        (n (length path)))
    (while (< i n)
      (when (eq (aref path i) ?/)
        (setq idx i))
      (setq i (1+ i)))
    (if (< idx 0)
        nil
      (substring path 0 (1+ idx)))))
(defun file-name-nondirectory (path)
  (nelisp--check-string path)
  (let ((idx -1) (i 0) (n (length path)))
    (while (< i n) (when (eq (aref path i) ?/) (setq idx i)) (setq i (1+ i)))
    (if (< idx 0) path (substring path (1+ idx)))))
;; An empty name is the CURRENT directory, so Emacs answers "./".  This
;; answered "/" -- turning a relative nothing into the filesystem root,
;; which is as wrong as a path result gets.
(defun file-name-as-directory (path)
  "Return PATH with a trailing `/' appended if not already present."
  (nelisp--check-string path)
  (cond
   ((= (length path) 0) "./")
   ((eq (aref path (1- (length path))) ?/) path)
   (t (concat path "/"))))
;; Emacs strips the whole run of trailing slashes, not one of them:
;; "a//" is "a".  Stripping exactly one left "a/", which still names a
;; directory and so defeats the point of calling this at all.  A name that
;; is nothing but slashes keeps one, matching Emacs on "/" and "///".
(defun directory-file-name (path)
  (nelisp--check-string path)
  (let ((n (length path)))
    (while (if (> n 1) (eq (aref path (1- n)) ?/) nil)
      (setq n (1- n)))
    (if (= n (length path)) path (substring path 0 n))))
;; Rust-min batch 7d (2026-05-07, Doc 50 stage 2): `expand-file-name'
;; and `file-truename' migrated from Rust to elisp.  expand-file-name
;; is pure path arithmetic + a `default-directory' lookup; it needs
;; ZERO new primitives (= file-name-as-directory + concat + aref are
;; all elisp-side).  file-truename adds 1 syscall primitive
;; (`nelisp--syscall-canonicalize' = std::fs::canonicalize wrapper)
;; for the symlink-resolve sliver, with elisp fall-back-on-error
;; matching the prior Rust `unwrap_or(full)' behaviour.
;;
;; The Rust impl had a `current_dir()' fallback for the case where
;; both BASE arg and `default-directory' were nil; NeLisp always
;; sets `default-directory' at startup so that fallback never fired
;; in practice and is dropped here.

;; `length' signals on an improper list, as Emacs does, so the tolerant
;; counterparts Emacs provides have to exist too -- without them there is no
;; way to ask about a dotted list at all.  `safe-length' counts cons cells
;; and stops; `proper-list-p' answers the length only when the list really
;; ends in nil, and nil otherwise.
(defun safe-length (list)
  (let ((n 0) (cur list))
    (while (consp cur)
      (setq n (1+ n))
      (setq cur (cdr cur)))
    n))

(defun proper-list-p (object)
  (let ((n 0) (cur object) (ok t))
    (while (if ok (consp cur) nil)
      (setq n (1+ n))
      (setq cur (cdr cur)))
    (if (null cur) n nil)))

(defun nelisp--path-split (s)
  ;; Split S on / and drop empty components, so a// collapses like Emacs.
  ;;
  ;; PERF (Doc 201 §6.8): this used to build each component one character at
  ;; a time, `(setq cur (concat cur (substring s i (1+ i))))' -- two
  ;; allocations and two interpreter calls per CHARACTER, where a component
  ;; needs one `substring' in total.  On a runtime whose basic-op cost is
  ;; ~0.15-0.4ms (§3) that made `expand-file-name' cost ~21ms for an
  ;; ordinary path, and `executable-find' calls it once per PATH entry:
  ;; three `executable-find' calls over a 59-entry PATH were essentially the
  ;; whole 7.6s that `vendor/ddskk/skk-vars.el' still took to load.  Now it
  ;; scans for separators and cuts one substring per component.
  (let ((out nil) (start 0) (i 0) (n (length s)))
    (while (< i n)
      (when (eq (aref s i) ?/)
        (when (> i start) (setq out (cons (substring s start i) out)))
        (setq start (1+ i)))
      (setq i (1+ i)))
    (when (> n start) (setq out (cons (substring s start n) out)))
    (nreverse out)))

;; This used to concatenate and stop -- no `.', no `..', no `~', no
;; collapsing of doubled slashes, and an empty NAME came back empty.  So
;; (expand-file-name "a/../b") answered /base/dir/a/../b and
;; (expand-file-name "~/x") answered ~/x, neither of which is a path
;; anything else can compare with `equal' against one Emacs produced.  For
;; a runtime meant to host an editor that is a daily defect: buffer names,
;; `locate-library' hits and every cache key built from a path are all
;; affected.  Measured 2026-08-19 against Emacs 30.1.
;; Windows-native standalone, Doc 201 §4 follow-up: this function was
;; POSIX-only, so on windows-nt a drive-letter name was not recognised as
;; absolute at all -- (expand-file-name "cmd.exe" "C:/Windows/System32")
;; answered "/C:/Windows/System32/cmd.exe", which no Windows API can open.
;; That is why `executable-find' still found nothing on a windows-native
;; build even with Doc 201 §1 (`path-separator') and §2 (access(F_OK) via
;; GetFileAttributesW) both fixed: PATH split into the right 59
;; drive-letter directories, and every candidate built from one was then
;; mangled beyond existence.  §2's own verification could not see it --
;; none of the programs it probed for (python/kakasi/look) were installed
;; on that host, so "not found" was the correct answer either way, and the
;; end-to-end path was never once exercised with a program that IS there.
;; `nelisp--windows-paths-p' is deliberately `boundp'-defensive: this
;; function is defined long before `system-type' becomes bound further down
;; this same file, and a call made in between must still answer POSIX-ly
;; rather than signal.
(defun nelisp--windows-paths-p ()
  (and (boundp 'system-type) (eq system-type 'windows-nt)))

(defun nelisp--path-drive (s)
  "Return the leading \"X:\" drive designator of S, or nil when absent."
  (if (and (>= (length s) 2) (eq (aref s 1) ?:))
      (let ((c (aref s 0)))
        (if (or (and (>= c ?A) (<= c ?Z)) (and (>= c ?a) (<= c ?z)))
            (substring s 0 2)
          nil))
    nil))

(defun nelisp--path-slashify (s)
  "Return S with every backslash replaced by a forward slash.
S itself is returned when it contains none, so the common case allocates
nothing."
  (let ((n (length s)) (i 0) (start 0) (parts nil))
    (while (< i n)
      (when (eq (aref s i) ?\\)
        (setq parts (cons (substring s start i) parts))
        (setq start (1+ i)))
      (setq i (1+ i)))
    (if (null parts)
        s
      (mapconcat 'identity (nreverse (cons (substring s start) parts)) "/"))))

(defun expand-file-name (path &optional base)
  (let* ((win (nelisp--windows-paths-p))
         (p (if (null path) "" path))
         (p (if win (nelisp--path-slashify p) p))
         (p (if (and (> (length p) 0) (eq (aref p 0) ?~)
                     (if (= (length p) 1) 1 (eq (aref p 1) ?/)))
                (let ((home (or (getenv "HOME") "~")))
                  (concat (if win (nelisp--path-slashify home) home)
                          (substring p 1)))
              p))
         (drive (if win (nelisp--path-drive p) nil))
         ;; Everything below works on the drive-less remainder and puts the
         ;; designator back at the end, so the "/" -collapsing loop stays the
         ;; single POSIX one it has always been.
         (p (if drive (substring p 2) p))
         (rooted (if (= (length p) 0) nil (eq (aref p 0) ?/)))
         ;; A drive designator makes the name absolute on its own: "C:foo" is
         ;; drive-relative in Windows, but this runtime keeps no per-drive
         ;; current directory, so it resolves at that drive's root.
         (absolute (if drive t rooted))
         (trailing (if (= (length p) 0) nil
                     (eq (aref p (- (length p) 1)) ?/)))
         ;; A rooted name with no designator inherits the base's drive, which
         ;; is what Emacs does; on any other target there is no drive to
         ;; inherit, so BASE is left untouched exactly as before.
         (need-base (if drive nil (if rooted win t)))
         (anchor "")
         (parts nil)
         (stack nil))
    (when need-base
      (let* ((b (or base
                    (and (boundp 'default-directory) default-directory)
                    "/"))
             (b (if win (nelisp--path-slashify b) b))
             (b (if (or (if win (nelisp--path-drive b) nil)
                        (if (> (length b) 0) (eq (aref b 0) ?/) nil))
                    b
                  (expand-file-name b)))
             (bdrive (if win (nelisp--path-drive b) nil)))
        (setq drive bdrive)
        (unless rooted
          (setq anchor (file-name-as-directory
                        (if bdrive (substring b 2) b))))))
    (setq parts (nelisp--path-split (if absolute p (concat anchor p))))
    (while parts
      (let ((c (car parts)))
        (cond
         ((equal c ".") nil)
         ((equal c "..") (setq stack (cdr stack)))
         (t (setq stack (cons c stack)))))
      (setq parts (cdr parts)))
    (setq stack (nreverse stack))
    (let ((res (concat (or drive "") "/" (mapconcat 'identity stack "/"))))
      (if (if trailing (> (length stack) 0) nil)
          (concat res "/")
        res))))

;; Emacs strips backup suffixes before asking about the extension, which
;; is why (file-name-extension "foo.txt~") is "txt" and not "txt~".  There
;; was no `file-name-sans-versions' here at all, so a backup name reported
;; an extension no file ever has -- enough to send a mode lookup or a
;; suffix comparison down the wrong path.  Two shapes are stripped, both
;; measured against Emacs 30.1: a trailing ~, and a trailing .~N~ where N
;; is digits.  Nothing else: "a~b.txt" and "foo.txt.~1~x" are left alone.
(defun file-name-sans-versions (path &optional _keep-backup-version)
  (let* ((n (length path)))
    (cond
     ((= n 0) path)
     ;; .~N~
     ((if (eq (aref path (1- n)) ?~)
          (let ((i (- n 2)) (digits 0))
            (while (if (>= i 0)
                       (if (>= (aref path i) ?0) (<= (aref path i) ?9) nil)
                     nil)
              (setq digits (1+ digits))
              (setq i (1- i)))
            (if (> digits 0)
                (if (>= i 1)
                    (if (eq (aref path i) ?~) (eq (aref path (1- i)) ?.) nil)
                  nil)
              nil))
        nil)
      (let ((i (- n 2)))
        (while (if (>= (aref path i) ?0) (<= (aref path i) ?9) nil)
          (setq i (1- i)))
        (substring path 0 (1- i))))
     ;; plain trailing ~
     ((eq (aref path (1- n)) ?~) (substring path 0 (1- n)))
     (t path))))

(defun file-name-extension (path &optional period)
  (let* ((non (file-name-sans-versions (file-name-nondirectory path)))
         (n (length non)) (idx -1) (i 0))
    (while (< i n) (when (eq (aref non i) ?.) (setq idx i)) (setq i (1+ i)))
    (if (or (< idx 0) (= idx 0))
        (if period "" nil)
      (substring non (if period idx (1+ idx))))))
(defun file-name-sans-extension (path)
  (nelisp--check-string path)
  (let* ((non (file-name-nondirectory path))
         (dir-len (- (length path) (length non)))
         (n (length non)) (idx -1) (i 0))
    (while (< i n) (when (eq (aref non i) ?.) (setq idx i)) (setq i (1+ i)))
    (if (or (< idx 0) (= idx 0)) path (substring path 0 (+ dir-len idx)))))

;; Doc 143 common pure string/seq predicates + builders.
;; IGNORE-CASE was accepted and ignored here too, so a case-insensitive
;; prefix or suffix test answered nil for anything that differed only in
;; case.  Same fold as `assoc-string', and the same reach.
(unless (fboundp 'string-prefix-p)
  (defun string-prefix-p (prefix string &optional ignore-case)
    ;; PREFIX is checked before STRING, and the predicate is `stringp' once
    ;; the lengths have been taken.
    (nelisp--check-seq-list prefix)
    (nelisp--check-seq-list string)
    (let ((pl (length prefix)))
      (when (<= pl (length string))
        (unless (stringp prefix) (signal 'wrong-type-argument (list 'stringp prefix)))
        (unless (stringp string) (signal 'wrong-type-argument (list 'stringp string))))
      (and (<= pl (length string))
           (let ((a (substring string 0 pl)))
             (if ignore-case
                 (string= (downcase prefix) (downcase a))
               (string= prefix a)))))))
(unless (fboundp 'string-suffix-p)
  (defun string-suffix-p (suffix string &optional ignore-case)
    ;; STRING first: (string-suffix-p 1.5 0.0) names 0.0, not 1.5.
    (nelisp--check-seq-list string)
    (nelisp--check-seq-list suffix)
    (let ((sl (length suffix)) (stl (length string)))
      (when (<= sl stl)
        (unless (stringp suffix) (signal 'wrong-type-argument (list 'stringp suffix)))
        (unless (stringp string) (signal 'wrong-type-argument (list 'stringp string))))
      (and (<= sl stl)
           (let ((a (substring string (- stl sl))))
             (if ignore-case
                 (string= (downcase suffix) (downcase a))
               (string= suffix a)))))))
;; `compare-strings' existed only in lisp/nelisp-stdlib-plist-str.el, which
;; the standalone does not load, so it was `void-function' here.  Same body,
;; moved to where it runs.
(unless (fboundp 'compare-strings)
  (defun compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
    (let* ((s1 str1) (s2 str2)
           (a (or start1 0))
           (b (or end1 (length s1)))
           (c (or start2 0))
           (d (or end2 (length s2)))
           (len1 (- b a))
           (len2 (- d c))
           (n (if (< len1 len2) len1 len2))
           (i 0)
           (result t))
      (while (and (< i n) (eq result t))
        (let* ((ch1 (aref s1 (+ a i)))
               (ch2 (aref s2 (+ c i)))
               (k1 (if ignore-case (downcase ch1) ch1))
               (k2 (if ignore-case (downcase ch2) ch2)))
          (cond
           ((< k1 k2) (setq result (- (1+ i))))
           ((> k1 k2) (setq result (1+ i)))
           (t (setq i (1+ i))))))
      (cond
       ((not (eq result t)) result)
       ((= len1 len2) t)
       ((< len1 len2) (- (1+ n)))
       (t (1+ n))))))
(unless (fboundp 'char-equal)
  (defun char-equal (a b)
    (nelisp--check-character a)
    (nelisp--check-character b)
    ;; `char-equal' folds case when `case-fold-search' is non-nil, which is
    ;; its whole difference from `eq' on two characters.
    (or (eq a b)
        (and (boundp 'case-fold-search) case-fold-search
             (integerp a) (integerp b)
             (eq (if (and (>= a ?A) (<= a ?Z)) (+ a 32) a)
                 (if (and (>= b ?A) (<= b ?Z)) (+ b 32) b))))))
(unless (fboundp 'string-to-list)
  (defun string-to-list (s)
    (nelisp--check-seq-list s)
    (if (listp s)
        s
      (let ((l nil) (i (1- (length s))))
        (while (>= i 0) (setq l (cons (aref s i) l)) (setq i (1- i)))
        l))))
(unless (fboundp 'number-sequence)
  (defun number-sequence (from &optional to inc)
    (if (null to)
        (list from)
      (unless (numberp from)
        (signal 'wrong-type-argument (list 'number-or-marker-p from)))
      (unless (numberp to)
        (signal 'wrong-type-argument (list 'number-or-marker-p to)))
      (when inc
        (unless (numberp inc)
          (signal 'wrong-type-argument (list 'number-or-marker-p inc)))
        (when (= inc 0) (signal 'error (list "The increment can not be zero"))))
      (let ((step (or inc 1)) (acc nil) (x from))
        (if (> step 0)
            (while (<= x to) (setq acc (cons x acc)) (setq x (+ x step)))
          (while (>= x to) (setq acc (cons x acc)) (setq x (+ x step))))
        (nreverse acc)))))
(unless (fboundp 'string-trim)
  ;; TRIM-LEFT and TRIM-RIGHT were accepted and ignored, so
  ;; (string-trim "xxaxx" "x+" "x+") answered "xxaxx" -- the caller asked for
  ;; a specific trim and got the whitespace default with no indication.
  ;; Delegating keeps the three functions consistent by construction: a fix
  ;; to `string-trim-left' cannot now leave `string-trim' behind.
  (defun string-trim (s &optional trim-left trim-right)
    ;; RIGHT runs first, so its regexp is the one `concat' complains about.
    (string-trim-left (string-trim-right s trim-right) trim-left)))
(unless (fboundp 'alist-get)
  (defun alist-get (key alist &optional default _remove testfn)
    (unless (proper-list-p alist)
      (signal 'wrong-type-argument (list 'listp alist)))
    (let ((cur alist) (found nil))
      (while (and cur (not found))
        (let ((e (car cur)))
          (if (and (consp e)
                   (if testfn (funcall testfn key (car e)) (eq key (car e))))
              (setq found e)
            (setq cur (cdr cur)))))
      (if found (cdr found) default))))
(unless (fboundp 'take)
  (defun take (n list)
    (unless (integerp n) (signal 'wrong-type-argument (list 'integerp n)))
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
  (cond ((listp seq) (nelisp--check-seq-list seq) seq)
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
    (nelisp--check-seq-list seq)
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
  ;; `func-arity' reports (1 . many) for a cl-generic, but the METHOD takes
  ;; one argument and a two-argument call IS an arity error.  Declaring
  ;; &rest made this accept what Emacs rejects -- the arity to copy is the
  ;; one that runs, not the one the generic advertises.
  (defun seq-empty-p (&rest a)
    (unless (= (length a) 1)
      (signal 'wrong-number-of-arguments (list '(1 . 1) (length a))))
    (let ((x (car a)))
      (if (listp x) (null x) (= (length x) 0)))))
(unless (fboundp 'seq-length)
  (defun seq-length (seq) (length seq)))
(unless (fboundp 'seq-elt)
  (defun seq-elt (seq n)
    (unless (sequencep seq) (signal 'wrong-type-argument (list 'sequencep seq)))
    (if (listp seq)
        (unless (integerp n) (signal 'wrong-type-argument (list 'integerp n)))
      (unless (integerp n) (signal 'wrong-type-argument (list 'fixnump n))))
    ;; `seq-elt' and `elt' disagree about an improper list, and only running
    ;; both says so: (seq-elt '(1 2 . 3) 5) names the TAIL 3, while
    ;; (elt '(1 2 . 3) 5) names the whole list.  Delegating to `nth' here
    ;; inherited the wrong one of the two.
    (if (listp seq)
        (let ((l seq) (i (if (< n 0) 0 n)))
          (while (and (> i 0) (consp l)) (setq l (cdr l)) (setq i (1- i)))
          (cond ((consp l) (car l))
                ((null l) nil)
                (t (signal 'wrong-type-argument (list 'listp l)))))
      (aref seq n))))
(unless (fboundp 'seq-contains-p)
  (defun seq-contains-p (seq elt &optional testfn)
    (let ((l (nelisp-seq--to-list seq)) (found nil))
      (while (and l (not found))
        (when (if testfn (funcall testfn elt (car l)) (equal elt (car l))) (setq found t))
        (setq l (cdr l)))
      found)))
(unless (fboundp 'seq-do)
  (defun seq-do (fn seq)
    (unless (sequencep seq) (signal 'wrong-type-argument (list 'sequencep seq)))
    (let ((probe seq))
      (while (consp probe) (setq probe (cdr probe)))
      (unless (or (null probe) (not (consp seq)))
        (signal 'wrong-type-argument (list 'listp probe))))
    (let ((l (nelisp-seq--to-list seq)))
      (while l (funcall fn (car l)) (setq l (cdr l)))
      ;; Emacs answers the SEQUENCE, not nil -- `seq-do' is the one in the
      ;; family whose return value is its argument, and callers chain on it.
      seq)))
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
(unless (fboundp 'cl-count-if)
  (defun cl-count-if (pred seq)
    "Return the number of elements of SEQ that satisfy PRED."
    (let ((cur (nelisp-seq--to-list seq)) (n 0))
      (while cur
        (when (funcall pred (car cur)) (setq n (1+ n)))
        (setq cur (cdr cur)))
      n)))
;; Doc 160 breadth: cl-lib / seq / string library gaps.
(unless (fboundp 'cl-find)
  (defun cl-find (item seq &rest kw)
    (let ((test (or (plist-get kw :test) #'eql))
          (key (or (plist-get kw :key) #'identity))
          (cur (nelisp-seq--to-list seq)) (found nil) (value nil))
      (while (and cur (not found))
        (when (funcall test item (funcall key (car cur)))
          (setq found t value (car cur)))
        (setq cur (cdr cur)))
      value)))
(unless (fboundp 'cl-reduce)
  (defun cl-reduce (fn seq &rest kw)
    (let* ((cur (nelisp-seq--to-list seq))
           (m (plist-member kw :initial-value))
           (acc (if m (cadr m) (if cur (car cur) (funcall fn)))))
      (unless m (setq cur (cdr cur)))
      (while cur (setq acc (funcall fn acc (car cur))) (setq cur (cdr cur)))
      acc)))
(unless (fboundp 'assoc-default)
  (defun assoc-default (key alist &optional test default)
    (let ((res default) (l alist) (tf (or test #'equal)) (done nil))
      (while (and l (not done))
        (let ((e (car l)))
          (if (consp e)
              (when (funcall tf key (car e)) (setq res (cdr e) done t))
            (when (funcall tf key e) (setq res default done t))))
        (setq l (cdr l)))
      res)))
(unless (fboundp 'cl-getf)
  (defun cl-getf (plist key &optional default)
    (let ((m (plist-member plist key))) (if m (cadr m) default))))
(unless (fboundp 'apply-partially)
  ;; The captured FN and ARGS were re-resolved on every call through the
  ;; closure's own names, which recursed until the nesting limit -- so a
  ;; partial application was not merely wrong, it never returned.
  (defun apply-partially (fn &rest args)
    (let ((nelisp--ap-fn fn) (nelisp--ap-args args))
      (lambda (&rest more)
        (apply nelisp--ap-fn (append nelisp--ap-args more))))))
;; `value<' is Emacs's default ordering, and `sort' falls back to it when no
;; predicate is given -- calling nil as a function is what it did before.
(unless (fboundp 'value<)
  (defun value< (a b)
    (cond
     ((and (numberp a) (numberp b)) (< a b))
     ((and (stringp a) (stringp b)) (string< a b))
     ((and (symbolp a) (symbolp b)) (string< (symbol-name a) (symbol-name b)))
     ((and (consp a) (consp b))
      (if (equal (car a) (car b)) (value< (cdr a) (cdr b)) (value< (car a) (car b))))
     ((and (null a) (null b)) nil)
     (t (signal 'wrong-type-argument (list 'value< a b))))))
(unless (fboundp 'seq-sort)
  (defun seq-sort (pred seq)
    (let ((l (sort (nelisp-seq--to-list seq) pred)))
      (cond ((listp seq) l)
            ((stringp seq) (apply #'string l))
            ((vectorp seq) (apply #'vector l))
            (t l)))))
(unless (fboundp 'ntake)
  (defun ntake (n list) (take n list)))
(unless (fboundp 'string-pad)
  (defun string-pad (s len &optional padding start)
    ;; Emacs checks LENGTH before STRING, so a call with both wrong names
    ;; the length.
    (nelisp--check-natnum len)
    (let ((pad (or padding 32)) (n (length s)))
      (if (>= n len) s
        (if start (concat (make-string (- len n) pad) s)
          (concat s (make-string (- len n) pad)))))))
(unless (fboundp 'string-chop-newline)
  (defun string-chop-newline (s)
    ;; `length' runs first and names `sequencep'; the comparison that follows
    ;; names `stringp'.  A vector reaches the second and a number the first.
    (unless (sequencep s) (signal 'wrong-type-argument (list 'sequencep s)))
    (when (= (length s) 0) (setq s s))
    (unless (or (= (length s) 0) (stringp s))
      (signal 'wrong-type-argument (list 'stringp s)))
    (if (and (> (length s) 0) (= (aref s (1- (length s))) 10))
        (substring s 0 (1- (length s))) s)))
(unless (fboundp 'gensym)
  (defun gensym (&optional prefix) (cl-gensym prefix)))
;; Doc 160 breadth: binding macros (when-let / if-let / pcase-let /
;; cl-destructuring-bind / cl-pushnew).
(unless (fboundp 'when-let*)
  (defmacro when-let* (bindings &rest body)
    (let ((form `(progn ,@body))
          (bs (if (and (consp bindings) (symbolp (car bindings))) (list bindings) bindings)))
      (dolist (b (reverse bs))
        (let* ((var (cond ((symbolp b) b) ((cdr b) (car b)) (t (gensym))))
               (val (cond ((symbolp b) b) ((cdr b) (cadr b)) (t (car b)))))
          (setq form `(let ((,var ,val)) (if ,var ,form nil)))))
      form)))
(unless (fboundp 'when-let)
  (defmacro when-let (bindings &rest body) `(when-let* ,bindings ,@body)))
(unless (fboundp 'if-let*)
  (defmacro if-let* (bindings then &rest else)
    (let ((form then)
          (elseform `(progn ,@else))
          (bs (if (and (consp bindings) (symbolp (car bindings))) (list bindings) bindings)))
      (dolist (b (reverse bs))
        (let* ((var (cond ((symbolp b) b) ((cdr b) (car b)) (t (gensym))))
               (val (cond ((symbolp b) b) ((cdr b) (cadr b)) (t (car b)))))
          (setq form `(let ((,var ,val)) (if ,var ,form ,elseform)))))
      form)))
(unless (fboundp 'if-let)
  (defmacro if-let (bindings then &rest else) `(if-let* ,bindings ,then ,@else)))
(unless (fboundp 'pcase-let)
  (defmacro pcase-let (bindings &rest body)
    (if (null bindings) `(progn ,@body)
      `(pcase ,(cadr (car bindings))
         (,(car (car bindings)) (pcase-let ,(cdr bindings) ,@body))))))
(unless (fboundp 'pcase-let*)
  (defmacro pcase-let* (bindings &rest body) `(pcase-let ,bindings ,@body)))
(unless (fboundp 'cl-destructuring-bind)
  (defmacro cl-destructuring-bind (arglist expr &rest body)
    (let ((val (gensym)) (binds nil) (i 0) (rest nil) (args arglist))
      (while args
        (let ((a (car args)))
          (cond ((eq a '&rest) (setq rest (cadr args) args nil))
                ((eq a '&optional) nil)
                (t (push `(,a (nth ,i ,val)) binds) (setq i (1+ i)))))
        (setq args (cdr args)))
      `(let* ((,val ,expr) ,@(reverse binds)
              ,@(when rest `((,rest (nthcdr ,i ,val)))))
         ,@body))))
(unless (fboundp 'cl-pushnew)
  (defmacro cl-pushnew (item place &rest _keys)
    `(let ((cl--x ,item))
       (if (member cl--x ,place) ,place (setq ,place (cons cl--x ,place))))))
;; Doc 160 breadth round 2: cl-lib predicates / accessors / seq / string.
(unless (fboundp 'cl-evenp) (defun cl-evenp (n) (= 0 (mod n 2))))
(unless (fboundp 'cl-oddp) (defun cl-oddp (n) (not (= 0 (mod n 2)))))
(unless (fboundp 'cl-plusp) (defun cl-plusp (n) (> n 0)))
(unless (fboundp 'cl-minusp) (defun cl-minusp (n) (< n 0)))
(unless (fboundp 'cl-first) (defun cl-first (l) (nth 0 l)))
(unless (fboundp 'cl-second) (defun cl-second (l) (nth 1 l)))
(unless (fboundp 'cl-third) (defun cl-third (l) (nth 2 l)))
(unless (fboundp 'cl-rest) (defun cl-rest (l) (cdr l)))
(unless (fboundp 'cl-typep)
  (defun cl-typep (val type)
    (cond ((eq type 'integer) (integerp val)) ((eq type 'number) (numberp val))
          ((eq type 'float) (floatp val)) ((eq type 'string) (stringp val))
          ((eq type 'symbol) (symbolp val)) ((eq type 'cons) (consp val))
          ((eq type 'list) (listp val)) ((eq type 'vector) (vectorp val))
          ((eq type 'null) (null val)) ((eq type 't) t) (t nil))))
(unless (fboundp 'cl-list*)
  (defun cl-list* (&rest args)
    (if (cdr args)
        (let* ((rev (reverse args)) (lst (car rev)) (rest (reverse (cdr rev))))
          (append rest lst))
      (car args))))
(unless (fboundp 'cl-remove)
  (defun cl-remove (item seq &rest _)
    (cl-remove-if (lambda (x) (eql x item)) (nelisp-seq--to-list seq))))
(unless (fboundp 'cl-delete) (defun cl-delete (item seq &rest _) (cl-remove item seq)))
(unless (fboundp 'cl-remove-if-not)
  (defun cl-remove-if-not (pred seq) (cl-remove-if (lambda (x) (not (funcall pred x))) seq)))
(unless (fboundp 'cl-count)
  (defun cl-count (item seq &rest _)
    (let ((n 0)) (dolist (x (nelisp-seq--to-list seq) n) (when (eql x item) (setq n (1+ n)))))))
(unless (fboundp 'cl-assoc) (defun cl-assoc (key alist &rest _) (assoc key alist)))
(unless (fboundp 'cl-sort) (defun cl-sort (seq pred &rest _) (sort (nelisp-seq--to-list seq) pred)))
(unless (fboundp 'cl-remove-duplicates)
  (defun cl-remove-duplicates (seq &rest _)
    (let ((acc nil)) (dolist (x (nelisp-seq--to-list seq) (nreverse acc)) (unless (member x acc) (push x acc))))))
;; An EMPTY sequence reaches `min' through `apply', and Emacs reports the
;; subr object there rather than the symbol: (seq-min []) is
;; (wrong-number-of-arguments #<subr min> 0).  Signalling here rather than
;; letting the empty apply fall through keeps both of Emacs's two shapes.
(unless (fboundp 'seq-min)
  (defun seq-min (seq)
    (let ((l (nelisp-seq--to-list seq)))
      (unless l (signal 'wrong-number-of-arguments (list '(builtin min) 0)))
      (apply #'min l))))
(unless (fboundp 'seq-max)
  (defun seq-max (seq)
    (let ((l (nelisp-seq--to-list seq)))
      (unless l (signal 'wrong-number-of-arguments (list '(builtin max) 0)))
      (apply #'max l))))
;; `reverse' is already type-preserving, and routing through a list threw
;; that away: (seq-reverse "a") answered (97).  A caller that reversed a
;; string got something `aref' still works on, which is why this survives.
(unless (fboundp 'seq-reverse)
  (defun seq-reverse (&rest a)
    (unless (= (length a) 1)
      (signal 'wrong-number-of-arguments (list '(1 . 1) (length a))))
    (reverse (car a))))
(unless (fboundp 'seq-concatenate)
  (defun seq-concatenate (type &rest seqs)
    (dolist (x seqs)
      (unless (sequencep x)
        (signal 'error (list (format "Cannot convert %s into a sequence" x)))))
    (unless (memq type '(list vector string))
      (signal 'error (list (format "Not a sequence type name: %S" type))))
    (let ((l (apply #'append (mapcar #'nelisp-seq--to-list seqs))))
      (cond ((eq type 'vector) (apply #'vector l)) ((eq type 'string) (apply #'string l)) (t l)))))
(unless (fboundp 'seq-mapcat)
  ;; TYPE was ignored, so (seq-mapcat #'list '(1 2) 'vector) answered a list.
  ;; A caller that asked for a vector got something `aref' still works on,
  ;; which is why this kind of ignored argument survives.
  (defun seq-mapcat (fn seq &optional type)
    ;; TYPE is looked at LAST: `seq-map' runs first, so the SEQUENCE and then
    ;; FN are what a bad call reports.  Checking TYPE up front named the
    ;; third argument for a call that fails on the second.
    (unless (sequencep seq) (signal 'wrong-type-argument (list 'sequencep seq)))
    (when (and (nelisp-seq--to-list seq) (not (functionp fn)))
      (signal (if (symbolp fn) 'void-function 'invalid-function) (list fn)))
    (when type
      (unless (memq type '(list vector string))
        (signal 'error (list (format "Not a sequence type name: %S" type)))))
    (let ((flat (apply #'append (mapcar (lambda (x) (nelisp-seq--to-list (funcall fn x)))
                                        (nelisp-seq--to-list seq)))))
      (cond ((eq type 'vector) (apply #'vector flat))
            ((eq type 'string) (apply #'string flat))
            (t flat)))))
(unless (fboundp 'seq-mapn)
  (defun seq-mapn (fn &rest seqs)
    (dolist (x seqs)
      (unless (sequencep x) (signal 'wrong-type-argument (list 'sequencep x))))
    (if (or (null seqs)
            (memq t (mapcar (lambda (x) (if (listp x) (null x) (= (length x) 0)))
                            seqs)))
        nil
      (unless (functionp fn)
        (signal (if (symbolp fn) 'void-function 'invalid-function) (list fn)))
      (apply #'cl-mapcar fn (mapcar #'nelisp-seq--to-list seqs)))))
(unless (fboundp 'seq-partition)
  (defun seq-partition (seq n)
    (unless (numberp n) (signal 'wrong-type-argument (list 'number-or-marker-p n)))
    ;; A chunk size of zero takes nothing and drops nothing, so the walk
    ;; never advanced: (seq-partition '(1 2 3) 0) did not answer nil, it
    ;; HUNG.  Emacs answers nil.
    (if (<= n 0)
        nil
      (when (consp seq)
        (let ((tl seq))
          (while (consp tl) (setq tl (cdr tl)))
          (when tl (signal 'wrong-type-argument (list 'sequencep tl)))))
      (let ((rest seq) (acc nil))
        (while (if (listp rest) rest (> (length rest) 0))
          (push (seq-take rest n) acc)
          (setq rest (seq-drop rest n)))
        (nreverse acc)))))
(unless (fboundp 'seq-group-by)
  (defun seq-group-by (fn seq)
    (let ((res nil))
      (dolist (x (nelisp-seq--to-list seq))
        (let* ((k (funcall fn x)) (cell (assoc k res)))
          (if cell (setcdr cell (cons x (cdr cell))) (push (cons k (list x)) res))))
      (mapcar (lambda (c) (cons (car c) (nreverse (cdr c)))) (nreverse res)))))
(unless (fboundp 'string-remove-prefix)
  (defun string-remove-prefix (prefix s) (if (string-prefix-p prefix s) (substring s (length prefix)) s)))
(unless (fboundp 'string-remove-suffix)
  (defun string-remove-suffix (suffix s) (if (string-suffix-p suffix s) (substring s 0 (- (length s) (length suffix))) s)))
(unless (fboundp 'string-blank-p)
  (defun string-blank-p (s)
    ;; Emacs answers the MATCH POSITION (0 for a blank string), not t --
    ;; it is `string-match-p' underneath, and callers use the index.
    (nelisp--check-string s)
    (string-match-p "\\`[ \t\n\r]*\\'" s)))
(unless (fboundp 'string-split)
  (defun string-split (s &optional sep omit trim)
    ;; Emacs checks SEPARATORS before STRING, so a call with both wrong
    ;; names the separator.
    (when sep (nelisp--check-string sep))
    (nelisp--check-string s)
    (split-string s sep omit trim)))
;; REGEXP was accepted and ignored -- the parameter was even named `_re' to
;; say so -- so (string-trim-left "xxab" "x+") answered "xxab".  A caller
;; that asked to strip a specific prefix got the default whitespace strip
;; and no indication.  The whitespace path stays a character loop: it is the
;; common call, it needs no regexp engine, and keeping it means this fix
;; cannot regress the callers that pass no REGEXP.
(unless (fboundp 'string-trim-left)
  (defun string-trim-left (s &optional re)
    ;; No hand-written REGEXP check: Emacs builds a regexp with `concat' and
    ;; whatever `concat' says IS the contract -- `sequencep' for a symbol,
    ;; `listp' for the tail of an improper list.  Two hand-written rules here
    ;; each got one of those cases right and the other wrong.
    (if (null re)
        (progn
          (nelisp--check-string s)
        (let ((i 0) (n (length s))) (while (and (< i n) (memq (aref s i) '(32 9 10 13))) (setq i (1+ i))) (substring s i)))
      (if (string-match (concat "\\`\\(?:" re "\\)") s) (substring s (match-end 0)) s))))
(unless (fboundp 'string-trim-right)
  (defun string-trim-right (s &optional re)
    (if (null re)
        (progn
          (nelisp--check-string s)
          (let ((n (length s))) (while (and (> n 0) (memq (aref s (1- n)) '(32 9 10 13))) (setq n (1- n))) (substring s 0 n)))
      (let ((i (string-match (concat "\\(?:" re "\\)\\'") s)))
        (if i (substring s 0 i) s)))))
;; `isnan' and `nbutlast' were absent, so a caller got `void-function' --
;; which reads as "NeLisp cannot do this" rather than "nobody wrote it yet".
(unless (fboundp 'isnan)
  (defun isnan (x)
    (if (floatp x) (/= x x) (signal 'wrong-type-argument (list 'floatp x)))))
(unless (fboundp 'nbutlast)
  (defun nbutlast (list &optional n)
    (let ((m (length list)) (k (or n 1)))
      (if (>= k m) nil (setcdr (nthcdr (- m k 1) list) nil) list))))
;; NAME identity is symbol identity in this runtime, so the probe is the
;; native `nelisp--intern-lookup' (Doc 163 Phase C), which reports a miss
;; instead of interning.  Falling back to `intern' -- which never answers
;; nil -- is what made a `(while (setq x (intern-soft ...)))' probe loop
;; run forever.  This shadows nothing: `intern-soft' was in
;; lisp/nelisp-stdlib-misc.el and never reached the prelude, so the
;; standalone had no `intern-soft' at all.
(defun intern-soft (name &optional obarray)
  "Return the symbol named NAME if it is interned, else nil.
NeLisp has one global intern table and no first-class obarray object, so a
non-nil OBARRAY is not honoured.  The probe is `nelisp--intern-lookup\', which
reports a miss instead of interning -- falling back to `intern\', which never
answers nil, is what made a `(while (setq x (intern-soft ...)))\' probe loop
run forever."
  (when (and obarray (not (obarrayp obarray)))
    (signal 'wrong-type-argument (list 'obarrayp obarray)))
  (cond ((symbolp name) name)
        ((stringp name) (nelisp--intern-lookup name))
        (t (signal 'wrong-type-argument (list 'stringp name)))))
(unless (fboundp 'vconcat)
  (defun vconcat (&rest seqs)
    (dolist (x seqs) (nelisp--check-seq-list x))
    (nelisp--doc200-check-string-mix seqs)
    (apply #'vector (apply #'append (mapcar (lambda (x) (append x nil)) seqs)))))
;; `string-to-number' existed only in lisp/nelisp-stdlib-plist-str.el, which
;; the standalone never loads, so the native integer-only parser answered:
;; "1.5" came back 1, and (string-to-number "ff" 16) came back 0 because the
;; RADIX argument had nowhere to go.  Both are wrong answers with no error --
;; the same shape as `compare-strings' and `lsh' before them.  This is that
;; file's text, unchanged, so `make ns-gate' polices the two copies.
(defun nelisp-stdlib--whitespace-p (ch)
  "Return non-nil when CH (= integer codepoint) is ASCII whitespace.
Matches the Emacs default whitespace class for `string-trim'."
  (or (eq ch ?\s) (eq ch ?\t) (eq ch ?\n) (eq ch ?\r)
      (eq ch ?\f) (eq ch 11)))                ; 11 = ?\v

(defun nelisp-stdlib--digit-value (ch radix)
  "Return integer 0..RADIX-1 encoded by CH, or nil if CH is not a digit
in the given RADIX (= 2..36).  Accepts both upper- and lowercase
letters for RADIX > 10."
  (let ((v (cond
            ((and (>= ch ?0) (<= ch ?9)) (- ch ?0))
            ((and (>= ch ?a) (<= ch ?z)) (+ 10 (- ch ?a)))
            ((and (>= ch ?A) (<= ch ?Z)) (+ 10 (- ch ?A)))
            (t nil))))
    (if (and v (< v radix)) v nil)))

(defun nelisp--pow10 (k)
  "10^K as a double, for 0 <= K <= 22 -- every one of those is exact."
  (let ((acc 1.0) (i 0))
    (while (< i k) (setq acc (* acc 10.0)) (setq i (1+ i)))
    acc))
(defun nelisp--scale10 (m e)
  "M times 10^E, in steps of at most 22 so each step is an exact power."
  (let ((v m) (k (abs e)))
    (while (> k 0)
      (let ((step (if (> k 22) 22 k)))
        (setq v (if (< e 0) (/ v (nelisp--pow10 step)) (* v (nelisp--pow10 step))))
        (setq k (- k step))))
    v))
(defun string-to-number (s &optional radix)
  "Parse a number from the leading portion of S.
RADIX (default 10) selects the integer base.  Float syntax (`.',
`e' / `E') is recognised only when RADIX is 10 or nil — otherwise
only the integer prefix is accepted (matching the host Emacs
contract).  Returns 0 when no leading digit is found.

Pure-elisp impl: int parse drives a digit loop; float branch uses
`(/ frac 1.0 ...)' for promote-on-mixed semantics and a multiply
loop for the exponent (= no `expt' / `float' primitive needed)."
  (nelisp--check-string s)
  (when radix
    (unless (integerp radix) (signal 'wrong-type-argument (list 'fixnump radix)))
    (when (or (< radix 2) (> radix 16))
      (signal 'args-out-of-range (list radix))))
  (let* ((r (or radix 10))
         (n (length s))
         (i 0))
    ;; Skip leading whitespace.
    (while (and (< i n) (nelisp-stdlib--whitespace-p (aref s i)))
      (setq i (1+ i)))
    (let ((sign 1))
      (cond
       ((and (< i n) (eq (aref s i) ?-))
        (setq sign -1)
        (setq i (1+ i)))
       ((and (< i n) (eq (aref s i) ?+))
        (setq i (1+ i))))
      (let ((int-part 0)
            (int-digits 0)
            ;; MANT/DEXP are the float path's mantissa and decimal exponent,
            ;; accumulated separately from INT-PART because INT-PART OVERFLOWS:
            ;; "1.44269504088896338700e+00" has a 20-digit fraction, and
            ;; multiplying that into an i64 wrapped it -- the answer came back
            ;; 0.1514..., a plausible-looking float about ten times too small.
            ;; Digits past the 17th cannot change a double, so they are
            ;; counted into DEXP rather than accumulated.
            (mant 0)
            (dexp 0))
        ;; Integer-digit loop.  INT-PART accumulates WITH the sign folded in
        ;; from the first digit (`(* sign d)', not a separate `(* sign
        ;; int-part)' after the loop) -- Doc 187 made the native `+'/`*'
        ;; this loop calls signal `overflow-error' past the fixnum boundary,
        ;; and the two boundaries are ASYMMETRIC: `most-negative-fixnum''s
        ;; own canonical literal, "-2305843009213693952", has a MAGNITUDE
        ;; (2305843009213693952) that by itself exceeds
        ;; `most-positive-fixnum' by one.  Accumulating unsigned-then-
        ;; negating hits that overflow check mid-parse on the magnitude
        ;; alone (regression measured this session: even the bare positive
        ;; token "2305843009213693952" started signalling); accumulating
        ;; WITH the sign never exceeds the boundary for any literal whose
        ;; final signed value is in range, since every digit prefix of a
        ;; signed decimal number has magnitude no greater than the full
        ;; number's.
        (let ((continue t))
          (while (and continue (< i n))
            (let ((d (nelisp-stdlib--digit-value (aref s i) r)))
              (cond
               (d
                (setq int-part (+ (* int-part r) (* sign d)))
                (if (< mant 100000000000000000)
                    (setq mant (+ (* mant 10) d))
                  (setq dexp (1+ dexp)))
                (setq i (1+ i))
                (setq int-digits (1+ int-digits)))
               (t (setq continue nil))))))
        (cond
         ;; Float branch (only when radix = 10 and we hit `.' / `e' / `E').
         ((and (= r 10)
               (< i n)
               (or (eq (aref s i) ?.)
                   (eq (aref s i) ?e)
                   (eq (aref s i) ?E)))
          (let ((frac-num 0)
                (frac-denom 1)
                (frac-digits 0)
                (exp-sign 1)
                (exp-val 0)
                (has-exp nil))
            ;; Optional fractional part.
            (when (and (< i n) (eq (aref s i) ?.))
              (setq i (1+ i))
              (let ((continue t))
                (while (and continue (< i n))
                  (let ((d (nelisp-stdlib--digit-value (aref s i) 10)))
                    (cond
                     (d
                      (setq frac-num (+ (* frac-num 10) d))
                      (setq frac-denom (* frac-denom 10))
                      (when (< mant 100000000000000000)
                        (setq mant (+ (* mant 10) d))
                        (setq dexp (1- dexp)))
                      (setq frac-digits (1+ frac-digits))
                      (setq i (1+ i)))
                     (t (setq continue nil)))))))
            ;; Optional exponent.
            (when (and (< i n)
                       (or (eq (aref s i) ?e) (eq (aref s i) ?E)))
              (setq i (1+ i))
              (cond
               ((and (< i n) (eq (aref s i) ?-))
                (setq exp-sign -1)
                (setq i (1+ i)))
               ((and (< i n) (eq (aref s i) ?+))
                (setq i (1+ i))))
              (let ((continue t))
                (while (and continue (< i n))
                  (let ((d (nelisp-stdlib--digit-value (aref s i) 10)))
                    (cond
                     (d
                      (setq exp-val (+ (* exp-val 10) d))
                      (setq i (1+ i))
                      (setq has-exp t))
                     (t (setq continue nil)))))))
            ;; Compute value: (sign * (int-part + frac-num/frac-denom)) * 10^exp.
            ;; A trailing `.' with nothing after it does NOT make a float:
            ;; Emacs reads "1." as the integer 1 and "-2." as -2.  Entering
            ;; the float branch on the `.' alone returned 1.0, which is a
            ;; different type flowing into whatever the caller does next.
            (if (and (= frac-digits 0) (not has-exp))
                int-part
              ;; Scaling by repeated multiplication by 10.0 or 0.1 drifted --
              ;; 0.1 is not representable, so "1e300" came back
              ;; 1.0000000000000002e+300.  Powers of ten up to 1e22 ARE exact,
              ;; so the scale is applied in one step where it fits and in
              ;; chunks of 22 where it does not.
              (let ((val (nelisp--scale10
                          (float mant)
                          (+ dexp (if has-exp (* exp-sign exp-val) 0)))))
                (if (< sign 0) (- 0.0 val) val)))))
         ;; Pure integer.  INT-PART already carries the sign (see the
         ;; digit-loop comment above); re-multiplying by SIGN here would
         ;; double-apply it.
         ((> int-digits 0) int-part)
         (t 0))))))

;; Rust-min (2026-05-06 batch 4): copy-tree + sort.

;; `sxhash' and its three siblings were absent, so any file that mentions one
;; -- lisp/nelisp-stdlib-misc.el among them -- died with `void-function' at
;; load time, which is how `standalone-reader-intern-soft-smoke' had been red.
;;
;; The contract a hash has to keep is that `equal' objects hash equal; the
;; VALUES are explicitly not specified by Emacs and differ between its own
;; versions, so matching Emacs's numbers is not a thing to aim at.  The depth
;; and length caps are Emacs's (3 and 7): without them hashing a long or
;; circular list walks forever, and a hash that hangs is worse than a coarse
;; one.
(unless (fboundp 'sxhash-equal)
  (defun nelisp--sxhash-string (str)
    (let ((h 0) (i 0) (n (length str)))
      (while (< i n)
        (setq h (logand (+ (* h 33) (aref str i)) 1073741823))
        (setq i (1+ i)))
      h))
  (defun nelisp--sxhash-walk (obj depth)
    (cond
     ((null obj) 0)
     ((eq obj t) 1)
     ((symbolp obj) (nelisp--sxhash-string (symbol-name obj)))
     ((stringp obj) (nelisp--sxhash-string obj))
     ((integerp obj) (logand obj 1073741823))
     ((floatp obj) (nelisp--sxhash-string (number-to-string obj)))
     ((>= depth 3) 0)
     ((consp obj)
      (let ((h 7) (k 0) (cur obj))
        (while (and (consp cur) (< k 7))
          (setq h (logand (+ (* h 33) (nelisp--sxhash-walk (car cur) (1+ depth)))
                          1073741823))
          (setq cur (cdr cur))
          (setq k (1+ k)))
        (unless (null cur)
          (setq h (logand (+ (* h 33) (nelisp--sxhash-walk cur (1+ depth)))
                          1073741823)))
        h))
     ((vectorp obj)
      (let ((h 11) (k 0) (n (length obj)))
        (while (and (< k n) (< k 7))
          (setq h (logand (+ (* h 33) (nelisp--sxhash-walk (aref obj k) (1+ depth)))
                          1073741823))
          (setq k (1+ k)))
        h))
     (t 0)))
  (defun sxhash-equal (obj) (nelisp--sxhash-walk obj 0))
  (defun sxhash (obj) (sxhash-equal obj))
  (defun sxhash-eq (obj) (nelisp--sxhash-walk obj 3))
  (defun sxhash-eql (obj) (nelisp--sxhash-walk obj 3)))
(unless (fboundp 'string-to-vector)
  (defun string-to-vector (s)
    (nelisp--check-seq-list s)
    (apply #'vector (append s nil))))
;; Absent, so callers got `void-function' -- which reads as "the runtime
;; cannot do this" rather than "nobody has written it yet".  The length
;; predicates walk only as far as they must, which is the reason they exist
;; instead of `(= (length x) n)'.
(unless (fboundp 'length=)
  (defun length= (seq n)
    (if (listp seq)
        (let ((k 0)) (while (and seq (<= k n)) (setq k (1+ k) seq (cdr seq))) (= k n))
      (= (length seq) n))))
(unless (fboundp 'length<)
  (defun length< (seq n)
    (if (listp seq)
        (let ((k 0)) (while (and seq (< k n)) (setq k (1+ k) seq (cdr seq))) (and (null seq) (< k n)))
      (< (length seq) n))))
(unless (fboundp 'length>)
  (defun length> (seq n)
    (if (listp seq)
        (let ((k 0)) (while (and seq (<= k n)) (setq k (1+ k) seq (cdr seq))) (> k n))
      (> (length seq) n))))
(unless (fboundp 'file-name-concat)
  (defun file-name-concat (directory &rest components)
    (let ((out (or directory "")))
      (dolist (c components)
        (when (and c (not (equal c "")))
          (setq out (if (or (equal out "")
                            (eq (aref out (1- (length out))) ?/))
                        (concat out c)
                      (concat out "/" c)))))
      out)))
(unless (fboundp 'string-distance)
  (defun string-distance (a b &optional _bytecompare)
    ;; Levenshtein, one row at a time: the full matrix is not needed and the
    ;; row form keeps this linear in space for the long strings callers pass.
    (let* ((la (length a)) (lb (length b))
           (prev (make-vector (1+ lb) 0))
           (cur (make-vector (1+ lb) 0))
           (i 0))
      (while (<= i lb) (aset prev i i) (setq i (1+ i)))
      (setq i 0)
      (while (< i la)
        (aset cur 0 (1+ i))
        (let ((j 0))
          (while (< j lb)
            (let ((cost (if (eq (aref a i) (aref b j)) 0 1)))
              (aset cur (1+ j) (min (1+ (aref cur j))
                                    (1+ (aref prev (1+ j)))
                                    (+ cost (aref prev j)))))
            (setq j (1+ j))))
        (let ((tmp prev)) (setq prev cur) (setq cur tmp))
        (setq i (1+ i)))
      (aref prev lb))))
;; Doc 160 breadth round 2: control/binding macros.
(unless (fboundp 'letrec)
  (defmacro letrec (bindings &rest body)
    `(let ,(mapcar #'car bindings)
       ,@(mapcar (lambda (b) `(setq ,(car b) ,(cadr b))) bindings)
       ,@body)))
(unless (fboundp 'named-let)
  (defmacro named-let (name bindings &rest body)
    `(cl-labels ((,name ,(mapcar #'car bindings) ,@body))
       (,name ,@(mapcar #'cadr bindings)))))
(unless (fboundp 'and-let*)
  (defmacro and-let* (bindings &rest body)
    (if body `(when-let* ,bindings ,@body)
      (let ((lastb (car (last bindings))))
        `(when-let* ,bindings ,(if (consp lastb) (car lastb) lastb))))))
(unless (fboundp 'thread-first)
  (defmacro thread-first (x &rest forms)
    (let ((result x))
      (dolist (form forms result)
        (setq result (if (listp form) `(,(car form) ,result ,@(cdr form)) (list form result)))))))
(unless (fboundp 'thread-last)
  (defmacro thread-last (x &rest forms)
    (let ((result x))
      (dolist (form forms result)
        (setq result (if (listp form) `(,@form ,result) (list form result)))))))
(unless (fboundp 'cl-decf)
  (defmacro cl-decf (place &optional n) `(cl-incf ,place ,(if n `(- ,n) -1))))
(unless (fboundp 'cl-flet)
  (defmacro cl-flet (bindings &rest body) `(cl-labels ,bindings ,@body)))
(unless (fboundp 'cl-values) (defun cl-values (&rest vals) vals))
(unless (fboundp 'cl-multiple-value-bind)
  (defmacro cl-multiple-value-bind (vars form &rest body)
    `(cl-destructuring-bind ,vars ,form ,@body)))
;; Doc 160 breadth round 3: number / set / char / string library gaps.
(unless (fboundp 'float)
  (defun float (x)
    (nelisp--check-number x)
    (if (floatp x) x (+ x 0.0))))
(unless (fboundp 'expt)
  (defun expt (b e)
  (nelisp--check-number b)
  (nelisp--check-number e)
    (cond ((and (integerp e) (>= e 0) (integerp b))
           (let ((r 1) (i 0))
             (while (< i e)
               (let ((next (* r b)))
                 ;; Doc 190 Phase B regression, found by the fencepost check
                 ;; ("expt overflow-check unaffected") this same phase's own
                 ;; smoke inherited from Phase A: `*' now PROMOTES a fixnum-
                 ;; boundary crossing to an exact Bignum instead of
                 ;; signalling, so `next' can genuinely be a Bignum here now
                 ;; -- something the div-round-trip check below was never
                 ;; written to expect (it assumes NEXT is always a plain
                 ;; fixnum).  `/' does not support a Bignum operand (Phase
                 ;; A's own recorded, still-unchanged gap), so falling
                 ;; through to `(/ next b)' on a promoted `next' signalled
                 ;; the WRONG condition, `wrong-type-argument', instead of
                 ;; `overflow-error'.  `expt' itself is OUT OF SCOPE for
                 ;; Phase B's promotion (this doc's own task brief: `+'/`-'/
                 ;; `*' only) -- it still signals `overflow-error' exactly
                 ;; as it did before, just via an explicit `bignump' guard
                 ;; now instead of (coincidentally, pre-Phase-B) always
                 ;; being pre-empted by the native `*''s OWN overflow signal
                 ;; before `next' could ever be bound to anything else.  The
                 ;; div-round-trip half stays as defense in depth for a true
                 ;; 64-bit-register wrap, even though `*' promoting rather
                 ;; than wrapping means it should no longer be reachable in
                 ;; practice.
                 (when (or (bignump next) (and (/= b 0) (/= (/ next b) r)))
                   (signal 'overflow-error nil))
                 (setq r next))
               (setq i (1+ i)))
             r))
          ((and (integerp e) (>= e 0)) (let ((r 1) (i 0)) (while (< i e) (setq r (* r b) i (1+ i))) r))
          ((integerp e) (/ 1.0 (expt b (- e))))
          ;; The half power is the non-integer exponent that actually turns
          ;; up, and `sqrt' answers it.  A general float exponent still
          ;; signals: it needs `exp'/`log', and the standalone links no libm
          ;; to take them from -- see the note on `sqrt' below.
          ((= b 0) (if (= e 0) 1.0 0.0))
          ((< b 0) (/ 0.0 0.0))
          ;; An INTEGRAL float exponent is the integer path, and a HALF
          ;; integer is that times `sqrt' -- both exact.  (expt 4 1.5) is
          ;; 4*2 = 8.0, where exp(1.5*log 4) lands one ULP low.
          ((= e (float (truncate e)))
           (float (expt b (truncate e))))
          ((= (* 2 e) (float (truncate (* 2 e))))
           (let ((n (truncate (- e 0.5))))
             (* (float (expt b n)) (sqrt (float b)))))
          (t (exp (* e (log (float b))))))))
(unless (fboundp 'sqrt)
  (defun nelisp--f64-split (a)
    "Dekker's split: A as HI + LO with HI carrying 26 significant bits."
    (let* ((c (* 134217729.0 a)) (hi (- c (- c a)))) (cons hi (- a hi))))
  (defun nelisp--two-prod-err (a b p)
    "The part of A*B that P, the rounded product, dropped."
    (let* ((sa (nelisp--f64-split a)) (sb (nelisp--f64-split b))
           (ahi (car sa)) (alo (cdr sa)) (bhi (car sb)) (blo (cdr sb)))
      (+ (+ (+ (- (* ahi bhi) p) (* ahi blo)) (* alo bhi)) (* alo blo))))
  (defun sqrt (x)
    "The square root of X.
Newton alone left the last bit wrong -- (sqrt 2) came back
1.414213562373095 where the host says 1.4142135623730951 -- because the
residual x - g*g cancels catastrophically in a double and cannot steer the
final rounding.  Computing that residual EXACTLY with Dekker's two-product
gives a correction that is right to the last bit, and the range reduction
to [1,4) by powers of four is exact, so the scaling back cannot spoil it.
Measured against the host over 24 arguments: only the largest finite double
still differs, by one ULP."
    (unless (numberp x) (signal 'wrong-type-argument (list 'numberp x)))
    (let ((xf (float x)))
      (cond
       ((< xf 0) (/ 0.0 0.0))
       ((= xf 0.0) 0.0)
       (t (let ((k 0) (m xf))
            (while (>= m 4.0) (setq m (/ m 4.0)) (setq k (1+ k)))
            (while (< m 1.0) (setq m (* m 4.0)) (setq k (1- k)))
            (let ((g m) (n 0))
              (while (< n 20) (setq g (/ (+ g (/ m g)) 2.0)) (setq n (1+ n)))
              (let* ((p (* g g)) (e (nelisp--two-prod-err g g p)) (r (- (- m p) e)))
                (setq g (+ g (/ r (* 2.0 g)))))
              (let ((acc g) (j (abs k)))
                (while (> j 0)
                  (setq acc (if (< k 0) (/ acc 2.0) (* acc 2.0)))
                  (setq j (1- j)))
                acc))))))))
(unless (fboundp 'isqrt)
  (defun isqrt (n)
    (if (< n 2) n (let ((x n) (y (/ (+ n 2) 2))) (while (< y x) (setq x y y (/ (+ x (/ n x)) 2))) x))))
(unless (fboundp 'remove)
  (defun remove (item seq)
    "Return a copy of SEQ without members `equal' to ITEM.
Type-preserving: a string answers a string and a vector a vector, as in
Emacs.  Answering a list of character codes for a string is a different
TYPE flowing into whatever the caller does next."
    (let ((kept (let (acc)
                  (dolist (x (nelisp-seq--to-list seq) (nreverse acc))
                    (unless (equal x item) (push x acc))))))
      (cond ((stringp seq) (apply #'string kept))
            ((vectorp seq) (apply #'vector kept))
            (t kept)))))
(unless (fboundp 'ffloor)
  (defun ffloor (x)
    ;; A FLOAT, not any number: (ffloor 65) signals in Emacs while
    ;; (floor 65) is 65.  The `f' family is the strict one.
    (unless (floatp x) (signal 'wrong-type-argument (list 'floatp x)))
    (float (floor x))))
(unless (fboundp 'fceiling)
  (defun fceiling (x)
    (unless (floatp x) (signal 'wrong-type-argument (list 'floatp x)))
    (float (ceiling x))))
(unless (fboundp 'ftruncate)
  (defun ftruncate (x)
    (unless (floatp x) (signal 'wrong-type-argument (list 'floatp x)))
    (float (truncate x))))
(unless (fboundp 'fround)
  (defun fround (x)
    (unless (floatp x) (signal 'wrong-type-argument (list 'floatp x)))
    (float (round x))))
(unless (fboundp 'cl-floor) (defun cl-floor (x &optional y) (let* ((d (or y 1)) (q (floor x d))) (list q (- x (* q d))))))
(unless (fboundp 'cl-ceiling) (defun cl-ceiling (x &optional y) (let* ((d (or y 1)) (q (ceiling x d))) (list q (- x (* q d))))))
(unless (fboundp 'cl-truncate) (defun cl-truncate (x &optional y) (let* ((d (or y 1)) (q (truncate x d))) (list q (- x (* q d))))))
(unless (fboundp 'cl-round) (defun cl-round (x &optional y) (let* ((d (or y 1)) (q (floor (+ (/ (float x) d) 0.5)))) (list q (- x (* q d))))))
(unless (fboundp 'cl-mod) (defun cl-mod (x y) (mod x y)))
(unless (fboundp 'cl-rem) (defun cl-rem (x y) (- x (* (truncate x y) y))))
(unless (fboundp 'cl-signum) (defun cl-signum (x) (cond ((> x 0) 1) ((< x 0) -1) (t 0))))
(unless (fboundp 'cl-union)
  (defun cl-union (a b &rest _) (let ((r (reverse a))) (dolist (x b) (unless (member x a) (push x r))) (nreverse r))))
(unless (fboundp 'cl-intersection)
  (defun cl-intersection (a b &rest _) (let (r) (dolist (x a (nreverse r)) (when (member x b) (push x r))))))
(unless (fboundp 'cl-subsetp)
  (defun cl-subsetp (a b &rest _) (let ((ok t)) (dolist (x a ok) (unless (member x b) (setq ok nil))))))
(unless (fboundp 'cl-position-if)
  (defun cl-position-if (pred seq)
    (let ((i 0) (res nil) (l (nelisp-seq--to-list seq)))
      (while (and l (not res)) (when (funcall pred (car l)) (setq res i)) (setq i (1+ i) l (cdr l))) res)))
(unless (fboundp 'cl-mapcan) (defun cl-mapcan (fn &rest lists) (apply #'append (apply #'cl-mapcar fn lists))))
(unless (fboundp 'assq-delete-all)
  (defun assq-delete-all (key alist)
    (unless (listp alist) (signal 'wrong-type-argument (list 'listp alist)))
    (let (acc) (dolist (e alist (nreverse acc)) (unless (and (consp e) (eq (car e) key)) (push e acc))))))
(unless (fboundp 'capitalize)
  (defun capitalize (obj)
    "Title-case OBJ: upcase each word-initial letter, downcase the rest.
Built from `concat\' rather than `aset\' because a mapped character can be
wider than the one it replaces once the mapping leaves ASCII."
    (unless (or (stringp obj)
                (and (integerp obj) (>= obj 0) (<= obj 4194303)))
      (signal 'wrong-type-argument (list 'char-or-string-p obj)))
    (unless (or (integerp obj) (stringp obj))
      (signal 'wrong-type-argument (list 'char-or-string-p obj)))
    (if (integerp obj) (nelisp--case-up-char obj)
      (let ((out "") (i 0) (n (length obj)) (prev nil))
        (while (< i n)
          (let* ((c (aref obj i))
                 (w (or (nelisp--case-letter-p c) (and (>= c ?0) (<= c ?9)))))
            (setq out (concat out (char-to-string
                                   (if w
                                       (if prev (nelisp--case-down-char c)
                                         (nelisp--case-up-char c))
                                     c))))
            (setq prev w))
          (setq i (1+ i)))
        out))))
(unless (fboundp 'cl-digit-char-p)
  (defun cl-digit-char-p (ch &optional radix)
    (let* ((r (or radix 10))
           (v (cond ((and (>= ch ?0) (<= ch ?9)) (- ch ?0))
                    ((and (>= ch ?a) (<= ch ?z)) (+ 10 (- ch ?a)))
                    ((and (>= ch ?A) (<= ch ?Z)) (+ 10 (- ch ?A))) (t nil))))
      (if (and v (< v r)) v nil))))
(unless (fboundp 'char-uppercase-p)
  (defun char-uppercase-p (ch)
    (nelisp--check-character ch)
    (and (>= ch ?A) (<= ch ?Z))))
(unless (fboundp 'string-lessp)
  (defun string-lessp (a b)
    ;; `string-lessp' takes a string OR a symbol, like `string=' -- checking
    ;; for a string outright would reject the symbol Emacs accepts.
    (unless (or (stringp a) (symbolp a)) (signal 'wrong-type-argument (list 'stringp a)))
    (unless (or (stringp b) (symbolp b)) (signal 'wrong-type-argument (list 'stringp b)))
    (let* ((sa (if (symbolp a) (symbol-name a) a)) (sb (if (symbolp b) (symbol-name b) b))
           (la (length sa)) (lb (length sb)) (i 0) (res nil) (done nil))
      (while (and (not done) (< i la) (< i lb))
        (let ((ca (aref sa i)) (cb (aref sb i)))
          (cond ((< ca cb) (setq res t done t)) ((> ca cb) (setq res nil done t))))
        (setq i (1+ i)))
      (if done res (< la lb)))))
(unless (fboundp 'string<)
  (defun string< (a b)
    ;; `string-lessp' accepts a symbol, and so does this -- but a VECTOR is
    ;; neither, and answering t for it made an ordering silently wrong.
    (unless (or (stringp a) (symbolp a)) (signal 'wrong-type-argument (list 'stringp a)))
    (unless (or (stringp b) (symbolp b)) (signal 'wrong-type-argument (list 'stringp b)))
    (string-lessp a b)))
(unless (fboundp 'message) (defun message (fmt &rest args) (if (null fmt) nil (apply #'format fmt args))))
(unless (fboundp 'string-equal-ignore-case)
  (defun string-equal-ignore-case (a b)
    ;; Check STRINGP first: routing through `downcase' named
    ;; `char-or-string-p', which is a different claim from what Emacs makes
    ;; about this function's arguments.
    (nelisp--check-string a)
    (nelisp--check-string b)
    (string-equal (downcase a) (downcase b))))
;; `make-hash-table' took ANY argument list: (make-hash-table nil \='(1 2 . 3))
;; answered a table, and a caller who misspelled a keyword got a table with
;; none of the properties it asked for -- silently.  Emacs signals, and the
;; data it carries is `signal-error' shaped: a proper-list argument (nil
;; included) SPLICES, so a nil key produces (error "Invalid argument list")
;; with nothing after the message.  Defined with `defun\=' rather than
;; `unless fboundp\=' on purpose: the name is a native builtin, and this
;; shadows it, calling the raw constructor underneath.
(defun nelisp--signal-invalid (msg arg)
  (signal 'error (cons msg (if (proper-list-p arg) arg (list arg)))))
(defun make-hash-table (&rest keys)
  (let ((ks keys))
    (while ks
      (let ((k (car ks)) (rest (cdr ks)))
        (unless (and (consp rest)
                     (memq k '(:test :size :rehash-size :rehash-threshold
                               :weakness :purecopy)))
          (nelisp--signal-invalid "Invalid argument list" k))
        (let ((v (car rest)))
          (cond
           ((eq k :test)
            (unless (memq v '(eq eql equal))
              (nelisp--signal-invalid "Invalid hash table test" v)))
           ((eq k :size)
            (unless (natnump v)
              (nelisp--signal-invalid "Invalid hash table size" v)))
           ((eq k :weakness)
            (unless (memq v '(nil key value key-or-value key-and-value t))
              (nelisp--signal-invalid "Invalid hash table weakness" v)))))
        (setq ks (cdr rest)))))
  (let ((h (nelisp--hash-table-make-raw))
        (test (or (plist-get keys :test) 'eql)))
    (aset (car h) 2 test)
    h))
(unless (fboundp 'byte-compile-file)
  (defun byte-compile-file (filename &optional _load)
    "Check FILENAME the way Emacs does and report a missing input file.
There is no byte compiler in the standalone runtime; answering
`void-function' said so in a way no caller could act on, and hid the
argument error entirely."
    (nelisp--check-string filename)
    ;; "cannot read" and "is not there" are different conditions, and
    ;; `file-exists-p' cannot tell them apart -- it answers nil for both, so
    ;; an unreadable file was reported as a missing one.  access(2) says
    ;; which: -13 is EACCES, anything else non-zero is treated as absent.
    (let* ((full (expand-file-name filename))
           (rc (nelisp--syscall-path-int 21 full 4)))   ; access R_OK
      (cond
       ((and (= rc 0) (file-directory-p full))
        (signal 'file-error (list "Read error" "Is a directory" full)))
       ((= rc 0) nil)
       ((= rc -13)
        (signal 'permission-denied
                (list "Opening input file" "Permission denied" full)))
       (t (signal 'file-missing
                  (list "Opening input file" "No such file or directory" full)))))))
(unless (fboundp 'string-greaterp)
  (defun string-greaterp (a b) (string-lessp b a)))
;; `string>' is an ALIAS in Emacs, and its absence here was not a missing
;; feature but a `void-function' from ordinary code.
(unless (fboundp 'string>)
  (defun string> (a b) (string-lessp b a)))
(unless (fboundp 'string-version-lessp)
  (defun nelisp--version-rank (c)
    "Collation weight for C in `string-version-lessp'.

NOT the code point.  Derived by sorting 1..127 with Emacs 30.1 and reading
the order off:

  .  ~  0-9  A-Z  a-z   then everything else in code-point order

So an ALPHANUMERIC sorts below a punctuation or control character --
(string-version-lessp \"n\" \"\\t\") is t, which plain code-point order gets
backwards.  That is the case a nil argument lands on, since nil compares as
its name \"nil\", and it is how this rule was noticed at all."
    (cond ((= c ?.) 0)
          ((= c ?~) 1)
          ((and (>= c ?0) (<= c ?9)) (+ 2 (- c ?0)))
          ((and (>= c ?A) (<= c ?Z)) (+ 12 (- c ?A)))
          ((and (>= c ?a) (<= c ?z)) (+ 38 (- c ?a)))
          (t (+ 64 c))))

  (defun string-version-lessp (a b)
    "Compare A and B, reading runs of digits as NUMBERS.
Not `string-lessp': (string-version-lessp \"a2\" \"a10\") is t while
`string-lessp' says nil, because 2 < 10 but \"2\" sorts after \"1\".
Delegating was wrong for every pair whose digits differ in length, which is
the only case this function exists for."
    (unless (or (stringp a) (symbolp a)) (signal 'wrong-type-argument (list 'stringp a)))
    (unless (or (stringp b) (symbolp b)) (signal 'wrong-type-argument (list 'stringp b)))
    (let* ((x (if (stringp a) a (symbol-name a)))
           (y (if (stringp b) b (symbol-name b)))
           (i 0) (j 0) (nx (length x)) (ny (length y))
           (done nil) (result nil))
      (while (not done)
        (cond
         ((and (>= i nx) (>= j ny)) (setq done t) (setq result nil))
         ((>= i nx) (setq done t) (setq result t))
         ((>= j ny) (setq done t) (setq result nil))
         ((and (>= (aref x i) ?0) (<= (aref x i) ?9)
               (>= (aref y j) ?0) (<= (aref y j) ?9))
          (let ((vx 0) (vy 0))
            (while (and (< i nx) (>= (aref x i) ?0) (<= (aref x i) ?9))
              (setq vx (+ (* vx 10) (- (aref x i) ?0))) (setq i (1+ i)))
            (while (and (< j ny) (>= (aref y j) ?0) (<= (aref y j) ?9))
              (setq vy (+ (* vy 10) (- (aref y j) ?0))) (setq j (1+ j)))
            (unless (= vx vy) (setq done t) (setq result (< vx vy)))))
         ((= (aref x i) (aref y j)) (setq i (1+ i)) (setq j (1+ j)))
         (t (setq done t)
            (setq result (< (nelisp--version-rank (aref x i))
                            (nelisp--version-rank (aref y j)))))))
      result)))
;; Doc 160 breadth round 3: control / binding macros.
;; `interactive' is a command-declaration marker.  When a command is called
;; non-interactively (the only mode on the headless standalone), Emacs skips
;; its interactive form entirely; the native evaluator instead evaluates the
;; leading `(interactive ...)' as a call, so define it as a no-op MACRO -- a
;; macro (not a defun) so the spec argument is never evaluated for effect.
(unless (fboundp 'interactive) (defmacro interactive (&rest _) nil))
;; `special-variable-p' is consulted by generator.el's CPS transform to decide
;; whether a `let*' binding inside an `iter-lambda' needs dynamic save/restore
;; (t) or can be alpha-renamed lexically (nil).  The bare reader cannot query a
;; symbol's special flag, but standalone code is lexical-binding, so the loop /
;; local bindings a generator introduces are lexical: answer nil so the lexical
;; rewrite path is taken.  (Free references to genuinely-special vars are NOT
;; let-bindings, so they are untouched by this and still resolve dynamically.)
(unless (fboundp 'special-variable-p)
  (defun special-variable-p (symbol)
    (nelisp--check-symbol symbol)
    (if (or (null symbol) (eq symbol t) (keywordp symbol)) t nil)))
;; Headless host frame: the standalone has no Emacs frame, so report the
;; controlling terminal's size from $COLUMNS/$LINES, falling back to the
;; conventional 80x24.  (Export the vars, or refine with a TIOCGWINSZ ioctl,
;; to track live resizes.)
(unless (fboundp 'frame-width)
  (defun frame-width (&optional frame)
    (when frame (signal 'wrong-type-argument (list 'framep frame)))
    (let ((c (getenv "COLUMNS")))
      (if (and c (> (length c) 0)) (string-to-number c) 80))))
(unless (fboundp 'frame-height)
  (defun frame-height (&optional frame)
    ;; There are no frames here, so nothing can BE one; Emacs still checks.
    (when frame (signal 'wrong-type-argument (list 'framep frame)))
    (let ((l (getenv "LINES")))
      (if (and l (> (length l) 0)) (string-to-number l) 25))))
;; `random' via a 31-bit LCG (glibc constants).  Deterministic -- adequate for
;; tests / sampling, NOT for cryptography (use a getrandom syscall for that).
(unless (boundp 'nelisp--random-state) (defvar nelisp--random-state 305419896))
(unless (fboundp 'random)
  (defun random (&optional limit)
    "Pseudo-random integer.  Integer LIMIT>0 -> 0..LIMIT-1; string LIMIT
reseeds from its characters; nil -> a full LCG value."
    (when (stringp limit)
      (let ((i 0) (n (length limit)) (s 305419896))
        (while (< i n)
          (setq s (logand (+ (* s 31) (aref limit i)) 2147483647) i (1+ i)))
        (setq nelisp--random-state (logand (+ s 1) 2147483647) limit nil)))
    (setq nelisp--random-state
          (logand (+ (* nelisp--random-state 1103515245) 12345) 2147483647))
    (if (and (integerp limit) (> limit 0))
        (mod nelisp--random-state limit)
      nelisp--random-state)))
;; Headless timers: the standalone has no asynchronous event loop, so
;; `run-at-time' fires its FUNCTION synchronously (a single shot, REPEAT
;; ignored).  This suits code that drives its own scheduler from the timer
;; callback -- e.g. nelisp-eventloop's `schedule-timer' only *enqueues* an event
;; from the callback, which is later processed when the actor loop is run.
;; `sit-for' has nothing to redisplay or block on, so it is a no-op returning t.
(unless (fboundp 'run-at-time)
  (defun run-at-time (time _repeat function &rest args)
         ;; TIME is validated before FUNCTION is touched, so a bad time is a
         ;; time error -- not `invalid-function' about an argument Emacs never
         ;; reached.
    (unless (or (null time) (numberp time)
                (and (stringp time) (string-match-p "[0-9]" time))
                ;; A time VALUE is (HIGH LOW ...) -- at least two integers.
                ;; A one-element cons is not one, and Emacs says so before
                ;; it looks at FUNCTION.
                (and (consp time) (integerp (car time))
                     (consp (cdr time)) (integerp (car (cdr time)))))
      (signal 'error (list "Invalid time specification")))
    (apply function args)
    (list 'nelisp--sync-timer function)))
;; There are no timers in this runtime, so nothing can BE one -- which makes
;; the type check the only honest thing this can do: accepting a string and
;; answering nil reads as "cancelled", and it cancelled nothing.
(unless (fboundp 'timerp) (defun timerp (_x) nil))
(unless (fboundp 'cancel-timer)
  (defun cancel-timer (timer)
    (unless (timerp timer) (signal 'wrong-type-argument (list 'timerp timer)))
    nil))
(unless (fboundp 'sit-for) (defun sit-for (&rest _) t))
(unless (fboundp 'cl-dolist) (defmacro cl-dolist (spec &rest body) `(dolist ,spec ,@body)))
(unless (fboundp 'cl-dotimes) (defmacro cl-dotimes (spec &rest body) `(dotimes ,spec ,@body)))
(unless (fboundp 'cl-assert)
  (defmacro cl-assert (form &rest _) `(unless ,form (error "Assertion failed: %S" ',form))))
(unless (fboundp 'cl-check-type)
  (defmacro cl-check-type (x type &rest _) `(unless (cl-typep ,x ',type) (error "Wrong type: %S is not %S" ,x ',type))))
(unless (fboundp 'ignore-error)
  (defmacro ignore-error (cond &rest body) `(condition-case nil (progn ,@body) (,cond nil))))
(unless (fboundp 'with-demoted-errors)
  (defmacro with-demoted-errors (fmt &rest body)
    (let ((f (if (stringp fmt) fmt "Error: %S")) (b (if (stringp fmt) body (cons fmt body))))
      `(condition-case with-demoted--e (progn ,@b) (error (message ,f with-demoted--e) nil)))))
(unless (fboundp 'seq-let)
  (defmacro seq-let (args seq &rest body) `(cl-destructuring-bind ,args (nelisp-seq--to-list ,seq) ,@body)))
(unless (fboundp 'dlet)
  (defmacro dlet (bindings &rest body)
    `(progn ,@(mapcar (lambda (b) `(defvar ,(if (consp b) (car b) b))) bindings)
            (let ,bindings ,@body))))
(unless (fboundp 'while-let)
  (defmacro while-let (spec &rest body)
    (let ((bs (if (and (consp spec) (symbolp (car spec))) (list spec) spec)))
      `(catch 'while-let--done
         (while t
           (let* ,(mapcar (lambda (b) (if (consp b) b (list b b))) bs)
             (unless (and ,@(mapcar (lambda (b) (if (consp b) (car b) b)) bs))
               (throw 'while-let--done nil))
             ,@body))))))
(unless (fboundp 'cl-letf)
  (defmacro cl-letf (bindings &rest body)
    (let ((saves nil) (sets nil) (restores nil))
      (dolist (b bindings)
        (let ((place (car b)) (val (cadr b)) (sv (gensym)))
          (cond
           ((and (consp place) (eq (car place) 'symbol-value))
            (let ((sym (cadr (cadr place))))
              (push `(,sv (symbol-value ',sym)) saves)
              (push `(set ',sym ,val) sets)
              (push `(set ',sym ,sv) restores)))
           ((and (consp place) (eq (car place) 'symbol-function))
            (let ((sym (cadr (cadr place))))
              (push `(,sv (and (fboundp ',sym) (symbol-function ',sym))) saves)
              (push `(fset ',sym ,val) sets)
              (push `(if ,sv (fset ',sym ,sv) (fmakunbound ',sym)) restores)))
           (t
            (push `(,sv ,place) saves)
            (push `(setq ,place ,val) sets)
            (push `(setq ,place ,sv) restores)))))
      `(let ,(nreverse saves)
         (unwind-protect (progn ,@(nreverse sets) ,@body)
           ,@(nreverse restores))))))
(unless (fboundp 'nelisp--check-seq-count)
  (defun nelisp--check-seq-count (seq n)
    "Check N then SEQ, in the order Emacs does, for `seq-take'/`seq-drop'.
A LIST goes through `take'/`nthcdr', which name `integerp' for anything
that is not an integer; every other sequence goes through the generic
path, which asks for a NUMBER first and only then for an integer."
    (if (listp seq)
        (unless (integerp n) (signal 'wrong-type-argument (list 'integerp n)))
      (unless (numberp n) (signal 'wrong-type-argument (list 'number-or-marker-p n)))
      (unless (sequencep seq) (signal 'wrong-type-argument (list 'sequencep seq))))
    n))
(unless (fboundp 'seq-take)
  (defun seq-take (&rest a)
    (unless (= (length a) 2)
      (signal 'wrong-number-of-arguments (list '(2 . 2) (length a))))
    (let ((seq (car a)) (n (car (cdr a))))
    (when (and (numberp n) (<= n 0) (not (listp seq)))
      (setq seq (substring seq 0 0)))
    (nelisp--check-seq-count seq n)
    (if (listp seq)
        (take n seq)
      (substring seq 0 (min (length seq) (max n 0)))))))
(unless (fboundp 'seq-drop)
  (defun seq-drop (&rest a)
    (unless (= (length a) 2)
      (signal 'wrong-number-of-arguments (list '(2 . 2) (length a))))
    (let ((seq (car a)) (n (car (cdr a))))
    ;; N <= 0 answers the SEQUENCE unlooked-at -- but only for an ARRAY:
    ;; (seq-drop [1 2] 0.0) is [1 2] while (seq-drop '(1 2) 0.0) still
    ;; names `integerp', because the list path goes through `nthcdr'.
    (if (and (numberp n) (<= n 0) (not (listp seq)))
        seq
    (nelisp--check-seq-count seq n)
    (if (listp seq)
        (nthcdr n seq)
      (substring seq (min (length seq) (max n 0))))))))
(unless (fboundp 'seq-subseq)
  (defun seq-subseq (seq start &optional end)
    (cond ((listp seq)
           (nelisp--check-seq-list seq)
           (let* ((len (length seq))
                  (s (if (< start 0) (+ len start) start))
                  (e (cond ((null end) len) ((< end 0) (+ len end)) (t end))))
             (when (or (< s 0) (> e len) (> s e))
               (signal 'args-out-of-range (if end (list seq start end) (list seq start))))
             (take (- e s) (nthcdr s seq))))
          ((sequencep seq) (if end (substring seq start end) (substring seq start)))
          (t (signal 'error (list (format "Unsupported sequence: %S" seq)))))))
(unless (fboundp 'seq-count)
  (defun seq-count (pred seq)
    (nelisp--check-seq-list seq)
    (let ((l (nelisp-seq--to-list seq)) (c 0))
      (while l (when (funcall pred (car l)) (setq c (1+ c))) (setq l (cdr l)))
      c)))
(unless (fboundp 'seq-position)
  (defun seq-position (seq elt &optional testfn)
    ;; `sequencep', not `listp': the fuzz case that suggested `listp' had a
    ;; DOTTED pair, which is a cons and passes `listp' -- the name Emacs
    ;; reports there comes from a later walk, not from this check.
    (unless (sequencep seq) (signal 'wrong-type-argument (list 'sequencep seq)))
    (let ((probe seq))
      (while (consp probe) (setq probe (cdr probe)))
      (unless (or (null probe) (not (consp seq)))
        (signal 'wrong-type-argument (list 'listp probe))))
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
    (unless (memq type '(list vector string))
      (signal 'error (list (format "Not a sequence type name: %S" type))))
    (let ((l (nelisp-seq--to-list seq)))
      (cond ((eq type 'list) l)
            ((eq type 'vector) (apply #'vector l))
            ((eq type 'string) (apply #'string l))
            (t l)))))
(unless (fboundp 'string-replace)
  (defun string-replace (from to in)
    ;; literal (non-regexp) replace-all of FROM with TO in IN
    ;; Emacs checks FROM, then IN, then TO -- and the offender it names is
    ;; the first one that is wrong in THAT order.  Checking in argument order
    ;; reported a different argument for the same call.
    (nelisp--check-string from)
    (when (= (length from) 0) (signal 'wrong-length-argument (list 0)))
    (nelisp--check-string in)
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
;; A non-cons argument becomes the tail rather than being skipped.  Emacs
;; allows it as the LAST argument and that is the dotted-tail idiom:
;; `(nconc (list 1 2) 3)' is (1 2 . 3).  This dropped it silently and
;; answered (1 2), so data handed to the last argument simply vanished --
;; and `(nconc 5)', which Emacs answers 5, came back nil.  nil arguments
;; are still skipped, which is what makes `(nconc (list 1) nil)' = (1).
(defun nconc (&rest lists)
  (let ((result nil) (tail nil))
    (while lists
      (let ((l (car lists)))
        (if (consp l)
            (progn
              (if tail (setcdr tail l) (setq result l))
              (setq tail l)
              (while (consp (cdr tail)) (setq tail (cdr tail))))
          (when l
            (if tail (setcdr tail l) (setq result l)))))
      (setq lists (cdr lists)))
    result))
(defun princ (object &optional _stream)
  (nelisp--write-stdout-bytes (nelisp--prn-to-string object nil))
  object)
(defun terpri (&optional stream _ensure)
  (when (and stream (not (functionp stream)))
    (signal (if (symbolp stream) 'void-function 'invalid-function) (list stream)))
  (when (and stream (not (functionp stream)))
    (signal (if (symbolp stream) 'void-function 'invalid-function) (list stream)))
  (nelisp--write-stdout-bytes "\n")
  nil)

;; Doc 143 more pure primitives.
(defun string (&rest chars)
  (dolist (c chars) (nelisp--check-character c))
  (apply #'concat (mapcar #'char-to-string chars)))
(defun prin1 (object &optional _stream)
  (nelisp--write-stdout-bytes (nelisp--prn-to-string object t))
  object)
;; `format-message' curves the grave accent and apostrophe in the FORMAT
;; string, which is the whole reason it exists as a separate function; this
;; was a plain alias for `format', so a message written with `like this'
;; came out with the ASCII quotes vendor Emacs replaces.  Only the format
;; string is curved, never the arguments -- that is Emacs's rule, and it is
;; what keeps a file name or a user string from being rewritten.
(defun nelisp--curve-quotes (s)
  (let ((i 0) (n (length s)) (out ""))
    (while (< i n)
      (let ((c (aref s i)))
        ;; Written as code points, not as `?\`' / "\u2018" literals: the
        ;; escapes this file can rely on are the ones the standalone reader
        ;; parses, and a character that reads wrong here would corrupt every
        ;; message rather than fail loudly.
        (setq out (concat out (cond ((eq c 96) (char-to-string 8216))
                                    ((eq c 39) (char-to-string 8217))
                                    (t (char-to-string c))))))
      (setq i (1+ i)))
    out))
(defun format-message (fmt &rest args)
  (nelisp--check-string fmt)
  (apply #'format (cons (nelisp--curve-quotes fmt) args)))
;; CASE-FOLD was accepted and ignored -- the parameter was even named
;; `_case-fold' to say so -- so `(assoc-string "ABC" (list "abc") t)'
;; answered nil where Emacs answers "abc".  A caller that asked for a
;; case-insensitive lookup got a case-sensitive one and no indication.
;; Folding is `downcase', so it reaches as far as `downcase' does: ASCII
;; today, which is where this was failing anyway.
(defun assoc-string (key alist &optional case-fold)
  (unless (listp alist) (setq alist nil))
  (let* ((k0 (cond ((symbolp key) (symbol-name key))
                   ((stringp key) key)
                   (t nil)))
         (k (and k0 (if case-fold (downcase k0) k0)))
         (found nil))
    (while (and (consp alist) (not found))
      (let* ((entry (car alist))
             (ek (if (consp entry) (car entry) entry))
             (eks0 (cond ((symbolp ek) (symbol-name ek))
                         ((stringp ek) ek)
                         (t nil)))
             (eks (and eks0 (if case-fold (downcase eks0) eks0))))
        (when (and eks (null k))
          (signal 'wrong-type-argument (list 'stringp key)))
        (if (and eks k (string= k eks))
            (setq found entry)
          (setq alist (cdr alist)))))
    found))

;; Doc 143 pure list utilities.
(defun assoc (key alist &optional testfn)
  (if (null testfn)
      (nelisp--assoc-raw key alist)
    (unless (listp alist) (signal 'wrong-type-argument (list 'listp alist)))
    (let ((cur alist) (found nil))
      (while (and (consp cur) (not found))
        (let ((pair (car cur)))
          (when (consp pair)
            (unless (functionp testfn)
              (signal (if (symbolp testfn) 'void-function 'invalid-function)
                      (list testfn))))
          (if (and (consp pair) (funcall testfn (car pair) key))
              (setq found pair)
            (setq cur (cdr cur)))))
      (when (and (not found) (not (null cur)))
        (signal 'wrong-type-argument (list 'listp alist)))
      found)))
(unless (fboundp 'rassq)
  (defun rassq (value alist)
    (let ((probe alist))
      (while (consp probe) (setq probe (cdr probe)))
      (unless (null probe) (signal 'wrong-type-argument (list 'listp alist))))
    (let ((found nil))
      (while (and alist (not found))
        (if (and (consp (car alist)) (eq (cdr (car alist)) value))
            (setq found (car alist))
          (setq alist (cdr alist))))
      found)))
(unless (fboundp 'rassoc)
  (defun rassoc (value alist)
    (unless (listp alist) (signal 'wrong-type-argument (list 'listp alist)))
    (let ((found nil))
      (while (and alist (not found))
        (if (and (consp (car alist)) (equal (cdr (car alist)) value))
            (setq found (car alist))
          (setq alist (cdr alist))))
      found)))
(unless (fboundp 'last)
  (defun last (list &optional n)
    ;; Emacs answers the object itself for a non-list -- (last t) is t --
    ;; because it walks with `cdr' rather than measuring first.  Measuring
    ;; with `length' made it signal `sequencep' instead.
    (if (not (consp list)) (if (and n (< n 0)) nil list)
      (let ((len (safe-length list)) (m (or n 1)))
        ;; A NEGATIVE N asks for zero elements and answers nil; only a
        ;; too-large positive N answers the whole list.  Running `nthcdr'
        ;; for the negative case walked into an improper tail and signalled
        ;; where Emacs answers nil.
        (cond
         ((< m 0) nil)
         ((> len m) (nthcdr (- len m) list))
         (t list))))))
(unless (fboundp 'butlast)
  (defun butlast (list &optional n)
    (when n (unless (numberp n) (signal 'wrong-type-argument (list 'number-or-marker-p n))))
    ;; A non-positive N answers LIST without looking at it, so
    ;; (butlast -7 -7) is -7 rather than a type error about -7.
    (if (and n (<= n 0))
        list
    (let* ((len (length list)) (m (or n 1)) (keep (- len m)) (acc nil) (i 0))
      (while (and (< i keep) list)
        (setq acc (cons (car list) acc))
        (setq list (cdr list))
        (setq i (1+ i)))
      (nreverse acc)))))
(unless (fboundp 'copy-tree)
  (defun copy-tree (tree &optional _vecp)
    (if (consp tree)
        (cons (copy-tree (car tree)) (copy-tree (cdr tree)))
      tree)))

;; A negative N is not an error in Emacs -- `nthcdr' treats it as zero, so
;; `(nth -1 '(1 2 3))' is 1.  This answered nil, quietly, for any negative
;; index.
(defun nth (n list) (car (nthcdr n list)))

;; A negative LENGTH answered nil, silently.  Emacs signals: asking for a
;; list of minus one thing is a caller bug, not an empty list.
(defun make-list (length object)
  (unless (and (integerp length) (>= length 0))
    (signal 'wrong-type-argument (list 'wholenump length)))
  (if (or (not (integerp length)) (< length 0))
      (signal 'wrong-type-argument (list 'natnump length))
    (let ((acc nil))
      (while (> length 0)
        (setq acc (cons object acc))
        (setq length (1- length)))
      acc)))

;; Emacs `reverse' takes any sequence and answers the SAME type: a vector
;; reverses to a vector, a string to a string.  This walked its argument as
;; a list whatever it was, so `(reverse [1 2 3])' answered (nil) -- one
;; element, and the wrong one -- and `(reverse "abc")' the same.  Not an
;; error, a plausible-looking value, which is the expensive kind.  Measured
;; 2026-08-19 against Emacs 30.1.
(defun reverse (seq)
  (cond
   ((null seq) nil)
   ((consp seq)
    (let ((acc nil) (tail seq))
      (while tail
        (setq acc (cons (car tail) acc))
        (setq tail (cdr tail)))
      acc))
   ((stringp seq)
    (let ((n (length seq)) (out "") (i 0))
      (while (< i n)
        (setq out (concat (substring seq i (1+ i)) out))
        (setq i (1+ i)))
      out))
   ((vectorp seq)
    (let* ((n (length seq)) (out (make-vector n nil)) (i 0))
      (while (< i n)
        (aset out (- (- n 1) i) (aref seq i))
        (setq i (1+ i)))
      out))
   (t (signal 'wrong-type-argument (list 'sequencep seq)))))

;; `nreverse' reverses a vector in place, as Emacs does, so a caller holding
;; the original sees the change.  A string goes through `reverse' and comes
;; back as a new string instead: this runtime's `aset' on a string is a
;; byte-level write, and reversing multibyte text in place through it would
;; scramble the encoding.  The value is right; only the identity differs,
;; and getting the value wrong to preserve identity would be the worse
;; trade.  Before this, a vector argument signalled and a string answered
;; (nil).
(defun nreverse (seq)
  (cond
   ((null seq) nil)
   ((consp seq)
    (let ((prev nil) (cur seq) next)
      (while cur
        (setq next (cdr cur))
        (setcdr cur prev)
        (setq prev cur)
        (setq cur next))
      prev))
   ((vectorp seq)
    (let* ((n (length seq)) (i 0) (j (- n 1)) tmp)
      (while (< i j)
        (setq tmp (aref seq i))
        (aset seq i (aref seq j))
        (aset seq j tmp)
        (setq i (1+ i))
        (setq j (- j 1)))
      seq))
   ((stringp seq) (reverse seq))
   (t (signal 'wrong-type-argument (list 'sequencep seq)))))

(unless (fboundp 'last)
  (defun last (list &optional n)
    "Return the last link of LIST.  Its `car' is the last element.\nIf LIST is nil, return nil.  If N is non-nil, return the Nth-to-last\nlink of LIST."
    (let* ((m (or n 1)) (cur list) (lead list))
      (if (<= m 0)
          nil
        (let ((i 0))
          (while (and (consp lead) (< i m))
            (setq lead (cdr lead)) (setq i (1+ i))))
        (while (consp lead) (setq cur (cdr cur)) (setq lead (cdr lead)))
        cur))))

(unless (fboundp 'butlast)
  (defun butlast (list &optional n)
    "Return a copy of LIST with the last N elements removed.\nIf N is omitted or nil, the last element is removed.  If N is zero\nor negative, return a full copy of LIST."
    (let ((m (or n 1)))
      (if (<= m 0) list
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
	 (let ((cur seq) (nelisp--diag-steps 0))
	   (while (consp cur)
	     (setq acc (cons (car cur) acc)) (setq cur (cdr cur))
	     ;; DIAGNOSTIC (gc-retention-edge campaign, Phase B, 2026-07-06):
	     ;; this cdr-walk has no cycle/terminator guard beyond `consp'.  A
	     ;; GC retention-edge bug can free a still-reachable cons and
	     ;; overwrite its cdr with a free-list link that re-enters this
	     ;; same chain, turning this walk into an allocating infinite
	     ;; loop (each iteration still `cons'es onto ACC) that only ends
	     ;; when the arena exhausts `ulimit -v' and `nl_os_alloc_fail'
	     ;; exits 88 -- tens of seconds later, no backtrace.  This bound
	     ;; converts that into an immediate, catchable `signal' carrying
	     ;; the original SEQ, the CUR cons at the moment of the trip, and
	     ;; the partial ACC, so a debugger can break on `bf_signal' and
	     ;; inspect the exact cons whose cdr the sweeper corrupted.  The
	     ;; bound (200000) is far above any legitimate top-level
	     ;; `append'/backquote-splice list length and far below what it
	     ;; would take to exhaust memory (millions of iterations), so
	     ;; this cannot misfire on real workloads.  See Doc 155 (nelisp
	     ;; GC lexframe-child-collection-bug) and its retention-edge
	     ;; addendum for the campaign this instrumentation serves.
	     (setq nelisp--diag-steps (1+ nelisp--diag-steps))
	     (when (> nelisp--diag-steps 200000)
	       (signal 'nelisp-diag-runaway-append-collect
		       (list seq cur acc nelisp--diag-steps))))
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

(defun nelisp--doc200-raw-high-string-p (string)
  "Non-nil when unibyte STRING contains a byte at least 128."
  (and (unibyte-string-p string)
       (let ((i 0) (n (length string)) (high nil))
         (while (and (< i n) (not high))
           (when (>= (aref string i) 128) (setq high t))
           (setq i (1+ i)))
         high)))

(defun nelisp--doc200-check-string-mix (sequences)
  "Signal when SEQUENCES require an unsupported multibyte raw-byte char."
  (let ((raw-high nil) (multibyte nil) (tail sequences))
    (while tail
      (let ((value (car tail)))
        (when (stringp value)
          (when (nelisp--doc200-raw-high-string-p value)
            (setq raw-high t))
          (when (multibyte-string-p value)
            (setq multibyte t))))
      (setq tail (cdr tail)))
    (when (and raw-high multibyte)
      (signal 'nelisp-raw-byte-unrepresentable nil))))

(defun append (&rest args)
  "Concatenate sequences ARGS into a fresh proper-list spine.\nNon-final args may be list / vector / string / nil.  The FINAL arg\nis used as the tail (= unchanged, can be any value).  Single-arg\ncall returns the arg unchanged (= no copy)."
  (nelisp--doc200-check-string-mix args)
  (cond ((null args) nil) ((null (cdr args)) (car args))
	(t
	 (let ((cur args) (acc nil) (tail nil))
	   (while (cdr cur)
	     (nelisp--check-seq-list (car cur))
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

;; Rust-min batch 6g (2026-05-06): `copy-sequence' partial migration.
;; cons / nil paths handled in elisp; other types (str / mutstr /
;; vector / atoms) return the input unchanged.  This drops the
;; previous Rust impl's fresh-cell semantics for Sexp::Str and
;; Sexp::MutStr (= they used to clone the underlying String); a
;; codebase grep for `(aset (copy-sequence ...))' returned 0 hits,
;; so no caller depends on that.  Vectors already shared their
;; underlying Vec via Rc clone, so behaviour is unchanged.
;; Improper list (= non-nil non-cons tail) signals
;; `wrong-type-argument' to match the previous list_elements path.
(defun copy-sequence (seq)
  "Return a copy of SEQ.  Doc 22 A4: strings and vectors are copied into a
FRESH buffer (the old `(t seq)' arm returned the same object, so a following
`aset' mutated the original / a string literal)."
  (cond
   ((null seq) nil)
   ((consp seq)
    (let ((acc nil) (cur seq))
      (while (consp cur)
        (setq acc (cons (car cur) acc))
        (setq cur (cdr cur)))
      (when cur
        (signal 'wrong-type-argument (list 'listp cur)))
      (nreverse acc)))
   ((stringp seq) (concat seq))
   ((vectorp seq)
    (let* ((n (length seq))
           (copy (make-vector n nil))
           (i 0))
      (while (< i n)
        (aset copy i (aref seq i))
        (setq i (1+ i)))
      copy))
   ;; The old arm here returned the object unchanged, so `(copy-sequence 5)'
   ;; answered 5 where Emacs signals -- and a caller that copied in order to
   ;; mutate went on to mutate the original.
   (t (signal 'wrong-type-argument (list 'sequencep seq)))))

(unless (fboundp 'memq)
  (defun memq (elt list)
    (unless (listp list) (signal 'wrong-type-argument (list 'listp list)))
    (let ((found nil))
      (while (and list (not found))
        (if (eq elt (car list)) (setq found list)
	  (setq list (cdr list))))
      found)))

(unless (fboundp 'member)
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
      found)))

(unless (fboundp 'assq)
  (defun assq (key alist)
    (unless (listp alist) (signal 'wrong-type-argument (list 'listp alist)))
    (let ((found nil))
      (while (and alist (not found))
        (let ((pair (car alist)))
	  (if (and (consp pair) (eq (car pair) key)) (setq found pair)
	    (setq alist (cdr alist)))))
      found)))

(unless (fboundp 'assoc)
  (defun assoc (key alist &optional testfn)
    (nelisp--check-list alist)
    (let ((found nil))
      (cond
       (testfn
        (while (and alist (not found))
          (let ((pair (car alist)))
            (unless (functionp testfn)
              (signal (if (symbolp testfn) 'void-function 'invalid-function)
                      (list testfn)))
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
      found)))

(defun mapcar (fn seq)
  "Apply FN to each element of SEQ (list, vector, or string); collect results.
Doc 22 A6: arrays are iterated by index via `aref'/`length' (cons-cell
walking only works for lists)."
  (nelisp--check-seq-list seq)
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
  (nelisp--check-seq-list seq)
  (if (or (vectorp seq) (stringp seq))
      (let ((n (length seq)) (i 0))
        (while (< i n) (funcall fn (aref seq i)) (setq i (1+ i)))
        seq)
    (let ((orig seq))
      (while seq (funcall fn (car seq)) (setq seq (cdr seq))) orig)))

(unless (fboundp 'nelisp--check-plist)
  (defun nelisp--check-plist (x)
    (unless (listp x) (signal 'wrong-type-argument (list 'plistp x)))
    x))

(defun plist-member (plist key &optional predicate)
  ;; Two rules, and only measuring both shows they are different: the walk
  ;; signals `plistp' naming the WHOLE plist when it runs off the end, and
  ;; says nothing at all when the key is found before it gets there --
  ;; (plist-member '(1 2 . 3) 1) answers (1 2 . 3).
  (let ((cur plist) (found nil) (done nil))
    (while (and (not found) (not done))
      (cond
       ((null cur) (setq done t))
       ((not (consp cur)) (signal 'wrong-type-argument (list 'plistp plist)))
       ((if predicate (funcall predicate (car cur) key) (eq (car cur) key))
        (setq found cur))
       ((null (cdr cur)) (setq done t))
       ((not (consp (cdr cur))) (signal 'wrong-type-argument (list 'plistp plist)))
       (t (setq cur (cdr (cdr cur))))))
    found))

(defun plist-get (plist key &optional predicate)
  ;; Emacs's `plist-get' ANSWERS NIL for a malformed plist, while
  ;; `plist-member' and `plist-put' signal `plistp'.  Checking all three "for
  ;; consistency" would be consistent with each other and wrong against
  ;; Emacs.  The early return is needed because the walk calls `car', and
  ;; `car' signals `listp' now -- so leniency has to be explicit rather than
  ;; inherited from a primitive that used to answer nil for anything.
  (unless (listp plist) (setq plist nil))
  (let ((cur plist) (found nil) (value nil))
    (if predicate
        (while (and (consp cur) (consp (cdr cur)) (not found))
          (if (funcall predicate (car cur) key)
              (progn (setq found t) (setq value (car (cdr cur))))
            (setq cur (cdr (cdr cur)))))
      (while (and (consp cur) (consp (cdr cur)) (not found))
        (if (eq (car cur) key)
            (progn (setq found t) (setq value (car (cdr cur))))
          (setq cur (cdr (cdr cur))))))
    value))

(defun plist-put (plist key value &optional predicate)
  ;; `plist-put' signals `plistp' for a non-list -- EXCEPT that Emacs
  ;; answers the KEY when the walk cannot start at all.  Measured both ways:
  ;; (plist-put -1 1 2) signals, (plist-put -1 ["a"] "e") answers ["a"].
  ;; The difference is whether the key is `eq'-comparable, so this cannot be
  ;; reproduced by a type check alone and the vector case is left as a
  ;; documented gap rather than guessed at.
  (unless (listp plist) (signal 'wrong-type-argument (list 'plistp plist)))
  ;; The walk steps only while there IS a pair, and complains only about
  ;; what is left over: (plist-put '(1 2 . 3) 1 X) finds its key at the head
  ;; and answers (1 X . 3), while (plist-put '(1) 100 32) runs out mid-pair
  ;; and signals `plistp' naming the whole plist.
  (let ((cur plist) (tail nil))
    (while (and (consp cur) (consp (cdr cur)) (not tail))
      (if (if predicate (funcall predicate (car cur) key) (eq (car cur) key))
          (setq tail cur)
        (setq cur (cdr (cdr cur)))))
    (cond
     (tail (setcar (cdr tail) value) plist)
     ((not (null cur)) (signal 'wrong-type-argument (list 'plistp plist)))
     ((null plist) (cons key (cons value nil)))
     (t (let ((end plist))
          (while (cdr (cdr end)) (setq end (cdr (cdr end))))
          (setcdr (cdr end) (cons key (cons value nil)))
          plist)))))

;; `(string-empty-p nil)' answered t, because `(length nil)' is 0 -- so the
;; commonest "no string here" value reported itself as an empty string.
;; Emacs compares with `string=', which is nil for a non-string.
;; Emacs's is (string= STRING ""), and `string=' accepts a SYMBOL as well
;; as a string -- so (string-empty-p nil) is nil, not an error, while
;; (string-empty-p 12354) signals `stringp'.  Checking for a string outright
;; got the number right and the symbol wrong.
(defun string-empty-p (s) (string-equal s ""))

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
      (let ((_ (when env
                 (unless (listp env)
                   (signal 'wrong-type-argument (list 'listp env)))))
            (cell (and env (symbolp (car form)) (assq (car form) env))))
        (if cell
            (if (cdr cell) (apply (cdr cell) (cdr form)) form)
          (let ((mfn (nelisp--macro-function (car form))))
            (if mfn (apply mfn (cdr form)) form))))
    form))

(defun macroexpand (form &optional environment)
  "Repeatedly macroexpand FORM until its head is no longer a macro.
ENVIRONMENT is the macro environment alist threaded to `macroexpand-1'
(Doc 22 A12); local macros in ENVIRONMENT shadow the global mirror."
  ;; A head that is not a symbol cannot be a macro, so this answers the form
  ;; without ever consulting ENVIRONMENT -- (macroexpand '(1) "abc") is (1)
  ;; while (macroexpand '(a) "abc") signals about the environment.
  (if (or (not (consp form)) (not (symbolp (car form))))
      form
    (let ((cur form) (again t))
      (while again
        (let ((next (macroexpand-1 cur environment)))
          (if (eq next cur) (setq again nil) (setq cur next))))
      cur)))

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
  ;; ENVIRONMENT is only reached once there IS a form to expand: Emacs
  ;; answers 48 for (macroexpand-all 48 -1.5) and signals for (a) with the
  ;; same bad environment.  Checking up front turned an answer into an error.
  ;; Two measurements, not one: (macroexpand-all '(1 2 . 3)) answers the form
  ;; -- an improper list has no macro head to expand -- but
  ;; (macroexpand-all '(1 2 . 3) 32) still signals `listp' for the bad
  ;; ENVIRONMENT.  So the environment is checked first and the walk is what
  ;; the improper list skips.
  (if (not (consp form))
      form
    (when environment
      (unless (listp environment)
        (signal 'wrong-type-argument (list 'listp environment))))
    (if (not (proper-list-p form))
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
                                                    environment))))))))))

(unless (fboundp 'macroexp-parse-body)
  (defun macroexp-parse-body (body)
    "Split BODY into declarations and remaining forms.
Return (DECLARATIONS . BODY-FORMS), matching the shape used by Emacs
macro helpers such as `iter-defun'.  A leading docstring and any
leading `(declare ...)' forms are treated as declarations."
    (let ((declarations nil)
          (cur body))
      (while (and cur (cdr cur)
                  (or (stringp (car cur))
                      (and (consp (car cur))
                           (memq (car (car cur))
                                 '(declare interactive cl-declare :documentation)))))
        (setq declarations (cons (car cur) declarations))
        (setq cur (cdr cur)))
      (cons (nreverse declarations) cur))))

;;;; --- cl-defun helper + macro (Stage 7.3.c) --------------------------

(defun nelisp--parse-cl-formals (formals)
  "Parse FORMALS list of a `cl-defun' form.
Returns a 4-element list (POSITIONAL OPTIONALS REST-OR-NIL KEYS) where
POSITIONAL / OPTIONALS are flat symbol lists, REST-OR-NIL is the
&rest var (or nil), and KEYS is a list of (KW PARAM DEFAULT) triples
— one per &key entry, with KW the leading-colon keyword interned from
PARAM's name.  &aux entries are silently dropped to match Rust
`sf_cl_defun' (build-tool/src/eval/special_forms.rs:389)."
  (let ((mode 'pos)
        (positional nil)
        (optionals nil)
        (rest-sym nil)
        (keys nil)
        (cursor formals))
    (while cursor
      (let ((f (car cursor)))
        (if (eq f '&optional)
            (setq mode 'opt)
          (if (eq f '&rest)
              (setq mode 'rest)
            (if (eq f '&key)
                (setq mode 'key)
              (if (eq f '&aux)
                  (setq mode 'aux)
                (if (eq mode 'pos)
                    (setq positional (cons f positional))
                  (if (eq mode 'opt)
                      (setq optionals (cons f optionals))
                    (if (eq mode 'rest)
                        (if (null rest-sym) (setq rest-sym f))
                      (if (eq mode 'key)
                          (let (param default kw)
                            (if (consp f)
                                (progn
                                  (setq param (car f))
                                  (setq default (car (cdr f))))
                              (setq param f)
                              (setq default nil))
                            (setq kw (intern (concat ":" (symbol-name param))))
                            (setq keys (cons (list kw param default) keys)))))))))))
        (setq cursor (cdr cursor))))
    (list (nreverse positional)
          (nreverse optionals)
          rest-sym
          (nreverse keys))))

(defun nelisp--cl-optional-vars (optionals)
  "Return the plain variable symbols of OPTIONALS (each VAR or (VAR DEFAULT))."
  (mapcar (lambda (o) (if (consp o) (car o) o)) optionals))

(defun nelisp--cl-optional-default-forms (optionals)
  "Return body-prelude forms binding each defaulted optional (Doc 22 A15).
For every (VAR DEFAULT) entry in OPTIONALS, emit (unless VAR (setq VAR
DEFAULT)) so the core lambda only ever sees a plain &optional symbol."
  (let ((acc nil) (cur optionals))
    (while cur
      (let ((o (car cur)))
        (when (consp o)
          (setq acc (cons (list 'unless (car o)
                                (list 'setq (car o) (car (cdr o))))
                          acc))))
      (setq cur (cdr cur)))
    (nreverse acc)))

(defmacro cl-defun (name formals &rest body)
  "Standalone `cl-defun' subset with positional, optional, rest and key args.
Optional parameters may carry a default as (VAR DEFAULT) (Doc 22 A15): the
core lambda binder accepts only plain &optional symbols, so the default is
desugared into a body prelude (unless VAR (setq VAR DEFAULT))."
  (let* ((parsed (nelisp--parse-cl-formals formals))
         (positional (car parsed))
         (optionals (car (cdr parsed)))
         (rest-sym (car (cdr (cdr parsed))))
         (keys (car (cdr (cdr (cdr parsed)))))
         (opt-vars (nelisp--cl-optional-vars optionals))
         (opt-defaults (nelisp--cl-optional-default-forms optionals))
         (body2 (append opt-defaults body)))
    (if (null keys)
        (if (null opt-defaults)
            ;; No optional defaults: keep the exact raw passthrough (safe for
            ;; any arglist shape the core lambda already accepts).
            (cons 'defun (cons name (cons formals body)))
          ;; Desugar optional defaults: plain &optional symbols + setup prelude.
          (let ((new-formals
                 (append positional
                         (if opt-vars (cons '&optional opt-vars) nil)
                         (if rest-sym (cons '&rest (cons rest-sym nil)) nil))))
            (cons 'defun (cons name (cons new-formals body2)))))
      (let* ((rest-name (or rest-sym '--cl-keys))
             (new-formals
              (append positional
                      (if opt-vars (cons '&optional opt-vars) nil)
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
        ;; Optional-default setup runs before the &key let* (Doc 22 A15).
        (cons 'defun
              (cons name
                    (cons new-formals
                          (append opt-defaults (cons let-form nil)))))))))

;; `cl-defsubst' shares `cl-defun's arglist handling (we do not inline), so it
;; inherits the optional-default desugar (Doc 22 A15).  VOID on the bare reader.
(unless (fboundp 'cl-defsubst)
  (fset 'cl-defsubst (symbol-function 'cl-defun)))

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

(defun nelisp-cl-macros--loop-build-parallel
    (for-clauses with-bindings collect-form sum-form count-form do-forms)
  "Build a lockstep loop over parallel `for PAT in LIST' FOR-CLAUSES.
FOR-CLAUSES is a list of (PATTERN . LIST-FORM) in source order; the loop
stops as soon as any list is exhausted (CL parallel-stepping semantics).
Supports a single collect / sum / count accumulator or `do' forms.  This
is the multi-`for' path the single-cursor `dolist' branches cannot model
\(e.g. generator.el's `let'->`let*' rewrite emits `for ... for ...')."
  (let ((cursors (mapcar (lambda (_c) (make-symbol "--loop-cur--")) for-clauses))
        (acc-sym (cond (collect-form (make-symbol "--loop-acc--"))
                       (sum-form (make-symbol "--loop-sum--"))
                       (count-form (make-symbol "--loop-count--"))))
        (cursor-binds nil) (and-conds nil) (var-binds nil) (advance nil)
        (fc nil) (cs nil))
    (setq fc for-clauses cs cursors)
    (while fc
      (let* ((pat (car (car fc)))
             (lst (cdr (car fc)))
             (cur (car cs))
             (src (list 'car cur)))
        (setq cursor-binds (append cursor-binds (list (list cur lst))))
        (setq and-conds (append and-conds (list cur)))
        (if (symbolp pat)
            (setq var-binds (append var-binds (list (list pat src))))
          (setq var-binds
                (append var-binds
                        (nelisp-cl-macros--loop-destructure-bindings pat src))))
        (setq advance (append advance (list cur (list 'cdr cur)))))
      (setq fc (cdr fc) cs (cdr cs)))
    (let* ((acc-init (if (or sum-form count-form) 0 nil))
           (body (cond
                  (collect-form
                   (list (list 'setq acc-sym (list 'cons collect-form acc-sym))))
                  (sum-form
                   (list (list 'setq acc-sym (list '+ acc-sym sum-form))))
                  (count-form
                   (list (list 'when count-form
                               (list 'setq acc-sym (list '+ acc-sym 1)))))
                  (t (reverse do-forms))))
           (iter-let (cons 'let (cons var-binds body)))
           (loop (cons 'while
                       (cons (cons 'and and-conds)
                             (list iter-let (cons 'setq advance)))))
           (all-binds (append (if acc-sym (list (list acc-sym acc-init)) nil)
                              cursor-binds with-bindings))
           (result (cond (collect-form (list 'nreverse acc-sym))
                         ((or sum-form count-form) acc-sym)
                         (t nil))))
      (cons 'let (cons all-binds
                       (cons loop (if result (list result) nil)))))))

(defun nelisp-cl-macros--loop-build-counted
    (step-init step-test step-incr collect-form sum-form count-form
     do-forms with-bindings)
  "Shared codegen for a counted `while'-driven cl-loop iteration.
STEP-INIT is the counter's `let' binding (VAR INIT-FORM), STEP-TEST the
loop continuation test, STEP-INCR the per-iteration counter update.
Used by both the numeric `for VAR from N {to,below} M' clause and the
`repeat N' clause -- the two counted (non `for...in...') iteration
shapes -- so a `collect'/`sum'/`count' clause is honoured the same way
the `for VAR in LIST' clauses already honour it, instead of only ever
wiring DO-FORMS."
  (let ((rev nil))
    (while do-forms (setq rev (cons (car do-forms) rev))
           (setq do-forms (cdr do-forms)))
    (cond
     (collect-form
      (let ((acc-sym (make-symbol "--loop-acc--")))
        (list 'let (cons (list acc-sym nil) (cons step-init with-bindings))
              (list 'while step-test
                    (list 'setq acc-sym (list 'cons collect-form acc-sym))
                    step-incr)
              (list 'nreverse acc-sym))))
     (sum-form
      (let ((acc-sym (make-symbol "--loop-sum--")))
        (list 'let (cons (list acc-sym 0) (cons step-init with-bindings))
              (list 'while step-test
                    (list 'setq acc-sym (list '+ acc-sym sum-form))
                    step-incr)
              acc-sym)))
     (count-form
      (let ((acc-sym (make-symbol "--loop-count--")))
        (list 'let (cons (list acc-sym 0) (cons step-init with-bindings))
              (list 'while step-test
                    (list 'when count-form (list 'setq acc-sym (list '+ acc-sym 1)))
                    step-incr)
              acc-sym)))
     (t
      (list 'let (cons step-init with-bindings)
            (list 'while step-test
                  (cons 'progn rev)
                  step-incr))))))

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
        (repeat-count nil)
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
         ((eq kw 'repeat)
          (setq repeat-count (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
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
     ;; Numeric `for VAR from N {to,below} M' [do/collect/sum/count FORM ...]
     ((and numeric-from (or numeric-to numeric-below))
      (let ((cmp (if numeric-to '<= '<))
            (limit (or numeric-to numeric-below)))
        (nelisp-cl-macros--loop-build-counted
         (list var numeric-from) (list cmp var limit)
         (list 'setq var (list '1+ var))
         collect-form sum-form count-form do-forms with-bindings)))
     ;; `repeat N' [do/collect/sum/count FORM ...] -- unconditional count,
     ;; no loop variable.
     (repeat-count
      (let ((n-sym (make-symbol "--loop-n--")))
        (nelisp-cl-macros--loop-build-counted
         (list n-sym repeat-count) (list '> n-sym 0)
         (list 'setq n-sym (list '1- n-sym))
         collect-form sum-form count-form do-forms with-bindings)))
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

(defun nelisp-cl-macros--struct-record (name parent slot-names &optional conc-name)
  "Push (NAME . (:slot-names SLOT-NAMES :parent PARENT)) into the
runtime struct registry.  Re-pushes shadow earlier entries — the
front-of-list wins on lookup.  Also (re-)registers every accessor's
slot index in `nelisp-cl-macros--accessor-info' so `setf' can find it.
CONC-NAME is the accessor-name prefix (default \"NAME-\"); pass \"\" for
`:conc-name nil'.  Must match the prefix the accessors are generated with."
  (setq nelisp-cl-macros--struct-info
        (cons (cons name (list :slot-names slot-names :parent parent))
              nelisp-cl-macros--struct-info))
  (let ((i 0)
        (prefix (or conc-name (concat (symbol-name name) "-"))))
    (dolist (s slot-names)
      (let ((acc (intern (concat prefix (symbol-name s)))))
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

Limitations: no `:type', no `setf' integration.  A leading docstring IS
accepted (and discarded), which it previously was not -- it was taken for a
slot name.  That only worked because `symbol-name' used to answer for a
string; once it signalled `symbolp', as Emacs does, every `cl-defstruct'
with a docstring stopped compiling.

Note: `(declare ...)' metadata is intentionally omitted because the
NeLisp Rust evaluator does not yet strip declare forms from macro
bodies (= Stage 4 follow-up).  Indent / edebug specs come back when
`defmacro' grows declare-handling parity with host Emacs."
  (when (and (stringp (car slots)) (cdr slots))
    (setq slots (cdr slots)))
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
         ;; `:conc-name' overrides the accessor prefix (default "NAME-");
         ;; `:conc-name nil' (or "") means no prefix — the slot symbol verbatim.
         ;; cl-preloaded's class structs use e.g. `(:conc-name cl--struct-class-)'.
         (conc-name-form (nelisp-cl-macros--struct-opt :conc-name options))
         (conc-name (cond ((eq conc-name-form nelisp-cl-macros--struct-absent)
                           (concat (symbol-name name) "-"))
                          ((null conc-name-form) "")
                          (t (format "%s" conc-name-form))))
         ;; `:predicate NAME' renames the predicate; `:predicate nil' suppresses it.
         (predicate (nelisp-cl-macros--struct-resolve-name
                     (nelisp-cl-macros--struct-opt :predicate options)
                     (intern (format "%s-p" name))))
         ;; cl-lib allows MULTIPLE `:constructor' options: a `(:constructor
         ;; nil)' suppresses the default make-NAME while additional
         ;; `(:constructor NAME ARGLIST)' cells still define BOA constructors.
         ;; cl-generic relies on this — `(assq :constructor ...)' would grab
         ;; only the leading `nil' and drop `cl--generic-make' etc.  Collect
         ;; every cell, then build a (CTOR-NAME . CTOR-ARGLIST) work-list
         ;; (ARGLIST nil = keyword form; a `nil' NAME suppresses).
         (constructor-cells
          (let ((cs nil))
            (dolist (o options)
              (when (and (consp o) (eq (car o) :constructor))
                (setq cs (cons o cs))))
            (nreverse cs)))
         (constructors
          (if constructor-cells
              (let ((out nil))
                (dolist (c constructor-cells)
                  (let ((cn (car (cdr c))))
                    (when cn
                      (setq out
                            (cons (cons cn
                                        (and (consp (cdr (cdr c)))
                                             (car (cdr (cdr c)))))
                                  out)))))
                (nreverse out))
            (list (cons (intern (format "make-%s" name)) nil))))
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
    (nelisp-cl-macros--struct-record name parent slot-names conc-name)
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
                  (list 'quote slot-names)
                  conc-name)
            forms)
      ;; Predicate form — uses --struct-isa for chain matching so
      ;; descendant records still satisfy the parent predicate when
      ;; this struct is later used as someone else's `:include'.
      ;; `:predicate nil' suppresses it (predicate = nil here).
      (when predicate
        (push (list 'defun predicate (list 'obj)
                    (list 'and
                          (list 'recordp 'obj)
                          (list 'nelisp-cl-macros--struct-isa
                                (list 'nelisp--record-type 'obj)
                                (list 'quote name))))
              forms))
      ;; Constructor forms (keyword args by default; positional
      ;; `(:constructor NAME ARGLIST)' when an arglist is given).  cl-lib
      ;; permits several `:constructor' options, so emit one defun per cell.
      (dolist (ctor constructors)
        (let ((constructor (car ctor))
              (constructor-arglist (cdr ctor)))
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
                  forms))))
      ;; Copier form (shallow copy via record-ref / make-record).
      (when copier
        (push (list 'defun copier (list src-sym)
                    (cons 'apply
                          (cons (list 'quote 'nelisp--make-record)
                                (cons (list 'quote name)
                                      (list (cons 'list copy-arg-forms))))))
              forms))
      ;; Accessor forms — one per slot, indexed positionally.  Names use
      ;; CONC-NAME (default "NAME-"); `:conc-name' / `:conc-name nil' change it.
      (setq i 0)
      (dolist (s slot-names)
        (let ((acc (intern (concat conc-name (symbol-name s)))))
          (push (list 'defun acc (list rec-sym)
                      (list 'nelisp--record-ref rec-sym i))
                forms))
        (setq i (1+ i)))
      ;; Result form: (progn DEFUN ... 'NAME).
      (cons 'progn
            (append (nreverse forms)
                    (list (list 'quote name)))))))


;;;; --- cl-generic subset (Doc 185) ----------------------------------------
;;
;; `cl-defgeneric'/`cl-defmethod' subset: type + eql specializers only
;; (struct dispatch via the already-working `nelisp-cl-macros--struct-isa'
;; ancestry walker above, NOT `cl-typep', which knows nothing about
;; `cl-defstruct'-generated types -- docs/design/185-cl-generic-subset.org
;; §1.2/§2.1a), primary methods only plus `cl-call-next-method'/
;; `cl-next-method-p' (§2.2), a lazily-built per-generic dispatch table
;; (§2.3/§3.4 -- this is the direct answer to Doc 157's 280-second eager
;; global-prefill wall, §1.4: nothing here does `eval'-based dispatcher
;; compilation, and no work happens for a generic until it is first
;; called).  Every unsupported form -- a method-combination qualifier
;; (`:before'/`:after'/`:around'/anything but none), a specializer kind
;; other than type/eql/unspecialized, a specializer on an argument
;; position other than 0 -- is a loud `error' at `cl-defmethod'
;; macroexpansion time, never a silent no-dispatch (§3.5).
;;
;; No `declare' in the macro bodies below -- see the `cl-defstruct'
;; comment above: the standalone reader does not yet strip `declare'
;; forms from macro bodies, so one here would break under
;; `target/nelisp' even though it loads fine under host Emacs.

(defvar nelisp-cl-generic--next-methods nil
  "Dynamic: the remaining (less-specific) applicable method functions for
the `cl-call-next-method' chain of the call in progress.  Bound by
`nelisp-cl-generic--invoke' and rebound by `cl-call-next-method' itself
around each further step, so a chain of N applicable methods can walk
itself in most-specific -> least-specific order.  Always `defvar'd
explicitly: this file's mirror copy in `scripts/nelisp-stdlib-prelude.el'
is `lexical-binding: nil' but this file itself is `lexical-binding: t',
and a plain `let' only gets real dynamic (special) scoping across
function calls when the variable has been declared special first.")

(defvar nelisp-cl-generic--call-args nil
  "Dynamic: the full argument list of the generic-function call in
progress.  `cl-call-next-method' with no arguments of its own reuses
this (matches real Emacs `cl-generic').")

(defvar nelisp-cl-generic--current-name nil
  "Dynamic: the generic-function symbol of the call in progress, purely
for `cl-no-next-method''s signal data -- not used for dispatch.")

(defvar nelisp-cl-generic--conditions-registered nil
  "Non-nil once `nelisp-cl-generic--ensure-conditions' has run.")

(defun nelisp-cl-generic--ensure-conditions ()
  "Register `cl-no-applicable-method'/`cl-no-next-method' as real error
conditions, the same two `put's `define-error' itself makes with a
nil/`error' PARENT -- but done directly, and lazily (called from
`nelisp-cl-generic--ensure', i.e. the first time any generic is actually
defined, not at this file's own top level).  Cannot call `define-error'
itself, or `put', at THIS point in the file: this prelude loads
sequentially, top to bottom, and both `define-error''s own `(unless
(fboundp ...) ...)' fallback AND plain `put' itself are defined LATER in
this same file than this cl-generic block -- confirmed this session,
calling either here is `void-function'.  Deferring to first use, well
after the whole prelude has finished loading, sidesteps the ordering
question entirely; the identical, host-Emacs-loadable mirror copy at the
same line in `lisp/nelisp-cl-macros.el' uses the same deferred form so
both copies stay in sync regardless."
  (unless nelisp-cl-generic--conditions-registered
    (setq nelisp-cl-generic--conditions-registered t)
    (put 'cl-no-applicable-method 'error-conditions '(cl-no-applicable-method error))
    (put 'cl-no-applicable-method 'error-message "No applicable method")
    (put 'cl-no-next-method 'error-conditions '(cl-no-next-method error))
    (put 'cl-no-next-method 'error-message "No next method")))

(defun nelisp-cl-generic--parse-specializer (arg-form)
  "Parse one position-0 arglist entry of a `cl-defmethod' form.
Return a plist: `(:kind unspecialized)' for a bare symbol,
`(:kind type :type-name TYPE)' for `(VAR TYPE)', or
`(:kind eql :value-form FORM)' for `(VAR (eql FORM))'.  Signals a loud
`error' for any other shape -- head/list-head specializers, `&context',
or anything else Doc 185 §2.1 does not support."
  (cond
   ((symbolp arg-form) (list :kind 'unspecialized))
   ((and (consp arg-form) (symbolp (car arg-form))
         (consp (cdr arg-form)) (null (cddr arg-form)))
    (let ((spec (car (cdr arg-form))))
      (cond
       ((and (consp spec) (eq (car spec) 'eql)
             (consp (cdr spec)) (null (cddr spec)))
        (list :kind 'eql :value-form (car (cdr spec))))
       ((symbolp spec) (list :kind 'type :type-name spec))
       (t (error "cl-defmethod: unsupported specializer form %S (Doc 185 \
§2.1 -- type name or (eql VALUE) only)"
                 arg-form)))))
   (t (error "cl-defmethod: unsupported specializer form %S (Doc 185 §2.1 \
-- type name or (eql VALUE) only)"
             arg-form))))

(defun nelisp-cl-generic--parse-arglist (arglist name)
  "Split a `cl-defmethod' ARGLIST into (PLAIN-ARGLIST . SPEC-PLIST).
SPEC-PLIST (from `nelisp-cl-generic--parse-specializer') describes
position 0 only -- Doc 185 §3.1 supports a single dispatch argument.  A
non-bare-symbol specializer at any later position, or any unrecognised
`&FOO' lambda-list keyword (e.g. `&context'), is a loud `error' naming
NAME and the position."
  (let ((plain nil) (spec nil) (i 0) (in-required t) (cur arglist))
    (while cur
      (let ((item (car cur)))
        (cond
         ((memq item '(&optional &rest &key &aux))
          (setq in-required nil)
          (push item plain))
         ((and (symbolp item) (> (length (symbol-name item)) 0)
               (eq (aref (symbol-name item) 0) ?&))
          (error "cl-defmethod %s: unsupported lambda-list keyword %S \
(Doc 185 subset: &optional/&rest/&key/&aux only)"
                 name item))
         ((not in-required)
          (push item plain))
         ((= i 0)
          (let ((parsed (nelisp-cl-generic--parse-specializer item)))
            (setq spec parsed)
            (push (if (consp item) (car item) item) plain))
          (setq i (1+ i)))
         (t
          (unless (symbolp item)
            (error "cl-defmethod %s: specializer on argument position %d \
not supported (Doc 185 §3.1 -- position 0 only)"
                   name i))
          (push item plain)
          (setq i (1+ i)))))
      (setq cur (cdr cur)))
    (cons (nreverse plain)
          (or spec (list :kind 'unspecialized)))))

(defun nelisp-cl-generic--builtin-type-p (type-name)
  "Non-nil iff TYPE-NAME is one of `cl-typep''s ten frozen builtins
(Doc 185 §2.1a -- `cl-typep' itself is never widened)."
  (memq type-name '(integer number float string symbol cons list vector null t)))

(defun nelisp-cl-generic--type-match (val type)
  "Doc 185 §3.2, verbatim: `cl-typep' for the ten builtins, struct
ancestry via `nelisp-cl-macros--struct-isa' for anything `recordp'."
  (cond
   ;; `funcall' with a QUOTED symbol, matching the identical fix (and its
   ;; full explanation) at the same line in this block's mirror copy,
   ;; `lisp/nelisp-cl-macros.el' -- a host-Emacs `ert'/autoload hazard
   ;; that copy has and this one, loaded only by the standalone reader,
   ;; does not; kept identical here anyway so the two copies stay
   ;; byte-for-byte in sync per this project's own mirror-consistency
   ;; convention (the `cl-loop' fix had to land in both; see AI.md).
   ((nelisp-cl-generic--builtin-type-p type) (funcall 'cl-typep val type))
   ((and (recordp val)
         (nelisp-cl-macros--struct-isa (nelisp--record-type val) type))
    t)
   (t nil)))

(defun nelisp-cl-generic--struct-parent (tag)
  "The immediate `:include' parent of struct type TAG, or nil."
  (let ((info (cdr (assq tag nelisp-cl-macros--struct-info))))
    (and info (car (cdr (memq :parent info))))))

(defun nelisp-cl-generic--struct-depth (tag target)
  "Number of `:include' hops from TAG up to TARGET.  Callers only call
this once `nelisp-cl-macros--struct-isa' has already confirmed TAG isa
TARGET, so the walk is expected to terminate at TARGET -- but the walk
is bounded defensively at the registry's own size rather than trusting
that invariant unconditionally: a `nelisp-cl-macros--struct-isa' that
lies (this session's own `s/(nelisp-cl-macros--struct-isa ...)/t)/'
`tools/gate-mutations.txt' row does exactly that) sends TAG walking up
through nil forever otherwise, hanging the whole dispatch instead of
signalling -- exactly the silent-vs-loud failure shape §3.5 exists to
avoid, just one level lower than a dispatch decision."
  (let ((n 0) (cur tag)
        (bound (1+ (length nelisp-cl-macros--struct-info))))
    (while (and (not (eq cur target)) (> bound 0))
      (setq cur (nelisp-cl-generic--struct-parent cur))
      (setq n (1+ n))
      (setq bound (1- bound)))
    (unless (eq cur target)
      (error "nelisp-cl-generic--struct-depth: %S never reaches %S via :include ancestry (nelisp-cl-macros--struct-isa said it would)"
             tag target))
    n))

(defun nelisp-cl-generic--same-specializer-p (a b)
  "Non-nil iff method entries A and B specialize the same way -- a new
entry this-equal to an existing one REPLACES it (`cl-defmethod'
redefinition, not accumulation; Doc 185 §3.4)."
  (and (eq (plist-get a :kind) (plist-get b :kind))
       (eq (plist-get a :type-name) (plist-get b :type-name))
       (eql (plist-get a :value) (plist-get b :value))))

(defun nelisp-cl-generic--register-method (name entry)
  "Install ENTRY on generic NAME's method list, replacing any existing
entry with the same specializer (`nelisp-cl-generic--same-specializer-p')
rather than accumulating a duplicate.  Invalidates NAME's cached
dispatch table (Doc 185 §3.4) so the next call rebuilds it."
  (let ((kept nil))
    (dolist (e (get name 'nelisp-cl-generic--methods))
      (unless (nelisp-cl-generic--same-specializer-p e entry)
        (push e kept)))
    (put name 'nelisp-cl-generic--methods (cons entry (nreverse kept)))
    (put name 'nelisp-cl-generic--dispatch-cache nil)))

(defun nelisp-cl-generic--build-dispatch-table (name)
  "Partition NAME's registered methods into the three static specializer
tiers (Doc 185 §3.3) and cache the result on NAME's plist (§3.4).  Struct
vs. builtin `:type' matching stays a per-call decision
(`nelisp-cl-generic--type-match') since a struct named by a specializer
may not have been `cl-defstruct'-registered yet at the time this table is
first built."
  (let (eql-methods type-methods unspecialized-methods)
    (dolist (e (get name 'nelisp-cl-generic--methods))
      (let ((k (plist-get e :kind)))
        (cond
         ((eq k 'eql) (push e eql-methods))
         ((eq k 'type) (push e type-methods))
         (t (push e unspecialized-methods)))))
    (let ((table (list :eql eql-methods :type type-methods
                        :unspecialized unspecialized-methods)))
      (put name 'nelisp-cl-generic--dispatch-cache table)
      table)))

(defun nelisp-cl-generic--dispatch-table (name)
  "NAME's cached dispatch table, building it lazily on first use
(Doc 185 §2.3/§3.4 -- this is the whole answer to Doc 157's eager,
global, `eval'-based ~280s prefill wall: nothing runs for a generic
until it is actually called, and the cost then is proportional only to
that one generic's own method count)."
  (or (get name 'nelisp-cl-generic--dispatch-cache)
      (nelisp-cl-generic--build-dispatch-table name)))

(defun nelisp-cl-generic--eql-match (methods value)
  "The first (only, by construction -- redefinition replaces, never
duplicates) eql-tier method whose `:value' is `eql' to VALUE, or nil."
  (let (hit)
    (dolist (e methods)
      (when (and (not hit) (eql (plist-get e :value) value))
        (setq hit e)))
    hit))

(defun nelisp-cl-generic--ordered-type-matches (methods value)
  "The `:type'-tier METHODS applicable to VALUE, most specific first
(Doc 185 §3.3).  Struct matches are ordered by
`nelisp-cl-generic--struct-depth' from VALUE's own runtime type -- always
a strict total order for one value, since `cl-defstruct''s `:include' is
single-parent only (§6.1), so every applicable struct specializer for a
given VALUE necessarily lies on that value's one ancestry chain.  Builtin
`cl-typep' matches carry no such order in this subset (§2.1a does not add
a type lattice on top of `cl-typep''s ten-symbol contract): two or more
of them matching the same VALUE is an ambiguous dispatch, signalled
loudly here rather than guessed at (§3.5)."
  (let (struct-hits builtin-hits)
    (dolist (e methods)
      (let ((tn (plist-get e :type-name)))
        (when (nelisp-cl-generic--type-match value tn)
          (if (nelisp-cl-generic--builtin-type-p tn)
              (push e builtin-hits)
            (push (cons (nelisp-cl-generic--struct-depth
                         (nelisp--record-type value) tn)
                        e)
                  struct-hits)))))
    (when (> (length builtin-hits) 1)
      (error "cl-generic: ambiguous dispatch -- builtin-type methods %S \
all match %S"
             (mapcar (lambda (e) (plist-get e :type-name)) builtin-hits)
             value))
    (append
     (mapcar #'cdr (sort struct-hits (lambda (a b) (< (car a) (car b)))))
     builtin-hits)))

(defun nelisp-cl-generic--applicable-methods (name value)
  "NAME's methods applicable to VALUE, most specific first: an `eql'
match (if any) outranks every type match, which outranks the
unspecialized fallback (if any) -- Doc 185 §3.3."
  (let ((table (nelisp-cl-generic--dispatch-table name)))
    (append
     (let ((hit (nelisp-cl-generic--eql-match (plist-get table :eql) value)))
       (and hit (list hit)))
     (nelisp-cl-generic--ordered-type-matches (plist-get table :type) value)
     (plist-get table :unspecialized))))

(defun nelisp-cl-generic--invoke (name args)
  "Dispatch a call to generic NAME with ARGS (Doc 185 §3-§3.5): find the
applicable methods for `(car ARGS)', most specific first, and call the
first one with `cl-call-next-method'/`cl-next-method-p' able to walk the
rest.  Signals `cl-no-applicable-method' when nothing matches."
  (let* ((applicable (nelisp-cl-generic--applicable-methods name (car args)))
         (fns (mapcar (lambda (e) (plist-get e :fn)) applicable)))
    (if (null fns)
        ;; Data shape is `(cons name args)' -- `(GENERIC ARG1 ARG2 ...)',
        ;; flat -- measured against real Emacs 30.1 this session (not the
        ;; nested `(list generic-name args)' Doc 185 §3.5's table text
        ;; says; that reading does not match what real `cl-generic'
        ;; actually signals, confirmed empirically, so this follows the
        ;; measured behaviour over the doc's imprecise transcription).
        (signal 'cl-no-applicable-method (cons name args))
      (let ((nelisp-cl-generic--next-methods (cdr fns))
            (nelisp-cl-generic--call-args args)
            (nelisp-cl-generic--current-name name))
        (apply (car fns) args)))))

(defun cl-call-next-method (&rest new-args)
  "Call the next-most-specific applicable method in the `cl-defmethod'
dispatch chain currently running (Doc 185 §2.2).  With no NEW-ARGS,
reuses the current call's own arguments (matches real Emacs
`cl-generic').  Signals `cl-no-next-method' if there is no next method
(§3.5)."
  (if (null nelisp-cl-generic--next-methods)
      (signal 'cl-no-next-method (list nelisp-cl-generic--current-name))
    (let* ((fn (car nelisp-cl-generic--next-methods))
           (rest (cdr nelisp-cl-generic--next-methods))
           (use-args (if new-args new-args nelisp-cl-generic--call-args)))
      (let ((nelisp-cl-generic--next-methods rest)
            (nelisp-cl-generic--call-args use-args))
        (apply fn use-args)))))

(defun cl-next-method-p ()
  "Non-nil iff `cl-call-next-method' would find a next method right now."
  (and nelisp-cl-generic--next-methods t))

(defun nelisp-cl-generic--make-dispatcher (name)
  "Build NAME's dispatcher as a literal lambda FORM (data), with NAME
spliced in as a quoted constant rather than closed over.  Required
because `scripts/nelisp-stdlib-prelude.el' loads with
`lexical-binding: nil' -- a `(lambda (&rest args) (... name ...))' built
by a running function would look up `name' by DYNAMIC scoping at call
time there, long after this function's own local binding is gone, not
capture it.  The same technique `cl-defstruct''s own constructor/accessor
codegen above already uses, for the same reason."
  (list 'lambda '(&rest nelisp-cl-generic--dispatch-args)
        (list 'nelisp-cl-generic--invoke (list 'quote name)
              'nelisp-cl-generic--dispatch-args)))

(defun nelisp-cl-generic--ensure (name)
  "Make NAME callable as a Doc 185 generic function, once.  Idempotent
and callable from both `cl-defgeneric' and `cl-defmethod' -- real
`cl-generic' allows a bare `cl-defmethod' with no preceding
`cl-defgeneric', and this subset does too."
  (nelisp-cl-generic--ensure-conditions)
  (unless (get name 'nelisp-cl-generic--dispatcher-installed)
    (put name 'nelisp-cl-generic--dispatcher-installed t)
    (defalias name (nelisp-cl-generic--make-dispatcher name))))

(defmacro cl-defgeneric (name arglist &rest body)
  "Declare NAME as a generic function over ARGLIST (Doc 185 §3.1).
Installs NAME as a dispatching function with no methods yet -- add
methods with `cl-defmethod'.  BODY may be a single leading docstring
only: this subset does not implement CLOS-style default-method bodies,
so a non-docstring BODY form is a loud macroexpansion-time `error'
rather than a silently-ignored default method."
  (ignore arglist)
  (when (and body (stringp (car body))) (setq body (cdr body)))
  (when body
    (error "cl-defgeneric %s: default-method body not supported by this \
subset -- declare with no body, add methods via cl-defmethod (Doc 185 \
§3.1)"
           name))
  `(prog1 ',name (nelisp-cl-generic--ensure ',name)))

(defmacro cl-defmethod (name &rest args)
  "Define a method on generic NAME (Doc 185's subset of real
`cl-defmethod').  No method-combination qualifier is supported --
`:before'/`:after'/`:around'/anything but none is a loud `error' naming
the qualifier (§3.5).  Exactly one specializer, on argument position 0
only: a type name (builtin `cl-typep' symbol or `cl-defstruct' name), an
`(eql VALUE)' form, or a bare (unspecialized) symbol (§2.1/§3.1).  The
method body can call `cl-call-next-method'/`cl-next-method-p' (§2.2)."
  (let (qualifier)
    (when (and args (not (listp (car args))))
      (setq qualifier (car args) args (cdr args)))
    (when qualifier
      (error "cl-defmethod %s: unsupported method-combination qualifier \
%S (Doc 185 subset: primary methods only)"
             name qualifier))
    (let* ((arglist (car args))
           (body (cdr args))
           (parsed (nelisp-cl-generic--parse-arglist arglist name))
           (plain-arglist (car parsed))
           (spec (cdr parsed))
           (kind (plist-get spec :kind))
           (type-name (plist-get spec :type-name))
           (value-form (and (eq kind 'eql) (plist-get spec :value-form))))
      `(prog1 ',name
         (nelisp-cl-generic--ensure ',name)
         (nelisp-cl-generic--register-method
          ',name
          (list :kind ',kind :type-name ',type-name :value ,value-form
                :fn (lambda ,plain-arglist ,@body)))))))

;; ---------------------------------------------------------------------------
;; Doc 156 breadth (2026-06-23): general stdlib functions needed to load/run
;; real library packages (rx.el, cl-generic.el) over the bare reader.  Each is
;; fboundp-gated so a future native builtin still wins.  rx's `rx--to-expr'
;; reads the special var `macroexpand-all-environment'; leaving it unbound is
;; an UNCATCHABLE abort in the reader, so declare it special with a nil default.
;; ---------------------------------------------------------------------------
(defvar macroexpand-all-environment nil
  "Environment of macros currently being expanded by `macroexpand-all'.")
(unless (fboundp 'characterp)
  (defun characterp (object &optional _ignore)
    "Return non-nil if OBJECT is a valid character (an integer 0..#x3FFFFF)."
    (and (integerp object) (>= object 0) (<= object #x3fffff))))
(unless (fboundp 'max-char)
  (defun max-char (&optional unicode)
    "Return the maximum character code.
With UNICODE non-nil the answer is the largest Unicode code point,
which is what a caller asking for a range check wants."
    (if unicode #x10ffff #x3fffff)))
(unless (fboundp 'mapcan)
  (defun mapcan (func sequence)
    "Apply FUNC to each element of SEQUENCE, `nconc' the results."
    (unless (sequencep sequence)
      (signal 'wrong-type-argument (list 'sequencep sequence)))
    (apply (function nconc) (mapcar func sequence))))
(unless (fboundp 'remq)
  (defun remq (elt list)
    "Return a copy of LIST with all `eq' occurrences of ELT removed."
    (unless (listp list) (signal 'wrong-type-argument (list 'listp list)))
    (unless (proper-list-p list)
      (signal 'wrong-type-argument (list 'listp list)))
    (let ((acc nil))
      (dolist (x list (nreverse acc))
        (unless (eq x elt) (setq acc (cons x acc)))))))
(unless (fboundp 'memql)
  (defun memql (elt list)
    "Return the tail of LIST whose car is `eql' to ELT, or nil."
    (unless (listp list) (signal 'wrong-type-argument (list 'listp list)))
    (let ((orig list) (found nil) (done nil))
      (while (not (or found done))
        (cond
         ((null list) (setq done t))
         ((not (consp list)) (signal 'wrong-type-argument (list 'listp orig)))
         ((eql elt (car list)) (setq found t))
         (t (setq list (cdr list)))))
      (and found list))))
(unless (fboundp 'decode-char)
  (defun decode-char (charset code-point)
    "Minimal `decode-char': return CODE-POINT unchanged (ucs identity)."
    (unless (memq charset '(ucs unicode iso-10646-1 emacs eight-bit ascii))
      (signal 'wrong-type-argument (list 'charsetp charset)))
    code-point))
(unless (fboundp 'car-less-than-car)
  ;; Sort predicate over (KEY . _) cells; rx's `(any "abc")' sorts char
  ;; intervals with it (a void sort predicate is an uncatchable abort).
  (defun car-less-than-car (a b) (< (car a) (car b))))
(unless (fboundp 'regexp-opt)
  ;; This was a plain alternation: (regexp-opt '("ab" "ac")) produced
  ;; "\\(?:ab\\|ac\\)" where Emacs produces "\\(?:a[bc]\\)".  Both match the same
  ;; strings, so nothing failed -- but `regexp-opt' output is embedded in
  ;; font-lock keyword tables and compared, and the two runtimes disagreeing
  ;; on the TEXT of a generated regexp is a difference that surfaces far from
  ;; here.  This is Emacs 30.1's algorithm: common prefix, then common suffix,
  ;; then split on the first character, with runs of single characters folded
  ;; into ranges.
  ;;
  ;; `regexp-opt-charset' uses a char-table in Emacs and a sorted list here.
  ;; The output is identical because a char-table iterates in increasing code
  ;; order, which is what the sort reproduces -- and a char-table would need
  ;; `map-char-table', which this runtime does not have.
  (defvar regexp-unmatchable "\\`a\\`"
    "A regexp that never matches anything.")

  (defun nelisp--common-prefix (strings)
    "Longest common prefix of STRINGS -- `try-completion\' with an empty seed."
    (if (null strings) ""
      (let ((pre (car strings)))
        (dolist (s (cdr strings))
          (let ((i 0) (n (min (length pre) (length s))))
            (while (and (< i n) (eq (aref pre i) (aref s i))) (setq i (1+ i)))
            (setq pre (substring pre 0 i))))
        pre)))

  (defun nelisp--leading-run (prefix strings)
    "The leading run of STRINGS that starts with PREFIX -- `all-completions\'."
    (let ((out nil) (cur strings) (done nil))
      (while (and cur (not done))
        (if (string-prefix-p prefix (car cur))
            (setq out (cons (car cur) out))
          (setq done t))
        (setq cur (cdr cur)))
      (nreverse out)))

  (defun regexp-opt-charset (chars)
    "Return a regexp matching a character in CHARS."
    (let ((bracket "") (dash "") (caret "") (rest nil))
      (dolist (char chars)
        (cond ((eq char ?\]) (setq bracket "]"))
              ((eq char ?^) (setq caret "^"))
              ((eq char ?-) (setq dash "-"))
              (t (setq rest (cons char rest)))))
      (setq rest (sort (delete-dups (nreverse rest)) (function <)))
      (let ((charset "") (start -1) (end -2))
        (dolist (c rest)
          (if (= (1- c) end)
              (setq end c)
            (if (> end (+ start 2))
                (setq charset (format "%s%c-%c" charset start end))
              (while (>= end start)
                (setq charset (format "%s%c" charset start))
                (setq start (1+ start))))
            (setq start c)
            (setq end c)))
        (when (>= end start)
          (if (> end (+ start 2))
              (setq charset (format "%s%c-%c" charset start end))
            (while (>= end start)
              (setq charset (format "%s%c" charset start))
              (setq start (1+ start)))))
        ;; `]\' must be first, `^\' must not be, `-\' must be first or last.
        (let ((all (concat bracket charset caret dash)))
          (cond ((= (length all) 0) regexp-unmatchable)
                ((= (length all) 1) (regexp-quote all))
                ((string-equal all "^-") "[-^]")
                (t (concat "[" all "]")))))))

  (defun regexp-opt-group (strings &optional paren lax)
    "Return a regexp matching a string in the sorted list STRINGS."
    (let* ((open-group (cond ((stringp paren) paren) (paren "\\(?:") (t "")))
           (close-group (if paren "\\)" ""))
           (open-charset (if lax "" open-group))
           (close-charset (if lax "" close-group)))
      (cond
       ((= (length strings) 0) "")
       ((= (length strings) 1)
        (if (= (length (car strings)) 1)
            (concat open-charset (regexp-quote (car strings)) close-charset)
          (concat open-group (regexp-quote (car strings)) close-group)))
       ((= (length (car strings)) 0)
        (concat open-charset (regexp-opt-group (cdr strings) t t) "?" close-charset))
       ((and (= (length (car strings)) 1)
             (let ((strs (cdr strings)))
               (while (and strs (/= (length (car strs)) 1)) (setq strs (cdr strs)))
               strs))
        (let ((letters nil) (rest nil))
          (dolist (s strings)
            (if (= (length s) 1)
                (setq letters (cons (string-to-char s) letters))
              (setq rest (cons s rest))))
          (if rest
              (concat open-group
                      (regexp-opt-group (nreverse rest))
                      "\\|" (regexp-opt-charset letters)
                      close-group)
            (concat open-charset (regexp-opt-charset letters) close-charset))))
       (t
        (let ((prefix (nelisp--common-prefix strings)))
          (if (> (length prefix) 0)
              (let* ((n (length prefix))
                     (suffixes (mapcar (lambda (s) (substring s n)) strings)))
                (concat open-group (regexp-quote prefix)
                        (regexp-opt-group suffixes t t) close-group))
            (let* ((sgnirts (mapcar (function reverse) strings))
                   (xiffus (nelisp--common-prefix sgnirts)))
              (if (> (length xiffus) 0)
                  (let* ((n (- (length xiffus)))
                         ;; Sorting matters for cases such as ("ad" "d").
                         (prefixes (sort (mapcar (lambda (s) (substring s 0 n)) strings)
                                         (function string-lessp))))
                    (concat open-group
                            (regexp-opt-group prefixes t t)
                            (regexp-quote (reverse xiffus))
                            close-group))
                (let* ((char (substring (car strings) 0 1))
                       (half1 (nelisp--leading-run char strings))
                       (half2 (nthcdr (length half1) strings)))
                  (concat open-group
                          (regexp-opt-group half1)
                          "\\|" (regexp-opt-group half2)
                          close-group)))))))))) 

  (defun regexp-opt (strings &optional paren)
    "Return a regexp matching any string in STRINGS, as Emacs builds it."
    ;; Emacs names `list-or-vector-p' here, not `listp': STRINGS may be
    ;; either, and a caller handed a string gets a predicate that says so.
    ;; Two predicates, by what the argument IS: a string is a sequence but
    ;; not a list or vector, so it names `list-or-vector-p'; anything that
    ;; is not a sequence at all names `sequencep'.  Measured -- one name for
    ;; both would be wrong half the time.
    (unless (or (listp strings) (vectorp strings))
      (signal 'wrong-type-argument
              (list (if (sequencep strings) 'list-or-vector-p 'sequencep)
                    strings)))
    (let* ((open (cond ((stringp paren) paren) (paren "\\(")))
           (re (if strings
                   (regexp-opt-group
                    (delete-dups (sort (copy-sequence strings) (function string-lessp)))
                    (or open t) (not open))
                 (concat (or open "\\(?:") regexp-unmatchable "\\)"))))
      (cond ((eq paren (quote words)) (concat "\\<" re "\\>"))
            ((eq paren (quote symbols)) (concat "\\_<" re "\\_>"))
            (t re)))))
;; `kbd' and `key-description' were absent, so a caller got `void-function'.
;; This pair covers the modifier-prefixed and named keys that appear in key
;; sequences written as text; a full `read-kbd-macro' (angle-bracket function
;; keys, <C-M-return>, kbd macros) is not here, and a token this does not
;; know is passed through as its own characters rather than guessed at.
(unless (fboundp 'kbd)
  (defun kbd (keys)
    (let ((words (split-string keys " " t)) (out nil))
      (dolist (w words)
        (let ((bits 0) (done nil))
          (while (not done)
            (cond
             ((and (> (length w) 2) (string-prefix-p "C-" w))
              (setq bits (logior bits 1)) (setq w (substring w 2)))
             ((and (> (length w) 2) (string-prefix-p "M-" w))
              (setq bits (logior bits 2)) (setq w (substring w 2)))
             ((and (> (length w) 2) (string-prefix-p "S-" w))
              (setq bits (logior bits 4)) (setq w (substring w 2)))
             (t (setq done t))))
          (let ((base (cond ((equal w "SPC") 32) ((equal w "RET") 13)
                            ((equal w "TAB") 9) ((equal w "ESC") 27)
                            ((equal w "DEL") 127)
                            ((= (length w) 1) (aref w 0))
                            (t nil))))
            (if (null base)
                (setq out (append out (append w nil)))
              (when (= 1 (logand bits 1))
                (setq base (if (and (>= (nelisp--case-up-char base) ?@)
                                    (<= (nelisp--case-up-char base) ?_))
                               (logand (nelisp--case-up-char base) 31)
                             base)))
              (when (= 4 (logand bits 4)) (setq base (nelisp--case-up-char base)))
              (when (= 2 (logand bits 2)) (setq base (logior base 134217728)))
              (setq out (append out (list base)))))))
      (if (let ((all t))
            (dolist (k out) (unless (and (integerp k) (>= k 0) (<= k 127))
                              (setq all nil)))
            all)
          (apply #'string out)
        (apply #'vector out)))))
(unless (fboundp 'single-key-description)
  (defun single-key-description (key &optional _no-angles)
    (let ((out ""))
      (when (/= 0 (logand key 134217728))
        (setq out "M-") (setq key (logand key (lognot 134217728))))
      (cond
       ((= key 32) (concat out "SPC"))
       ((= key 13) (concat out "RET"))
       ((= key 9) (concat out "TAB"))
       ((= key 27) (concat out "ESC"))
       ((= key 127) (concat out "DEL"))
       ((< key 27) (concat out "C-" (char-to-string (+ key 96))))
       ((< key 32) (concat out "C-" (char-to-string (+ key 64))))
       (t (concat out (char-to-string key)))))))
(unless (fboundp 'key-description)
  (defun key-description (keys &optional _prefix)
    (mapconcat #'single-key-description (append keys nil) " ")))
(unless (fboundp 'help-add-fundoc-usage)
  (defun help-add-fundoc-usage (docstring arglist)
    "Append the usage line Emacs appends, rather than dropping ARGLIST."
    (cond
     ((eq arglist t) (if (stringp docstring) docstring ""))
     ((stringp arglist)
      (if (string-match "\\`([^ ]+\\(.*\\))\\'" arglist)
          (concat (if (stringp docstring) docstring "")
                  "\n\n(fn" (match-string 1 arglist) ")")
        (signal 'error (list "Unrecognized usage format"))))
     (t (nelisp--check-seq-list arglist)
        (nelisp--help-fundoc-usage docstring arglist)))))
(unless (fboundp 'nelisp--help-usage-arg)
  (defun nelisp--help-usage-arg (a)
    "A as it appears in a usage line: the placeholder upcased, structure kept.
`&optional' and `&rest' are lambda-list keywords, not placeholders, and
Emacs leaves them alone."
    (cond
     ((and (symbolp a) a (> (length (symbol-name a)) 0)
           (= (aref (symbol-name a) 0) ?&))
      (symbol-name a))
     ((and (symbolp a) a) (upcase (symbol-name a)))
     ;; Only the CAR of a nested form is upcased -- (a (b c)) becomes
     ;; (fn A (B c)), not (fn A (B C)).  Measured; upcasing the whole form
     ;; reads as three placeholders where Emacs shows one and a literal.
     ((consp a)
      (format "%S" (cons (if (and (symbolp (car a)) (car a))
                             (intern (upcase (symbol-name (car a))))
                           (car a))
                         (cdr a))))
     (t (format "%S" a)))))
(unless (fboundp 'nelisp--help-fundoc-usage)
  (defun nelisp--help-fundoc-usage (docstring arglist)
    (concat (let ((d (if (stringp docstring) docstring "")))
              (while (and (> (length d) 0) (= (aref d (1- (length d))) 10))
                (setq d (substring d 0 (1- (length d)))))
              d)
            "\n\n(fn"
            ;; Emacs UPCASES the argument NAMES -- that is what makes them
            ;; read as placeholders -- but only the names.  Anything that is
            ;; not a symbol is printed as itself, so (fn "a" "b") rather
            ;; than the (fn A B) an unconditional upcase produced.
            (mapconcat (lambda (a) (concat " " (nelisp--help-usage-arg a)))
                       (append arglist nil) "")
            ")")))
(unless (fboundp 'help-split-fundoc)
  (defun help-split-fundoc (docstring _def &optional section)
    "Return nil: nothing here embeds a usage line in a docstring.
Emacs answers nil when there is none, NOT (nil . DOCSTRING) -- callers test
the result and take the whole docstring themselves when it is nil, so the
cons made them use nil as the usage line."
    (when docstring (nelisp--check-string docstring))
    (if (and (eq section t) docstring) (cons nil docstring) nil)))

;; cl-macs / macroexp helpers that cl-generic.el (and other gv/cl users) need at
;; macro-expansion time.  All fboundp-gated.  Together with the `setf' get/gethash/
;; alist-get/macro places above they let cl-generic.el build its dispatch closure
;; (`cl-generic-define') and register methods; full type-dispatch additionally
;; needs the cl-preloaded built-in-class hierarchy, which is C-core-coupled and a
;; separate reader item (Doc 156).
(unless (fboundp 'cl-function)
  ;; Minimal: a plain `(function FUNC)'.  The real `cl-function' also rewrites
  ;; Common-Lisp lambda-lists (&key/&aux/destructuring); cl-generic methods reach
  ;; it with the specializer already stripped, so a simple arglist suffices here.
  (defmacro cl-function (func) (list 'function func)))
(unless (fboundp 'macroexp--fgrep)
  (defun macroexp--fgrep (bindings sexp)
    "Conservative: return the BINDINGS whose variable appears anywhere in SEXP.
A safe over-approximation of \"which bindings might be used\" — enough for the
cl-generic method-arg liveness heuristic."
    ;; BINDINGS is only looked at when the SEXP walk reaches something to
    ;; look up, and an EMPTY vector yields nothing: (macroexp--fgrep 0 [])
    ;; is nil while (macroexp--fgrep 0.0 ["a" "b"]) names `listp'.  The
    ;; emptiness is the whole difference; measured three ways to find it.
    (unless (and (vectorp sexp) (= (length sexp) 0))
      (unless (listp bindings)
        (signal 'wrong-type-argument (list 'listp bindings)))
      (unless (proper-list-p bindings)
        (signal 'wrong-type-argument (list 'listp bindings))))
    (let ((res nil))
      (dolist (b (if (listp bindings) bindings nil) (nreverse res))
        (let ((sym (and (consp b) (car b)))
              (stack (list sexp)) (found nil))
          (unless sym (setq stack nil))
          (while (and stack (not found))
            (let ((x (pop stack)))
              (cond ((eq x sym) (setq found t))
                    ((consp x) (push (car x) stack) (push (cdr x) stack)))))
          (when found (push b res)))))))
(unless (fboundp 'with-memoization)
  (defmacro with-memoization (place &rest code)
    "Return PLACE if non-nil, else evaluate CODE, cache it in PLACE, return it.
Minimal: PLACE is evaluated twice (cl-generic's places are side-effect free)."
    (list 'or place (list 'setf place (cons 'progn code)))))
;; General builtins (defuns persist into the AOT boot image; these are missing
;; from the bare reader and needed by oclosure / cl-generic and many packages).
(unless (fboundp 'ignore)
  (defun ignore (&rest _arguments) "Do nothing and return nil." nil))
(unless (fboundp 'always)
  (defun always (&rest _arguments) "Do nothing and return t." t))
;; `current-load-list' is an Emacs global tracking the current file's load
;; history; cl-generic and other libs `push' onto it.  Reading it unbound is an
;; uncatchable abort in the reader — declare it special with a nil default.
(defvar current-load-list nil)
;; In Emacs these two are always BOUND -- nil when nothing is being loaded
;; from a file, a path when there is one -- so `(or load-file-name
;; buffer-file-name)', which is how a file asks where it lives, always reads.
;; Here they were undeclared, so that idiom raised `void-variable' instead:
;; `src/nelisp-cc-runtime.el' died on it at load time, and with it
;; `nelisp-aot-compiler' and every native compile that needed it.  nil is a
;; value Emacs itself produces for both, and the one call site in this tree
;; says so in its own docstring ("nil at runtime").
;;
;; What is still missing is the other half of the contract: this runtime's
;; `load' does not SET `load-file-name' while it loads.  Measured, not
;; assumed -- `PROBE load-file-name during load' in tools/ai/runtime-probe.el
;; reports which of the two answers this binary gives.
(defvar load-file-name nil)
(defvar buffer-file-name nil)
(unless (fboundp 'closurep)
  ;; The reader represents a closure as a `(closure ENV ARGS BODY)' list.
  (defun closurep (object) (eq (car-safe object) 'closure)))
(unless (fboundp 'byte-code-function-p)
  ;; The reader has no byte-code objects (everything is interpreted).
  (defun byte-code-function-p (_object) nil))
;; NOTE (Doc 157 §5): `compiled-function-p' is intentionally NOT defined here.
;; Defining it (correctly returning nil) lets cl-generic's `cl--generic-compiler'
;; defvar init succeed and bind the EVAL-based dispatcher compiler — at which
;; point cl-generic.el's load eagerly builds ~15-20 dispatchers (per internal
;; method + `cl--generic-prefill-dispatchers'), each an `(eval BIG-LAMBDA t)' on
;; the slow reader interpreter, and the load does not finish within 280s.
;; Withholding it keeps cl-generic *loadable* (the compiler stays unbound, so
;; cl-defmethod aborts gracefully on first use instead of hanging the load).
;; Full cl-generic dispatch needs the reader to gain compiled functions
;; (byte-compilation) so the dispatchers compile fast — a reader-core perf item.
(unless (fboundp 'interpreted-function-p)
  (defun interpreted-function-p (object) (eq (car-safe object) 'closure)))
(unless (fboundp 'cl--find-class)
  (defun cl--find-class (type) (get type 'cl--class)))
;; `(setf (cl--find-class NAME) CLASS)' is how cl-preloaded / oclosure /
;; cl-defstruct register a class object.  In host Emacs this is a gv-setter;
;; here it routes through the `cl-simple-setter' property -> `cl--set-find-class'.
;; The DEFUN is baked (defuns persist); the property `(put ...)' is registered in
;; cl-lib.el's standalone setter block instead, because a top-level `(put ...)' in
;; this AOT-baked prelude does NOT survive into the boot image (only definitions
;; do).  Class registration is a prerequisite for the built-in-class type lattice
;; oclosure/cl-generic dispatch needs, which is itself a separate C-core-coupled
;; reader item — see Doc 156.
(unless (fboundp 'cl--set-find-class)
  (defun cl--set-find-class (type class) (put type 'cl--class class) class))

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

;; Doc 224-ish (2026-07-04) — accept BOTH backquote-family spellings.
;;
;; This reader's own char-level desugar (src/nelisp-reader.el,
;; src/nelisp-read.el, lisp/nelisp-cc-reader-parser.el) always turns a
;; source-level `\=`'/`,'/`,@' character into `(backquote FORM)' /
;; `(comma X)' / `(comma-at X)', using these plain multi-character
;; convenience names instead of real Emacs's own punctuation-named
;; symbols (real Emacs's reader/backquote.el represent the same three
;; things as the symbols whose print-names are literally "`", ",", and
;; ",@", requiring backslash-escaping to reference directly, e.g.
;; `\=`').  Forms loaded through a host-side GNU Emacs replay/.repl
;; pipeline (as opposed to being read fresh by this reader) can arrive
;; already `read' by a real Emacs and preserve ITS symbol spelling
;; instead of this reader's convenience names -- observed for org.el's
;; `org-with-wide-buffer' (org-macs.el), whose stored macro body's head
;; symbol is `eq' to (intern "`"), not to `backquote': calling it hit
;; `(void-function \=`)' because only the `backquote' name had a macro
;; function bound, and the literal "`"-named symbol had none.  Rather
;; than special-case that one macro, `nelisp--bq-expand'/
;; `-expand-list' below recognize either spelling wherever a
;; backquote/comma/comma-at marker is checked, and `\=`' is defmacro'd
;; below as an alias so the outer form is itself dispatchable as a
;; macro under whichever spelling it happens to carry.  `,'/`,@' are
;; deliberately NOT given independent macro/function bindings here:
;; real Emacs does not bind them either (they only have meaning nested
;; inside a backquote template, consumed structurally by the walker),
;; so a bare `(, X)'/`(,@ X)' outside a template still signals
;; void-function on both this reader and real Emacs, matching real
;; Emacs semantics.
(defun nelisp--bq-tag-p (form tag punct)
  "Non-nil if FORM is a cons whose car is the backquote-family marker
TAG (this reader's own convenience symbol, e.g. `comma') or the real
Emacs-style symbol named the literal punctuation string PUNCT (e.g.
\",\")."
  (and (consp form)
       (let ((head (car form)))
         (or (eq head tag) (eq head (intern punct))))))

(defun nelisp--bq-expand (form &optional level)
  "Return the expansion of FORM under `backquote' at nesting LEVEL.
LEVEL defaults to 1 (directly inside one backquote).  A `,'/`,@' at
LEVEL 1 fires immediately (its argument is evaluated at macro-expanded
runtime); above LEVEL 1 it is preserved as inert marker data one level
shallower, so a matching further `,' can still cancel it down to 0
\(host Emacs `backquote.el' depth semantics -- see the nested-backquote
commentary above `nelisp--bq-tag-p')."
  (let ((level (or level 1)))
    (cond
     ((vectorp form)
      (signal 'error (list "nelisp-bq: vector quasi not supported")))
     ((not (consp form))
      (list 'quote form))
     ((nelisp--bq-tag-p form 'comma ",")
      (if (= level 1)
          (cadr form)
        (list 'list (list 'quote 'comma)
              (nelisp--bq-expand (cadr form) (1- level)))))
     ((nelisp--bq-tag-p form 'comma-at ",@")
      (if (= level 1)
          (signal 'error (list "nelisp-bq: top-level ,@ not allowed"))
        (list 'list (list 'quote 'comma-at)
              (nelisp--bq-expand (cadr form) (1- level)))))
     ((nelisp--bq-tag-p form 'backquote "`")
      ;; A nested backquote increments the level for its own content and
      ;; is itself rebuilt as inert `(backquote ...)' data -- it is only
      ;; ever "consumed" by a comma at the matching depth, never by
      ;; simply appearing inside an outer backquote.
      (list 'list (list 'quote 'backquote)
            (nelisp--bq-expand (cadr form) (1+ level))))
     (t (nelisp--bq-expand-list form level)))))

(defun nelisp--bq-expand-list (form level)
  "Walk list FORM at nesting LEVEL, producing the expansion.
Recognises both (... ,X ...) interior unquote and (... . ,X) dotted
unquote / (... . ,@X) dotted splice patterns, at any LEVEL (see
`nelisp--bq-expand')."
  (let ((parts nil)        ; alist entries (KIND . EXPR) where KIND = list|splice
        (cur form)
        (tail-expr nil)
        (done nil)
        (has-splice nil))
    (while (and (not done) (consp cur))
      (let ((head (car cur)))
        (cond
         ;; cdr-position bare `comma' → source had `. ,X'.
         ((or (eq head 'comma) (eq head (intern ",")))
          (if (= level 1)
              (setq tail-expr (cadr cur))
            (setq tail-expr (list 'list (list 'quote 'comma)
                                   (nelisp--bq-expand (cadr cur) (1- level)))))
          (setq done t))
         ;; cdr-position bare `comma-at' → source had `. ,@X'.
         ((or (eq head 'comma-at) (eq head (intern ",@")))
          (if (= level 1)
              (progn (setq tail-expr (cadr cur)) (setq has-splice t))
            (setq tail-expr (list 'list (list 'quote 'comma-at)
                                   (nelisp--bq-expand (cadr cur) (1- level)))))
          (setq done t))
         (t
          (let ((elem head))
            (cond
             ((and (consp elem)
                   (or (eq (car elem) 'comma-at) (eq (car elem) (intern ",@"))))
              (if (= level 1)
                  (progn
                    (setq has-splice t)
                    (push (cons 'splice (cadr elem)) parts))
                (push (cons 'list
                             (list 'list (list 'quote 'comma-at)
                                   (nelisp--bq-expand (cadr elem) (1- level))))
                      parts)))
             ((and (consp elem)
                   (or (eq (car elem) 'comma) (eq (car elem) (intern ","))))
              (if (= level 1)
                  (push (cons 'list (cadr elem)) parts)
                (push (cons 'list
                             (list 'list (list 'quote 'comma)
                                   (nelisp--bq-expand (cadr elem) (1- level))))
                      parts)))
             (t
              (push (cons 'list (nelisp--bq-expand elem level)) parts))))
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

(defmacro \` (form)
  "Alias for `backquote', bound under real Emacs's own back-quote
symbol name so a form headed by that literal punctuation-named symbol
\(rather than this reader's `backquote' convenience name -- see the
commentary above `nelisp--bq-tag-p') is itself dispatchable as a
macro call."
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

(defun nelisp--setf-place-macro-p (head)
  "Non-nil if HEAD is a macro whose `(HEAD ...)' place form `setf' can expand.
The reader represents a macro function as `(macro . FUNCTION)'."
  (and (symbolp head) (fboundp head)
       (eq (car-safe (symbol-function head)) 'macro)))

(defun nelisp--setf-1 (place val)
  "Return the assignment form realising `(setf PLACE VAL)'.  See `setf'.
Doc 156: adds `(get S P)' → `put', `(gethash K H)' → `puthash',
`(alist-get K A)' → assq update/prepend, and macro-place expansion (so a
generalized place defined as a macro, e.g. cl-generic's `(cl--generic NAME)'
= `(get NAME ...)', is recursively re-dispatched).  These let cl-generic and
other gv-using libraries load/run on the bare reader."
  (cond
   ((symbolp place) (list 'setq place val))
   ((and (consp place) (eq (car place) 'car))
    (list 'setcar (cadr place) val))
   ((and (consp place) (eq (car place) 'cdr))
    (list 'setcdr (cadr place) val))
   ((and (consp place) (eq (car place) 'aref))
    (list 'aset (cadr place) (caddr place) val))
   ((and (consp place) (eq (car place) 'nth))
    (list 'setcar (list 'nthcdr (cadr place) (caddr place)) val))
   ((and (consp place) (eq (car place) 'get))
    (cons 'put (append (cdr place) (list val))))
   ((and (consp place) (eq (car place) 'gethash))
    (list 'puthash (cadr place) val (caddr place)))
   ((and (consp place) (eq (car place) 'alist-get))
    (let ((k (cadr place)) (a (caddr place)) (cell (make-symbol "setf-cell")))
      (list 'let (list (list cell (list 'assq k a)))
            (list 'if cell (list 'setcdr cell val)
                  (nelisp--setf-1 a (list 'cons (list 'cons k val) a))))))
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
    (list 'nelisp--record-set (cadr place)
          (cdr (assq (car place) nelisp-cl-macros--accessor-info))
          val))
   ((and (consp place) (nelisp--setf-place-macro-p (car place)))
    (nelisp--setf-1 (macroexpand-1 place) val))
   (t
    (signal 'error
            (list "setf: unsupported place"
                  (and (consp place) (car place)))))))

(defmacro setf (&rest pairs)
  "Generalised assignment macro (NeLisp minimal).
Each pair PLACE VAL assigns VAL to PLACE.  Supported PLACE shapes:
  - SYMBOL                 → `(setq SYMBOL VAL)'
  - (car X)  / (cdr X)     → `(setcar X VAL)' / `(setcdr X VAL)'
  - (aref V I) / (nth I L) → `(aset V I VAL)' / `(setcar (nthcdr I L) VAL)'
  - (get S P)              → `(put S P VAL)'
  - (gethash K H)          → `(puthash K VAL H)'
  - (alist-get K A)        → assq-update or prepend `(K . VAL)'
  - (ACCESSOR REC)         where ACCESSOR is a registered cl-defstruct
                            slot accessor → `(nelisp--record-set REC I VAL)'
  - registered simple / struct setter → calls the setter
  - a MACRO place          → macroexpand and re-dispatch
Other shapes signal a host `error' at expand time (see `nelisp--setf-1')."
  (when (null pairs) (signal 'error (list "setf: empty body")))
  (let ((forms nil))
    (while pairs
      (push (nelisp--setf-1 (car pairs) (cadr pairs)) forms)
      (setq pairs (cdr (cdr pairs))))
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
;;   (quote DATUM)      literal 等価 (`equal' 比較 -- symbol/number/string/
;;                      list/vector 問わず構造比較。`eq' だと quoted list
;;                      等の compound datum が freshly-consed な runtime
;;                      値と一致しない)
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
        ;; `equal', not `eq': a `(quote DATUM)' pattern (e.g. the
        ;; literal-list clause selector `'(t t)' in vendor cond-let.el's
        ;; `cond-let--prepare-clauses') must match any value that is
        ;; STRUCTURALLY the same, not merely the same object.  `eq'
        ;; happens to work for the common case of a quoted symbol
        ;; (interned, so `eq'-comparable) but silently never matches a
        ;; quoted compound datum (list/vector/string) compared against a
        ;; freshly-consed runtime value of the same shape -- `(eq (list
        ;; t t) '(t t))' is nil in both this reader and real Emacs.  That
        ;; silent non-match let a later, structurally-overlapping
        ;; backquote-pattern clause (e.g. `` `(t ,_) '') win instead,
        ;; selecting the wrong helper macro out of a `pcase' dispatch
        ;; that assumed exact-match precedence -- root cause of the
        ;; nelisp-emacs-lib Doc 33 item 239 `cond-let*' repro
        ;; `(cond-let* ([x 1] [x (+ x 1)] x) (t 99))' => `void-variable:
        ;; x' (the wrongly-selected non-sequential `cond-let--when-let'
        ;; expands a `(+ x 1)' binding form that runs before `x' is
        ;; bound; the correctly-selected `cond-let--when-let*' does not).
        (cons (list 'equal value-form (list 'quote (car rest))) nil))
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
       ;; The reader gives a backquote pattern the head `\=`', not
       ;; `backquote', so testing only for the latter meant NO backquote
       ;; pattern was ever recognised -- and the fallback below matched
       ;; everything, so the first backquote clause in any `pcase' won
       ;; regardless of the value.  There are 47 of them in this tree.
       ;; The always-match fallback hid it completely; making that fallback
       ;; signal is what brought it out.
       ((if (eq head 'backquote) 1 (eq head '\`))
        (nelisp-pcase--backquote (car rest) value-form))
       ((eq head 'app)
        ;; (app FUN PAT): apply FUN to the value, match PAT on the result.
        (nelisp-pcase--test (car (cdr rest))
                            (list 'funcall (list 'function (car rest))
                                  value-form)))
       ;; An unrecognised pattern head used to build the test `t', so it
       ;; matched EVERYTHING: `(pcase 5 ((app 1+ 7) (quote seven))
       ;; (_ (quote other)))' answered seven where Emacs answers other, and
       ;; a made-up head matched just as happily.  A dispatch construct
       ;; quietly taking the wrong branch is about the worst failure a
       ;; dispatch construct has, and the branch body can do anything.
       ;;
       ;; Signalling instead, which is what Emacs does for a head it does
       ;; not know.  For a head Emacs DOES know and this does not, Emacs
       ;; would evaluate it and this stops -- a deviation, but a named and
       ;; loud one that says which pattern is missing, rather than a silent
       ;; wrong answer.  Measured before changing it: across scripts/,
       ;; lisp/ and src/ the only pattern heads in use are quote (245),
       ;; backquote (47) and or (32), all handled, so nothing in the tree
       ;; relies on the old always-match.
       ;; (cl-type TYPE) -- kept in sync with lisp/nelisp-pcase.el's copy.
       ;; Built into the engine rather than registered through a
       ;; `pcase-macroexpander' property, because this prelude loads before
       ;; `get'/`put' exist.  Real Emacs registers it in cl-macs.el.
       ((eq head 'cl-type)
        (nelisp-pcase--test
         (list 'pred (list 'lambda (list 'v)
                           (list 'cl-typep 'v
                                 (list 'quote (car rest)))))
         value-form))
       (t (error "Unknown %s pattern: %S" head pattern)))))
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
    ;; The test is wrapped in the bindings collected so far, because a
    ;; later sub-pattern may READ an earlier one: `(and n (guard (> n 3)))'
    ;; binds n and then tests it.  `pcase' puts bindings around the clause
    ;; BODY only, so without this the guard ran with n unbound and every
    ;; such clause answered void-variable rather than matching or not.
    (let ((joined (cons 'and (let ((rev nil))
                               (while tests
                                 (setq rev (cons (car tests) rev))
                                 (setq tests (cdr tests)))
                               rev))))
      (cons (if bindings (list 'let bindings joined) joined)
            bindings))))

;; Bindings inside an `or' arm are dropped, so a clause that relies on one
;; answers void-variable rather than matching.  Emacs compiles each arm to
;; its own branch and binds from whichever matched; doing that here means
;; either re-evaluating the arm tests to pick the branch, or restructuring
;; the (TEST . BINDINGS) protocol these builders share.  Left alone on
;; measurement rather than on guess: across scripts/, lisp/ and src/ no
;; `or' arm binds anything -- all 32 uses are quoted-symbol or literal
;; alternatives -- so nothing in the tree needs it today.
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
  ;; The reader spells these `\=,' and `\=,@', not `comma' and `comma-at'.
  ;; Matching only the long names meant no unquote was ever recognised, so
  ;; `(,a ,b)' fell through to the literal arm and compared the VALUE
  ;; against the pattern `((\=, a) (\=, b))' -- which never matches anything.
  ;; Together with the head being `\=`' rather than `backquote', that made
  ;; the whole backquote pattern family dead, and the dispatcher's
  ;; always-match fallback hid it: every backquote clause matched, so the
  ;; first one in a `pcase' won whatever the value was.  Both spellings are
  ;; accepted now.
  (cond
   ((and (consp pat) (if (eq (car pat) 'comma) 1 (eq (car pat) '\,)))
    (let ((sym (car (cdr pat))))
      (cond
       ((eq sym '_) (cons t nil))
       ((symbolp sym) (cons t (list (list sym value-form))))
       (t (nelisp-pcase--test sym value-form)))))
   ((and (consp pat) (if (eq (car pat) 'comma-at) 1 (eq (car pat) '\,@)))
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

(defun nelisp-pcase--distribute-or (cases)
  "Split every clause whose pattern is a top-level `or' into one per arm."
  (let ((out nil))
    (dolist (c cases)
      (let ((pat (car c)))
        (if (and (consp pat) (eq (car pat) 'or) (cdr pat))
            (dolist (arm (cdr pat))
              (setq out (cons (cons arm (cdr c)) out)))
          (setq out (cons c out)))))
    (let ((rev nil))
      (while out (setq rev (cons (car out) rev)) (setq out (cdr out)))
      rev)))

(defmacro pcase (expr &rest cases)
  "Dispatch EXPR through CASES.
See `nelisp-pcase--test' for supported pattern shapes.

Rust-min migration (= moved out of build-tool/src/eval/special_forms.rs)."
  (let ((value-sym (make-symbol "--pcase-value--"))
        (cond-clauses nil))
    ;; A top-level `or' is distributed over the clause list first:
    ;;   ((or P1 P2) BODY)  ->  (P1 BODY) (P2 BODY)
    ;; `nelisp-pcase--or' builds ONE test for all the arms and drops their
    ;; bindings, because the (TEST . BINDINGS) protocol has no way to say
    ;; "these bindings only if THAT arm matched" -- so (pcase 5 ((or (and
    ;; (pred integerp) n) n) n)) reached its body with `n' unbound.  Giving
    ;; each arm its own clause is what makes the binding belong to the arm
    ;; that matched, and it costs a copy of the body.  An `or' nested inside
    ;; another pattern still goes through the binding-less builder; that case
    ;; fails loudly with `void-variable' rather than answering wrongly.
    (setq cases (nelisp-pcase--distribute-or cases))
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
(defun boundp (sym)
    (nelisp--check-symbol sym)
    ;; The self-evaluating symbols are bound to themselves, so `boundp'
    ;; answers t for them -- looking them up in the global table said nil.
  (if (or (null sym) (eq sym t) (keywordp sym))
      t
    (nelisp--env-globals-is-bound sym)))
(defun set (sym val)
    ;; t, nil and every keyword are self-evaluating in Emacs and cannot be
    ;; assigned; `symbolp' was the wrong complaint (they ARE symbols) and a
    ;; keyword was assigned outright.
    (when (or (eq sym t) (null sym) (keywordp sym))
      (signal 'setting-constant (list sym)))
    (nelisp--check-symbol sym)
  (nelisp--env-globals-set-value sym val)
  val)
(unless (fboundp 'defalias)
  (defun defalias (sym def &rest _)
    (when (and (null sym) def) (signal 'setting-constant (list sym)))
    ;; A symbol aliased to ITSELF would loop at call time; Emacs refuses it
    ;; up front, and storing it left a definition nothing could call.
    (when (and (symbolp def) def (eq def sym))
      (signal 'cyclic-function-indirection (list sym)))
    (nelisp--check-symbol sym)
    (if (and (symbolp def) (not (fboundp def)))
        (eval (list 'defun sym '(&rest args)
                    (list 'apply (list 'quote def) 'args)))
      (fset sym def))
    sym))
(unless (fboundp 'fmakunbound)
  (defun fmakunbound (sym)
    (nelisp--check-symbol sym)
    ;; nil and t are constants: Emacs refuses to unbind them, and says so
    ;; with `setting-constant' rather than a type error.
    (when (memq sym '(nil t)) (signal 'setting-constant (list sym)))
    (fset sym nil) sym))
(unless (fboundp 'functionp)
  (defun functionp (x)
    ;; nil and t are symbols but can never be fbound, and `fboundp' signals
    ;; for a non-symbol now -- so ask about the shape before asking it.
    (or (and (consp x) (eq (car x) 'lambda))
        (and (symbolp x) (not (memq x '(nil t))) (fboundp x)))))
(unless (fboundp 'recordp) (defun recordp (x) nil))
(unless (fboundp 'nlistp) (defun nlistp (x) (not (listp x))))
;; `eql' compares numbers of the SAME TYPE: (eql 0.0 0) is nil, where `='
;; says t.  Routing both through `=' made a float and an integer identical,
;; which is the one thing `eql' exists to distinguish.
(unless (fboundp 'eql)
  (defun eql (a b)
    (cond
     ((and (floatp a) (floatp b)) (= a b))
     ((and (integerp a) (integerp b)) (= a b))
     ((or (numberp a) (numberp b)) nil)
     (t (eq a b)))))
(unless (fboundp 'encode-coding-string)
  (defun encode-coding-string (str coding &optional _nocopy)
    (nelisp--check-string str)
    (nelisp--check-symbol coding)
    ;; `utf-8' and `latin-1' both answer the string unchanged (every string
    ;; is already UTF-8 bytes here); an UNKNOWN coding system is a
    ;; `coding-system-error', the condition Emacs signals.
    (when (and coding (not (memq coding '(utf-8 latin-1 binary no-conversion
                                          us-ascii undecided prefer-utf-8))))
      (signal 'coding-system-error (list coding)))
    (when nil
      (signal 'error
              (list (format "encode-coding-string stub: only utf-8 supported, got %S"
                            coding))))
    str))
(unless (fboundp 'decode-coding-string)
  (defun decode-coding-string (str coding &optional _nocopy)
    (nelisp--check-string str)
    (nelisp--check-symbol coding)
    ;; `utf-8' and `latin-1' both answer the string unchanged (every string
    ;; is already UTF-8 bytes here); an UNKNOWN coding system is a
    ;; `coding-system-error', the condition Emacs signals.
    (when (and coding (not (memq coding '(utf-8 latin-1 binary no-conversion
                                          us-ascii undecided prefer-utf-8))))
      (signal 'coding-system-error (list coding)))
    (when nil
      (signal 'error
              (list (format "decode-coding-string stub: only utf-8 supported, got %S"
                            coding))))
    str))
;; `bufferp' used to be a permanent, unconditional `nil' here (Doc 188
;; §1.4 -- "no Sexp is a buffer" was true before this file had a buffer
;; object).  The real definition lives in the Doc 188 P1 buffer section
;; below (search for "Doc 188 P1"), for the same reason `point-min'/
;; `point-max' moved: `unless (fboundp ...)' has to guard the ONE
;; definition that actually runs, not an earlier stub that already
;; claimed the name.
(unless (fboundp 'set-buffer-multibyte)
  (defun set-buffer-multibyte (flag)
    "Answer FLAG, as Emacs does; there is no buffer to change here."
    flag))
;; Doc 200 string representation primitives are native standalone builtins.
;; Do not install Elisp fallbacks for them here: a fallback binding shadows
;; the native apply dispatch even though `fboundp' cannot see that dispatch.
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
           (rc (wrf filename bytes))
           ;; `wrf' reports bytes written; `length' counts characters.  The
           ;; two agree only on ASCII, so this compared a short read against
           ;; a correct one and signalled on every write that carried a
           ;; multibyte character -- with the file on disk already right.
           ;; The compiler writes its own object file through here, which is
           ;; how an ASCII source still lost native compilation.
           (expected (string-bytes bytes)))
      (unless (= rc expected)
        (signal 'error
                (list (format "write-region stub: wrf returned %S (expected %S bytes) path=%s"
                              rc expected filename)))))
	    nil))
;; ---------------------------------------------------------------------
;; Doc 188 P1 -- buffer object model (ported from src/nelisp-buffer.el).
;;
;; The standard names below (`insert'/`buffer-string'/`with-current-
;; buffer'/`generate-new-buffer'/`kill-buffer'/`point'/`goto-char'/
;; `point-min'/`point-max'/`bufferp') used to operate on an ad-hoc
;; 4-slot vector `(vector 'buffer NAME CONTENT LIVE-P)' that `insert'
;; never actually wrote to -- it wrote a DIFFERENT, buffer-unconnected
;; variable (the old `nelisp--with-temp-file-contents', removed below),
;; so `buffer-string' always read back whatever `generate-new-buffer'
;; had seeded (""), no matter what had been `insert'ed (Doc 188 §1.3,
;; confirmed live this session: `(let ((b (generate-new-buffer
;; "probe"))) (with-current-buffer b (insert "abc")) (aref b 2))'
;; answered "" -- not "abc").  `point'/`goto-char' were void-function,
;; and `point-min'/`point-max' were hardcoded to 1 regardless of buffer
;; content (moved from earlier in this file to below; see the comments
;; left in their place).
;;
;; This section replaces that vector with `src/nelisp-buffer.el''s real,
;; already-tested (host-Emacs ERT: test/nelisp-buffer-test.el,
;; test/nelisp-marker-test.el, test/nelisp-editor-test.el) 1-indexed
;; gap-buffer/marker/overlay/text-property model -- the Layer-1
;; substrate of record per Doc 33 §12's 2026-08-22 amendment (owner-
;; approved; Doc 188 §2.1/§6.1).  The struct/function bodies immediately
;; below are ported verbatim from that file under the same `nelisp-'-
;; prefixed names (so that file's own host-Emacs test suite and its
;; `nelisp-with-buffer'/`nelisp-current-buffer'/`nelisp-set-buffer'
;; ambient-buffer API keep working, unmodified, wherever it is loaded);
;; see that file for the authoritative source and full documentation.
;;
;; Only the STANDARD Emacs names after the port are new here.  Each
;; stays `unless (fboundp ...)'-guarded per this file's own convention,
;; and each threads this file's own `nelisp--current-buffer' variable
;; through to the ported functions explicitly, rather than relying on
;; the ported file's own ambient `nelisp-buffer--current' -- so the two
;; "current buffer" trackers never need to agree for the standard-name
;; path to work.
;;
;; Doc 188 P1 ships the object model plus `insert'/`point'/`goto-char'/
;; `point-min'/`point-max'/`buffer-string' (its own stated exit bar) and
;; the surrounding lifecycle names needed to make that a COHERENT set:
;; `generate-new-buffer'/`get-buffer'/`buffer-live-p'/`kill-buffer'/
;; `with-current-buffer'/`with-temp-buffer'/`bufferp'/`insert-file-
;; contents'.  It deliberately does NOT wire `current-buffer'/`set-
;; buffer'/`buffer-substring'/`erase-buffer' (Doc 188 §2.2/§3 P2), nor
;; any marker/overlay/text-property/search standard name (P3-P5) -- the
;; struct's marker/overlay/text-property slots and the ported file's own
;; internal shift helpers ARE present (`nelisp-insert'/`nelisp-delete-
;; region' call them unconditionally, so they could not be left out of
;; the port), but nothing below exposes `set-marker'/`make-overlay'/etc.
;; under their standard Emacs names.

(require 'cl-lib)

(cl-defstruct (nelisp-buffer
               (:constructor nelisp-buffer--make)
               (:copier nil))
  name
  (before-gap "")
  (after-gap "")
  (modified nil)
  (markers nil)
  (overlays nil)
  (narrow-start nil)
  (narrow-end nil)
  (text-properties nil))       ; list of (START END PROP-PLIST) intervals

(cl-defstruct (nelisp-marker
               (:constructor nelisp-marker--make)
               (:copier nil))
  (buffer nil)
  (position 1)
  (insertion-type nil))

(cl-defstruct (nelisp-overlay
               (:constructor nelisp-overlay--make)
               (:copier nil))
  (buffer nil)
  (start 1)
  (end 1)
  (front-advance nil)
  (rear-advance nil)
  (props nil))

(defvar nelisp-buffer--registry
  (make-hash-table :test 'equal)
  "Name -> `nelisp-buffer' map.  Ported from src/nelisp-buffer.el.")

(defvar nelisp-buffer--current nil
  "The ported file's OWN ambient current buffer.  Unused by the
standard-name wrappers below (they thread `nelisp--current-buffer'
explicitly instead, see the section header comment above); kept so
`nelisp-with-buffer'/`nelisp-current-buffer'/`nelisp-set-buffer' -- the
`nelisp-'-prefixed API -- still work self-consistently on their own.")

(defun nelisp-buffer--reset-registry ()
  "Clear the NeLisp buffer registry.  Test hygiene only."
  (clrhash nelisp-buffer--registry)
  (setq nelisp-buffer--current nil))

(defun nelisp-generate-new-buffer (name)
  "Return a fresh `nelisp-buffer', uniquifying NAME via `<N>' suffix."
  (let* ((base name)
         (final name)
         (count 0))
    (while (gethash final nelisp-buffer--registry)
      (setq count (1+ count))
      (setq final (format "%s<%d>" base count)))
    (let ((buf (nelisp-buffer--make :name final)))
      (puthash final buf nelisp-buffer--registry)
      buf)))

(defun nelisp-get-buffer-create (name)
  "Return the buffer named NAME, creating it if absent."
  (or (gethash name nelisp-buffer--registry)
      (let ((buf (nelisp-buffer--make :name name)))
        (puthash name buf nelisp-buffer--registry)
        buf)))

(defun nelisp-get-buffer (name)
  "Return the buffer named NAME, or nil if absent."
  (gethash name nelisp-buffer--registry))

(defun nelisp-kill-buffer (buf)
  "Remove BUF from the registry.  Returns t on success."
  (let ((name (nelisp-buffer-name buf)))
    (when (gethash name nelisp-buffer--registry)
      (remhash name nelisp-buffer--registry)
      (when (eq nelisp-buffer--current buf)
        (setq nelisp-buffer--current nil))
      t)))

(defun nelisp-buffer-list ()
  "Return a list of live NeLisp buffers."
  (let (result)
    (maphash (lambda (_ buf) (push buf result))
             nelisp-buffer--registry)
    result))

(defun nelisp-current-buffer ()
  "Return the currently selected NeLisp buffer, or nil."
  nelisp-buffer--current)

(defun nelisp-set-buffer (buf)
  "Set BUF as the current NeLisp buffer.  Returns BUF."
  (setq nelisp-buffer--current buf)
  buf)

(defmacro nelisp-with-buffer (buf &rest body)
  "Evaluate BODY with BUF as the NeLisp current buffer."
  (declare (indent 1))
  `(let ((nelisp-buffer--current ,buf))
     ,@body))

(defun nelisp-buffer--ambient (buf-or-nil)
  "Resolve BUF-OR-NIL to an actual buffer (defaulting to current)."
  (or buf-or-nil nelisp-buffer--current
      (error "No NeLisp current buffer")))

(defun nelisp-buffer-size (&optional buf)
  "Return the length of BUF's visible (unrestricted) text."
  (let ((b (nelisp-buffer--ambient buf)))
    (+ (length (nelisp-buffer-before-gap b))
       (length (nelisp-buffer-after-gap b)))))

(defun nelisp-point (&optional buf)
  "Return the current point in BUF (1-based)."
  (1+ (length (nelisp-buffer-before-gap
               (nelisp-buffer--ambient buf)))))

(defun nelisp-point-min (&optional buf)
  "Return the narrowed point-min of BUF (defaults to 1)."
  (or (nelisp-buffer-narrow-start
       (nelisp-buffer--ambient buf))
      1))

(defun nelisp-point-max (&optional buf)
  "Return the narrowed point-max of BUF."
  (let ((b (nelisp-buffer--ambient buf)))
    (or (nelisp-buffer-narrow-end b)
        (1+ (nelisp-buffer-size b)))))

(defun nelisp-buffer-string (&optional buf)
  "Return the entire text of BUF as a new string."
  (let ((b (nelisp-buffer--ambient buf)))
    (concat (nelisp-buffer-before-gap b)
            (nelisp-buffer-after-gap b))))

(defun nelisp-buffer-substring (start end &optional buf)
  "Return the substring between 1-based START and END in BUF."
  (let ((b (nelisp-buffer--ambient buf)))
    (substring (nelisp-buffer-string b) (1- start) (1- end))))

(defun nelisp-char-after (&optional pos buf)
  "Return the character at POS (default point) in BUF, or nil."
  (let* ((b (nelisp-buffer--ambient buf))
         (p (or pos (nelisp-point b)))
         (total (nelisp-buffer-string b))
         (idx (1- p)))
    (and (>= idx 0) (< idx (length total))
         (elt total idx))))

(defun nelisp-buffer--shift-markers-on-insert (buf at inserted-len)
  "Advance markers at or past AT by INSERTED-LEN."
  (dolist (m (nelisp-buffer-markers buf))
    (when (nelisp-marker-p m)
      (let ((pos (nelisp-marker-position m)))
        (cond
         ((< pos at) nil)
         ((and (= pos at) (null (nelisp-marker-insertion-type m))) nil)
         (t (setf (nelisp-marker-position m) (+ pos inserted-len))))))))

(defun nelisp-buffer--shift-markers-on-delete (buf start end)
  "Collapse markers inside [START, END] to START, shift markers past END."
  (let ((delta (- end start)))
    (dolist (m (nelisp-buffer-markers buf))
      (when (nelisp-marker-p m)
        (let ((pos (nelisp-marker-position m)))
          (cond
           ((<= pos start) nil)
           ((>= pos end)
            (setf (nelisp-marker-position m) (- pos delta)))
           (t
            (setf (nelisp-marker-position m) start))))))))

(defun nelisp-buffer--shift-overlays-on-insert (buf at inserted-len)
  "Update overlay endpoints when INSERTED-LEN chars land at AT."
  (dolist (o (nelisp-buffer-overlays buf))
    (when (nelisp-overlay-p o)
      (let ((s (nelisp-overlay-start o))
            (e (nelisp-overlay-end o)))
        (cond
         ((< s at) nil)
         ((and (= s at) (null (nelisp-overlay-front-advance o))) nil)
         (t (setf (nelisp-overlay-start o) (+ s inserted-len))))
        (cond
         ((< e at) nil)
         ((and (= e at) (null (nelisp-overlay-rear-advance o))) nil)
         (t (setf (nelisp-overlay-end o) (+ e inserted-len))))))))

(defun nelisp-buffer--shift-overlays-on-delete (buf start end)
  "Collapse overlay endpoints falling in [START, END] to START; shift
endpoints past END backwards by (END - START)."
  (let ((delta (- end start)))
    (dolist (o (nelisp-buffer-overlays buf))
      (when (nelisp-overlay-p o)
        (let ((s (nelisp-overlay-start o))
              (e (nelisp-overlay-end o)))
          (setf (nelisp-overlay-start o)
                (cond
                 ((<= s start) s)
                 ((>= s end) (- s delta))
                 (t start)))
          (setf (nelisp-overlay-end o)
                (cond
                 ((<= e start) e)
                 ((>= e end) (- e delta))
                 (t start))))))))

(defun nelisp-buffer--shift-text-properties-on-insert (buf at len)
  "Expand text-property intervals straddling AT; shift those past AT."
  (dolist (ival (nelisp-buffer-text-properties buf))
    (let ((s (nth 0 ival))
          (e (nth 1 ival)))
      (cond ((< s at) nil)
            (t (setcar ival (+ s len))))
      (cond ((< e at) nil)
            ((= e at) nil)
            (t (setcar (cdr ival) (+ e len)))))))

(defun nelisp-buffer--shift-text-properties-on-delete (buf start end)
  "Collapse text-property intervals within [START, END] and shift later
ones."
  (let ((delta (- end start)))
    (dolist (ival (nelisp-buffer-text-properties buf))
      (let ((s (nth 0 ival))
            (e (nth 1 ival)))
        (setcar ival
                (cond
                 ((<= s start) s)
                 ((>= s end) (- s delta))
                 (t start)))
        (setcar (cdr ival)
                (cond
                 ((<= e start) e)
                 ((>= e end) (- e delta))
                 (t start)))))))

(defun nelisp-goto-char (pos &optional buf)
  "Move point to POS in BUF, rebalancing the gap.
POS is clamped into [point-min, point-max] per Emacs semantics."
  (let* ((b (nelisp-buffer--ambient buf))
         (total (nelisp-buffer-string b))
         (lo (nelisp-point-min b))
         (hi (nelisp-point-max b))
         (clamped (max lo (min hi pos)))
         (idx (1- clamped)))
    (setf (nelisp-buffer-before-gap b) (substring total 0 idx))
    (setf (nelisp-buffer-after-gap b) (substring total idx))
    clamped))

(defun nelisp-insert (text &optional buf)
  "Insert TEXT at point in BUF.  TEXT must be a string."
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (let* ((b (nelisp-buffer--ambient buf))
         (before (nelisp-buffer-before-gap b))
         (at (1+ (length before)))
         (n (length text)))
    (setf (nelisp-buffer-before-gap b) (concat before text))
    (setf (nelisp-buffer-modified b) t)
    (nelisp-buffer--shift-markers-on-insert b at n)
    (nelisp-buffer--shift-overlays-on-insert b at n)
    (nelisp-buffer--shift-text-properties-on-insert b at n))
  nil)

(defun nelisp-delete-region (start end &optional buf)
  "Delete the text between 1-based START and END (exclusive) in BUF."
  (let* ((b (nelisp-buffer--ambient buf))
         (size (nelisp-buffer-size b))
         (lo 1)
         (hi (1+ size))
         (s (min start end))
         (e (max start end)))
    (when (or (< s lo) (> e hi))
      (signal 'args-out-of-range (list start end)))
    (let* ((total (nelisp-buffer-string b))
           (si (1- s))
           (ei (1- e)))
      (setf (nelisp-buffer-before-gap b) (substring total 0 si))
      (setf (nelisp-buffer-after-gap b) (substring total ei))
      (setf (nelisp-buffer-modified b) t)
      (nelisp-buffer--shift-markers-on-delete b s e)
      (nelisp-buffer--shift-overlays-on-delete b s e)
      (nelisp-buffer--shift-text-properties-on-delete b s e)))
  nil)

(defun nelisp-erase-buffer (&optional buf)
  "Clear BUF entirely.  Markers / overlays collapse to `point-min'."
  (let ((b (nelisp-buffer--ambient buf)))
    (setf (nelisp-buffer-before-gap b) "")
    (setf (nelisp-buffer-after-gap b) "")
    (setf (nelisp-buffer-modified b) t)
    (dolist (m (nelisp-buffer-markers b))
      (when (nelisp-marker-p m)
        (setf (nelisp-marker-position m) 1)))
    (dolist (o (nelisp-buffer-overlays b))
      (when (nelisp-overlay-p o)
        (setf (nelisp-overlay-start o) 1)
        (setf (nelisp-overlay-end o) 1)))
    (setf (nelisp-buffer-text-properties b) nil))
  nil)

(defun nelisp-buffer-modified-p (&optional buf)
  "Return non-nil if BUF has been modified since creation/last reset."
  (nelisp-buffer-modified (nelisp-buffer--ambient buf)))

(defun nelisp-buffer-set-modified (flag &optional buf)
  "Set BUF's modified flag to FLAG (t/nil)."
  (setf (nelisp-buffer-modified (nelisp-buffer--ambient buf))
        (and flag t))
  flag)

(defun nelisp-markerp (obj)
  "Return non-nil when OBJ is a `nelisp-marker'."
  (nelisp-marker-p obj))

(defun nelisp-make-marker ()
  "Return a marker not yet attached to any buffer."
  (nelisp-marker--make))

(defun nelisp-copy-marker (buf pos &optional insertion-type)
  "Return a fresh marker inside BUF at POS."
  (let ((m (nelisp-marker--make :buffer buf
                                :position pos
                                :insertion-type insertion-type)))
    (push m (nelisp-buffer-markers buf))
    m))

(defun nelisp-set-marker (marker pos &optional buf)
  "Re-point MARKER at POS, optionally moving it to BUF."
  (cond
   ((null pos)
    (when-let* ((old (nelisp-marker-buffer marker)))
      (setf (nelisp-buffer-markers old)
            (delq marker (nelisp-buffer-markers old))))
    (setf (nelisp-marker-buffer marker) nil)
    (setf (nelisp-marker-position marker) 1))
   (t
    (let ((target (or buf (nelisp-marker-buffer marker))))
      (unless target (error "nelisp-set-marker: no target buffer"))
      (when (and (nelisp-marker-buffer marker)
                 (not (eq target (nelisp-marker-buffer marker))))
        (setf (nelisp-buffer-markers (nelisp-marker-buffer marker))
              (delq marker (nelisp-buffer-markers
                            (nelisp-marker-buffer marker))))
        (push marker (nelisp-buffer-markers target)))
      (unless (nelisp-marker-buffer marker)
        (push marker (nelisp-buffer-markers target)))
      (setf (nelisp-marker-buffer marker) target)
      (setf (nelisp-marker-position marker) pos))))
  marker)

(defun nelisp-marker-delete (marker)
  "Unlink MARKER from its buffer's marker list.  Returns nil."
  (when-let* ((b (nelisp-marker-buffer marker)))
    (setf (nelisp-buffer-markers b)
          (delq marker (nelisp-buffer-markers b))))
  (setf (nelisp-marker-buffer marker) nil)
  nil)

(defun nelisp-overlayp (obj)
  "Return non-nil when OBJ is a `nelisp-overlay'."
  (nelisp-overlay-p obj))

(defun nelisp-make-overlay (start end &optional buf
                                  front-advance rear-advance)
  "Create an overlay covering [START, END) in BUF."
  (let* ((b (nelisp-buffer--ambient buf))
         (o (nelisp-overlay--make :buffer b :start start :end end
                                  :front-advance front-advance
                                  :rear-advance rear-advance)))
    (push o (nelisp-buffer-overlays b))
    o))

(defun nelisp-delete-overlay (o)
  "Unlink O from its buffer's overlay list.  Returns nil."
  (when-let* ((b (nelisp-overlay-buffer o)))
    (setf (nelisp-buffer-overlays b)
          (delq o (nelisp-buffer-overlays b))))
  (setf (nelisp-overlay-buffer o) nil)
  nil)

(defun nelisp-overlay-put (o prop val)
  "Store (PROP . VAL) on overlay O.  Returns VAL."
  (let ((cell (assq prop (nelisp-overlay-props o))))
    (if cell
        (setcdr cell val)
      (push (cons prop val) (nelisp-overlay-props o))))
  val)

(defun nelisp-overlay-get (o prop)
  "Return the value of PROP stored on O, or nil."
  (cdr (assq prop (nelisp-overlay-props o))))

(defun nelisp-overlays-at (pos &optional buf)
  "Return the list of overlays in BUF whose range covers POS."
  (let ((b (nelisp-buffer--ambient buf))
        result)
    (dolist (o (nelisp-buffer-overlays b))
      (when (nelisp-overlayp o)
        (when (and (>= pos (nelisp-overlay-start o))
                   (< pos (nelisp-overlay-end o)))
          (push o result))))
    (nreverse result)))

(defun nelisp-overlays-in (start end &optional buf)
  "Return the list of overlays in BUF overlapping [START, END)."
  (let ((b (nelisp-buffer--ambient buf))
        result)
    (dolist (o (nelisp-buffer-overlays b))
      (when (nelisp-overlayp o)
        (let ((s (nelisp-overlay-start o))
              (e (nelisp-overlay-end o)))
          (when (and (< s end) (> e start))
            (push o result)))))
    (nreverse result)))

(defun nelisp-put-text-property (start end prop val &optional buf)
  "Store PROP=VAL for text in [START, END) of BUF."
  (let ((b (nelisp-buffer--ambient buf)))
    (push (list start end (list prop val))
          (nelisp-buffer-text-properties b))
    val))

(defun nelisp-get-text-property (pos prop &optional buf)
  "Return the value of PROP at POS in BUF, or nil."
  (let ((b (nelisp-buffer--ambient buf))
        (hit nil))
    (dolist (ival (nelisp-buffer-text-properties b))
      (unless hit
        (let ((s (nth 0 ival))
              (e (nth 1 ival))
              (pl (nth 2 ival)))
          (when (and (>= pos s) (< pos e)
                     (plist-member pl prop))
            (setq hit (cons :v (plist-get pl prop)))))))
    (and hit (cdr hit))))

(defun nelisp-text-property-intervals (&optional buf)
  "Return a shallow copy of BUF's text-property interval list."
  (copy-sequence
   (nelisp-buffer-text-properties (nelisp-buffer--ambient buf))))

(defun nelisp-remove-text-properties (start end props &optional buf)
  "Drop each key in PROPS from any interval overlapping [START, END)."
  (let ((b (nelisp-buffer--ambient buf)))
    (dolist (ival (nelisp-buffer-text-properties b))
      (let ((s (nth 0 ival))
            (e (nth 1 ival)))
        (when (and (< s end) (> e start))
          (let* ((pl (nth 2 ival))
                 (new (let (out)
                        (while pl
                          (unless (memq (car pl) props)
                            (push (car pl) out)
                            (push (cadr pl) out))
                          (setq pl (cddr pl)))
                        (nreverse out))))
            (setcar (cddr ival) new))))))
  nil)

(defun nelisp-narrow-to-region (start end &optional buf)
  "Restrict visible range of BUF to [START, END]."
  (let* ((b (nelisp-buffer--ambient buf))
         (size (nelisp-buffer-size b))
         (lo 1)
         (hi (1+ size))
         (s (max lo (min hi (min start end))))
         (e (max lo (min hi (max start end)))))
    (setf (nelisp-buffer-narrow-start b) s)
    (setf (nelisp-buffer-narrow-end b) e)
    nil))

(defun nelisp-widen (&optional buf)
  "Remove the narrowing of BUF."
  (let ((b (nelisp-buffer--ambient buf)))
    (setf (nelisp-buffer-narrow-start b) nil)
    (setf (nelisp-buffer-narrow-end b) nil))
  nil)

(provide 'nelisp-buffer)

;; ---- standard-name wiring onto the model above (Doc 188 §2.2) --------

(defvar nelisp--current-buffer nil
  "The ambient current buffer for the STANDARD names below (`point',
`insert', `buffer-string', ...).  Seeded to a real `*scratch*' buffer
further down this file (search for \"Doc 188 P1 scratch/Messages
seed\"), matching Emacs (there is always a current buffer) and this
file's own earlier, never-honored intent -- the old ad-hoc vector
implementation seeded a `*scratch*'/`*Messages*' alist entry but never
actually pointed this variable at either of them (Doc 188 §1.3).  The
seed call itself has to come after `nelisp--make-record' is defined
-- much further down -- since `nelisp-generate-new-buffer' allocates a
`cl-defstruct' record and a first attempt placed the seed call here,
immediately after the model, and got a live `void-function:
(nelisp--make-record)' on every single invocation of the binary
(caught by the same probe workflow this doc's own §1.3 used, before
this file was committed).")
(defvar nelisp--pending-processes nil)
(defvar nelisp--process-props nil)
(defvar coding-system-for-write nil)
(defvar coding-system-for-read nil)
;; `system-type'/`system-configuration' used to be hardcoded Linux-x86_64
;; literals here regardless of what the binary was actually built for (a
;; Windows PE build's `system-type' answered `gnu/linux'; real-machine
;; finding, 2026-08-23).  `nelisp--target-os-code'/`nelisp--target-arch-code'
;; are per-target COMPILE-TIME constants baked directly into the binary --
;; see `nelisp-standalone--target-os-code-forms' in
;; scripts/nelisp-standalone-build.el -- so this now answers for the target
;; the binary was actually emitted for, on every platform, including ones
;; this repo's own build host cannot execute to check (Windows/macOS
;; cross-targets: source-inspected, not run).
(defvar system-type
  (let ((nelisp--os-code (nelisp--target-os-code)))
    (cond ((= nelisp--os-code 1) 'darwin)
          ((= nelisp--os-code 2) 'windows-nt)
          (t 'gnu/linux))))
;; Byte-identical to the packages/nelisp-process copy so `make ns-gate'
;; polices the two.  It has to be here as well: the standalone does not load
;; that package, so the variable was void.
;;
;; Bug (found chasing a v1.1.1 nelisp-skk-ime regression): this used to be a
;; fixed ":" regardless of target OS.  On windows-nt, `getenv "PATH"' answers
;; a real Windows path list ("C:\...;C:\...;..."), and `executable-find'
;; splits it on `path-separator'.  Splitting a drive-letter path on ":"
;; shreds every entry at its own drive-letter colon ("C:\Users\...;C" then
;; "\Program Files\...;C" and so on), so a 59-entry PATH became well over a
;; hundred garbage fragments, most starting with a bare "\..." that
;; `file-exists-p' still dutifully stats.  Each miss on this host costs on
;; the order of 100 ms, so `(executable-find "python")' alone -- one call
;; among the five vendor/ddskk makes at load time -- cost several seconds,
;; and the fix landed only after chasing what looked like a GC regression.
(unless (boundp 'path-separator)
  (defconst path-separator
    (if (and (boundp 'system-type) (eq system-type 'windows-nt)) ";" ":")))
;; Emacs 30 defines `path-separator' as a FUNCTION as well as a variable,
;; and `(path-separator)' answers ":".
(unless (fboundp 'path-separator)
  (defun path-separator () path-separator))
(defvar system-configuration
  (let ((nelisp--os-code (nelisp--target-os-code))
        (nelisp--arch-code (nelisp--target-arch-code)))
    (cond
     ((= nelisp--os-code 1) "aarch64-apple-darwin")
     ((= nelisp--os-code 2) (if (= nelisp--arch-code 1)
                                "aarch64-pc-windows-msvc"
                              "x86_64-pc-windows-msvc"))
     (t (if (= nelisp--arch-code 1)
            "aarch64-unknown-linux-gnu"
          "x86_64-pc-linux-gnu")))))

(unless (fboundp 'bufferp)
  (defun bufferp (obj) (nelisp-buffer-p obj)))
(unless (fboundp 'point-min)
  (defun point-min () (nelisp-point-min nelisp--current-buffer)))
(unless (fboundp 'point-max)
  (defun point-max () (nelisp-point-max nelisp--current-buffer)))
(unless (fboundp 'point)
  (defun point () (nelisp-point nelisp--current-buffer)))
(unless (fboundp 'goto-char)
  (defun goto-char (pos) (nelisp-goto-char pos nelisp--current-buffer)))
(unless (fboundp 'generate-new-buffer)
  (defun generate-new-buffer (name &optional _inhibit-buffer-hooks)
    (nelisp--check-string name)
    (when (= (length name) 0)
      (signal 'error (list "Empty string for buffer name is not allowed")))
    (nelisp-generate-new-buffer name)))
(unless (fboundp 'get-buffer)
  (defun get-buffer (buffer-or-name)
    (cond
     ((nelisp-buffer-p buffer-or-name) buffer-or-name)
     ((stringp buffer-or-name) (nelisp-get-buffer buffer-or-name))
     (t (signal 'wrong-type-argument (list 'stringp buffer-or-name))))))
(unless (fboundp 'buffer-live-p)
  (defun buffer-live-p (buffer)
    "T when BUFFER is still in the registry under its own name.
`nelisp-kill-buffer' only removes the registry entry (the struct itself
carries no separate liveness flag), so a stale reference is live iff
the registry still maps its name back to this exact object."
    (and (nelisp-buffer-p buffer)
         (eq buffer (nelisp-get-buffer (nelisp-buffer-name buffer))))))
(unless (fboundp 'kill-buffer)
  (defun kill-buffer (&optional buffer-or-name)
    "Kill BUFFER-OR-NAME, defaulting to the CURRENT buffer per the Emacs
contract -- the old stub answered t for a nil argument without killing
anything (Doc 188 §2.2)."
    (let ((b (if buffer-or-name (get-buffer buffer-or-name) nelisp--current-buffer)))
      (unless b (signal 'error (list "No buffer to kill")))
      (when (eq b nelisp--current-buffer) (setq nelisp--current-buffer nil))
      (or (nelisp-kill-buffer b) t))))
(unless (fboundp 'with-current-buffer)
  (defmacro with-current-buffer (buffer-or-name &rest body)
    (let ((b (make-symbol "buf")))
      `(let ((,b (get-buffer ,buffer-or-name)))
         (unless ,b (signal 'error (list (format "No such buffer: %S" ,buffer-or-name))))
         (let ((nelisp--current-buffer ,b))
           ,@body)))))
(unless (fboundp 'with-temp-buffer)
  (defmacro with-temp-buffer (&rest body)
    (let ((b (make-symbol "buf")))
      `(let* ((,b (nelisp-generate-new-buffer " *temp*"))
              (nelisp--current-buffer ,b))
         (unwind-protect
             (progn ,@body)
           (nelisp-kill-buffer ,b))))))
(when (fboundp 'rdf)
  ;; Keep the public compatibility name on `rdf' so callers get the same
  ;; value-returning read path used by the short builtin.
  (fset 'nelisp--syscall-read-file (symbol-function 'rdf)))
(unless (fboundp 'insert)
  (defun insert (&rest strings)
    (dolist (s strings)
      (nelisp-insert s nelisp--current-buffer))
    nil))
(unless (fboundp 'buffer-string)
  (defun buffer-string ()
    (nelisp-buffer-string nelisp--current-buffer)))

;; ---- Doc 188 P2: current-buffer / set-buffer / buffer-substring /
;; erase-buffer -- the four names §3's P2 bullet lists as still void
;; after P1 (`with-current-buffer'/`with-temp-buffer' shipped early,
;; as part of P1's own coherent lifecycle set; see the P1 record's
;; "Not wired" list).  Same threading discipline as `point'/`goto-
;; char' above: `nelisp--current-buffer' only, never the ported
;; file's own ambient `nelisp-buffer--current', so the two "current
;; buffer" trackers cannot desync for this standard-name path.

(unless (fboundp 'current-buffer)
  (defun current-buffer ()
    "Return the current NeLisp buffer object."
    nelisp--current-buffer))

(unless (fboundp 'set-buffer)
  (defun set-buffer (buffer-or-name)
    "Make BUFFER-OR-NAME current and return it.

Error wording probed against Emacs 30.1 (Doc 188 P2): a NAME that
resolves to no buffer signals `(error \"No buffer named %s\")'; a
buffer object that has since been killed signals `(error \"Selecting
deleted buffer\")' even though `get-buffer' happily returns a dead
buffer object unchanged.  Wrong-type input (`(set-buffer 42)') is
handled by `get-buffer' itself re-used below -- Emacs's own error
there, `(wrong-type-argument stringp 42)', already matches what
`get-buffer' already signals (Doc 188 P1)."
    (let ((b (get-buffer buffer-or-name)))
      (cond
       ((null b)
        (signal 'error (list (format "No buffer named %s" buffer-or-name))))
       ((not (buffer-live-p b))
        (signal 'error (list "Selecting deleted buffer")))
       (t
        (setq nelisp--current-buffer b)
        b)))))

(unless (fboundp 'buffer-substring)
  (defun buffer-substring (start end)
    "Return the text between START and END in the current buffer.

Real Emacs accepts START/END in EITHER order and signals
`args-out-of-range' (data = current buffer, START, END -- in that
original, unsorted order) outside [`point-min', `point-max'] -- both
probed against Emacs 30.1 (Doc 188 P2).  The underlying `nelisp-
buffer-substring' (ported verbatim from src/nelisp-buffer.el, kept
byte-identical for `ns-gate') assumes START <= END and does no range
check at all, so both real-Emacs behaviors are handled in this
wrapper instead of touching that file."
    (unless (integerp start)
      (signal 'wrong-type-argument (list 'integer-or-marker-p start)))
    (unless (integerp end)
      (signal 'wrong-type-argument (list 'integer-or-marker-p end)))
    (let* ((b nelisp--current-buffer)
           (lo (nelisp-point-min b))
           (hi (nelisp-point-max b))
           (s (min start end))
           (e (max start end)))
      (when (or (< s lo) (> e hi))
        (signal 'args-out-of-range (list b start end)))
      (nelisp-buffer-substring s e b))))

(unless (fboundp 'erase-buffer)
  (defun erase-buffer ()
    "Delete all text in the current buffer.  Returns nil (Emacs 30.1
probe, Doc 188 P2).  Real Emacs widens first; the ported `nelisp-
erase-buffer' does not reset narrowing -- but no standard name in
this tree can SET narrowing yet (`narrow-to-region'/`widen' are not
wired to standard names by any phase through P2), so that gap is
unreachable from the surface this phase builds, not silently papered
over."
    (nelisp-erase-buffer nelisp--current-buffer)))

(unless (fboundp 'with-temp-file)
  (defmacro with-temp-file (file &rest body)
    "Real buffer-backed `with-temp-file' (Doc 188 P1): BODY runs with a
fresh temp buffer current, same as `with-temp-buffer', then its
`buffer-string' is written to FILE.  Replaces the old dedicated
`nelisp--with-temp-file-contents' variable, which `insert' used to
write instead of ever touching a real buffer."
    (let ((b (make-symbol "buf")))
      `(let* ((,b (nelisp-generate-new-buffer " *temp-file*"))
              (nelisp--current-buffer ,b))
         (unwind-protect
             (progn ,@body
                    (write-region (nelisp-buffer-string ,b) nil ,file))
           (nelisp-kill-buffer ,b))))))
(unless (fboundp 'ignore-errors)
  (defmacro ignore-errors (&rest body)
    `(condition-case nil (progn ,@body) (error nil))))
(unless (fboundp 'insert-file-contents)
  (defun insert-file-contents (filename &rest _args)
    (let ((contents (or (nelisp--syscall-read-file filename) "")))
      (nelisp-insert contents nelisp--current-buffer)
      (list filename (length contents)))))
(unless (fboundp 'insert-file-contents-literally)
  (defun insert-file-contents-literally (filename &rest args)
    (nelisp--check-string filename)
    (unless (file-exists-p filename)
      ;; Emacs reports the ABSOLUTE name -- a relative one leaves the
      ;; reader guessing which directory the call was made from.
      (signal 'file-missing (list "Opening input file" "No such file or directory"
                                  (expand-file-name filename))))
    (apply #'insert-file-contents filename args)))
(unless (fboundp 'processp)
  (defun processp (process)
    (or (and (vectorp process) (> (length process) 0) (eq (aref process 0) 'process))
        ;; Doc 194 P0: a `network-process' is a THIRD, sibling tagged-vector
        ;; shape -- see the `process-get'/`process-put'/`process-status'/
        ;; `process-live-p' arms just below, each extended the same way.
        (and (vectorp process) (> (length process) 0) (eq (aref process 0) 'network-process))
        (and (fboundp 'nelisp-process-object-p)
             (nelisp-process-object-p process)))))
(unless (fboundp 'process-get)
  (defun process-get (process key)
    (unless (processp process) (signal 'wrong-type-argument (list 'processp process)))
    (cond
     ((and (vectorp process) (> (length process) 0) (eq (aref process 0) 'process))
      (cdr (assq key (aref process 4))))
     ;; [0]='network-process [1]=NAME [2]=STATUS [3]=FD [4]=PROPS-ALIST
     ;; (Doc 194 S3.1) -- same slot-4 props-alist convention as the
     ;; `process' fallback shape just above, read/written identically.
     ((and (vectorp process) (> (length process) 0) (eq (aref process 0) 'network-process))
      (cdr (assq key (aref process 4))))
     ((and (fboundp 'nelisp-process-object-p)
           (nelisp-process-object-p process))
      (cdr (assq key (cdr (assq process nelisp--process-props)))))
     (t nil))))
(unless (fboundp 'process-put)
  (defun process-put (process key value)
    (unless (processp process)
      (signal 'wrong-type-argument (list 'processp process)))
    (cond
     ((and (vectorp process) (> (length process) 0) (eq (aref process 0) 'process))
      (let ((cell (assq key (aref process 4))))
        (if cell
            (setcdr cell value)
          (aset process 4 (cons (cons key value) (aref process 4))))))
     ((and (vectorp process) (> (length process) 0) (eq (aref process 0) 'network-process))
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
    ;; nil means the CURRENT BUFFER, and a buffer with no process is an
    ;; error naming that buffer -- not a type complaint about nil, which is
    ;; a perfectly good argument here.
    (when (null process)
      (signal 'error
              (list (format "Buffer %s has no process"
                            ;; Doc 188 P1: the current buffer is now a
                            ;; `nelisp-buffer' struct, not the old
                            ;; `(vector 'buffer NAME ...)'.
                            (if (nelisp-buffer-p nelisp--current-buffer)
                                (nelisp-buffer-name nelisp--current-buffer)
                              "*scratch*")))))
    ;; A STRING is a process NAME and answers nil; anything else is
    ;; `processp'.  Measured -- the two look the same from a single failing
    ;; case, and the earlier reading here came from a string argument.
    (unless (or (stringp process) (processp process)
                (and (vectorp process) (> (length process) 0)
                     (eq (aref process 0) 'process)))
      (signal 'wrong-type-argument (list 'processp process)))
    (cond
     ((and (fboundp 'nelisp-process-object-p)
           (nelisp-process-object-p process))
      (nelisp--process-status-symbol (nelisp-process-status process)))
     ((and (vectorp process) (> (length process) 0)
           (eq (aref process 0) 'process))
      (aref process 2))
     ;; Doc 194 P0: `network-process' status is a symbol already matching
     ;; Emacs's own contract (measured against real Emacs 30.1, Doc 194
     ;; S1.3) -- `open'/`closed'/`failed'/`listen'/`connect', never
     ;; `run'/`exit' (the SUBPROCESS vocabulary) -- so this is a plain
     ;; slot read, no symbol translation needed.
     ((and (vectorp process) (> (length process) 0)
           (eq (aref process 0) 'network-process))
      (aref process 2))
     ;; Emacs answers NIL for anything else -- it accepts a buffer or a
     ;; process NAME too, so "not a process object" is not an error here.
     ;; Signalling made a caller probing whether a process was live get an
     ;; error instead of "no".
     (t nil))))
(unless (fboundp 'process-exit-status)
  (defun process-exit-status (process)
    (cond
     ((and (fboundp 'nelisp-process-object-p)
           (nelisp-process-object-p process))
      (nelisp-process-exit-status process))
     ((and (vectorp process) (> (length process) 0) (eq (aref process 0) 'process))
      (aref process 3))
     (t (signal 'wrong-type-argument (list 'processp process))))))
(unless (fboundp 'process-live-p)
  (defun process-live-p (process)
    ;; Answers nil for a non-process, unlike `process-status' beside it --
    ;; so it cannot simply delegate, which is what made it inherit the
    ;; signal.
    ;;
    ;; Doc 194 P0: real Emacs's own `process-live-p' documentation says
    ;; non-nil for STATUS run/open/listen/connect/stop -- not only `run'
    ;; (the subprocess-only vocabulary this used to hardcode).  A
    ;; `network-process' never reaches `run' at all (S1.3), so widening
    ;; this to the full set is required for it to ever report live, and is
    ;; a safe generalization for the existing `process'/native shapes
    ;; too: neither ever produces `open'/`listen'/`connect' today, so
    ;; this is not a behaviour change for them.
    (and (or (processp process)
             (and (vectorp process) (> (length process) 0)
                  (eq (aref process 0) 'process)))
         (and (memq (process-status process) '(run open listen connect)) t))))
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
  ;; PERF (Doc 201 §6.8): splitting PATH costs ~0.43s on a real 2.3KB,
  ;; 59-entry Windows one -- `nelisp--split-on-char' is a per-character walk
  ;; and this runtime charges ~0.2ms per basic operation (§3).  Nothing about
  ;; that walk depends on which program is being looked for, and
  ;; `vendor/ddskk/skk-vars.el' alone calls `executable-find' three times at
  ;; load, so it was paid three times for an identical answer.  The cache key
  ;; is the PATH string itself, so a `setenv' invalidates it by construction
  ;; -- there is no staleness window to reason about, and no hook to forget.
  (defvar nelisp--path-entries-key nil)
  (defvar nelisp--path-entries-value nil)
  (defun nelisp--path-entries (path sep)
    "Return PATH split on SEP, reusing the previous answer for the same PATH."
    (if (equal path nelisp--path-entries-key)
        nelisp--path-entries-value
      (setq nelisp--path-entries-key path)
      (setq nelisp--path-entries-value (nelisp--split-on-char path sep nil))
      nelisp--path-entries-value))
  (defun executable-find (command &optional _remote)
    (nelisp--check-string command)
    ;; "" is a string that names nothing, so the answer is nil -- not the
    ;; type complaint an emptiness check used to raise about a valid string.
    (if (= (length command) 0)
        nil
    (if (string-match-p "/" command)
        (and (file-exists-p command) command)
      (let* ((separator (if (boundp 'path-separator) path-separator ":"))
             ;; PERF (cold-start hand-off, 2026-08-30): `split-string' here
             ;; resolves to `nlre-split-string' (lisp/nelisp-stdlib-regexp.el),
             ;; which runs the SEPARATOR through the full regexp engine and
             ;; retries a match at every position from the current split
             ;; point -- ~59 nlre-string-match calls against a real ~2.3KB
             ;; PATH, each itself an O(remaining-length) position scan.
             ;; Measured directly: 3.2s for one PATH split vs 0.4s for the
             ;; plain byte-compare `nelisp--split-on-char' below on the same
             ;; input.  PATH's separator is always a single literal byte
             ;; (";" or ":"), never a regexp, so there is nothing here for
             ;; the regexp engine to buy.
             (dirs (nelisp--path-entries
                    (or (getenv "PATH") "/usr/local/bin:/usr/bin:/bin")
                    (aref separator 0)))
             found)
        ;; PERF (Doc 201 §6.8): the probe below is a plain `concat', not an
        ;; `expand-file-name'.  Expansion costs ~13ms here -- three
        ;; full-string scans plus a component walk, all in interpreted elisp
        ;; -- and this loop used to pay it once per PATH ENTRY, 59 times on
        ;; an ordinary Windows PATH, which was what was left of
        ;; `vendor/ddskk/skk-vars.el''s load time once §5.1/§5.2/§6.5 were
        ;; in.  `file-exists-p' does not care: it hands the name to the OS,
        ;; which resolves `..', doubled separators and (on windows-nt) mixed
        ;; `\' and `/' exactly as it would after expansion.  So the sweep
        ;; probes cheaply and expands ONCE, on the entry that answered yes,
        ;; and the value returned is the same expanded name as before.
        (while (and dirs (not found))
          (let* ((dir (if (equal (car dirs) "") "." (car dirs))))
            (when (file-exists-p (concat (file-name-as-directory dir) command))
              (setq found (expand-file-name command dir))))
          (setq dirs (cdr dirs)))
        found)))))
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
        (when (and (vectorp proc) (> (length proc) 0) (eq (aref proc 0) 'process))
          (aset proc 2 'exit)
          (aset proc 3 0))
        (let ((sentinel (process-get proc :sentinel)))
          (when sentinel
            (funcall sentinel proc "finished\n")))))
    t))
;; Doc 184 P3: hook `nl_repl_loop' (scripts/nelisp-standalone-build.el)
;; calls on every blank-line Enter in `--repl', so a REPL session can see
;; deferred timers/backgrounded process output between prompts without
;; the user calling `accept-process-output' by hand.  This baked-in
;; default is a NO-OP -- it costs nothing and changes nothing for a REPL
;; session that never loads the real event loop.  Loading
;; packages/nelisp-process-adapter/src/nelisp-process-adapter.el (Doc 184
;; P1-P3) unconditionally redefines this to a real bounded pump sharing
;; the exact same poll loop as its `accept-process-output'.  A
;; `--load'/`--eval' batch run never reaches `nl_repl_loop' at all, so
;; this hook existing or not is invisible there either way -- matching
;; Emacs's own batch-mode contract of not pumping outside an explicit
;; wait.
(unless (fboundp 'nelisp--repl-idle-pump)
  (defun nelisp--repl-idle-pump () nil))
(unless (fboundp 'set-file-modes)
  (defun set-file-modes (filename mode &optional _flag)
    "Apply MODE to FILENAME via chmod(2) when a syscall primitive exists.
No-ops on substrates without `nelisp--syscall-path-int' (the historic stub)."
    (unless (integerp mode) (signal 'wrong-type-argument (list 'fixnump mode)))
    (nelisp--check-string filename)
    (when (fboundp 'nelisp--syscall-path-int)
      (let ((rc (nelisp--syscall-path-int 90 (expand-file-name filename) mode)))
        (unless (= rc 0)
          (signal 'file-missing
                  (list "Doing chmod" "No such file or directory"
                        (expand-file-name filename))))))
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
    (defun sort (seq &optional pred)
      ;; A STRING is a sequence but not sortable: Emacs names
      ;; `list-or-vector-p', which says which two shapes it does take.
      (unless (or (listp seq) (vectorp seq))
        (signal 'wrong-type-argument (list 'list-or-vector-p seq)))
      (setq pred (or pred (function value<)))
      (if (vectorp seq)
          (let ((l (nelisp-stdlib--msort (append seq nil) pred)) (i 0))
            (while l (aset seq i (car l)) (setq l (cdr l)) (setq i (1+ i)))
            seq)
        (nelisp-stdlib--msort seq pred)))))

;; --- Wave-2 (C): symbol plists (put/get) + define-error -----------------
;; The standalone reader has no per-symbol plist slot, so model the global
;; symbol-plist store as one hash-table keyed by symbol (gethash/puthash use
;; symbol-eq on the name).  Each value is a property plist (NAME VAL NAME VAL...).
(unless (boundp 'nelisp-stdlib--symbol-plists)
  (setq nelisp-stdlib--symbol-plists (make-hash-table)))
(unless (fboundp 'symbol-plist)
  (defun symbol-plist (sym)
    (nelisp--check-symbol sym)
    (gethash sym nelisp-stdlib--symbol-plists)))
(unless (fboundp 'setplist)
  (defun setplist (sym plist)
    (nelisp--check-symbol sym)
    (puthash sym plist nelisp-stdlib--symbol-plists)
    plist))
(unless (fboundp 'get)
  (defun get (sym prop)
    (nelisp--check-symbol sym)
    (plist-get (gethash sym nelisp-stdlib--symbol-plists) prop)))
(unless (fboundp 'put)
  (defun put (sym prop val)
    (nelisp--check-symbol sym)
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
    (when (consp parent)
      (dolist (p parent)
        (unless (symbolp p) (signal 'wrong-type-argument (list 'symbolp p)))
        (unless (get p 'error-conditions)
          (signal 'error (list (format "Unknown signal ‘%s’" p))))))
    (let* ((parent (or parent 'error))
           (conditions
            (if (consp parent)
                (apply #'append
                       (mapcar (lambda (p) (get p 'error-conditions)) parent))
              (get parent 'error-conditions))))
      (put name 'error-conditions (cons name conditions))
      (put name 'error-message message)
      ;; Emacs answers what its LAST `put' returned, which is MESSAGE --
      ;; measured, not reasoned from the name.
      message)))
;; Seed the root `error' condition so derived errors inherit it.
(unless (get 'error 'error-conditions)
  (put 'error 'error-conditions (list 'error))
  (put 'error 'error-message "error"))
;; `error-message-string' can only name a condition it has a message for, and
;; only `error' itself had one -- so every builtin condition printed through
;; the raw-sexp fallback: "(wrong-type-argument listp 5)" where Emacs says
;; "Wrong type argument: listp, 5".  These are the texts Emacs carries,
;; verbatim, with the `file-error' members marked so their data is joined the
;; way Emacs joins it.
(dolist (row '((wrong-type-argument "Wrong type argument")
               (args-out-of-range "Args out of range")
               (void-variable "Symbol's value as variable is void")
               (void-function "Symbol's function definition is void")
               (invalid-function "Invalid function")
               (wrong-number-of-arguments "Wrong number of arguments")
               (wrong-length-argument "Wrong length argument")
               (arith-error "Arithmetic error")
               (range-error "Arithmetic range error")
               (overflow-error "Arithmetic overflow error")
               (setting-constant "Attempt to set a constant symbol")
               (no-catch "No catch for tag")
               (end-of-file "End of file during parsing")
               (invalid-regexp "Invalid regexp")
               (search-failed "Search failed")
               (circular-list "List contains a loop")
               (cl-assertion-failed "Assertion failed")
               (scan-error "Scan error")
               (quit "Quit")
               (user-error "")
               (coding-system-error "Invalid coding system")
               (charsetp "Invalid charset")))
  (put (car row) 'error-message (car (cdr row)))
  (unless (get (car row) 'error-conditions)
    (put (car row) 'error-conditions (list (car row) 'error))))
(dolist (row '((file-error "File error")
               (file-missing "File is missing")
               (file-already-exists "File already exists")
               (permission-denied "Cannot access file or directory")))
  (put (car row) 'error-message (car (cdr row)))
  (unless (get (car row) 'error-conditions)
    (put (car row) 'error-conditions (list (car row) 'file-error 'error))))
(defmacro cl-loop (&rest clauses) (nelisp-cl-macros--loop-build clauses))

;; --- Doc 143: wire the elisp Sexp printer into the reader runtime ---------
;; prin1-to-string / prin1 were void in the reader (lisp/nelisp-stdlib-prn.el
;; was never bootstrapped here), so all print-then-read roundtrips failed with
;; "Wrong type argument".  Functions below are copied verbatim from
;; lisp/nelisp-stdlib-prn.el; deps char-to-string/aref/length/concat/nreverse/
;; number-to-string/substring/symbol-name are reader primitives, string-search
;; + the record accessors are added here.

;; These signal `sequencep' where Emacs signals `stringp', because the first
;; thing they touch is `length' and `length' checks the weaker predicate.  A
;; handler written for the condition Emacs documents does not fire, which is
;; worse than the wrong message: the caller's recovery path never runs.
;; Record primitives backing cl-defstruct.  Representation = a plain
;; vector `[TAG slot0 slot1 ...]': index 0 is the type tag, slots are
;; 1-based in the vector but 0-based / tag-excluded through the
;; -record-ref/-set API (the contract cl-defstruct accessors assume,
;; per the macro docstring "0-based and excludes the tag").
(unless (fboundp 'nelisp--make-record)
  (defun nelisp--make-record (type-tag &rest slots)
    "Build a genuine tag-12 record (TYPE-TAG, SLOTS...) for cl-defstruct.
Doc 156: was `(apply #\\='vector ...)', but the reader now exposes a native
`recordp' tag-12 check (Doc 22 A14) that rejected those vectors, so every
`NAME-p' struct predicate returned nil.  Native records keep `aref'/`aset'
(record slot read/write, A14 + the bf_aset tag-12 follow-up) working and make
`recordp' agree, so predicates and setters are consistent."
    (apply 'record type-tag slots)))
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
;; Doc 22 A14: the bare reader now builds genuine tag-12 Records (via the
;; native `record' builtin) and `recordp' is a native tag-12 check, so prefer
;; it.  Fall back to the permissive vectorp-based form only if no native
;; `recordp' is bound (the old workaround when records surfaced as vectors).
(unless (fboundp 'recordp) (defun recordp (x) (vectorp x)))

;; Doc 188 P1 scratch/Messages seed.  Must run after `nelisp--make-
;; record' (just above) is defined: `nelisp-generate-new-buffer'
;; allocates a `cl-defstruct' record, and this call needs to actually
;; run at prelude load time (real Emacs always has a current buffer),
;; not merely be defined -- see the long comment on the
;; `nelisp--current-buffer' `defvar' for why this is not positioned
;; next to it.
(setq nelisp--current-buffer (nelisp-generate-new-buffer "*scratch*"))
(defvar nelisp--messages-buffer (nelisp-generate-new-buffer "*Messages*")
  "The seeded `*Messages*' buffer object, matching real Emacs's always-
present pair of default buffers.  A `defvar' rather than a bare top-
level call to `nelisp-generate-new-buffer' so this stays a top-level
DEFINITION for `make prelude-toplevel-check' -- see that tool's own
Commentary for why a bare call at the prelude's top level is treated
as a likely mistake.")

;; Hash-table predicate + iteration for the reader's builtin hash table.
;; The builtin `make-hash-table' returns the cons pair (MARKER . DATA) where
;; MARKER is an integer metadata slot and DATA is a bucket vector.
;; `make-hash-table'
;; / `gethash' / `puthash' / `hash-table-count' ship as native builtins, but
;; `maphash' ships only as a no-op stub and `hash-table-p' is absent -- an
;; incomplete substrate, not a minimal one.  Complete it here in the core
;; stdlib (these are the ops over the core-owned representation): the elisp
;; `maphash' overrides the stub, and `hash-table-p' keys off the integer car
;; (an alist has a cons car, a plist a keyword car, so the discrimination is
;; clean for the shapes Elisp passes to `hash-table-p').
;; Keyed off the NAMED marker, not "a cons whose car is an integer" -- under
;; the old rule '(1) was a hash table, so (gethash K '(1 2 3)) answered nil
;; instead of signalling, and nothing could tell a table from a pair well
;; enough to print one.
(defun hash-table-p (x)
  (and (consp x)
       (let ((m (car x)))
         (and (vectorp m) (> (length m) 1) (eq (aref m 0) 'hash-table)))))
(defun maphash (fn table)
  (unless (hash-table-p table) (signal 'wrong-type-argument (list 'hash-table-p table)))
  ;; The core `make-hash-table' returns (Int(0) . BUCKETS) where BUCKETS is a
  ;; vector whose slots are node lists of (KEY . VALUE) pairs.  (A legacy shape
  ;; stored a flat ((KEY . VALUE) ...) alist directly in the cdr.)  Walk both.
  (let ((data (cdr table)))
    (if (vectorp data)
        (let ((i 0) (n (length data)))
          (while (< i n)
            (let ((node (aref data i)))
              (while (consp node)
                (let ((entry (car node)))
                  (when (consp entry) (funcall fn (car entry) (cdr entry))))
                (setq node (cdr node))))
            (setq i (1+ i))))
      (let ((node data))
        (while (consp node)
          (let ((entry (car node)))
            (when (consp entry) (funcall fn (car entry) (cdr entry))))
          (setq node (cdr node))))))
  nil)

;; Doc 22 C1: hash-table introspection over the core `(Int(0) . alist)' shape.
;; The reader ignores `:test' -- every table uses the native key compare
;; (wf_key_eq: ints by value, symbols by name, strings by bytes = `equal'
;; semantics), so the effective and only honest test to report is `equal'.
;; `copy-hash-table' therefore preserves behaviour with a plain entry copy and
;; needs no test argument; the marker (car) MUST stay the integer 0 that
;; `hash-table-p' keys off, so the requested `:test' cannot be stashed there.
(unless (fboundp 'hash-table-test)
  (defun hash-table-test (table)
    ;; The requested test IS recorded now -- marker slot 2 -- so this reports
    ;; what the caller asked for instead of a fixed answer.  Lookup is still
    ;; `equal'-shaped underneath; that is a separate divergence and is not
    ;; hidden by reporting the request accurately.
    (unless (hash-table-p table)
      (signal 'wrong-type-argument (list 'hash-table-p table)))
    (or (and (> (length (car table)) 2) (aref (car table) 2)) 'eql)))
(unless (fboundp 'copy-hash-table)
  (defun copy-hash-table (table)
    (unless (hash-table-p table)
      (signal 'wrong-type-argument (list 'hash-table-p table)))
    (let ((new (make-hash-table)))
      (maphash (lambda (k v) (puthash k v new)) table)
      new)))
(unless (fboundp 'hash-table-keys)
  (defun hash-table-keys (table)
    (let ((acc nil))
      (maphash (lambda (k _v) (setq acc (cons k acc))) table)
      (nreverse acc))))
(unless (fboundp 'hash-table-values)
  (defun hash-table-values (table)
    (let ((acc nil))
      (maphash (lambda (_k v) (setq acc (cons v acc))) table)
      (nreverse acc))))

;;; nelisp-stdlib-prn.el --- elisp Sexp printer / serializer  -*- lexical-binding: t; -*-

;; Phase 7 Stage 7.1.2 (2026-05-07, Doc 64).
;;
;; elisp re-implementation of `prin1-to-string' / `prin1' / `terpri'.
;; `princ' / `print' already live in `lisp/nelisp-stdlib-misc.el'
;; (= batch 6e/6i) on top of `prin1-to-string'; promoting
;; `prin1-to-string' to elisp here also routes those two through the
;; pure-elisp printer.  The Rust dispatch arm + `bi_prin1_to_string'
;; function body in `build-tool/src/eval/builtins.rs' are removed in
;; the same commit (= Stage 7.1.4 in Doc 64).
;;
;; Float formatting matches the prior Rust `Sexp' Display closely
;; enough for substrate use: `(number-to-string X)' (= `%g')
;; followed by a `.0' suffix when the result lacks `.', `e' or `E'.
;; Edge cases (very large / NaN / inf) are passed through unchanged.
;;
;; Reader-macro abbreviation: a 2-element cons `(QUOTE-TAG ARG)' whose
;; head is one of `quote' / `function' or the punctuation-named symbols
;; `\`' / `,' / `,@' is rendered with the corresponding prefix (`\''
;; / `#\'' / `\`' / `,' / `,@').
;;
;; The MVP omits cycle detection (`#1=...#1#'); circular structures
;; recurse infinitely and abort via `max-lisp-eval-depth', matching
;; the prior Rust impl.  Cycle-safe printing is Stage 7.1.5 follow-up.

;; ---- core dispatcher ----

(defun nelisp--prn-chunks-add (state chunk)
  "Append CHUNK to STATE without reversing the accumulated chunk list."
  (let ((cell (cons chunk nil)))
    (if (car state)
        (setcdr (cdr state) cell)
      (setcar state cell))
    (setcdr state cell)
    state))

(defun nelisp--prn-chunks-string (state)
  "Return the concatenation of chunks held in STATE."
  (apply #'concat (car state)))

(defun nelisp--prn-string-escaped (s)
  "Return S with `\"' and `\\' escaped, which is what Emacs `prin1' escapes.
Measured on Emacs 30.1 rather than assumed: a newline, tab, carriage return,
BEL and NUL all pass through VERBATIM inside a printed string -- only the
two characters that would end the literal or start an escape are doubled.
Escaping \\n as well made every printed string containing a newline differ
from Emacs, which `make emacs-parity' caught the first time a case had one.
Char comparisons use raw integer codepoints (34 / 92) to sidestep any
difference in how `?\\X' literals get parsed by the bundled reader vs the
host.  For a tag-14/15 unibyte string, Doc 200 additionally requires every
byte >= 128 to print as octal, never as a raw byte mistaken for UTF-8."
  (let ((chunks (cons nil nil))
        (i 0)
        (n (length s))
        (unibyte (and (fboundp 'unibyte-string-p)
                      (unibyte-string-p s))))
    (while (< i n)
      (let ((c (aref s i)))
        (cond
         ((= c 34) (nelisp--prn-chunks-add chunks "\\\"")) ; ?\"
         ((= c 92) (nelisp--prn-chunks-add chunks "\\\\")) ; ?\\
         ((and unibyte (>= c 128))
          (nelisp--prn-chunks-add
           chunks
           (concat "\\"
                   (char-to-string (+ 48 (/ c 64)))
                   (char-to-string (+ 48 (logand (/ c 8) 7)))
                   (char-to-string (+ 48 (logand c 7))))))
         (t        (nelisp--prn-chunks-add chunks (char-to-string c)))))
      (setq i (1+ i)))
    (nelisp--prn-chunks-string chunks)))

(defun nelisp--prn-symbol-char-needs-escape-p (c)
  "Return non-nil when C terminates or escapes a reader symbol atom.
This mirrors the reader atom-terminator predicate.  A backslash also
needs escaping because the reader consumes it as an escape prefix."
  (or (= c 92) (= c 40) (= c 41) (= c 91) (= c 93) (= c 39)
      (= c 96) (= c 44) (= c 59) (= c 34) (= c 32) (= c 9)
      (= c 10) (= c 13) (= c 11) (= c 12)))

;; Per-character escaping is not enough: a symbol whose whole NAME would
;; read back as a NUMBER, or as the dot of a dotted pair, needs a leading
;; backslash so the reader sees a symbol.  Emacs prints (intern "12") as
;; \\12 and (intern ".") as \\., and the empty name as ## -- this printed
;; 12, . and the empty string, none of which read back as what was printed.
;; A print-then-read round trip silently changed the type.
(defun nelisp--prn-symbol-needs-leading-escape-p (s)
  (let ((n (length s)))
    (cond
     ((= n 0) nil)                      ; handled by the ## case
     ((string= s ".") t)
     (t
      ;; Would the reader take this whole name for a number?
      (let ((i 0) (seen-digit nil) (ok t))
        (while (and ok (< i n))
          (let ((c (aref s i)))
            (cond
             ((and (>= c ?0) (<= c ?9)) (setq seen-digit t))
             ((and (= i 0) (or (eq c ?-) (eq c ?+))) nil)
             ((or (eq c ?.) (eq c ?e) (eq c ?E)) nil)
             (t (setq ok nil))))
          (setq i (1+ i)))
        (and ok seen-digit))))))

(defun nelisp--prn-symbol-escaped (s)
  "Return S with reader atom terminators escaped for readable printing."
  (if (= (length s) 0)
      "##"
    (let ((chunks (cons nil nil)) (i 0) (n (length s)))
      (when (nelisp--prn-symbol-needs-leading-escape-p s)
        (nelisp--prn-chunks-add chunks "\\"))
      (while (< i n)
        (let ((c (aref s i)))
          (when (nelisp--prn-symbol-char-needs-escape-p c)
            (nelisp--prn-chunks-add chunks "\\"))
          (nelisp--prn-chunks-add chunks (char-to-string c)))
        (setq i (1+ i)))
      (nelisp--prn-chunks-string chunks))))

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
                         ((string= tag-name "`")         "`")
                         ((string= tag-name ",")         ",")
                         ((string= tag-name ",@")        ",@")
                         (t nil))))
      (when prefix
        (concat prefix (nelisp--prn-to-string arg escape))))))

;; `print-length' and `print-level' existed nowhere, so both were ignored:
;; a long list printed in full and a deep one printed to the bottom.  That
;; is not only a formatting difference -- they are the only bound on output
;; size, and without them a circular structure prints until something gives
;; out.  Emacs answers "(1 2 ...)" and "(1 (2 ...))".
(defvar print-length nil)
(defvar print-level nil)
(defun nelisp--prn-list-body (lst escape &optional depth)
  (setq depth (or depth 0))
  (let ((chunks (cons nil nil)) (cur lst) (first t) (count 0))
    (while (and (consp cur)
                (if print-length (< count print-length) t))
      (unless first (nelisp--prn-chunks-add chunks " "))
      (nelisp--prn-chunks-add chunks
                              (nelisp--prn-to-string (car cur) escape depth))
      (setq first nil)
      (setq count (1+ count))
      (setq cur (cdr cur)))
    (when (and (consp cur) print-length)
      (nelisp--prn-chunks-add chunks " ...")
      (setq cur nil))
    (unless (null cur)
      (nelisp--prn-chunks-add chunks " . ")
      (nelisp--prn-chunks-add chunks (nelisp--prn-to-string cur escape depth)))
    (nelisp--prn-chunks-string chunks)))

(defun nelisp--prn-vector (vec escape &optional depth)
  (setq depth (or depth 0))
  (let ((n (length vec)) (chunks (cons nil nil)))
    (nelisp--prn-chunks-add chunks "[")
    (let ((i 0) (lim (if print-length (if (< print-length n) print-length n) n)))
      (while (< i lim)
        (when (> i 0) (nelisp--prn-chunks-add chunks " "))
        (nelisp--prn-chunks-add chunks
                                (nelisp--prn-to-string (aref vec i) escape depth))
        (setq i (1+ i)))
      (when (< lim n)
        (when (> lim 0) (nelisp--prn-chunks-add chunks " "))
        (nelisp--prn-chunks-add chunks "...")))
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

(defun nelisp--prn-deref-env (env)
  "ENV with each captured CELL replaced by the value it holds.
The cell has to go before the list is printed, not while it is being
printed: a value that is itself a list has to be seen as a cons for the
printer to write (a 1) rather than (a . (1))."
  (if (not (consp env))
      env
    (let ((out nil) (l env))
      (while (consp l)
        (let ((e (car l)))
          (setq out (cons (if (consp e)
                              (cons (car e) (nelisp--cell-value (cdr e)))
                            e)
                          out)))
        (setq l (cdr l)))
      (nreverse out))))
(defun nelisp--prn-hash-table-p (x)
  (and (consp x)
       (let ((m (car x)))
         (and (vectorp m) (> (length m) 1) (eq (aref m 0) 'hash-table)))))
(defun nelisp--prn-hash-table (obj escape)
  "Print OBJ as Emacs prints a hash table.
The entries come out in bucket order rather than insertion order -- this
representation does not record insertion order at all, so the ORDER is not
claimed to match, only the shape."
  (let ((parts nil))
    (maphash (lambda (k v)
               (setq parts
                     (cons (concat (nelisp--prn-to-string k escape 0) " "
                                   (nelisp--prn-to-string v escape 0))
                           parts)))
             obj)
    (let* ((m (car obj))
           (tst (and (> (length m) 2) (aref m 2)))
           (head (if (and tst (not (eq tst 'eql)))
                     (concat "#s(hash-table test " (symbol-name tst))
                   "#s(hash-table")))
      (if (null parts)
          (concat head ")")
        (concat head " data ("
                (mapconcat 'identity (nreverse parts) " ") "))")))))
(defun nelisp--prn-to-string (obj escape &optional depth)
  (setq depth (or depth 0))
  (cond
   ((null obj) "nil")
   ((eq obj t) "t")
   ((integerp obj) (number-to-string obj))
   ((floatp obj)   (nelisp--prn-float obj))
   ((symbolp obj)
    (if escape
        (nelisp--prn-symbol-escaped (symbol-name obj))
      (symbol-name obj)))
   ((stringp obj)
    (if escape (concat "\"" (nelisp--prn-string-escaped obj) "\"") obj))
   ;; Tagged objects print as themselves.  A buffer came out as
   ;; [buffer "x" "" t], a hash table as its 2048-slot bucket vector, and a
   ;; closure as the (closure ENV ARGS . BODY) list it is made of -- each of
   ;; them readable Lisp that means something else.
   ((nelisp--prn-hash-table-p obj) (nelisp--prn-hash-table obj escape))
   ((and (consp obj) (eq (car obj) 'builtin) (consp (cdr obj)))
    (concat "#<subr " (symbol-name (car (cdr obj))) ">"))
   ((and (consp obj) (eq (car obj) 'closure)
         (consp (cdr obj)) (consp (cdr (cdr obj))))
    (concat "#[" (nelisp--prn-to-string (car (cdr (cdr obj))) escape depth)
            " " (nelisp--prn-to-string (cdr (cdr (cdr obj))) escape depth)
            " " (nelisp--prn-to-string (nelisp--prn-deref-env (car (cdr obj)))
                                       escape depth)
            "]"))
   ((consp obj)
    ;; Depth is a PARAMETER, not a special variable.  A free
    ;; `nelisp--prn-depth' worked in the standalone and broke
    ;; test/nelisp-stdlib-test.el, which evaluates only the `defun' forms of
    ;; lisp/nelisp-stdlib-prn.el -- so the defvar never ran and the counter
    ;; was void.  Threading it also means a caller cannot forget to rebind.
    (if (and print-level (>= depth print-level))
        "..."
      (or (nelisp--prn-reader-macro-abbrev obj escape)
          (concat "(" (nelisp--prn-list-body obj escape (1+ depth)) ")"))))
   ;; `print-level' bounds LIST nesting only -- Emacs prints
   ;; [1 [2 [3 [4]]]] in full at print-level 2, and only the list arm above
   ;; counts depth.  Measured rather than assumed; the first cut guarded
   ;; both and truncated vectors Emacs does not.
   ((and (vectorp obj) (> (length obj) 1) (eq (aref obj 0) 'buffer))
    (concat "#<buffer " (aref obj 1) ">"))
   ;; Doc 188 P1 (2026-08-23): buffers are now `nelisp-buffer' records
   ;; (the ported src/nelisp-buffer.el model), not the ad-hoc vector the
   ;; clause above still recognises for any stray old-shape value.
   ;; Without this clause a real buffer fell through to the generic
   ;; `nelisp--prn-record' arm below and printed as
   ;; `#s(nelisp-buffer "x" "" "" nil nil nil nil nil nil)' instead of
   ;; Emacs's own `#<buffer x>' -- caught by `make emacs-parity' going
   ;; red on the `generate-new-buffer' calls already in
   ;; test/nelisp-shadow-differential-cases.el before this fix landed.
   ((and (fboundp 'nelisp-buffer-p) (nelisp-buffer-p obj))
    (concat "#<buffer " (nelisp-buffer-name obj) ">"))
   ((vectorp obj) (nelisp--prn-vector obj escape (1+ depth)))
   ((recordp obj) (nelisp--prn-record obj escape))
   ;; NOT (format "#<unprintable %S>" obj): `%S' re-enters this function on
   ;; the same object, and for a lexical binding CELL -- which is what a
   ;; closure's captured environment holds -- that recursed until the nesting
   ;; limit.  So (format "%S" (let ((a 1)) (lambda () a))) did not print a
   ;; closure, it ABORTED, and every caller that formats a function value hit
   ;; it.  The fallback has to be a literal.
   ;; A lexical CELL is the one remaining shape, and it is what a closure's
   ;; captured environment holds -- deref it so (let ((a 1)) (lambda () a))
   ;; prints its ((a . 1)) the way Emacs does.
   ((not (eq (nelisp--cell-value obj) obj))
    (nelisp--prn-to-string (nelisp--cell-value obj) escape depth))
   (t "#<unprintable>")))

;; OVERRIDES: a non-list is `(wrong-type-argument . consp)' -- the data is
;; the bare symbol, not a list -- and an unrecognised key reports
;; `symbolp nil'.  Accepting anything meant a caller's print settings were
;; dropped with nothing to show for it.
(defconst nelisp--print-override-keys
  '(length level circle quoting escape-newlines escape-control-characters
    escape-nonascii escape-multibyte charset-text-property
    unreadable-function float-format integers-as-characters)
  "Keys `prin1' accepts in its OVERRIDES argument.")
(defun nelisp--check-print-overrides (overrides)
  (cond
   ((null overrides) nil)
   ((eq overrides t) nil)
   ((not (consp overrides)) (signal 'wrong-type-argument 'consp))
   (t (let ((l overrides))
        (while (consp l)
          (let ((e (car l)))
            (unless (consp e) (signal 'wrong-type-argument 'consp))
            (unless (memq (car e) nelisp--print-override-keys)
              (signal 'wrong-type-argument (list 'symbolp nil))))
          (setq l (cdr l)))))))
(unless (fboundp 'prin1-to-string)
  (defun prin1-to-string (object &optional noescape overrides)
    (nelisp--check-print-overrides overrides)
    (nelisp--prn-to-string object (not noescape))))

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
        (cond
         ((= c 92)
          ;; A symbol backslash quotes the following character, so it cannot
          ;; terminate this atom even when it normally would.
          (setq i (if (< (1+ i) n) (+ i 2) (1+ i))))
         ((or (= c 32) (= c 9) (= c 10) (= c 13) (= c 12)
              (= c 40) (= c 41) (= c 91) (= c 93)
              (= c 34) (= c 39) (= c 96) (= c 44) (= c 59))
          (setq stop t))
         (t (setq i (1+ i))))))
    i))

(defun nelisp--rd-symbol-unescape (tok)
  "Remove symbol backslashes, retaining their following characters literally."
  (let ((out "") (i 0) (n (length tok)))
    (while (< i n)
      (let ((c (aref tok i)))
        (if (and (= c 92) (< (1+ i) n))
            (setq out (concat out (char-to-string (aref tok (1+ i))))
                  i (+ i 2))
          (setq out (concat out (char-to-string c))
                i (1+ i)))))
    out))

(defun nelisp--rd-numeric-token-p (tok)
  "Return non-nil when TOK is integer or float syntax, as a host reads it.
This used to accept any arrangement of digits, dots, `e'/`E' and signs
with at least one digit, which is not what a number looks like: `7.1.4'
passed and `string-to-number' then answered 7, while the native lexer
called the same token a float and its parser gave up on it.  Measured
against `read-from-string' on a host, 2026-08-19 -- 7.1.4, 1e2e3, 1e2.3,
12e, 1+, 1-, 0x10 and ... are all symbols there; 1.2e3, 1e+2, .5, 1.5e-3
are numbers; `1.' is the integer 1."
  (let ((n (length tok)) (i 0) (seen-digit nil) (dots 0) (es 0)
        (exp-digits 0) (in-exp nil) (prev-e nil) (ok t))
    (when (and (> n 0) (let ((c (aref tok 0))) (or (= c 43) (= c 45))))
      (setq i 1))
    (when (= i n) (setq ok nil))
    (while (and ok (< i n))
      (let ((c (aref tok i)))
        (cond
         ((and (>= c 48) (<= c 57))
          (setq seen-digit t prev-e nil)
          (when in-exp (setq exp-digits (1+ exp-digits))))
         ;; One dot, and not inside the exponent.
         ((= c 46)
          (setq prev-e nil dots (1+ dots))
          (when (or (> dots 1) in-exp) (setq ok nil)))
         ;; One exponent marker, and only after a digit.
         ((or (= c 101) (= c 69))
          (setq es (1+ es))
          (if (or (> es 1) (not seen-digit))
              (setq ok nil)
            (setq in-exp t prev-e t)))
         ;; A sign is only legal at the start (handled above) or right
         ;; after the exponent marker; `1+' and `1-' are symbols.
         ((or (= c 43) (= c 45))
          (unless prev-e (setq ok nil))
          (setq prev-e nil))
         (t (setq ok nil))))
      (setq i (1+ i)))
    (and ok seen-digit (or (not in-exp) (> exp-digits 0)))))

;; Doc 190 Phase A: non-nil when TOK (already confirmed numeric syntax by
;; `nelisp--rd-numeric-token-p') is a PLAIN decimal integer -- no `.'/`e'/
;; `E' -- the shape `nl--read-int' (the bignum-aware native reader entry,
;; `nelisp-standalone--applyfn-bignum-helpers') accepts.  Float tokens
;; keep going through `string-to-number' unchanged, below.
(defun nelisp--rd-int-token-p (tok)
  (let ((i 0) (n (length tok)) (plain t))
    (while (< i n)
      (let ((c (aref tok i)))
        (when (or (= c 46) (= c 101) (= c 69)) (setq plain nil)))
      (setq i (1+ i)))
    plain))

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
       ((= c 96) (let ((r (nelisp--rd-one s (1+ i) n))) (cons (list (intern "`") (car r)) (cdr r))))
       ((= c 44)
        (if (and (< (1+ i) n) (= (aref s (1+ i)) 64))
            (let ((r (nelisp--rd-one s (+ i 2) n))) (cons (list (intern ",@") (car r)) (cdr r)))
          (let ((r (nelisp--rd-one s (1+ i) n))) (cons (list (intern ",") (car r)) (cdr r)))))
        ((= c 63) ; ?  -- character literal
        ;; This arm did not exist, so `?x' fell through to the atom path
        ;; and `read-from-string' answered the SYMBOL `?x' where Emacs
        ;; answers the integer 120.  Found by routing `read' through the
        ;; native parser (Doc 201 §6.9 item 5) and comparing the two
        ;; readers on the same input: the native one was right.  Every
        ;; expected value in the corpus this was written against was read
        ;; out of stock Emacs 30.1, not derived from the manual.
        ;;
        ;; Covers `?C' and the single-character escapes.  The modifier
        ;; syntaxes (`?\C-x', `?\M-x', `?\^x') and the numeric ones
        ;; (`?A', `?A', `?é') are NOT covered and fall through
        ;; to the atom path exactly as the whole arm used to -- wrong in
        ;; the same way it was before rather than newly wrong, and the
        ;; corpus says so.
        (if (>= (1+ i) n)
            (let* ((e (nelisp--rd-atom-end s i n)))
              (cons (intern (substring s i e)) e))
          (let ((c1 (aref s (1+ i))))
            (if (= c1 92) ; backslash
                (if (>= (+ i 2) n)
                    (cons (intern (substring s i (nelisp--rd-atom-end s i n)))
                          (nelisp--rd-atom-end s i n))
                  (let* ((c2 (aref s (+ i 2)))
                         (v (cond ((= c2 110) 10)   ; n
                                  ((= c2 116) 9)    ; t
                                  ((= c2 114) 13)   ; r
                                  ((= c2 102) 12)   ; f
                                  ((= c2 101) 27)   ; e
                                  ((= c2 115) 32)   ; s
                                  ((= c2 97) 7)     ; a
                                  ((= c2 98) 8)     ; b
                                  ((= c2 100) 127)  ; d
                                  ((= c2 118) 11)   ; v
                                  ((= c2 48) 0)     ; 0
                                  (t c2))))         ; \ \" \? \( ...
                    (cons v (+ i 3))))
              (cons c1 (+ i 2))))))
       ((= c 35) ; #
         (if (and (< (1+ i) n) (= (aref s (1+ i)) 39))
             (let ((r (nelisp--rd-one s (+ i 2) n))) (cons (list 'function (car r)) (cdr r)))
           (let* ((e (nelisp--rd-atom-end s i n))
                  (tok (substring s i e)))
             (cons (intern (nelisp--rd-symbol-unescape tok)) e))))
       (t
         (let* ((e (nelisp--rd-atom-end s i n))
                (raw-tok (substring s i e))
                (escaped (nelisp--rd-symbol-unescape raw-tok)))
           (cons (cond ((nelisp--rd-numeric-token-p raw-tok)
                        ;; Doc 190 Phase A: `read'/`read-from-string''s own
                        ;; numeric-token path.  A plain decimal-integer
                        ;; token routes through the bignum-aware native
                        ;; `nl--read-int' instead of `string-to-number' (Doc
                        ;; 187 made `string-to-number''s own int loop signal
                        ;; `overflow-error' on this exact input; PROMOTING to
                        ;; a Bignum here is what makes `(read (prin1-to-
                        ;; string big))' round-trip -- see
                        ;; `scripts/standalone-bignum-smoke.el'.  Float
                        ;; tokens (anything with `.'/`e'/`E') are unaffected.
                        (if (and (nelisp--rd-int-token-p raw-tok) (fboundp 'nl--read-int))
                            (nl--read-int raw-tok)
                          (string-to-number raw-tok)))
                      ;; `intern' on "nil"/"t" allocates a fresh Symbol Sexp
                      ;; that is NOT `eq' to the canonical nil/t sentinel this
                      ;; runtime's `while'/`if'/`car'/`cdr' etc. compare
                      ;; against -- it prints as "nil"/"t" (name-based printer)
                      ;; but is truthy, so any caller that reads dynamic text
                      ;; (e.g. an artifact manifest's `:features nil') and then
                      ;; loops `(while features ...)' spins forever.  Route the
                      ;; two self-evaluating symbols through the literal
                      ;; embedded here so they resolve to the SAME sentinel
                      ;; `defun'/`if' bodies use throughout the interpreter.
                       ((string= raw-tok "nil") nil)
                       ((string= raw-tok "t") t)
                       (t (intern escaped)))
                 e)))))))

(unless (fboundp 'read-from-string)
  ;; PERF (Doc 201 §6.9 item 5): this is `nelisp--rd-one', a reader written
  ;; in interpreted Elisp, and on the standalone it charges roughly a basic
  ;; operation per character -- 26.410s for 9114 bytes of nested conses,
  ;; where the NATIVE parser that drives `load' reads the same input in
  ;; 0.003s with an identical result.  Ninety-odd call sites in this tree
  ;; use it.
  ;;
  ;; It is NOT rerouted here, because the native reader
  ;; (`nelisp--read-all-from-string-native') answers a list of forms and no
  ;; position, and this function's contract is `(FORM . END-INDEX)' -- the
  ;; index is what callers iterate on.  Guessing it (last non-whitespace
  ;; character, say) would be wrong the moment a comment follows the form,
  ;; and a silently wrong index is worse than a slow correct one.
  ;;
  ;; If you do not need the index, `read' already takes the native path, and
  ;; `load' has always used it.  The real fix is a native single-form
  ;; builtin that returns the cursor as well: `bf_read_all_from_string_native'
  ;; (scripts/nelisp-standalone-build.el) already drives
  ;; `nelisp_reader_parse_one' with a cursor slot whose position sits at
  ;; offset 8, so the parser side of that exists and only the Sexp-level
  ;; `(FORM . INDEX)' return and its dispatch arm are missing.
  (defun read-from-string (string &optional start end)
    (nelisp--check-string string)
    (when start (unless (integerp start) (signal 'wrong-type-argument (list 'integerp start))))
    (when end (unless (integerp end) (signal 'wrong-type-argument (list 'integerp end))))
    (when (or (and end (> end (length string)))
              (and start (> start (length string))))
      (signal 'args-out-of-range (list string start end)))
    (let ((given start))
      (when (and start (< start 0))
        (setq start (+ (length string) start))
        (when (< start 0)
          (signal 'args-out-of-range (list string given end)))))
    (let* ((base (or start 0))
           (s (if (or start end) (substring string base (or end (length string))) string))
           (r (nelisp--rd-one s 0 (length s))))
      ;; Nothing but whitespace is `end-of-file' in Emacs, not a nil value at
      ;; position 0.  Answering (nil . 0) means a caller reading forms in a
      ;; loop never learns it reached the end, and reads nil for ever.
      (when (and (null (car r)) (>= (cdr r) (length s))
                 (not (string-match-p "[^ \t\n\r\f]" s)))
        (signal 'end-of-file nil))
      (cons (car r) (+ base (cdr r))))))

(unless (fboundp 'read)
  (defun read (&optional stream)
    (unless (or (null stream) (stringp stream) (functionp stream))
      (signal (if (symbolp stream) 'void-function 'invalid-function) (list stream)))
    (if (stringp stream)
        ;; PERF (Doc 201 §6.9 item 5): `read-from-string' below is
        ;; `nelisp--rd-one', a reader written in interpreted Elisp, and on
        ;; the standalone it costs a basic operation per character.  The
        ;; SAME parser that drives `load' is also exposed to Lisp as
        ;; `nelisp--read-all-from-string-native'.  Measured on 9114 bytes of
        ;; nested conses: 26.410s through `read-from-string', 0.003s through
        ;; the native reader, identical results.
        ;;
        ;; `read' can take that path exactly, because it answers the FIRST
        ;; form and discards the position -- which is the one part of
        ;; `read-from-string''s contract the bulk reader cannot supply, and
        ;; the reason that function is left alone (see its own comment).
        ;; An empty or all-whitespace string reads as no forms at all, where
        ;; `read' owes an `end-of-file' signal, so that case falls through
        ;; to the Elisp path rather than being special-cased twice.
        (let ((forms (and (fboundp 'nelisp--read-all-from-string-native)
                          (nelisp--read-all-from-string-native stream))))
          (if forms (car forms) (car (read-from-string stream))))
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
(define-error 'nelisp-raw-byte-unrepresentable
  "Raw byte cannot appear in a multibyte string (Doc 200 §4)")

;; `nelisp-unsupported-primitive': a NeLisp-specific (not an Emacs-standard)
;; condition for a primitive that exists on this substrate but cannot do its
;; job here -- as opposed to `void-function', which means the name itself was
;; never defined.  First consumer: the standalone reader's native `nl-ffi-
;; call' fallback arm (`nelisp-standalone--applyfn-ffi-unsupported-form',
;; scripts/nelisp-standalone-build.el) for a build with no dynamic FFI
;; linkage -- see docs/design/100-phase-47-dynamic-link-elisp.org section 7.
;; Registered here, not there, because `nelisp-unsupported-primitive' is NOT
;; itself a listed `nl-safe-unsafe-primitives' name (unlike `nl-ffi-call'),
;; so this file -- loaded by every entry path uniformly -- is the ordinary,
;; unrestricted place for it.  Coordinated with branch
;; `feat/socket-primitives-p1', which introduces the same symbol for the
;; same purpose on a different primitive family.  Plain `put' (matching the
;; block above) rather than `define-error' so this still seeds correctly if
;; a future edit moves it above that defun.
(put 'nelisp-unsupported-primitive 'error-conditions
     '(nelisp-unsupported-primitive error))
(put 'nelisp-unsupported-primitive 'error-message
     "Primitive not supported by this NeLisp build")

;; Doc 152 gate-G: polyfill standard Emacs builtins missing from standalone
;; NeLisp so the anvil-pkg ERT suite's helpers (with-mock, registry-clear, ...)
;; run for real instead of signalling void-function.  All guarded so a real
;; builtin (host Emacs / future NeLisp primitive) always wins.
(defvar load-path nil)
(defvar features nil)
(defvar nelisp--environment nil)
(defvar nelisp--environment-loaded nil
  "Non-nil once the OS environment has been read into `nelisp--environment'.")

(defun nelisp--environment-load ()
  "Seed `nelisp--environment' from the operating system, once.

`getenv' answered nil for everything here, and had since this runtime
existed: the alist it reads was never filled from anywhere.  Nothing was
broken -- nothing had been connected.  Measured 2026-08-19: anything keyed
on HOME or XDG_CACHE_HOME therefore did not work, the native-exec cache
among them, and `PROBE getenv' read `nil' with the note \"pass parameters
as variables in a driver file\".

/proc/self/environ needs no new primitive: it is a file, the runtime can
read files, and its contents are NUL-separated VAR=VAL.  Off Linux the read
returns nil and this is exactly as it was, which is why the probe reports
what it measured rather than assuming.

Since 2026-08-19 this is the fallback rather than the source on Linux: the
boot walker fills the alist from the entry stack before any Lisp runs, which
the checked allocator needs (it has to decide whether to arm before the boot
watermark freezes, long before a file can be read).  This function still
matters where that walker is a no-op -- macOS today -- and as the path that
runs when the alist arrives empty for any other reason.

Entries already in `nelisp--environment' win, so a `setenv' before the
first `getenv' is not overwritten by the value the process started with."
  (unless nelisp--environment-loaded
    (setq nelisp--environment-loaded t)
    ;; A non-empty alist here means the boot walker already published the
    ;; real environment -- from the entry stack on Linux, from
    ;; GetEnvironmentStringsW on Windows -- and reading /proc would only
    ;; re-derive what is already present, since existing entries win
    ;; anyway.  `setenv' loads before it writes, so a user entry can
    ;; never be what this sees.
    (unless nelisp--environment
      (let ((raw (and (fboundp 'nelisp--syscall-read-file)
                    (nelisp--syscall-read-file "/proc/self/environ"))))
      (when (stringp raw)
        (let ((n (length raw)) (i 0) (start 0))
          (while (<= i n)
            (when (or (= i n) (= (aref raw i) 0))
              (when (> i start)
                (let* ((entry (substring raw start i))
                       (m (length entry))
                       (j 0)
                       (split nil))
                  (while (and (< j m) (null split))
                    (when (= (aref entry j) 61) (setq split j))
                    (setq j (1+ j)))
                  (when (and split (> split 0))
                    (let ((name (substring entry 0 split)))
                      (unless (assoc name nelisp--environment)
                        (setq nelisp--environment
                              (cons (cons name (substring entry (1+ split)))
                                    nelisp--environment)))))))
              (setq start (1+ i)))
            (setq i (1+ i)))))))))

(unless (fboundp 'getenv)
  (defun getenv (variable)
    (nelisp--environment-load)
    (cdr (assoc variable nelisp--environment))))
(unless (fboundp 'setenv)
  (defun setenv (variable value &optional _substitute)
    (nelisp--environment-load)
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
(defun nelisp--temp-name-process-token ()
  "Return a token that no other live process shares, for temp names.
Prefers the real process id; falls back to the clock, marked with a leading
`t' so a name built from the weaker source can be recognised as such."
  (let* ((stat (and (fboundp 'nelisp--syscall-read-file)
                    (nelisp--syscall-read-file "/proc/self/stat")))
         (pid (and (stringp stat) (> (length stat) 0)
                   (string-to-number stat))))
    (if (and (integerp pid) (> pid 0))
        (number-to-string pid)
      (let ((now (and (fboundp 'current-time) (current-time))))
        (cond
         ;; (HIGH LOW MICROSEC PICOSEC), the default shape.
         ((and (consp now) (consp (cdr now)))
          (format "t%d-%d-%d" (nth 0 now) (nth 1 now) (or (nth 2 now) 0)))
         ;; (TICKS . HZ), when `current-time-list' is nil.
         ((consp now) (format "t%d-%d" (car now) (cdr now)))
         (t "t0"))))))
(defvar nelisp--temp-name-counter 0)
(defvar nelisp--temp-name-nonce nil
  "This process's share of `make-temp-name' output, computed on first use.")
;; Emacs guarantees the process id is part of a `make-temp-name' result, so
;; that two processes cannot produce the same name.  A counter and the prefix
;; length cannot guarantee it: both start at the same place in every process,
;; so three runs of the same binary all answered "pfx-4-4" then "pfx-5-4".
;; `make-temp-file' below builds on this and creates without O_EXCL, so those
;; runs would have shared one file with neither noticing.
;;
;; `emacs-pid' does not exist here.  Linux publishes the id as the first field
;; of /proc/self/stat, which `nelisp--syscall-read-file' can read and
;; `string-to-number' will take the leading integer of.  Where there is no
;; /proc the clock stands in -- weaker, since two processes reaching it in the
;; same microsecond still match, but that is unlikely rather than certain.
(unless (fboundp 'make-temp-name)
  (defun make-temp-name (prefix)
    (nelisp--check-string prefix)
    (unless nelisp--temp-name-nonce
      (setq nelisp--temp-name-nonce (nelisp--temp-name-process-token)))
    (setq nelisp--temp-name-counter (1+ nelisp--temp-name-counter))
    (format "%s%s-%d" prefix nelisp--temp-name-nonce
            nelisp--temp-name-counter)))
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
;;
;; Doc 201 §4 item 2: the trichotomy's middle step asks whether PATH
;; accepts a trailing slash, and Windows answers YES for an ordinary file
;; -- so on a windows-native build every regular file read as a directory,
;; `file-regular-p' answered nil for all of them and `file-directory-p'
;; answered t.  Now that this target has a real `stat'
;; (`nelisp--syscall-stat-field' -> `nl_os_stat_path' ->
;; GetFileAttributesExW), the file/directory answer comes from st_mode.
;; The access(2) F_OK check stays FIRST and the trichotomy stays as the
;; fallback: absence is still one syscall (the shape `executable-find'
;; walks a whole PATH with), and a target whose `stat' is still -ENOSYS
;; (macos) keeps exactly the behaviour it had.
(when (fboundp 'nelisp--syscall-path-int)
  (fset 'nelisp--syscall-stat
        (lambda (path)
          (if (not (= 0 (nelisp--syscall-path-int 21 path 0)))
              'absent
            (let ((mode (if (fboundp 'nelisp--syscall-stat-field)
                            (nelisp--syscall-stat-field path 24)
                          -1)))
              (if (>= mode 0)
                  ;; S_IFMT 0o170000 = 61440, S_IFDIR 0o40000 = 16384.
                  ;; Everything else keeps answering `file', which is what
                  ;; the trichotomy did with the same inputs.
                  (if (= (logand mode 61440) 16384) 'directory 'file)
                (if (= 0 (nelisp--syscall-path-int 21 (concat path "/") 0))
                    'directory
                  'file)))))))
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
  (defun nelisp--file-attribute-nth (attrs i)
    "ATTRS element I, naming the TAIL of an improper list as `listp'.
`nth' over the same value names the whole list; these accessors do not,
and only running both says which."
    (let ((l attrs))
      (while (and (> i 0) (consp l)) (setq l (cdr l)) (setq i (1- i)))
      (cond ((consp l) (car l))
            ((null l) nil)
            (t (signal 'wrong-type-argument (list 'listp l))))))
  (defun file-attribute-size (attrs) (nelisp--file-attribute-nth attrs 7)))
(unless (fboundp 'file-attribute-modification-time)
  (defun file-attribute-modification-time (attrs)
    (nelisp--file-attribute-nth attrs 5)))
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
    (when separators (nelisp--check-string separators))
    (nelisp--check-string string)
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
  (defun file-writable-p (filename)
    (nelisp--check-string filename)
    (let ((full (expand-file-name filename)))
      (if (file-exists-p full)
          (= 0 (nelisp--syscall-path-int 21 full 2))
        (= 0 (nelisp--syscall-path-int
              21 (or (file-name-directory full) ".") 2))))))
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
    (nelisp--check-string directory)
    (unless (or recursive (file-directory-p (expand-file-name directory)))
      (signal 'file-missing
              (list "Removing directory" "No such file or directory"
                    (expand-file-name directory))))
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
    (nelisp--check-string filename)
    (and (stringp filename)
         (> (length filename) 0)
         (let ((c (aref filename 0)))
           (or (= c 47) (= c 126)             ; "/" or "~"
               ;; Doc 201 §4 follow-up: on windows-nt a drive-letter name
               ;; ("C:/x", "C:\\x") and a rooted backslash name ("\\x") are
               ;; absolute too, and answering nil for one is what sent
               ;; `expand-file-name' anchoring "C:/Windows" onto "/".  A
               ;; backslash name stays relative on every other target, where
               ;; it is an ordinary character in a file name.
               (and (nelisp--windows-paths-p)
                    (or (= c 92) (and (nelisp--path-drive filename) t))))))))
(unless (fboundp 'rename-file)
  (defun rename-file (file newname &optional ok-if-already-exists)
    (nelisp--check-string file)
    (nelisp--check-string newname)
    (when (and (not ok-if-already-exists)
               (file-exists-p (expand-file-name newname)))
      (signal 'file-already-exists
              (list "File already exists" (expand-file-name newname))))
    (let ((rc (nelisp--syscall-path2 82 file newname)))
      (unless (= rc 0)
        ;; `file-missing' with Emacs's three fields, not an `error' string:
        ;; a handler can match on the condition and read the name out of it.
        (signal 'file-missing
                (list "Renaming" "No such file or directory"
                      (expand-file-name file) (expand-file-name newname)))))
    nil))
(unless (fboundp 'make-symbolic-link)
  (defun make-symbolic-link (target linkname &optional ok-if-already-exists)
    (nelisp--check-string target)
    (nelisp--check-string linkname)
    ;; Unlink unconditionally: a DANGLING symlink is not `file-exists-p' --
    ;; it follows the link -- so guarding on that left the old link in place
    ;; and symlink(2) came back EEXIST for a call Emacs answers nil.
    (when ok-if-already-exists
      (nelisp--syscall-path 87 linkname))     ; unlink existing, if any
    (let ((rc (nelisp--syscall-path2 88 target linkname)))
      (unless (= rc 0)
        (error "make-symbolic-link: rc=%S %s -> %s" rc target linkname)))
    nil))
(unless (fboundp 'file-executable-p)
  (defun file-executable-p (filename)
    ;; "" is `default-directory' after expansion, and a directory you can
    ;; enter is executable -- testing the empty name itself answered nil.
    (nelisp--check-string filename)
    (= 0 (nelisp--syscall-path-int 21 (expand-file-name filename) 1))))  ; X_OK
;; A bare PREFIX (no directory component -- every caller in this tree passes
;; one) hardcoded "/tmp/" and ignored both `temporary-file-directory' (never
;; bound here; see `nelisp-artifact--temp-directory' for the same defensive
;; check on that variable in lisp/nelisp-artifact.el) and TMPDIR.  Measured
;; 2026-08-21: `TMPDIR=/some/dir (make-temp-file "x-")' still created under
;; /tmp.  An ABSOLUTE PREFIX fared worse -- it was concatenated straight onto
;; "/tmp/" regardless, producing a nonsense doubled path no caller here
;; exercises but Emacs supports (PREFIX may be a full path; the resulting
;; name is used as-is).  `expand-file-name' already implements exactly that
;; split -- it ignores its second argument when the first is absolute -- so
;; joining PREFIX against the resolved directory through it, rather than a
;; hand-rolled `concat', gets both cases right together: this is the same
;; `(expand-file-name prefix temporary-file-directory)' shape Emacs's own
;; `make-temp-file' uses before calling `make-temp-name'.
(unless (fboundp 'make-temp-file)
  (defun make-temp-file (prefix &optional dir-flag suffix text)
    (unless (sequencep prefix) (signal 'wrong-type-argument (list 'sequencep prefix)))
    (let* ((tmpdir (if (and (boundp 'temporary-file-directory)
                             (stringp temporary-file-directory)
                             (> (length temporary-file-directory) 0))
                        temporary-file-directory
                      (or (getenv "TMPDIR") "/tmp")))
           (path (concat (make-temp-name (expand-file-name prefix tmpdir))
                         (or suffix ""))))
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
    (nelisp--check-symbol list-var)
    (unless (listp (symbol-value list-var))
      (signal 'wrong-type-argument (list 'listp (symbol-value list-var))))
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
  (defun lwarn (type level message &rest _args)
    "Answer nil: no warning buffer here, but MESSAGE is still a string."
    (unless (stringp message) (signal 'wrong-type-argument (list 'stringp message)))
    nil))
(unless (fboundp 'identity)
  (defun identity (x) x))
(unless (fboundp 'booleanp)
  (defun booleanp (x)
    (or (eq x t) (eq x nil))))
(unless (fboundp 'error-message-string)
  ;; Emacs's rule, followed rather than approximated:
  ;;   * `error' and `user-error' take their message from the first datum;
  ;;   * anything else takes it from the condition's `error-message', which
  ;;     is curved (Emacs runs the property text through its quoting, so
  ;;     "Symbol's" prints as "Symbol’s" -- but a message that came from
  ;;     the DATA is never rewritten, which is why the curving is applied
  ;;     only on the property branch);
  ;;   * a `file-error' promotes its first datum to the message;
  ;;   * remaining data are joined ": " then ", ", printed with `prin1'
  ;;     except under `file-error' / `end-of-file' / `user-error', which
  ;;     `princ' them.
  (defun error-message-string (error-descriptor)
    (unless (listp error-descriptor)
      (signal 'wrong-type-argument (list 'listp error-descriptor)))
    (if (not (consp error-descriptor))
        "peculiar error"
      (let* ((errname (car error-descriptor))
             (_ (unless (symbolp errname)
                  (signal 'wrong-type-argument (list 'symbolp errname))))
             (from-data (or (eq errname 'error) (eq errname 'user-error)))
             (file-error (and (not from-data)
                              (memq 'file-error (get errname 'error-conditions))))
             (body (if from-data (cdr error-descriptor) error-descriptor))
             (errmsg (if from-data (car body)
                       (nelisp--curve-quotes (or (get errname 'error-message)
                                                 "peculiar error"))))
             (tail (cdr body))
             (plain (or file-error (eq errname 'end-of-file)
                        (eq errname 'user-error))))
        (when (and file-error tail)
          (setq errmsg (car tail))
          (setq tail (cdr tail)))
        (let ((out (cond ((stringp errmsg) errmsg)
                         ((null errmsg) "peculiar error")
                         (t (format "%S" errmsg))))
              (sep ": "))
          (while tail
            (setq out (concat out sep
                              (if (and plain (stringp (car tail)))
                                  (car tail)
                                (format "%S" (car tail)))))
            (setq sep ", ")
            (setq tail (cdr tail)))
          out)))))
(unless (fboundp 'format-time-string)
  (defun format-time-string (&rest _args)
    "1970-01-01"))
(unless (fboundp 'replace-regexp-in-string)
  ;; This used to recognise ONE regexp -- "[^A-Za-z0-9_]" -- and answer STRING
  ;; unchanged for every other pattern.  A caller got its input back with no
  ;; indication that nothing had been replaced, which is the worst shape for
  ;; this failure: the result is a plausible string.  The walk below is the
  ;; one in Emacs's `subr.el': match, keep the unmatched prefix, expand the
  ;; replacement, advance -- with the empty-match guard that stops it looping.
  (defun nelisp--rris-expand (rep string literal)
    "Expand \\N / \\& / \\\\ in REP against the current match on STRING."
    (if literal
        rep
      (let ((i 0) (n (length rep)) (out ""))
        (while (< i n)
          (let ((c (aref rep i)))
            (if (and (= c ?\\) (< (1+ i) n))
                (let ((d (aref rep (1+ i))))
                  (cond
                   ((= d ?&)
                    (setq out (concat out (substring string (match-beginning 0)
                                                     (match-end 0))))
                    (setq i (+ i 2)))
                   ((and (>= d ?0) (<= d ?9))
                    (let ((g (- d ?0)))
                      (setq out (concat out (or (and (match-beginning g)
                                                     (substring string
                                                                (match-beginning g)
                                                                (match-end g)))
                                                ""))))
                    (setq i (+ i 2)))
                   ((= d ?\\) (setq out (concat out "\\")) (setq i (+ i 2)))
                   (t (setq out (concat out (char-to-string d))) (setq i (+ i 2)))))
              (setq out (concat out (char-to-string c)))
              (setq i (1+ i)))))
        out)))
  (defun nelisp--rris-case (matched rep)
    "Apply Emacs's FIXEDCASE=nil adjustment: MATCHED all-caps upcases REP."
    (let ((i 0) (n (length matched)) (letters 0) (uppers 0))
      (while (< i n)
        (let ((c (aref matched i)))
          (when (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z)))
            (setq letters (1+ letters))
            (when (and (>= c ?A) (<= c ?Z)) (setq uppers (1+ uppers)))))
        (setq i (1+ i)))
      (if (and (> letters 1) (= letters uppers)) (upcase rep) rep)))
  (defun nelisp--rris (regexp rep string
                              &optional fixedcase literal
                              subexp start)
    (let ((l (length string))
          (from (or start 0))
          (pieces nil))
      (while (and (< from l) (string-match regexp string from))
        (let* ((mb (match-beginning (or subexp 0)))
               (me (match-end (or subexp 0)))
               (whole-e (match-end 0))
               (matched (substring string mb me))
               (text (if (stringp rep)
                         (nelisp--rris-expand rep string literal)
                       (funcall rep (substring string (match-beginning 0)
                                               (match-end 0))))))
          (when (= whole-e (match-beginning 0))
            (setq whole-e (min l (1+ (match-beginning 0)))))
          (setq pieces (cons (if fixedcase text (nelisp--rris-case matched text))
                             (cons (substring string from mb) pieces)))
          (setq from (max whole-e (if (= me mb) (min l (1+ mb)) me)))))
      (setq pieces (cons (substring string from l) pieces))
      (apply #'concat (nreverse pieces)))))
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
  (nelisp--check-string string)
    ;; `aref' answers the CHARACTER at a char index, decoding UTF-8 (Doc
    ;; 161) -- correct for text, wrong here.  STRING is arbitrary bytes
    ;; (this is the encoder `nelisp-artifact.el' calls on a compiled
    ;; object's raw content), and a byte >= 128 does not stand alone as
    ;; a valid UTF-8 sequence, so `aref' misreads both the byte value
    ;; and the char/byte boundary -- and `length' undercounts the same
    ;; string for the same reason (this is `nelisp-elf--byte-length's
    ;; `length'-vs-`string-bytes' defect again, one caller upstream: a
    ;; write-region call that now passes its own count check can still
    ;; carry a corrupted `:object-base64' payload from here).  `string-
    ;; byte' / `string-bytes' are the byte-clean pair: raw byte at a
    ;; byte index, and a byte count that does not shrink for high bytes.
    (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
          (i 0)
          (len (string-bytes string))
          (chunks nil)
          (parts nil)
          (part-count 0))
      (while (< i len)
        (let* ((a (string-byte string i))
               (have-b (< (1+ i) len))
               (have-c (< (+ i 2) len))
               (b (if have-b (string-byte string (1+ i)) 0))
               (c (if have-c (string-byte string (+ i 2)) 0))
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
  (defun base64-decode-string (string &optional base64url ignore-invalid)
    ;; Bytes, not characters.  `char-to-string' builds the CHARACTER whose
    ;; code is the byte, and a byte >= 128 is then held in its multi-byte
    ;; UTF-8 form, so decoding "yMnK" answered (195 136 195 137 195 138)
    ;; where every other base64 answers (200 201 202) -- the payload silently
    ;; doubled and no longer matched what was encoded.
    ;;
    ;; The ENCODER above was already byte-clean (`string-byte' /
    ;; `string-bytes', see its own comment); this is the same defect on the
    ;; way back, so the pair did not round-trip for any input with a high
    ;; byte -- which is most compiled output.  `unibyte-string' keeps the byte.
    (nelisp--check-string string)
    ;; Emacs REFUSES what it cannot decode: a character outside the alphabet
    ;; and a group short of four both signal, and skipping them quietly
    ;; produced a shorter string that read as a successful decode.  In
    ;; base64url mode the padding is optional, which is why the same input
    ;; can be an error in one mode and two bytes in the other.
    (let ((j 0) (n (length string)) (kept 0))
      (while (< j n)
        (let ((ch (aref string j)))
          (cond
           ((or (= ch 10) (= ch 13) (= ch 9) (= ch 32)) nil)
           ((= ch ?=) (setq kept (1+ kept)))
           ((and base64url (or (= ch ?-) (= ch ?_))) (setq kept (1+ kept)))
           ((>= (nelisp--base64-value ch) 0) (setq kept (1+ kept)))
           (t (unless ignore-invalid (signal 'error (list "Invalid base64 data"))))))
        (setq j (1+ j)))
      (if (or base64url ignore-invalid)
          (when (= 1 (mod kept 4)) (signal 'error (list "Invalid base64 data")))
        (unless (= 0 (mod kept 4))
          (signal 'error (list "Invalid base64 data")))))
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
                    (cons (unibyte-string (logand (ash triple -16) 255))
                          bytes))
              (setq byte-count (1+ byte-count))
              (unless (= c -2)
                (setq bytes
                      (cons (unibyte-string (logand (ash triple -8) 255))
                            bytes))
                (setq byte-count (1+ byte-count)))
              (unless (= d -2)
                (setq bytes
                      (cons (unibyte-string (logand triple 255))
                            bytes))
                (setq byte-count (1+ byte-count)))
              (when (>= byte-count 128)
                (setq chunks (nelisp--base64-flush-chunk bytes chunks))
                (setq bytes nil)
                (setq byte-count 0))
              (setq vals-count 0))))
        (setq i (1+ i)))
      (when (>= vals-count 2)
        ;; A PADDING character reached this flush as -2 and was shifted in as
        ;; a value, so an ignore-invalid decode of "a!b=" produced two bytes
        ;; of noise instead of one byte of answer.
        (let ((triple (logior (ash a 18) (ash b 12)
                              (if (and (>= vals-count 3) (>= c 0)) (ash c 6) 0))))
          (setq bytes (cons (unibyte-string (logand (ash triple -16) 255)) bytes))
          (when (and (>= vals-count 3) (>= c 0))
            (setq bytes (cons (unibyte-string (logand (ash triple -8) 255)) bytes)))))
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
(unless (fboundp 'nelisp--json-key)
  (defun nelisp--json-key (sym)
    "SYM's name as a JSON key: a keyword loses its leading colon."
    (let ((n (symbol-name sym)))
      (if (and (> (length n) 0) (= (aref n 0) ?:)) (substring n 1) n))))
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
    ;; The offender Emacs names is the whole KEYS list, not the element the
    ;; walk stopped at.
    (when (and _keys (not (and (listp _keys) (= 0 (mod (length _keys) 2)))))
      (signal 'wrong-type-argument (list 'plistp _keys)))
    (cond
     ((null obj) "{}")
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
     ;; A LIST is an OBJECT in Emacs -- an alist or a plist, with symbol
     ;; keys -- not an array.  Serialising it as [..] produced valid JSON of
     ;; the wrong shape, which is the failure a caller finds last.  Only a
     ;; vector is an array.
     ((vectorp obj)
      (let ((parts "") (first t) (i 0) (n (length obj)))
        (while (< i n)
          (setq parts (concat parts (if first "" ",") (json-serialize (aref obj i)))
                first nil i (1+ i)))
        (concat "[" parts "]")))
     ((consp obj)
      (if (consp (car obj))
          (let ((parts "") (first t) (l obj))
            (while (consp l)
              (let ((e (car l)))
                (unless (and (consp e) (symbolp (car e)))
                  (signal 'wrong-type-argument
                          (list 'symbolp (if (consp e) (car e) e))))
                (setq parts (concat parts (if first "" ",")
                                    "\"" (nelisp--json-escape
                                           (nelisp--json-key (car e))) "\":"
                                    (json-serialize (cdr e)))
                      first nil l (cdr l))))
            (concat "{" parts "}"))
        (let ((parts "") (first t) (l obj))
          (while (consp l)
            (let ((k (car l)))
              (unless (consp (cdr l))
                (signal 'wrong-type-argument (list 'consp (cdr l))))
              (unless (symbolp k) (signal 'wrong-type-argument (list 'symbolp k)))
              (setq parts (concat parts (if first "" ",")
                                  "\"" (nelisp--json-escape (nelisp--json-key k)) "\":"
                                  (json-serialize (car (cdr l))))
                    first nil l (cdr (cdr l)))))
          (concat "{" parts "}"))))
     ;; A bare symbol is not a JSON value: only nil/t and the two keyword
     ;; sentinels are, and everything else names `json-value-p'.  Encoding
     ;; the symbol NAME as a string produced valid JSON that said something
     ;; the caller never wrote.
     (t (signal 'wrong-type-argument (list 'json-value-p obj))))))

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

;; Idempotent capture rule (applies to every `nelisp--native-X' save below):
;; this prelude is baked into the standalone image AND re-loaded at runtime.
;; On the first (bake) pass `nelisp--native-X' is unbound, so the capture saves
;; the genuine native builtin.  A bare `(fset 'nelisp--native-X (symbol-function
;; 'X))' on the second (runtime re-load) pass would re-capture the elisp wrapper
;; that this file installs over X, turning the wrapper's `nelisp--native-X'
;; delegate into a self-call -> infinite recursion / silent empty result (this
;; is what broke `format'/`substring'/`equal'/`floor' and every macro built on
;; them, e.g. `cl-defstruct').  Guard each capture with `unless fboundp' so the
;; saved native implementation survives re-loads untouched.
;;
;; A1: 2-arg `floor'/`ceiling'/`truncate' ignored the divisor.  Capture the
;; native 1-arg implementation, fix the 2-arg integer path with a toward-zero
;; quotient (`/') plus a floor/ceil sign adjustment (the reader has no `%').
(unless (fboundp 'nelisp--native-floor) (fset 'nelisp--native-floor (symbol-function 'floor)))
(unless (fboundp 'nelisp--native-ceiling) (fset 'nelisp--native-ceiling (symbol-function 'ceiling)))
(unless (fboundp 'nelisp--native-truncate) (fset 'nelisp--native-truncate (symbol-function 'truncate)))

(defun nelisp--int-floor-div (x div)
  "Integer floor division X/DIV toward negative infinity (DIV /= 0)."
  (let* ((q (/ x div))
         (r (- x (* q div))))
    (if (and (not (= r 0)) (if (< div 0) (> r 0) (< r 0)))
        (- q 1)
      q)))

(defun floor (x &optional div)
  "Return the largest integer <= X (1-arg) or <= X/DIV (2-arg)."
  (nelisp--check-number x)
  (when div
    (nelisp--check-number div)
    (when (= div 0) (signal 'arith-error nil)))
  (cond
   ((null div) (nelisp--native-floor x))
   ((and (integerp x) (integerp div)) (nelisp--int-floor-div x div))
   (t (nelisp--native-floor (/ x div)))))

;; `round' was missing entirely, next to `floor'/`ceiling'/`truncate'.
;; Emacs rounds halves to EVEN, not away from zero: (round 0.5)=0 and
;; (round 1.5)=2, and (round 7 2)=4 while (round -7 2)=-4.  Rounding the
;; obvious way would be wrong on exactly the inputs a test picks.
(defun round (x &optional div)
  "Return X (1-arg) or X/DIV (2-arg) rounded to the nearest integer.
A half is rounded to the even neighbour, as in Emacs."
  (nelisp--check-number x)
  (when div
    (nelisp--check-number div)
    (when (= div 0) (signal 'arith-error nil)))
  (let ((v (if div (/ (float x) (float div)) x)))
    (if (integerp v) v
      (let* ((f (floor v)) (d (- v f)))
        (cond ((< d 0.5) f)
              ((> d 0.5) (1+ f))
              ((= 0 (% f 2)) f)
              (t (1+ f)))))))

(defun ceiling (x &optional div)
  "Return the smallest integer >= X (1-arg) or >= X/DIV (2-arg)."
  (nelisp--check-number x)
  (when div
    (nelisp--check-number div)
    (when (= div 0) (signal 'arith-error nil)))
  (cond
   ((null div) (nelisp--native-ceiling x))
   ((and (integerp x) (integerp div))
    (- (nelisp--int-floor-div (- x) div)))
   (t (nelisp--native-ceiling (/ x div)))))

(defun truncate (x &optional div)
  "Truncate X (1-arg) or X/DIV (2-arg) toward zero."
  (nelisp--check-number x)
  (when div
    (nelisp--check-number div)
    (when (= div 0) (signal 'arith-error nil)))
  (cond
   ((null div) (nelisp--native-truncate x))
   ((and (integerp x) (integerp div)) (/ x div))
   (t (nelisp--native-truncate (/ x div)))))

;; Rust-min batch 6l (2026-05-06): `mod' migrated from Rust to
;; elisp.  Reproduces the previous `bi_mod' contract exactly:
;;   r = euclidean_mod(a, |b|)   (always >= 0)
;;   result = sign(b) * r
;; Built from `/' (NeLisp int-div = trunc toward zero) plus a
;; sign-adjust step.  This matches NeLisp's prior Rust semantics,
;; not host Emacs's pure floor-mod — the two differ only when
;; sign(a) != sign(b), and a codebase grep confirmed no extant
;; caller passes a negative divisor.
;;
;; fix/small-primitives-parity (2026-07-06): the all-integer branch
;; above is left EXACTLY as-is (including its documented divergence
;; from host Emacs on a negative divisor) -- nothing here changes
;; for two integer operands.  The bug was in reusing that same
;; int-shaped formula when either operand is a float: `/' on floats
;; is a true (non-truncating) division, so `n * (/ a n)' collapses
;; back to exactly `a' and every float `mod' silently returned 0
;; (e.g. `(mod 5.5 2)' => 0.0 instead of 1.5).  Floats now take the
;; standard host-Emacs floor-mod formula `a - b * (floor a b)'
;; instead, which:
;;   - matches host Emacs whenever a float operand is involved,
;;     including mixed int/float args and negative operands (see
;;     `test/nelisp-mod-float-test.el' for the value table this was
;;     checked against), and
;;   - naturally produces a NaN result for a zero float-involving
;;     divisor with NO explicit special case: `(floor (/ a 0.0))' is
;;     +-inf, and `b * +-inf' with `b' = 0 is NaN per IEEE 754, so
;;     `a - NaN' is NaN -- exactly host Emacs's `(mod 5.5 0.0)' =>
;;     0.0e+NaN (as opposed to the all-integer branch, which still
;;     signals `arith-error' on a zero divisor, also matching host
;;     Emacs).
(defun mod (a b)
  ;; Types before arithmetic: (mod :key 0) is a TYPE error in Emacs, not an
  ;; arithmetic one -- checking the divisor first reported "Arithmetic
  ;; error" for a call whose problem was the dividend.
  (unless (numberp a) (signal 'wrong-type-argument (list 'number-or-marker-p a)))
  (unless (numberp b) (signal 'wrong-type-argument (list 'number-or-marker-p b)))
  (if (or (floatp a) (floatp b))
      (if (= b 0)
          (/ 0.0 0.0)
        (- a (* b (floor (/ a b)))))
    (progn
      ;; Emacs signals the CONDITION `arith-error', not a generic `error'
      ;; whose message happens to say so -- a handler keys on the condition.
      (when (= b 0) (signal 'arith-error nil))
      (let* ((n (if (< b 0) (- b) b))
             (r (- a (* n (/ a n)))))
        (when (< r 0) (setq r (+ r n)))
        ;; R is now the remainder against |B|, in [0, |B|).  Emacs `mod'
        ;; returns a value with the sign of B, so for a negative B the answer
        ;; is R + B, not -R: `(mod 7 -3)' is -2 (7 = -3 * -3 + -2), and -1 is
        ;; what negating gives.  Zero has no sign to carry and stays 0.
        (if (and (< b 0) (/= r 0)) (+ r b) r)))))

;; `most-positive-fixnum' / `most-negative-fixnum' were unbound (a latent
;; `void-variable' crash: src/nelisp-bytecode.el:1207 already reads
;; `most-positive-fixnum' live, as the "no upper bound" sentinel for a
;; bytecode function's &rest arg count, and would have signalled the
;; moment any compiled function with a &rest parameter was actually
;; called).  MEASURED (2026-08-22), not assumed: `(ash 1 61)' on the
;; standalone wraps to a NEGATIVE value (-2305843009213693952) while
;; `(ash 1 60)' does not, and the exact boundary
;; `(+ (ash 1 60) (- (ash 1 60) 1))' round-trips to 2305843009213693951 --
;; i.e. this runtime's fixnums are Emacs's own 61-bit-magnitude, 62-bit-
;; signed range on a 64-bit host, bit for bit.  Confirmed against Emacs
;; 30.1 directly: `most-positive-fixnum' there is the same
;; 2305843009213693951.
(unless (boundp 'most-positive-fixnum)
  (defconst most-positive-fixnum 2305843009213693951
    "Largest value that is a valid fixnum in this runtime.  See the
comment above this definition for how that value was measured."))
(unless (boundp 'most-negative-fixnum)
  (defconst most-negative-fixnum -2305843009213693952
    "Smallest value that is a valid fixnum in this runtime.  See
`most-positive-fixnum'."))

;; A3: native `equal' never compared vectors element-wise.  Capture native
;; `equal' for the atom/string/number leaves and recurse over cons + vector.
(unless (fboundp 'nelisp--native-equal) (fset 'nelisp--native-equal (symbol-function 'equal)))
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
(unless (fboundp 'nelisp--native-substring) (fset 'nelisp--native-substring (symbol-function 'substring)))
(defun substring (seq &optional from to)
  "Return the SEQ slice [FROM, TO); vector support added (Doc 22 A5).
FROM is OPTIONAL in Emacs -- (substring \"abc\") is \"abc\" -- and requiring
it here turned that call into a `wrong-number-of-arguments'.  Both indices
are checked for integerness BEFORE any range arithmetic, so
(substring \"abcdef\" 12354 \"z\") names \"z\" rather than reporting a range
computed from it."
  (unless (arrayp seq) (signal 'wrong-type-argument (list 'arrayp seq)))
  (when from
    (unless (integerp from) (signal 'wrong-type-argument (list 'integerp from))))
  (when to
    (unless (integerp to) (signal 'wrong-type-argument (list 'integerp to))))
  (setq from (or from 0))
  (if (vectorp seq)
      (let* ((n (length seq))
             (s (if (< from 0) (+ n from) from))
             (e (if to (if (< to 0) (+ n to) to) n))
             ;; The string path signals for an index outside the sequence;
             ;; the vector path used to build a vector of negative length
             ;; instead, so it failed later and somewhere else.
             (_ (when (or (< s 0) (> s n) (< e 0) (> e n) (> s e))
                  (signal 'args-out-of-range (list seq from to))))
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
(unless (fboundp 'nelisp--native-format) (fset 'nelisp--native-format (symbol-function 'format)))

(defun nelisp--digit-char-p (ch) (and (>= ch 48) (<= ch 57)))

;; ---- Doc 159 §3/§4: precision-aware float conversions for `format' ----
;; The native `%f/%e/%g' conversion ignores the precision field, so the
;; prelude `format' below (which delegates the conversion) printed the full
;; default form: `(format "%.2f" 3.14159)' => "3.14159", `%.3e' => "%e".
;; The precision-aware bodies live in the AOT dialect as `nelisp--fmt-float'
;; (= `m5_fmt_float_body', round-half-to-even) rather than as baked prelude
;; elisp, which has a silent size threshold (Doc 159 §4).  Known deviation:
;; the exact-half tie on non-binary-exact values, e.g. `(format "%.1f" 0.05)'
;; => "0.0" vs Emacs "0.1" (the f64->decimal extraction cannot see the
;; sub-ULP excess).

(defun nelisp-format-hexfloat (value &optional precision upcase)
  "Format VALUE as a C99 hexadecimal float: \"0x1.8p+0\", \"0x1p+0\", \"inf\".

PRECISION is the number of hex fraction digits; nil means unspecified, which
strips trailing-zero nibbles (and the point with them).  UPCASE non-nil gives
the %A spelling (\"0X1P+0\", \"INF\").

This is a NeLisp superset with no Emacs counterpart, which is why it is NOT
reachable as `(format \"%a\" ...)': Emacs signals for %a, so accepting it there
would be a divergence pointing the other way.  The formatter is the one
described in Doc 159 sec 11 and matches glibc printf(\"%a\") exactly."
  (unless (numberp value)
    (signal 'wrong-type-argument (list 'numberp value)))
  (when precision
    (unless (integerp precision)
      (signal 'wrong-type-argument (list 'integerp precision)))
    (when (< precision 0)
      (signal 'args-out-of-range (list precision))))
  (nelisp--fmt-float (if (integerp value) (+ value 0.0) value)
                     (if upcase 65 97)
                     (or precision -1)))

(defun nelisp--fmt-epos (s)
  "Index of the exponent marker in S, or nil when there is none.
`inf' and `nan' have no exponent, which is how the `#'-on-%g path below
tells a finite rendering from one it must leave alone."
  (let ((i 0) (n (length s)) (r nil))
    (while (and (null r) (< i n))
      (let ((c (aref s i)))
        (if (or (= c 101) (= c 69)) (setq r i) (setq i (1+ i)))))
    r))

(defun nelisp--format-check-arg (conv arg)
  "Return ARG, or signal the way Emacs does when it cannot match CONV.
The native delegate formats whatever bits it is handed rather than
complaining, so a wrong-typed argument came back as plausible-looking
output instead of an error: (format \"%o\" [1]) answered a raw pointer
value, (format \"%d\" nil) answered 0, and (format \"%c\" \"a\") answered a
NUL.  Emacs signals for every one of those."
  (cond
   ;; d i o x X e E f F g G a A -- any number, integer or float.  A marker is
   ;; NOT accepted: Emacs signals for one too (measured).
   ((or (= conv 100) (= conv 105) (= conv 111) (= conv 120) (= conv 88)
        (= conv 101) (= conv 69) (= conv 102) (= conv 70)
        (= conv 103) (= conv 71) (= conv 97) (= conv 65))
    (unless (numberp arg)
      (signal 'error (list "Format specifier doesn’t match argument type"))))
   ;; c -- an integer, and then a valid character code.  Emacs draws the line
   ;; in two places: a float is a specifier mismatch, a negative integer is
   ;; `(wrong-type-argument characterp -1)'.
   ((= conv 99)
    (unless (integerp arg)
      (signal 'error (list "Format specifier doesn’t match argument type")))
    (unless (and (>= arg 0) (<= arg 4194303))
      (signal 'wrong-type-argument (list 'characterp arg)))))
  arg)

(defun format (template &rest args)
  "Format TEMPLATE with ARGS honoring %[flags][width][.prec]conv (Doc 22 A7).
Width, left-justify (-), zero-pad (0), sign (+/space) and string precision
(.N) are applied in elisp; %f/%F honor their .PRECISION via
`nelisp--ffmt-f' (Doc 159 §3); %S uses `prin1-to-string' and the remaining
conversions are delegated to native `format', which lacks only the field-width
layer."
  (nelisp--check-string template)
  (let ((n (length template)) (i 0) (out "") (argp args))
    (while (< i n)
      (let ((ch (aref template i)))
        (if (= ch 37)                   ; ?%
            (let ((j (1+ i))
                  (f- nil) (f0 nil) (fplus nil) (fspace nil) (fhash nil)
                  (width nil) (prec nil) (scan t))
              (while (and scan (< j n))
                (let ((c (aref template j)))
                  (cond
                   ((= c 45) (setq f- t j (1+ j)))        ; -
                   ((= c 48) (setq f0 t j (1+ j)))        ; 0
                   ((= c 43) (setq fplus t j (1+ j)))     ; +
                   ((= c 32) (setq fspace t j (1+ j)))    ; space
                   ((= c 35) (setq fhash t j (1+ j)))     ; #
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
                  ;; Emacs does not pass a dangling "%" through: "%", "%5",
                  ;; "%-" and "%." all signal.
                  (signal 'error
                          (list "Format string ends in middle of format specifier"))
                (let ((conv (aref template j)))
                  (setq i (1+ j))
                  ;; Emacs accepts s S d i o x X e f g c and %% -- everything
                  ;; else is "Invalid format operation".  This accepted the C
                  ;; uppercase spellings (%E %F %G) because the conversion char
                  ;; was handed straight to the native delegate, and answered
                  ;; the SPEC STRING itself ("%b", "%z") for the ones the
                  ;; delegate did not know either.
                  ;;
                  ;; `a'/`A' are rejected too, and that is a deliberate
                  ;; NARROWING: the C99 hex-float conversions used to be a
                  ;; NeLisp superset (Doc 159 sec 11).  Parity with Emacs won
                  ;; -- a program that runs here and signals there is a
                  ;; divergence whichever direction it points.  The hex-float
                  ;; formatter itself is still in the AOT dialect
                  ;; (`m5_fmt_hexfloat'), it just has no route through
                  ;; `format' any more.
                  (unless (or (= conv 115) (= conv 83) (= conv 100) (= conv 105)
                              (= conv 111) (= conv 120) (= conv 88) (= conv 101)
                              (= conv 102) (= conv 103) (= conv 99) (= conv 37))
                    (signal 'error
                            (list (concat "Invalid format operation %"
                                          (char-to-string conv)))))
                  (if (= conv 37)        ; ?%
                      (setq out (concat out "%"))
                    (let* ((arg (nelisp--format-check-arg conv (car argp)))
                           (body (if (and (numberp arg)
                                          (or (= conv 102) (= conv 70)   ; f F
                                              (= conv 101) (= conv 69)   ; e E
                                              (= conv 103) (= conv 71)   ; g G
                                              ;; a/A can no longer reach here:
                                              ;; the conversion check above
                                              ;; rejects them for Emacs parity.
                                              ;; Kept so the routing is still
                                              ;; visible if %a is ever exposed
                                              ;; through a non-`format' entry
                                              ;; point (Doc 159 sec 11).
                                              (= conv 97) (= conv 65)))  ; a A
                                     (nelisp--fmt-float
                                      (if (integerp arg) (+ arg 0.0) arg)
                                      conv (if (or (= conv 97) (= conv 65))
                                               (or prec -1) (or prec 6)))
                                   ;; %d/%i/%o/%x/%X of a float: truncate toward
                                   ;; zero like Emacs (native reads the raw bits
                                   ;; otherwise).  Doc 159 §13.
                                    (if (= conv 83) ; ?S
                                        (prin1-to-string arg)
                                      (nelisp--native-format
                                       (concat "%" (char-to-string conv))
                                       (if (and (floatp arg)
                                                (or (= conv 100) (= conv 105) (= conv 111)
                                                    (= conv 120) (= conv 88)))
                                           (truncate arg) arg))))))
                      (setq argp (cdr argp))
                      ;; `#' on %g keeps the trailing zeros %g strips, and
                      ;; keeps the point even when nothing follows it
                      ;; ("%#.1g" of 1.0 is "1.").  C picks %f or %e from the
                      ;; exponent, so read the exponent back out of an %e
                      ;; rendering instead of guessing it; inf/nan have none
                      ;; and are left alone.
                      (when (and fhash (= conv 103) (numberp arg))
                        (let* ((v (if (integerp arg) (+ arg 0.0) arg))
                               (p (if prec (if (= prec 0) 1 prec) 6))
                               (estr (nelisp--fmt-float v 101 (1- p)))
                               (epos (nelisp--fmt-epos estr)))
                          (when epos
                            (let* ((x (string-to-number (substring estr (1+ epos))))
                                   (usef (and (< x p) (>= x -4)))
                                   (fp (if usef (- p 1 x) (1- p)))
                                   (str (if usef (nelisp--fmt-float v 102 fp) estr)))
                              (when (= fp 0)
                                (setq str (if usef
                                              (concat str ".")
                                            (concat (substring str 0 epos) "."
                                                    (substring str epos)))))
                              (setq body str)))))
                      (when (and prec (or (= conv 115) (= conv 83))   ; s S
                                 (> (length body) prec))
                        (setq body (substring body 0 prec)))
                      ;; `#' is the alternate form, and it reaches only o/x/X
                      ;; here: Emacs writes "0xa" / "0XFF" / "010", omits the
                      ;; prefix for a zero value, and puts it AFTER the sign
                      ;; ("-0xa").  The native delegate does not implement `#'
                      ;; at all -- it hands back the spec string unchanged --
                      ;; so the prefix has to be built here.
                      (when (and fhash (or (= conv 111) (= conv 120) (= conv 88))
                                 (> (length body) 0))
                        (let* ((neg (= (aref body 0) 45))
                               (mag (if neg (substring body 1) body)))
                          (unless (or (string= mag "0") (= (length mag) 0))
                            (setq mag (cond ((= conv 120) (concat "0x" mag))
                                            ((= conv 88) (concat "0X" mag))
                                            ((= (aref mag 0) 48) mag)
                                            (t (concat "0" mag))))
                            (setq body (if neg (concat "-" mag) mag)))))
                      ;; `+' and a leading space apply to the integer
                      ;; conversions as well -- "%+05x" of 10 is "+000a" in
                      ;; Emacs, and o/x/X/i were all missing from this list.
                      (when (and (or (= conv 100) (= conv 105) (= conv 111)
                                     (= conv 120) (= conv 88)
                                     (= conv 102) (= conv 101) (= conv 103))
                                 (> (length body) 0) (not (= (aref body 0) 45)))
                        (cond (fplus (setq body (concat "+" body)))
                              (fspace (setq body (concat " " body)))))
                      (when (and width (< (length body) width))
                        (let ((pad (- width (length body))))
                          (cond
                           (f- (setq body (concat body (make-string pad 32))))
                           ((and f0
                                 ;; d/i/o/x/X always produce digits, so `0'
                                 ;; always applies.  The old guard asked whether
                                 ;; the first character was a DECIMAL digit,
                                 ;; which is false for hex output beginning a-f:
                                 ;; "%02x" of 10 space-padded to " a" where
                                 ;; Emacs writes "0a".  For f/e/g the guard is
                                 ;; still needed -- inf/nan space-pad (Doc 159
                                 ;; §13).
                                 (if (or (= conv 100) (= conv 105) (= conv 111)
                                         (= conv 120) (= conv 88))
                                     t
                                   (and (or (= conv 102) (= conv 101) (= conv 103))
                                        (let ((k (if (and (> (length body) 0)
                                                          (or (= (aref body 0) 45)
                                                              (= (aref body 0) 43)
                                                              (= (aref body 0) 32)))
                                                     1 0)))
                                          (and (< k (length body))
                                               (>= (aref body k) 48)
                                               (<= (aref body k) 57))))))
                            ;; The zeros go after the sign AND after a `#'
                            ;; prefix: Emacs writes "-00ff" and "0x00a", never
                            ;; "00-ff" or "000xa".
                            (let ((k 0))
                              (when (and (> (length body) 0)
                                         (or (= (aref body 0) 45) (= (aref body 0) 43)
                                             (= (aref body 0) 32)))
                                (setq k 1))
                              (when (and (or (= conv 120) (= conv 88))
                                         (> (length body) (+ k 1))
                                         (= (aref body k) 48)
                                         (or (= (aref body (+ k 1)) 120)
                                             (= (aref body (+ k 1)) 88)))
                                (setq k (+ k 2)))
                              (setq body (concat (substring body 0 k)
                                                 (make-string pad 48)
                                                 (substring body k)))))
                           (t (setq body (concat (make-string pad 32) body))))))
                      (setq out (concat out body)))))))
          (setq out (concat out (char-to-string ch)) i (1+ i)))))
    out))

;; A13: `type-of' is VOID on the bare reader (returns nil for everything), and
;; native `functionp' fails to recognise a `(lambda ...)' / `(closure ...)' /
;; `(builtin ...)' cons (returns nil).  Provide a predicate-composed `type-of'
;; and a corrected `functionp' so function type-dispatch works.
(unless (fboundp 'nelisp--native-functionp) (fset 'nelisp--native-functionp (symbol-function 'functionp)))
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

;; ---- Doc 22 reader-core gap fix A9 + with-output-to-string ----
;;
;; native princ/prin1/terpri ignore a buffer/function STREAM and the
;; `standard-output' variable (which was VOID).  `standard-output' is now
;; declared special so a dynamic `let' binding is visible to these functions
;; (dynamic binding DOES work on the bare reader -- only `symbol-value' of a
;; let-bound special crashes, A8, which we avoid).  We honor a FUNCTION stream
;; (called once per character, host contract); buffers do not retain inserted
;; text on the bare reader so the buffer branch is best-effort only.  This
;; unblocks `with-output-to-string' (Doc 16 round 12).

(defvar standard-output nil
  "Output stream for `princ'/`prin1'/`print'/`terpri' (Doc 22 A9).")

(unless (fboundp 'nelisp--native-princ) (fset 'nelisp--native-princ (symbol-function 'princ)))
(unless (fboundp 'nelisp--native-prin1) (fset 'nelisp--native-prin1 (symbol-function 'prin1)))
(unless (fboundp 'nelisp--native-terpri) (fset 'nelisp--native-terpri (symbol-function 'terpri)))

(defun nelisp--emit-to-stream (str stream)
  "Send string STR to STREAM: function = one funcall per character;
buffer = best-effort insert; nil/t/other = native stdout."
  (cond
   ((functionp stream)
    (let ((i 0) (n (length str)))
      (while (< i n) (funcall stream (aref str i)) (setq i (1+ i)))))
   ((bufferp stream)
    (with-current-buffer stream (insert str)))
   (t (nelisp--native-princ str))))

(defun princ (object &optional stream)
  ;; A STREAM that is not a function is `invalid-function' in Emacs -- this
  ;; ignored it and printed to stdout, so output went somewhere the caller
  ;; did not ask for and nothing said so.
  "Print OBJECT with no quoting to STREAM or `standard-output' (Doc 22 A9)."
  (when (and stream (not (eq stream t)) (not (functionp stream)))
    (signal (if (symbolp stream) 'void-function 'invalid-function)
            (list stream)))
  (let ((s (or stream standard-output)))
    (if (or (null s) (eq s t))
        (nelisp--native-princ object)
      (nelisp--emit-to-stream
       (if (stringp object) object (nelisp--native-format "%s" object)) s)))
  object)

(defun prin1 (object &optional stream overrides)
  ;; A STREAM that is not a function is `invalid-function' in Emacs -- this
  ;; ignored it and printed to stdout, so output went somewhere the caller
  ;; did not ask for and nothing said so.
  "Print OBJECT in read syntax to STREAM or `standard-output' (Doc 22 A9)."
  (nelisp--check-print-overrides overrides)
  (when (and stream (not (eq stream t)) (not (functionp stream)))
    (signal (if (symbolp stream) 'void-function 'invalid-function)
            (list stream)))
  (let ((s (or stream standard-output)))
    (if (or (null s) (eq s t))
        (nelisp--native-prin1 object)
      (nelisp--emit-to-stream (prin1-to-string object) s)))
  object)

(defun terpri (&optional stream _ensure)
  "Output a newline to STREAM or `standard-output' (Doc 22 A9).
The LATER definition is the live one -- an earlier copy in this file
already had this check and it never ran.

ENSURE is accepted and not acted on: Emacs only skips the newline when it
can see the output COLUMN, which it can for a buffer or a marker, and this
runtime has neither.  Measured directly, every non-function stream is
`invalid-function' with ENSURE set or not."
  (when (and stream (not (functionp stream)) (not (eq stream t)))
    (signal (if (symbolp stream) 'void-function 'invalid-function) (list stream)))
  (let ((s (or stream standard-output)))
    (if (or (null s) (eq s t))
        (nelisp--native-terpri)
      (nelisp--emit-to-stream "\n" s)))
  t)

(unless (fboundp 'print)
  (defun print (object &optional stream)
    (when (and stream (not (eq stream t)) (not (functionp stream)))
      ;; A SYMBOL stream is looked up as a function first, so it reports
      ;; `void-function'; anything else is `invalid-function'.
      (signal (if (symbolp stream) 'void-function 'invalid-function)
              (list stream)))
    "Output OBJECT in read syntax, surrounded by newlines (Doc 22 A9)."
    (terpri stream)
    (prin1 object stream)
    (terpri stream)
    object))

(unless (fboundp 'write-char)
  (defun write-char (character &optional stream)
  (unless (integerp character) (signal 'wrong-type-argument (list 'fixnump character)))
    (unless (integerp character) (signal 'wrong-type-argument (list 'fixnump character)))
    (when (and stream (not (functionp stream)))
      ;; A SYMBOL stream is looked up as a function first, so it reports
      ;; `void-function'; anything else is `invalid-function'.
      (signal (if (symbolp stream) 'void-function 'invalid-function)
              (list stream)))
    "Output CHARACTER to STREAM or `standard-output' (Doc 22 A9)."
    (let ((s (or stream standard-output)))
      (if (or (null s) (eq s t))
          (nelisp--native-princ (char-to-string character))
        (nelisp--emit-to-stream (char-to-string character) s)))
    character))

(unless (fboundp 'with-output-to-string)
  (defmacro with-output-to-string (&rest body)
    "Execute BODY with `standard-output' bound to a string accumulator and
return the accumulated output (Doc 22 A9)."
    `(let ((nelisp--wos-acc ""))
       (let ((standard-output
              (lambda (nelisp--wos-ch)
                (setq nelisp--wos-acc
                      (concat nelisp--wos-acc (char-to-string nelisp--wos-ch))))))
         ,@body)
       nelisp--wos-acc)))

;; ---- Doc 22 reader-core gap fix A8 partial: `dlet' ----
;;
;; Dynamic binding works on the bare reader (a function sees a `let' binding of
;; a `defvar'-declared special, and `let' unwinds it correctly), so `dlet' is
;; just a matter of declaring each variable special before the `let'.  The only
;; residual limitation is that `symbol-value' of a dlet-bound variable still
;; returns the GLOBAL value rather than the dynamic binding (bare reader core
;; limitation); read such variables with a direct reference instead.
(unless (fboundp 'dlet)
  (defmacro dlet (binders &rest body)
    "Like `let' but bind each variable dynamically (Doc 22 A8).
Each variable is `defvar'-declared special first so the binding is visible
across function calls.  NB: reading a dlet-bound variable via `symbol-value'
returns the global value on the bare reader; use a direct reference."
    (let ((decls nil) (b binders))
      (while b
        (let ((e (car b)))
          (setq decls (cons (list 'defvar (if (consp e) (car e) e)) decls)))
        (setq b (cdr b)))
      (append (list 'progn) (nreverse decls)
              (list (append (list 'let binders) body))))))

;; ---------------------------------------------------------------------
;; Hooks: add-hook / remove-hook / run-hooks / run-hook-with-args /
;; run-hook-with-args-until-success / run-hook-with-args-until-failure.
;;
;; Emacs 30 semantics over ordinary symbol values, measured against
;; Emacs 30.1 (2026-08-22).  There are no buffers in this runtime, so a
;; hook variable cannot have a value distinct per buffer: `add-hook' and
;; `remove-hook' accept LOCAL and IGNORE it.  This is a DOCUMENTED
;; DIVERGENCE from Emacs, not an oversight -- a silent no-op is the
;; honest choice precisely because the local/global split LOCAL exists
;; to select cannot exist here at all.  Emacs would instead call
;; `make-local-variable' on HOOK and splice a `t' marker into the new
;; buffer-local value (that marker is still handled below, because a
;; hand-built hook list can contain one even without buffer-locals).
;;
;; DEPTH ordering (`add-hook'): default depth 0; a plain (non-`t', non-
;; integer) DEPTH argument is Emacs's documented backward-compatibility
;; case and means 90.  Insertion keeps the hook's list sorted ascending
;; by depth; a NEW function at the same depth as an existing one goes
;; AFTER it when DEPTH is strictly positive and BEFORE it otherwise
;; (Emacs's own wording) -- which is why two `(add-hook 'h f)' calls at
;; the shared default depth 0 leave the most-recently-added function
;; FIRST.  Emacs does not store per-function depths in the hook's own
;; list value (there is no room: the list holds functions and, DEPTH-
;; blind, the `t' marker) -- they live in a private table, and neither
;; does this runtime: `nelisp--hook-depths' maps HOOK to an alist of
;; (FUNCTION . DEPTH).  A function already present in the hook's value
;; with no recorded depth (spliced in by hand, not via `add-hook') is
;; treated as depth 0, Emacs's own documented default.  Re-adding a
;; function that is already a member is a no-op -- notably, it does NOT
;; move the function to a newly-requested depth; `add-hook' checks
;; membership before it ever looks at DEPTH (measured: adding a function
;; at depth -10, then again at depth 50, leaves it exactly where the
;; first call put it).
;;
;; The `t' element (Emacs: "run the global value here too") is honored
;; even though it can only ever mean "run this same value again": with
;; no buffer-local/global split, a hook's own value doubles as its own
;; "global value".  Measured against Emacs 30.1 with a plain (non-
;; buffer-local) hook list containing `t': the first pass runs the list
;; once; each `t' met on the first pass triggers ONE re-run of the
;; WHOLE value from its start (fetched once and cached, then re-walked
;; -- not re-fetched -- on every subsequent `t'), and any `t' met DURING
;; that re-run is skipped rather than triggering a further expansion (or
;; this would never terminate).  `(fn1 t fn2 t)' therefore calls fn1
;; three times and fn2 three times, in the order fn1 fn1 fn2 fn2 fn1
;; fn2 -- reproduced exactly by `nelisp--run-hook-value' below.
(unless (boundp 'nelisp--hook-depths)
  (defvar nelisp--hook-depths (make-hash-table :test 'eq)
    "HOOK symbol -> alist of (FUNCTION . DEPTH), for `add-hook' ordering."))

(unless (fboundp 'nelisp--hook-list-p)
  (defun nelisp--hook-list-p (val)
    "Return non-nil if VAL is a \"list of hook functions\" shape.
A hook value is instead a SINGLE function to call directly when it
satisfies `functionp' (this is what keeps a raw lambda form or closure
-- itself a cons -- from being walked as a list of functions) or is not
a cons at all (typically a symbol naming a function)."
    (and (consp val) (not (functionp val)))))

(unless (fboundp 'add-hook)
  (defun add-hook (hook function &optional depth local)
    "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present (`equal').  See Emacs's
`add-hook' for DEPTH; LOCAL is accepted and ignored -- see the block
comment above this definition for why that is the honest behavior here.

(fn HOOK FUNCTION &optional DEPTH LOCAL)"
    (ignore local)
    (let ((d (cond ((null depth) 0) ((integerp depth) depth) (t 90))))
      (unless (boundp hook) (set hook nil))
      (let ((val (symbol-value hook)))
        (when (and val (not (nelisp--hook-list-p val)))
          (setq val (list val)))
        (unless (member function val)
          (let ((depths (gethash hook nelisp--hook-depths))
                (before nil) (cur val) (done nil))
            (while (and cur (not done))
              (let ((fd (or (cdr (assoc (car cur) depths)) 0)))
                (if (or (> fd d) (and (= fd d) (<= d 0)))
                    (setq done t)
                  (push (car cur) before)
                  (setq cur (cdr cur)))))
            (setq val (append (nreverse before) (list function) cur))
            (puthash hook (cons (cons function d) depths) nelisp--hook-depths))
          (set hook val))))
    nil))

(unless (fboundp 'remove-hook)
  (defun remove-hook (hook function &optional local)
    "Remove from the value of HOOK the function FUNCTION.
LOCAL is accepted and ignored; see `add-hook'.

(fn HOOK FUNCTION &optional LOCAL)"
    (ignore local)
    (when (boundp hook)
      (let ((val (symbol-value hook)))
        (if (not (nelisp--hook-list-p val))
            (when (equal val function) (set hook nil))
          (when (member function val)
            (let (acc)
              (dolist (f val) (unless (equal f function) (push f acc)))
              (set hook (nreverse acc)))
            (let ((depths (gethash hook nelisp--hook-depths)))
              (when depths
                (let (kept)
                  (dolist (pair depths)
                    (unless (equal (car pair) function) (push pair kept)))
                  (puthash hook (nreverse kept) nelisp--hook-depths))))))))
    nil))

(unless (fboundp 'nelisp--run-hook-call)
  (defun nelisp--run-hook-call (fn args mode)
    "Call FN with ARGS; for MODE `until-success'/`until-failure', `throw'
to the `nelisp--run-hook' tag with the short-circuit result once FN's
return value decides the outcome.  Always returns nil (the caller reads
outcomes only through the throw or the final return of the walk)."
    (let ((r (apply fn args)))
      (cond
       ((eq mode 'until-success) (when r (throw 'nelisp--run-hook r)))
       ((eq mode 'until-failure) (unless r (throw 'nelisp--run-hook nil)))))
    nil))

(unless (fboundp 'nelisp--run-hook-value)
  (defun nelisp--run-hook-value (val args mode)
    "Run hook value VAL against ARGS per MODE (`all' / `until-success' /
`until-failure') and return the MODE-appropriate result.  See the block
comment above `add-hook' for the `t'-element algorithm this reproduces."
    (catch 'nelisp--run-hook
      (cond
       ((null val) (if (eq mode 'until-failure) t nil))
       ((not (nelisp--hook-list-p val))
        (nelisp--run-hook-call val args mode)
        nil)
       (t
        (let ((global nil) (have-global nil) (cur val))
          (while cur
            (let ((elt (car cur)))
              (if (eq elt t)
                  (progn
                    (unless have-global
                      (setq have-global t)
                      (setq global (if (nelisp--hook-list-p val) val (list val))))
                    (let ((gcur global))
                      (while gcur
                        (unless (eq (car gcur) t)
                          (nelisp--run-hook-call (car gcur) args mode))
                        (setq gcur (cdr gcur)))))
                (nelisp--run-hook-call elt args mode)))
            (setq cur (cdr cur)))
          (if (eq mode 'until-failure) t nil)))))))

(unless (fboundp 'run-hooks)
  (defun run-hooks (&rest hooks)
    "Run each hook in HOOKS.  Each argument should be a symbol, a hook
variable; a void hook variable is treated as nil (a no-op), not an
error.  See `add-hook' for what a hook's value may be.

(fn &rest HOOKS)"
    (dolist (hook hooks)
      (nelisp--run-hook-value (if (boundp hook) (symbol-value hook) nil) nil 'all))
    nil))

(unless (fboundp 'run-hook-with-args)
  (defun run-hook-with-args (hook &rest args)
    "Run HOOK with the specified arguments ARGS.  The final return value
is unspecified, matching Emacs.  A void HOOK is a no-op.

(fn HOOK &rest ARGS)"
    (nelisp--run-hook-value (if (boundp hook) (symbol-value hook) nil) args 'all)
    nil))

(unless (fboundp 'run-hook-with-args-until-success)
  (defun run-hook-with-args-until-success (hook &rest args)
    "Run HOOK with ARGS, stopping at the first function that returns
non-nil, and return that value.  Return nil if all functions return
nil, if there are none to call, or if HOOK is void.

(fn HOOK &rest ARGS)"
    (nelisp--run-hook-value (if (boundp hook) (symbol-value hook) nil) args 'until-success)))

(unless (fboundp 'run-hook-with-args-until-failure)
  (defun run-hook-with-args-until-failure (hook &rest args)
    "Run HOOK with ARGS, stopping at the first function that returns
nil, and return nil.  Otherwise (all functions return non-nil, there
are none to call, or HOOK is void) return non-nil.

(fn HOOK &rest ARGS)"
    (nelisp--run-hook-value (if (boundp hook) (symbol-value hook) nil) args 'until-failure)))

;; ---------------------------------------------------------------------
;; map.el subset: map-elt / map-put! / map-delete / map-keys / map-values
;; / map-pairs / map-length / map-do / mapp, over alists, plists (Emacs
;; 27+ rule: a cons whose car is not itself a cons, i.e. not an alist
;; pair, is treated as a plist) and hash-tables.
;;
;; `map-put!' is the one place Emacs's own contract is NOT "always
;; mutate": measured against Emacs 30.1, `map-put!' on a PLAIN alist
;; VALUE (not a place `setf' can reassign) mutates in place -- via
;; `setcdr' on the matching pair -- only when KEY is already present.
;; Adding a NEW key to an alist means consing a new pair onto the
;; FRONT, which needs to replace the list's head; a bare function
;; cannot do that to its caller's variable, so Emacs signals
;; `map-not-inplace' rather than silently doing nothing (Emacs's own
;; docstring: "If it cannot [modify MAP in place], it signals the
;; `map-not-inplace' error.  To insert an element without modifying
;; MAP, use `map-insert'.").  A PLIST is different: a new key/value
;; pair can be NCONC'd onto the END of the existing cons chain, which
;; mutates the last cons's cdr and needs no new head -- so `map-put!'
;; on a plist never signals for a new key.  A hash-table always mutates
;; via `puthash' and never signals.  `map-put!' returns VALUE (the
;; third argument) in every non-signaling case -- not the map -- this
;; is Emacs's own documented return, not a shortcut taken here.
;;
;; `map-delete' is documented by Emacs itself as NOT reliably
;; destructive for a list-backed map: "if MAP is a list ... and you're
;; deleting the [element that empties it, e.g. the sole/first element],
;; the list isn't actually destructively modified ... So if you're
;; using this on a list, you have to say (setq map (map-delete map
;; key))".  This implementation takes Emacs at its documented word
;; instead of chasing the partial, position-dependent in-place splicing
;; its C-free `defun' does for other cases: alist/plist deletion here
;; always returns a new list and never mutates the original cons chain.
;; Every well-behaved caller was already going to reassign from the
;; return value per Emacs's own advice, so this is a documented
;; narrowing, not a functional gap.  Hash-table deletion mutates via
;; `remhash' and returns the (same) table, matching Emacs exactly.
(unless (get 'map-not-inplace 'error-conditions)
  (define-error 'map-not-inplace "Cannot modify map in-place"))

(unless (fboundp 'nelisp--plist-p)
  (defun nelisp--plist-p (val)
    "Return non-nil if VAL looks like a plist rather than an alist.
Emacs 27+'s map.el rule: a non-empty list is a plist when its first
element is not itself a cons (an alist's elements are (KEY . VALUE)
pairs, so an alist's CAR is always a cons)."
    (and (consp val) (not (consp (car val))))))

(unless (fboundp 'mapp)
  (defun mapp (map)
    "Return non-nil if MAP is a map (alist/plist, hash-table, array, ...).

(fn MAP)"
    (or (listp map) (hash-table-p map) (arrayp map))))

(unless (fboundp 'map-elt)
  (defun map-elt (map key &optional default testfn)
    "Look up KEY in MAP and return its associated value, or DEFAULT.
MAP is an alist, a plist, or a hash-table.  TESTFN, if non-nil, is used
in place of `equal' to compare KEY against an alist's/plist's keys (a
hash-table always uses its own `:test').

(fn MAP KEY &optional DEFAULT TESTFN)"
    (cond
     ((hash-table-p map) (gethash key map default))
     ((nelisp--plist-p map)
      (let ((cur map) (found nil) (result default))
        (while (and cur (not found))
          (if (funcall (or testfn #'eq) (car cur) key)
              (progn (setq result (cadr cur)) (setq found t))
            (setq cur (cddr cur))))
        result))
     (t
      (let ((cur map) (found nil) (result default))
        (while (and cur (not found))
          (if (funcall (or testfn #'equal) (caar cur) key)
              (progn (setq result (cdar cur)) (setq found t))
            (setq cur (cdr cur))))
        result)))))

(unless (fboundp 'map-put!)
  (defun map-put! (map key value &optional testfn)
    "Associate KEY with VALUE in MAP, modifying MAP in place, and return
VALUE.  Signals `map-not-inplace' when MAP is an alist and KEY is not
already present -- see the block comment above this section.

(fn MAP KEY VALUE &optional TESTFN)"
    (cond
     ((hash-table-p map) (puthash key value map))
     ((nelisp--plist-p map)
      (let ((cur map) (found nil))
        (while (and cur (not found))
          (if (funcall (or testfn #'eq) (car cur) key)
              (progn (setcar (cdr cur) value) (setq found t))
            (setq cur (cddr cur))))
        (unless found
          (let ((last map))
            (while (cddr last) (setq last (cddr last)))
            (setcdr (cdr last) (list key value))))))
     (t
      (let ((cur map) (found nil))
        (while (and cur (not found))
          (if (funcall (or testfn #'equal) (caar cur) key)
              (progn (setcdr (car cur) value) (setq found t))
            (setq cur (cdr cur))))
        (unless found (signal 'map-not-inplace (list map))))))
    value))

(unless (fboundp 'map-delete)
  (defun map-delete (map key &optional testfn)
    "Delete KEY from MAP and return the resulting map.
For a hash-table this mutates MAP (via `remhash') and returns MAP
itself.  For an alist/plist this ALWAYS returns a new list -- see the
block comment above this section for why that, not partial in-place
splicing, is the honest match for Emacs's own documented contract.

(fn MAP KEY &optional TESTFN)"
    (cond
     ((hash-table-p map) (remhash key map) map)
     ((nelisp--plist-p map)
      (let (acc (cur map))
        (while cur
          (if (funcall (or testfn #'eq) (car cur) key)
              (setq cur (cddr cur))
            (push (car cur) acc) (push (cadr cur) acc) (setq cur (cddr cur))))
        (nreverse acc)))
     (t
      (let (acc)
        (dolist (pair map)
          (unless (funcall (or testfn #'equal) (car pair) key) (push pair acc)))
        (nreverse acc))))))

(unless (fboundp 'map-keys)
  (defun map-keys (map)
    "Return the list of keys in MAP.

(fn MAP)"
    (cond
     ((hash-table-p map) (let (ks) (maphash (lambda (k _v) (push k ks)) map) (nreverse ks)))
     ((nelisp--plist-p map)
      (let (ks (cur map)) (while cur (push (car cur) ks) (setq cur (cddr cur))) (nreverse ks)))
     (t (mapcar #'car map)))))

(unless (fboundp 'map-values)
  (defun map-values (map)
    "Return the list of values in MAP.

(fn MAP)"
    (cond
     ((hash-table-p map) (let (vs) (maphash (lambda (_k v) (push v vs)) map) (nreverse vs)))
     ((nelisp--plist-p map)
      (let (vs (cur map)) (while cur (push (cadr cur) vs) (setq cur (cddr cur))) (nreverse vs)))
     (t (mapcar #'cdr map)))))

(unless (fboundp 'map-pairs)
  (defun map-pairs (map)
    "Return the elements of MAP as a list of (KEY . VALUE) pairs.

(fn MAP)"
    (cond
     ((hash-table-p map)
      (let (ps) (maphash (lambda (k v) (push (cons k v) ps)) map) (nreverse ps)))
     ((nelisp--plist-p map)
      (let (ps (cur map))
        (while cur (push (cons (car cur) (cadr cur)) ps) (setq cur (cddr cur)))
        (nreverse ps)))
     (t (copy-sequence map)))))

(unless (fboundp 'map-length)
  (defun map-length (map)
    "Return the number of elements in MAP.

(fn MAP)"
    (cond
     ((hash-table-p map) (hash-table-count map))
     ((nelisp--plist-p map) (/ (length map) 2))
     (t (length map)))))

(unless (fboundp 'map-do)
  (defun map-do (function map)
    "Call FUNCTION with two arguments KEY and VALUE for each element in MAP.

(fn FUNCTION MAP)"
    (cond
     ((hash-table-p map) (maphash function map))
     ((nelisp--plist-p map)
      (let ((cur map))
        (while cur (funcall function (car cur) (cadr cur)) (setq cur (cddr cur)))))
     (t (dolist (pair map) (funcall function (car pair) (cdr pair)))))
    nil))
