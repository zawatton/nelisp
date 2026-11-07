import subprocess,sys,json,concurrent.futures as cf,os
S=sys.argv[1]; BIN='./target/nelisp'
PKGS=[f'packages/{p}/src' for p in 'nl-prelude nl-condition nl-num nl-ns nl-hygiene nl-static nl-typecheck nl-contract nl-safe nl-clj nl-signal nl-ffi nl-libffi nelisp-actor nelisp-control-flow nelisp-process nelisp-network'.split()]
T=[
# control
('tagbody/go','(let ((i 0)) (cl-tagbody top (setq i (1+ i)) (when (< i 3) (go top))) i)'),
('block/return-from','(cl-block x (cl-return-from x 7))'),
('true multiple values','(cl-multiple-value-list (cl-values 1 2))'),
('mv is list(not distinct)','(cl-values 1 2)'),
('tail call 1e7 interp','(progn (fset (quote tc1) (lambda (n) (if (= n 0) (quote ok) (tc1 (1- n))))) (tc1 10000000))'),
('deep recursion recoverable','(condition-case e (progn (fset (quote rc1) (lambda (n) (1+ (rc1 n)))) (rc1 0)) (error (car e)))'),
('symbol-macrolet','(cl-symbol-macrolet ((a 42)) (+ a 1))'),
('macrolet','(cl-macrolet ((m (x) (list (quote 1+) x))) (m 1))'),
('labels','(cl-labels ((f (n) (if (= n 0) 0 (f (1- n))))) (f 5))'),
('progv','(progn (defvar pv1 nil) (cl-progv (quote (pv1)) (quote (5)) pv1))'),
('compiler-macro','(progn (cl-define-compiler-macro cm1 (x) x) (fboundp (quote cl-define-compiler-macro)))'),
('load-time-value','(cl-load-time-value (+ 1 2))'),
('eval-when','(cl-eval-when (eval) 1)'),
('destructuring-bind','(cl-destructuring-bind (a (b &key c) &rest d) (quote (1 (2 :c 3) 4)) (list a b c d))'),
('defsetf/gv','(let ((l (list 1))) (setf (car l) 2) (cl-incf (car l)) l)'),
('define-modify-macro eq','(fboundp (quote gv-define-setter))'),
('deftype/typep','(progn (cl-deftype my-int () (quote (integer 0 10))) (list (cl-typep 5 (quote my-int)) (cl-typep 50 (quote my-int))))'),
('typep compound','(list (cl-typep 1 (quote (or string integer))) (cl-typep "a" (quote (satisfies stringp))))'),
('subtypep','(list (fboundp (quote cl-subtypep)) (fboundp (quote subtypep)))'),
('loop complex','(cl-loop for i from 1 to 3 for j in (quote (a b c)) collect (cons i j) into r finally return r)'),
('format-loop for-hash','(let ((h (make-hash-table))) (puthash 1 2 h) (cl-loop for k being the hash-keys of h using (hash-values v) collect (list k v)))'),
# conditions
('handler-bind(cl-)','(list (fboundp (quote handler-bind)) (fboundp (quote nl-handler-bind)))'),
('handler-bind runs before unwind','(let (r) (condition-case nil (handler-bind ((error (lambda (e) (setq r (quote seen))))) (error "x")) (error nil)) r)'),
('nl restart-case','(progn (require (quote nl-condition)) (nl-restart-case (nl-invoke-restart (quote foo) 5) (foo (x) (* x 2))))'),
('condition hierarchy define-error','(progn (define-error (quote e1) "e1" (quote arith-error)) (condition-case e (signal (quote e1) nil) (arith-error (quote ok))))'),
('warn','(fboundp (quote warn))'),
('debugger interactive','(list (fboundp (quote debug)) (boundp (quote debugger)) (fboundp (quote backtrace-frames)) (fboundp (quote backtrace-frame)) (fboundp (quote mapbacktrace)))'),
# clos
('defclass','(progn (require (quote eieio)) (eval (quote (defclass c1 () ((a :initarg :a :accessor c1-a :initform 3)))) t) (c1-a (make-instance (quote c1))))'),
('slot-value/with-slots','(progn (require (quote eieio)) (eval (quote (defclass c2 () ((a :initarg :a)))) t) (let ((o (make-instance (quote c2) :a 1))) (with-slots (a) o (list a (slot-value o (quote a))))))'),
('multiple inheritance','(progn (require (quote eieio)) (eval (quote (progn (defclass m1 () ()) (defclass m2 () ()) (defclass m3 (m1 m2) ()))) t) (list (object-of-class-p (make-instance (quote m3)) (quote m2))))'),
('defgeneric multi-dispatch','(progn (cl-defgeneric g1 (a b)) (cl-defmethod g1 ((a integer) (b string)) (quote is)) (cl-defmethod g1 ((a string) (b integer)) (quote si)) (list (g1 1 "x") (g1 "x" 1)))'),
('around/before/after','(let (l) (cl-defgeneric g2 (a)) (cl-defmethod g2 ((a integer)) (push 1 l) (quote p)) (cl-defmethod g2 :before ((a integer)) (push 0 l)) (cl-defmethod g2 :after ((a integer)) (push 2 l)) (cl-defmethod g2 :around ((a integer)) (push 3 l) (cl-call-next-method)) (list (g2 1) l))'),
('eql specializer','(progn (cl-defmethod g3 ((a (eql 5))) (quote five)) (g3 5))'),
('change-class','(list (fboundp (quote change-class)) (fboundp (quote cl-change-class)))'),
('redefine class updates instances','(list (fboundp (quote update-instance-for-redefined-class)))'),
('define-method-combination','(list (fboundp (quote define-method-combination)) (fboundp (quote cl-define-method-combination)))'),
('MOP','(list (fboundp (quote class-of)) (fboundp (quote class-slots)) (fboundp (quote eieio-class-slots)) (fboundp (quote compute-applicable-methods)) (fboundp (quote cl-find-method)) (fboundp (quote cl-generic-p)) (fboundp (quote find-class)))'),
('funcallable-instance','(list (fboundp (quote oclosure-define)) (fboundp (quote make-funcallable-instance)))'),
('defstruct','(progn (cl-defstruct (s1 (:include nil)) a (b 2)) (list (s1-b (make-s1 :a 1)) (s1-p (make-s1)) (s1-a (copy-s1 (make-s1 :a 9)))))'),
# numbers
('bignum','(expt 2 100)'),
('bignum div big','(/ (expt 10 40) (expt 10 20))'),
('bignum mod big','(mod (expt 10 40) (+ 7 (expt 10 20)))'),
('rational native(reader)','(read "1/3")'),
('rational native(eval /)','(/ 1 3)'),
('nl rational','(progn (require (quote nl-num)) (nl-num-pr-str (nl-+ (nl-rational 1 3) (nl-rational 1 6))))'),
('nl complex','(progn (require (quote nl-num)) (nl-num-pr-str (nl-* (nl-complex 1 2) (nl-complex 3 4))))'),
('sqrt -1 complex','(sqrt -1.0)'),
('single-float','(read "1.5f0")'),
('double literal d0','(read "1.5d0")'),
('float funcs','(list (ffloor 1.5) (frexp 8.0) (ldexp 1.0 3) (logb 8.0) (copysign 1.0 -2.0) (isnan 0.0e+NaN))'),
('hyperbolic','(list (fboundp (quote sinh)) (fboundp (quote cl-sinh)))'),
('isqrt etc','(list (cl-isqrt 17) (cl-gcd 12 18) (cl-lcm 4 6) (logcount 7) (ash 1 70))'),
('bit ops ldb/dpb','(list (fboundp (quote ldb)) (fboundp (quote cl-ldb)) (fboundp (quote dpb)) (fboundp (quote integer-length)))'),
('truncate 2 values','(cl-truncate 7 2)'),
('random-state','(list (cl-random 10) (cl-random-state-p (cl-make-random-state t)))'),
# arrays
('make-array multi-dim','(cl-make-array 3)'),
('aref 2d','(list (fboundp (quote make-array)) (fboundp (quote cl-make-array)) (fboundp (quote array-rank)) (fboundp (quote array-dimensions)))'),
('multidim reader','(read "#2A((1 2)(3 4))")'),
('fill pointer/adjustable','(list (fboundp (quote vector-push-extend)) (fboundp (quote fill-pointer)))'),
('bool-vector','(let ((b (make-bool-vector 4 nil))) (aset b 1 t) (list b (bool-vector-count-population b)))'),
('bit vector reader','(read "#*101")'),
('char type distinct','(list (characterp ?a) (integerp ?a) (type-of ?a))'),
('specialized typed vec','(list (fboundp (quote make-byte-vector)) (fboundp (quote unibyte-string)))'),
# reader
('case fold','(eq (read "FOO") (read "foo"))'),
('read eval #.','(read "#.(+ 1 2)")'),
('feature exprs #+','(read "#+foo 1 2")'),
('#b #o #x','(list (read "#b101") (read "#o17") (read "#xff") (read "#36rZ"))'),
('circular','(let ((x (read "#1=(a . #1#)"))) (eq x (cdr x)))'),
('uninterned','(symbol-name (read "#:foo"))'),
('readtable obj','(list (fboundp (quote readtablep)) (boundp (quote readtable)) (fboundp (quote set-macro-character)))'),
('pkg sym read','(read "foo:bar")'),
('nl-ns-reader','(progn (require (quote nl-ns)) (list (fboundp (quote nl-ns-in)) (featurep (quote nl-ns))))'),
('obarray','(let ((o (obarray-make))) (intern "x" o) (and (intern-soft "x" o) t))'),
('shorthand','(boundp (quote read-symbol-shorthands))'),
# printer/format
('format ~ ','(list (format "%S %s %d %x %o %c %e %f %g" (quote a) "b" 1 255 8 ?z 1.0 1.0 1.0))'),
('format ~R directive','(list (fboundp (quote cl-format)) (fboundp (quote formatter)))'),
('print-circle','(let ((print-circle t) (x (list 1))) (setcdr x x) (prin1-to-string x))'),
('print-length/level','(let ((print-length 2) (print-level 1)) (prin1-to-string (quote (1 (2) 3 4))))'),
('pretty printer','(list (fboundp (quote pp)) (fboundp (quote pp-to-string)) (fboundp (quote pprint-logical-block)))'),
('print-object hook','(list (fboundp (quote cl-print-object)) (fboundp (quote print-object)) (fboundp (quote cl-prin1)))'),
('print-base','(boundp (quote print-base))'),
# packages/symbols
('make-package','(list (fboundp (quote defpackage)) (fboundp (quote make-package)))'),
('symbol-plist','(progn (put (quote sp1) (quote k) 1) (symbol-plist (quote sp1)))'),
# streams/io
('streams','(list (fboundp (quote make-string-output-stream)) (fboundp (quote with-output-to-string)) (fboundp (quote with-temp-buffer)) (fboundp (quote read-char)) (fboundp (quote open)))'),
('with-output-to-string','(with-output-to-string (princ "x"))'),
('byte file io','(list (fboundp (quote insert-file-contents)) (fboundp (quote insert-file-contents-literally)) (fboundp (quote write-region)))'),
('file ops','(list (fboundp (quote file-attributes)) (fboundp (quote directory-files)) (fboundp (quote expand-file-name)) (fboundp (quote file-truename)) (fboundp (quote make-directory)) (fboundp (quote delete-file)))'),
('gray streams','(list (fboundp (quote stream-read-char)) (fboundp (quote fundamental-stream)))'),
('pathname','(list (fboundp (quote pathname)) (fboundp (quote make-pathname)))'),
('time','(list (fboundp (quote current-time)) (fboundp (quote float-time)) (fboundp (quote decode-time)) (fboundp (quote encode-time)) (fboundp (quote format-time-string)) (fboundp (quote sleep-for)))'),
# compiler/runtime
('compile','(list (fboundp (quote byte-compile)) (fboundp (quote native-compile)) (fboundp (quote compile)) (fboundp (quote cl-compile)))'),
('byte-compile runs','(funcall (byte-compile (quote (lambda (x) (+ x 1)))) 1)'),
('disassemble','(list (fboundp (quote disassemble)))'),
('declare types used','(list (fboundp (quote cl-the)) (fboundp (quote cl-declaim)) (fboundp (quote cl-proclaim)))'),
('trace','(list (fboundp (quote trace-function)) (fboundp (quote trace)) (fboundp (quote advice-add)))'),
('describe/doc','(list (fboundp (quote describe-function)) (fboundp (quote documentation)) (fboundp (quote func-arity)) (fboundp (quote help-function-arglist)))'),
('apropos','(list (fboundp (quote apropos)) (fboundp (quote apropos-internal)) (fboundp (quote mapatoms)))'),
('profiler','(list (fboundp (quote profiler-start)) (fboundp (quote benchmark-run)) (fboundp (quote benchmark-call)) (fboundp (quote time)))'),
('inspect/step','(list (fboundp (quote inspect)) (fboundp (quote step)) (fboundp (quote edebug)) (fboundp (quote eval-defun)))'),
('save image','(list (fboundp (quote dump-emacs-portable)) (fboundp (quote save-lisp-and-die)))'),
('gc','(list (fboundp (quote garbage-collect)) (boundp (quote gc-cons-threshold)) (fboundp (quote memory-use-counts)) (fboundp (quote room)))'),
('finalizer/weak','(let ((h (make-hash-table :weakness (quote key)))) (list (hash-table-weakness h) (fboundp (quote make-finalizer)) (fboundp (quote make-weak-pointer))))'),
('hash tests','(progn (define-hash-table-test (quote ht1) (quote equal) (quote sxhash-equal)) (hash-table-test (make-hash-table :test (quote ht1))))'),
# threads
('threads names','(list (fboundp (quote make-thread)) (fboundp (quote make-mutex)) (fboundp (quote make-condition-variable)) (fboundp (quote thread-join)) (fboundp (quote nelisp-thread-create)) (fboundp (quote nelisp-thread-spawn)))'),
('atomics','(list (fboundp (quote atomic-incf)) (fboundp (quote compare-and-swap)) (fboundp (quote nelisp-atomic-cas)))'),
# ffi/os
('ffi','(progn (require (quote nl-ffi)) (list (fboundp (quote nl-ffi-call)) (fboundp (quote ffi:defun))))'),
('ffi callback','(list (fboundp (quote nl-ffi-callback)) (fboundp (quote alien-callable-function)))'),
('dlopen','(list (fboundp (quote nl-ffi-dlopen)) (fboundp (quote nl-dlopen)))'),
('process','(list (fboundp (quote make-process)) (fboundp (quote call-process)) (fboundp (quote start-process)))'),
('sockets','(list (fboundp (quote make-network-process)) (fboundp (quote open-network-stream)))'),
('getenv/args','(list (fboundp (quote getenv)) (boundp (quote command-line-args)) (fboundp (quote kill-emacs)))'),
('unicode','(list (upcase "é") (get-char-code-property ?a (quote general-category)) (fboundp (quote string-glyph-compose)) (fboundp (quote char-uppercase-p)) (fboundp (quote ucs-normalize-NFC-string)))'),
('generic seq','(list (cl-remove-if (function cl-evenp) (vector 1 2 3)) (cl-position 2 (list 1 2)) (cl-sort (list 2 1) (function <)) (cl-map (quote list) (function 1+) (vector 1)) (cl-subseq "abc" 1) (cl-reduce (function +) (list 1 2)) (cl-mismatch "a" "b") (cl-search "b" "ab"))'),
('list misc','(list (cl-getf (list :a 1) :a) (cl-tree-equal 1 1) (cl-subst 1 2 (list 2)) (cl-acons 1 2 nil) (cl-pairlis (list 1) (list 2)) (cl-union (list 1) (list 2)) (cl-intersection (list 1) (list 1)) (cl-ldiff nil nil))'),
]
PRE="(progn (require 'cl-lib) (require 'cl-macs) (require 'cl-extra) (require 'cl-seq) (require 'cl-generic))"
def run(t):
    n,f=t
    src=f"(progn {PRE} (condition-case e (prin1 (eval (quote {f}) t)) (error (princ \"ERR:\") (prin1 e))))"
    args=[BIN]
    for p in PKGS: args+=['-L',p]
    args+=['--eval',src]
    try:
        r=subprocess.run(['bash','-c','ulimit -v 8388608; exec "$@"','_']+args,capture_output=True,text=True,timeout=60,errors='replace')
        o=(r.stdout.strip()[-260:]) ; rc=r.returncode
        if rc: o=f'[rc={rc}] '+o+' | '+r.stderr.strip()[-120:]
    except subprocess.TimeoutExpired: o='[TIMEOUT 60s]'
    return n,o
with cf.ThreadPoolExecutor(5) as ex:
    res=list(ex.map(run,T))
for n,o in res: print(f'{n}\t{o}')
