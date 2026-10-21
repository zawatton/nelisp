;;; standalone-symbols-check.el --- GNU 31.1 symbol and variable oracle -*- lexical-binding: t; -*-

;; Doc 226 S1.3.  Every case prints one line "SYM label VALUE", where VALUE
;; is the case's result or (error . DATA).  The golden output
;; (standalone-symbols-check.expected) is GNU Emacs 31.1's:
;;   emacs -Q --batch -l test/standalone-symbols-check.el
;; NeLisp runs it with `nelisp --load'.  The cases cover the redirects of
;; GNU's struct Lisp_Symbol (lisp.h:771-829): PLAINVAL values, VARALIAS
;; chains (data.c indirect_variable, eval.c Fdefvaralias), the trapped
;; writes of constants and watched variables (data.c set_internal,
;; notify_variable_watchers), specbind/unbind_to of special variables
;; (eval.c:3656, :3940), function and plist cells, and the obarray
;; (lread.c intern, intern-soft, unintern, obarray-make, mapatoms).

(defvar symfix--log nil)

(defmacro symfix-case (label &rest body)
  `(princ (format "SYM %s %S\n" ',label
                  (condition-case err (progn ,@body)
                    (error (cons 'error err))))))

(defun symfix-watch (symbol newval operation where)
  (push (list symbol newval operation where) symfix--log))

(defun symfix-take-log ()
  (prog1 (nreverse symfix--log) (setq symfix--log nil)))

;;; PLAINVAL
(defvar symfix-a 1)
(symfix-case plain-read (list symfix-a (symbol-value 'symfix-a) (default-value 'symfix-a)))
(symfix-case plain-set (list (set 'symfix-a 2) symfix-a (setq symfix-a 3) (symbol-value 'symfix-a)))
(symfix-case plain-boundp (list (boundp 'symfix-a) (boundp 'symfix-never-bound)
                                (special-variable-p 'symfix-a)
                                (special-variable-p 'symfix-never-bound)))
(symfix-case plain-void (symbol-value 'symfix-never-bound))
(symfix-case plain-void-default (default-value 'symfix-never-bound))
(symfix-case plain-set-default (list (set-default 'symfix-a 4) symfix-a (default-value 'symfix-a)))
(symfix-case plain-makunbound
  (let ((r (makunbound 'symfix-a)))
    (list r (boundp 'symfix-a)
          (condition-case e symfix-a (error e))
          (progn (setq symfix-a 5) symfix-a))))
(symfix-case plain-fresh-setq (progn (set 'symfix-fresh 9) (list (boundp 'symfix-fresh) (symbol-value 'symfix-fresh))))
(symfix-case defvar-keeps (progn (defvar symfix-a 100) symfix-a))
(symfix-case defvar-value (progn (defvar symfix-d (+ 1 2) "doc") (list symfix-d (special-variable-p 'symfix-d))))
(symfix-case defconst (progn (defconst symfix-c 10 "doc")
                             (list symfix-c (special-variable-p 'symfix-c)
                                   (progn (setq symfix-c 11) symfix-c)
                                   (progn (defconst symfix-c 12) symfix-c))))

;;; Constants (SYMBOL_NOWRITE)
(symfix-case set-nil (set 'nil 1))
(symfix-case set-t (setq t 1))
(symfix-case set-keyword (set :symfix-kw 1))
(symfix-case set-keyword-self (list (set :symfix-kw :symfix-kw) :symfix-kw (keywordp :symfix-kw)))
(symfix-case makunbound-nil (makunbound 'nil))
(symfix-case let-constant (let ((nil 1)) 2))
(symfix-case boundp-constants (list (boundp nil) (boundp t) (boundp :symfix-kw) (symbol-value :symfix-kw)))

;;; Special let / let* (specbind / unbind_to)
(defvar symfix-s 'outer)
(defun symfix-read-s () symfix-s)
(symfix-case let-special (list (let ((symfix-s 'inner)) (list symfix-s (symfix-read-s) (symbol-value 'symfix-s)))
                               symfix-s))
(symfix-case let*-special (let* ((symfix-s 'one) (symfix-x (symfix-read-s)) (symfix-s 'two))
                            (list symfix-x (symfix-read-s) symfix-s)))
(defvar symfix-x nil)
(symfix-case let-nested (let ((symfix-s 1))
                          (let ((symfix-s 2))
                            (setq symfix-s 3)
                            (list (symfix-read-s)
                                  (let ((symfix-s 4)) (symfix-read-s))
                                  (symfix-read-s)))))
(symfix-case let-restored symfix-s)
(symfix-case let-setq-inner (list (let ((symfix-s 'b)) (setq symfix-s 'c) (symfix-read-s)) symfix-s))
(symfix-case let-makunbound-inner
  (list (let ((symfix-s 'b)) (makunbound 'symfix-s) (boundp 'symfix-s)) symfix-s))
(symfix-case let-unbound-outer
  (list (let ((symfix-unbound-outer 1)) (symbol-value 'symfix-unbound-outer))
        (boundp 'symfix-unbound-outer)))
(symfix-case let-nonlocal-exit
  (list (catch 'symfix-tag (let ((symfix-s 'thrown)) (throw 'symfix-tag (symfix-read-s))))
        symfix-s
        (condition-case nil (let ((symfix-s 'signalled)) (error "x")) (error (symfix-read-s)))))
(symfix-case let-set-default (list (let ((symfix-s 'b)) (set-default 'symfix-s 'c) (list symfix-s (default-value 'symfix-s)))
                                   symfix-s))

;;; Lexical let
(symfix-case let-lexical (let ((symfix-lex 1)) (list symfix-lex (boundp 'symfix-lex)
                                                     (condition-case e (symbol-value 'symfix-lex) (error e)))))
(symfix-case let*-lexical (let* ((symfix-lex 1) (symfix-lex2 (+ symfix-lex 1))) (list symfix-lex symfix-lex2)))
(symfix-case closure-lexical (let ((symfix-lex 5)) (funcall (lambda () (setq symfix-lex (1+ symfix-lex)) symfix-lex))))

;;; VARALIAS
(defvar symfix-base 5)
(symfix-case alias-define (list (defvaralias 'symfix-al 'symfix-base) (indirect-variable 'symfix-al)
                                symfix-al (symbol-value 'symfix-al) (boundp 'symfix-al)
                                (special-variable-p 'symfix-al)))
(symfix-case alias-set (list (set 'symfix-al 6) symfix-base (setq symfix-al 7) symfix-base (default-value 'symfix-al)))
(symfix-case alias-let (list (let ((symfix-al 8)) (list symfix-base symfix-al)) symfix-base))
(symfix-case alias-let-base (list (let ((symfix-base 9)) (list symfix-al (symbol-value 'symfix-al))) symfix-al))
(defvaralias 'symfix-al2 'symfix-al)
(symfix-case alias-chain (list (indirect-variable 'symfix-al2) symfix-al2 (progn (set 'symfix-al2 10) symfix-base)))
(symfix-case alias-makunbound (list (makunbound 'symfix-al2) (boundp 'symfix-base) (boundp 'symfix-al)
                                    (progn (setq symfix-base 11) symfix-al2)))
(symfix-case alias-cycle (defvaralias 'symfix-base 'symfix-al2))
(symfix-case alias-constant (defvaralias 'nil 'symfix-base))
(symfix-case alias-indirect-plain (list (indirect-variable 'symfix-base) (indirect-variable 'symfix-never-bound)
                                        (indirect-variable 12)))
(defvar symfix-old 'old-value)
(defvar symfix-new)
(symfix-case alias-takes-value (list (defvaralias 'symfix-old2 'symfix-new2) (boundp 'symfix-old2)))
(symfix-case alias-of-bound
  (progn (defvar symfix-target2 'target)
         (defvar symfix-src2 'src)
         (list (condition-case e (defvaralias 'symfix-src2 'symfix-target2) (error e))
               symfix-src2 symfix-target2)))
(symfix-case alias-doc (progn (defvaralias 'symfix-al3 'symfix-base "alias doc")
                              (list (get 'symfix-al3 'variable-documentation) symfix-al3)))

;;; Variable watchers (SYMBOL_TRAPPED_WRITE)
(defvar symfix-w 1)
(add-variable-watcher 'symfix-w #'symfix-watch)
(symfix-case watch-set (progn (set 'symfix-w 2) (setq symfix-w 3) (set-default 'symfix-w 4) (symfix-take-log)))
(symfix-case watch-let (progn (let ((symfix-w 5)) (setq symfix-w 6)) (symfix-take-log)))
(symfix-case watch-makunbound (progn (makunbound 'symfix-w) (setq symfix-w 7) (symfix-take-log)))
(symfix-case watch-get (list (get-variable-watchers 'symfix-w)))
(defvaralias 'symfix-wal 'symfix-w)
(symfix-case watch-alias (progn (setq symfix-wal 8) (let ((symfix-wal 9)) symfix-w)
                                (list symfix-w (symfix-take-log) (get-variable-watchers 'symfix-wal))))
(defvar symfix-w2 'w2)
(add-variable-watcher 'symfix-w2 #'symfix-watch)
(symfix-case watch-defvaralias (progn (defvaralias 'symfix-wal2 'symfix-w2) (symfix-take-log)))
(defvar symfix-w3 'w3)
(add-variable-watcher 'symfix-wal3 #'symfix-watch)
(symfix-case watch-defvaralias-new (progn (defvaralias 'symfix-wal3 'symfix-w3)
                                          (setq symfix-wal3 'after)
                                          (list (symfix-take-log) symfix-w3)))
(symfix-case watch-remove (progn (remove-variable-watcher 'symfix-w #'symfix-watch)
                                 (setq symfix-w 10)
                                 (list symfix-w (symfix-take-log) (get-variable-watchers 'symfix-w))))

;;; FORWARDED (GNU DEFVAR_INT max-lisp-eval-depth, DEFVAR_BOOL print-escape-newlines)
(symfix-case fwd-int-let
  (let ((old max-lisp-eval-depth))
    (list (let ((max-lisp-eval-depth (+ old 7))) (- max-lisp-eval-depth old))
          (= old max-lisp-eval-depth) (special-variable-p 'max-lisp-eval-depth))))
(symfix-case fwd-int-type
  (let ((old max-lisp-eval-depth))
    (list (condition-case e (setq max-lisp-eval-depth 'symfix-x) (error e))
          (condition-case e (set 'max-lisp-eval-depth 2.5) (error e))
          (condition-case e (let ((max-lisp-eval-depth "x")) 1) (error e))
          (= old max-lisp-eval-depth))))
(symfix-case fwd-makunbound (makunbound 'max-lisp-eval-depth))
(symfix-case fwd-alias-refused (defvaralias 'max-lisp-eval-depth 'symfix-a))
(symfix-case fwd-alias-to
  (let ((old max-lisp-eval-depth))
    (defvaralias 'symfix-depth 'max-lisp-eval-depth)
    (list (= symfix-depth old)
          (let ((symfix-depth (+ old 1))) (- max-lisp-eval-depth old))
          (= old max-lisp-eval-depth))))
(symfix-case fwd-bool
  (list (let ((print-escape-newlines 5)) print-escape-newlines)
        print-escape-newlines
        (progn (setq print-escape-newlines 'yes)
               (prog1 print-escape-newlines (setq print-escape-newlines nil)))
        (boundp 'print-escape-newlines)))

;;; Function cells
(symfix-case fset (list (fboundp 'symfix-f) (functionp (fset 'symfix-f (lambda (x) (* x 2))))
                        (fboundp 'symfix-f) (funcall 'symfix-f 21)
                        (functionp (symbol-function 'symfix-f))))
(symfix-case symbol-function-void (list (symbol-function 'symfix-void-fn) (fboundp 'symfix-void-fn)))
(symfix-case void-function (symfix-void-fn 1))
(symfix-case fmakunbound (list (fmakunbound 'symfix-f) (fboundp 'symfix-f) (symbol-function 'symfix-f)
                               (condition-case e (symfix-f 1) (error e))))
(symfix-case defalias (list (defalias 'symfix-g 'car) (symbol-function 'symfix-g) (symfix-g '(1 2))
                            (indirect-function 'symfix-g)))
(symfix-case fset-nil (fset 'nil 'car))
(symfix-case fset-t-ok (list (fboundp 't)))

;;; Property lists
(symfix-case plist (list (symbol-plist 'symfix-p) (put 'symfix-p 'k1 'v1) (put 'symfix-p 'k2 'v2)
                         (get 'symfix-p 'k1) (get 'symfix-p 'k3) (symbol-plist 'symfix-p)
                         (put 'symfix-p 'k1 'v3) (symbol-plist 'symfix-p)))
(symfix-case setplist (list (setplist 'symfix-p '(a 1 b 2)) (get 'symfix-p 'b) (symbol-plist 'symfix-p)))
(symfix-case plist-nil-t (list (get nil 'symfix-none) (put 'symfix-q 'p nil) (symbol-plist 'symfix-q)))

;;; Obarray (lread.c)
(symfix-case intern-eq (list (eq (intern "symfix-i1") (intern "symfix-i1")) (eq (intern "symfix-i1") 'symfix-i1)
                             (symbol-name (intern "symfix-i1"))))
(symfix-case intern-soft (list (intern-soft "symfix-i1") (intern-soft "symfix-never-interned-x")
                               (intern-soft 'symfix-i1) (intern-soft "nil") (intern-soft "t")))
(symfix-case unintern
  (let ((old (intern "symfix-u1")))
    (set old 'value-u1)
    (list (unintern "symfix-u1" obarray) (intern-soft "symfix-u1")
          (eq old (intern "symfix-u1")) (symbol-value old) (boundp (intern "symfix-u1")))))
(symfix-case unintern-missing (unintern "symfix-never-interned-y" obarray))
(symfix-case make-symbol (let ((a (make-symbol "symfix-i1")))
                           (set a 3) (put a 'p 4) (fset a 'cdr)
                           (list (eq a 'symfix-i1) (symbol-name a) (symbol-value a) (get a 'p)
                                 (funcall a '(1 2)) (intern-soft a) (boundp 'symfix-unrelated))))
(symfix-case obarray-make
  (let ((ob (obarray-make)))
    (let ((x (intern "symfix-i1" ob)))
      (list (obarrayp ob) (obarrayp obarray) (eq x 'symfix-i1) (eq x (intern "symfix-i1" ob))
            (eq (intern-soft "symfix-i1" ob) x) (intern-soft "car" ob)
            (progn (set x 'ob-value) (symbol-value x))
            (boundp 'symfix-i1)))))
(symfix-case mapatoms-ob
  (let ((ob (obarray-make)) (names nil))
    (dolist (n '("b1" "a1" "c1")) (intern n ob))
    (unintern "a1" ob)
    (mapatoms (lambda (s) (push (symbol-name s) names)) ob)
    (sort names #'string<)))
(symfix-case mapatoms-global
  (let ((n 0))
    (intern "symfix-map-1") (intern "symfix-map-2")
    (mapatoms (lambda (s) (when (string-prefix-p "symfix-map-" (symbol-name s)) (setq n (1+ n)))))
    n))
(symfix-case symbol-name-eq (let ((s (intern "symfix-name"))) (eq (symbol-name s) (symbol-name s))))
(symfix-case symbol-name-types (list (symbol-name nil) (symbol-name t) (symbol-name :symfix-kw)
                                     (condition-case e (symbol-name 3) (error e))))

(princ "SYM-DONE\n")
