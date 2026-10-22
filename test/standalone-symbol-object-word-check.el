;;; standalone-symbol-object-word-check.el --- every symbol consumer accepts the interned symbol's object WORD -*- lexical-binding: t; -*-

;; Doc 226 D9 stage 3 makes an interned symbol's Lisp value its object WORD
;; (class 110, kind 4; GNU's Lisp_Object for a symbol is the pointer to the
;; struct Lisp_Symbol).  Today an interned symbol is a 32-byte tag-4 view
;; and the object WORD exists only inside the runtime.  (nelisp--debug-switch
;; 44 SYM) hands Lisp that WORD, so each consumer can be run on both
;; representations of the same symbol before any producer is switched: the
;; results must be equal, errors included, and a change made through one
;; must show through the other.
;;
;; Prints "SOWC checks=N failures=F PASS|FAIL" last.

(defvar sowc-checks 0)
(defvar sowc-failures 0)
(defun sowc-fail (label &rest data)
  (setq sowc-failures (1+ sowc-failures))
  (princ (format "SOWC FAIL %s %S\n" label data)))

(defvar sowc-counter 0)
(defun sowc-fresh (&optional prefix)
  "A new interned symbol with state, and its object WORD."
  (let* ((s (intern (format "%s-%d" (or prefix "sowc") (setq sowc-counter (1+ sowc-counter))))))
    (set s 10)
    (fset s (lambda (x) (list 'f x)))
    (put s 'p 1)
    s))

(defun sowc-norm (result name)
  "RESULT printed, with the fresh symbol's NAME replaced: the two runs use two symbols."
  (let ((str (format "%S" result)) (start 0))
    (while (string-match (regexp-quote name) str start)
      (setq str (replace-match "<S>" t t str)
            start (+ (match-beginning 0) 3)))
    str))

(defmacro sowc-case (label &rest body)
  "Run BODY twice on a fresh symbol: with X its view, then with X its object WORD.
The results (or errors) must be equal; BODY must not mention the other one."
  (declare (indent 1))
  `(let* ((sowc-n (format "sowc-case-%s" ',label))
          (run (lambda (use-object)
                 (let* ((s (sowc-fresh sowc-n))
                        (x (if use-object (nelisp--debug-switch 49 s) s)))
                   (sowc-norm
                    (condition-case err
                        (list 'ok (progn ,@body) (symbol-value s) (get s 'q))
                      (error (list 'error (car err))))
                    (symbol-name s))))))
     (setq sowc-checks (1+ sowc-checks))
     (let ((view (funcall run nil)) (object (funcall run t)))
       (unless (equal view object)
         (sowc-fail ',label view object)))))

;; The object WORD really is one: not the view, same symbol.
(let* ((s (sowc-fresh "sowc-ident"))
       (o (nelisp--debug-switch 49 s)))
  (setq sowc-checks (1+ sowc-checks))
  (unless (and (symbolp o) (eq o s) (eq s o) (equal o s) (eql o s)
               (eq (symbol-name o) (symbol-name s)))
    (sowc-fail 'identity o s)))

;;; reads
(sowc-case symbolp (symbolp x))
(sowc-case name (symbol-name x))
(sowc-case value (symbol-value x))
(sowc-case boundp (list (boundp x) (fboundp x)))
(sowc-case function (list (funcall (symbol-function x) 1) (functionp x)))
(sowc-case plist (list (get x 'p) (symbol-plist x)))
(sowc-case default-value (list (default-value x) (default-boundp x)))
(sowc-case special-p (special-variable-p x))
(sowc-case indirect (list (indirect-variable x) (functionp (indirect-function x))))
(sowc-case interned (eq (intern-soft (symbol-name x)) x))
(sowc-case keyword-null (list (keywordp x) (null x) (booleanp x) (atom x) (consp x)))

;;; calls with the symbol as the function
(sowc-case funcall (funcall x 1))
(sowc-case apply (apply x '(2)))
(sowc-case eval-head (eval (list x 4) t))
(sowc-case eval-ref (eval x t))
(sowc-case eval-quote (eval (list 'quote x) t))
(sowc-case mapcar (mapcar x '(1 2)))
(sowc-case func-arity (func-arity x))

;;; writes through the object, read through the view
(sowc-case set (progn (set x 11) (symbol-value s)))
(sowc-case setq (progn (eval (list 'setq x 12) t) (symbol-value s)))
(sowc-case set-default (progn (set-default x 13) (symbol-value s)))
(sowc-case put (progn (put x 'q 2) (get s 'q)))
(sowc-case setplist (progn (setplist x '(p 5 q 6)) (list (get s 'p) (get s 'q))))
(sowc-case fset (progn (fset x (lambda () 'new)) (funcall s)))
(sowc-case defalias (progn (defalias x (lambda () 'alias)) (funcall s)))
(sowc-case makunbound (progn (makunbound x) (list (boundp s) (boundp x))))
(sowc-case fmakunbound (progn (fmakunbound x) (list (fboundp s) (fboundp x))))
(sowc-case defvaralias (let ((base (intern (format "%s-base" sowc-n))))
                         (set base 77)
                         (defvaralias x base)
                         (list (symbol-value s) (indirect-variable x))))

;;; binding
(sowc-case let-dynamic (progn (eval (list 'defvar x) t)
                              (eval (list 'let (list (list x 20)) (list 'symbol-value (list 'quote s))) t)))
(sowc-case let-restores (progn (eval (list 'defvar x) t)
                               (eval (list 'let (list (list x 20)) nil) t)
                               (symbol-value s)))

;;; identity: eq, hashing, lists, vectors
(sowc-case eq (list (eq x s) (eq s x) (equal x s) (eql x s) (eq x x)))
(sowc-case memq (list (memq x (list 'a s)) (memq s (list 'a x)) (member x (list s)) (delq x (list 'a s))))
(sowc-case assq (list (assq x (list (cons s 1))) (assq s (list (cons x 2))) (rassq x (list (cons 1 s)))))
(sowc-case plist-get (list (plist-get (list s 1) x) (plist-get (list x 2) s)
                           (plist-put (list s 1) x 3)))
(sowc-case hash-eq (let ((h (make-hash-table :test 'eq)))
                     (puthash x 1 h) (list (gethash s h) (gethash x h) (hash-table-count h)
                                           (progn (puthash s 2 h) (hash-table-count h)))))
(sowc-case hash-equal (let ((h (make-hash-table :test 'equal)))
                        (puthash x 1 h) (list (gethash s h) (progn (puthash s 2 h) (hash-table-count h)))))
(sowc-case hash-eql (let ((h (make-hash-table :test 'eql)))
                      (puthash x 1 h) (list (gethash s h) (progn (puthash s 2 h) (hash-table-count h)))))
(sowc-case sxhash (list (= (sxhash-eq x) (sxhash-eq s)) (= (sxhash-equal x) (sxhash-equal s))
                        (= (sxhash-eql x) (sxhash-eql s))))
(sowc-case vector (let ((v (vector x s))) (list (eq (aref v 0) s) (eq (aref v 1) x) (eq (aref v 0) (aref v 1)))))
(sowc-case cons (let ((c (cons x s))) (list (eq (car c) s) (eq (cdr c) x))))
(sowc-case property-key (progn (put 'sowc-holder x 3) (list (get 'sowc-holder s) (get 'sowc-holder x))))
(sowc-case obarray (let ((n 0)) (mapatoms (lambda (a) (when (eq a x) (setq n (1+ n))))) n))

;;; printing and strings
(sowc-case format (list (format "%S" x) (format "%s" x) (prin1-to-string x) (prin1-to-string (list x s))))
(sowc-case string-fns (list (string= x (symbol-name s)) (string< x (concat (symbol-name s) "z"))
                            (symbol-name x) (length (symbol-name x))))
(sowc-case sort (mapcar #'symbol-name (sort (list x (intern "sowc-aaa")) #'string<)))
(sowc-case concat (concat "<" (symbol-name x) ">"))

;;; bytecode, non-local exits, buffer-local variables, watchers, types
(sowc-case bytecode-varref (funcall (make-byte-code 0 "\10\207" (vector x) 1)))
(sowc-case bytecode-call (funcall (make-byte-code 0 "\300\301\302\"\207" (vector x 1 nil) 3)))
(sowc-case catch-throw (list (catch x (throw s 5)) (catch s (throw x 6))))
(sowc-case signal (progn (put x 'error-conditions (list x 'error))
                         (put x 'error-message "sowc error")
                         (list (condition-case e (signal x '(1)) (error (car e)))
                               (condition-case e (signal s '(2)) (error (car e)))
                               (condition-case e (error "boom") (error (car e))))))
(sowc-case buffer-local (with-temp-buffer
                          (set (make-local-variable x) 5)
                          (list (symbol-value s) (local-variable-p s) (local-variable-p x)
                                (buffer-local-value x (current-buffer)))))
(sowc-case watcher (let ((log nil))
                     (add-variable-watcher x (lambda (sym new op where) (push (list new op) log)))
                     (set x 31) (set s 32)
                     (list (nreverse log) (and (get-variable-watchers x) t))))
(sowc-case type-of (list (type-of x) (cl-typep x 'symbol) (symbolp x) (sequencep x)))
(sowc-case lists (list (remq x (list 'a s 'b)) (delete x (list 'a s)) (remove x (list s))
                       (assoc x (list (cons s 1))) (delq s (list x 'z)) (length (list x s))))
(sowc-case intern-soft-obj (list (eq (intern-soft x) s) (eq (intern-soft s) x)))
(sowc-case unintern (let ((name (symbol-name s))) (list (eq (intern-soft name) s) (and (unintern x obarray) t) (intern-soft name))))

;;; survival of a collection
(sowc-case gc (let ((kept (list x (vector x) (cons x x))))
                (garbage-collect) (garbage-collect)
                (list (eq (car kept) s) (eq (aref (cadr kept) 0) s) (eq (cdr (caddr kept)) s)
                      (symbol-name (car kept)) (symbol-value (car kept)))))

(princ (format "SOWC checks=%d failures=%d %s\n" sowc-checks sowc-failures
               (if (= sowc-failures 0) "PASS" "FAIL")))
(when (> sowc-failures 0) (error "symbol object WORD check failed"))
