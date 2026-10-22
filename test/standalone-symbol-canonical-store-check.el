;;; standalone-symbol-canonical-store-check.el --- a stored symbol is its object WORD -*- lexical-binding: t; -*-

;; Doc 226 D9 stage 3.2.  GNU's Lisp_Object for a symbol is the pointer to its
;; struct Lisp_Symbol.  NeLisp stored an interned symbol as a 32-byte view
;; (a tag-4 box) in every cons cell, vector slot, variable and plist; storing
;; a view whose symbol has its object (made when the reader or `intern'
;; answered it) now stores the object's class-110 WORD instead.
;;
;; (nelisp--debug-switch 51 X) answers what X's WORD is: 4 a legacy symbol view,
;; 104 an interned symbol object, 116 an uninterned one, 2 a direct cons, -1 an
;; immediate.  Passing a value to a function stores it in an argument list, so
;; the probe sees an object where the program holds a view of a symbol that has
;; one; the views left in a running image are the heads and names that native
;; code made before the mirror existed (the (builtin NAME) definitions).
;;
;; Everything a program can observe must be the same for either form of the
;; same symbol: eq, equal, hashing, name, plist, function and value cells,
;; dispatch of a builtin through its name.

(defvar scsc-checks 0)
(defvar scsc-failures 0)
(defun scsc-check (ok label)
  (setq scsc-checks (1+ scsc-checks))
  (unless ok
    (setq scsc-failures (1+ scsc-failures))
    (princ (format "SCSC FAIL %s\n" label))))

(defun scsc-class (x) (nelisp--debug-switch 51 x))

;; The probe itself.
(scsc-check (eql (scsc-class 5) -1) "a fixnum is an immediate")
(scsc-check (eql (scsc-class nil) -1) "nil is an immediate")
(scsc-check (eql (scsc-class (cons 1 2)) 2) "a cons is a direct cons")
(scsc-check (eql (scsc-class (make-symbol "scsc-u")) 116) "an uninterned symbol is a kind-16 object")

;; Symbols the reader made are objects wherever the program keeps them.
(scsc-check (eql (scsc-class (car '(scsc-a))) 104) "a symbol in a quoted list is an object")
(scsc-check (eql (scsc-class 'scsc-quoted) 104) "a quoted symbol is an object")
(scsc-check (eql (scsc-class (car (read-from-string "scsc-read"))) 104)
            "a read symbol is an object")
(let ((l (list 'scsc-b 'scsc-c)))
  (scsc-check (eql (scsc-class (car l)) 104) "a symbol in a built list is an object")
  (scsc-check (eql (scsc-class (cadr l)) 104) "and the next one"))
(let ((v (vector 'scsc-d 'scsc-e)))
  (scsc-check (eql (scsc-class (aref v 0)) 104) "a symbol in a vector is an object")
  (aset v 1 (intern "scsc-f"))
  (scsc-check (eql (scsc-class (aref v 1)) 104) "aset stores an interned symbol as its object"))
(let ((x (intern "scsc-g")))
  (scsc-check (eql (scsc-class (let ((y x)) y)) 104) "a variable holds the object")
  (setq scsc-global (intern "scsc-h"))
  (scsc-check (eql (scsc-class scsc-global) 104) "setq stores the object")
  (set 'scsc-global2 (intern "scsc-i"))
  (scsc-check (eql (scsc-class (symbol-value 'scsc-global2)) 104) "set stores the object"))
(put 'scsc-p 'scsc-prop (intern "scsc-j"))
(scsc-check (eql (scsc-class (get 'scsc-p 'scsc-prop)) 104) "put stores the object")
(let ((h (make-hash-table :test 'eq)))
  (puthash (intern "scsc-k") (intern "scsc-l") h)
  (scsc-check (eq (gethash 'scsc-k h) 'scsc-l) "an eq table finds a key by its object")
  (scsc-check (eql (scsc-class (gethash 'scsc-k h)) 104) "and holds the value as an object"))
(scsc-check (eql (scsc-class (car (list (intern "scsc-m")))) 104) "list stores intern's answer as an object")
(scsc-check (stringp (symbol-name 'scsc-n)) "a symbol's name is still a string")
(setq scsc-global3 'scsc-o)
(scsc-check (eql (scsc-class scsc-global3) 104) "defvar-less setq of a quoted symbol")

;; The reader's quote-family heads are the interned symbols, as objects.
(let* ((f1 (car (read-from-string "'x"))) (f2 (car (read-from-string "#'x")))
       (f3 (car (read-from-string "`(a ,b ,@c)")))
       (inner (cadr f3)) (comma (nth 1 inner)) (comma-at (nth 2 inner)))
  (scsc-check (and (eq (car f1) 'quote) (eql (scsc-class (car f1)) 104)) "the head of 'x is the quote object")
  (scsc-check (and (eq (car f2) 'function) (eql (scsc-class (car f2)) 104)) "the head of #'x is the function object")
  (scsc-check (and (eq (car f3) (intern "`")) (eql (scsc-class (car f3)) 104)) "the head of a backquote form is an object")
  (scsc-check (and (eq (car comma) (intern ",")) (eql (scsc-class (car comma)) 104)) "the head of a comma form is an object")
  (scsc-check (and (eq (car comma-at) (intern ",@")) (eql (scsc-class (car comma-at)) 104)) "the head of a comma-at form is an object")
  (scsc-check (equal f1 '(quote x)) "and 'x still reads as (quote x)"))

;; The unbound marker, nil and t stay what they were.
(scsc-check (eq (car (list nil)) nil) "nil in a list is nil")
(scsc-check (eq (car (list t)) t) "t in a list is t")
(defvar scsc-void)
(scsc-check (not (boundp 'scsc-void)) "a defvar without a value is void")
(scsc-check (condition-case nil (progn scsc-void nil) (void-variable t)) "reading it signals")
(set 'scsc-void 1)
(makunbound 'scsc-void)
(scsc-check (not (boundp 'scsc-void)) "makunbound leaves it void")

;; A symbol the image holds as a view and the same symbol as the program's object:
;; the definition of a builtin, (builtin NAME), was made at boot before the mirror
;; was attached.
(let* ((def (symbol-function 'car)) (marker (car def)) (name (cadr def)))
  (scsc-check (eq marker 'builtin) "eq: the definition's marker and the quoted symbol")
  (scsc-check (eq 'builtin marker) "eq: the other way")
  (scsc-check (eq name 'car) "eq: the definition's name and the quoted symbol")
  (scsc-check (eql marker 'builtin) "eql")
  (scsc-check (equal (list marker name) '(builtin car)) "equal on the lists")
  (scsc-check (equal (memq 'builtin (list 'a marker)) (list 'builtin)) "memq finds a symbol in a list")
  (scsc-check (equal (memq marker (list 'a 'builtin)) (list 'builtin)) "memq finds the definition's symbol")
  (scsc-check (equal (assq marker (list (cons 'builtin 1))) (cons 'builtin 1)) "assq")
  (scsc-check (equal (delq marker (list 'builtin 'other)) '(other)) "delq")
  (scsc-check (equal (symbol-name marker) "builtin") "the name")
  (scsc-check (equal (format "%S %s" marker name) "builtin car") "the print")
  (scsc-check (= (sxhash-equal marker) (sxhash-equal 'builtin)) "same sxhash")
  (let ((h (make-hash-table :test 'equal)))
    (puthash marker 1 h)
    (scsc-check (eql (gethash 'builtin h) 1) "an equal table finds a quoted symbol by the definition's"))
  (let ((h (make-hash-table :test 'eq)))
    (puthash 'builtin 1 h)
    (scsc-check (eql (gethash marker h) 1) "an eq table finds the definition's symbol by the quoted one"))
  (scsc-check (equal (funcall name '(9 8)) 9) "a builtin runs through the definition's name"))

;; Builtins are called through the symbol however it is stored (the builtin
;; dispatch reads the static id of the symbol).
(let ((fn (car (list 'car))) (plus (car (list '+))) (mem (car (list 'memq))))
  (scsc-check (eql (scsc-class fn) 104) "car is an object in a list")
  (scsc-check (equal (funcall fn '(1 2)) 1) "funcall through the object")
  (scsc-check (equal (apply plus '(1 2 3)) 6) "apply through the object")
  (scsc-check (equal (funcall mem 'b '(a b c)) '(b c)) "memq through the object")
  (scsc-check (equal (eval (list fn '(quote (7 8)))) 7) "eval of a form headed by the object"))

;; A user function named by the object.
(defun scsc-double (x) (* 2 x))
(let ((f (car (list 'scsc-double))))
  (scsc-check (eql (scsc-class f) 104) "a user function's name is an object")
  (scsc-check (eql (funcall f 4) 8) "funcall of a user function by object")
  (scsc-check (eql (apply f '(5)) 10) "apply of a user function by object")
  (scsc-check (eql (eval (list f 6)) 12) "eval of a form headed by it"))
(defmacro scsc-twice (x) (list '* 2 x))
(scsc-check (eql (scsc-twice 4) 8) "a macro defined and used")
(scsc-check (equal (macroexpand '(scsc-twice 1)) '(* 2 1)) "macroexpand")

;; Special forms and variables by object.
(scsc-check (eql (let ((x 1) (y 2)) (+ x y)) 3) "let of lexical variables")
(defvar scsc-dyn 10)
(defun scsc-read-dyn () scsc-dyn)
(scsc-check (eql (let ((scsc-dyn 11)) (scsc-read-dyn)) 11) "a dynamic let binds the variable")
(scsc-check (eql scsc-dyn 10) "and restores it")
(scsc-check (equal (condition-case e (signal 'scsc-err '(1)) (scsc-err (cdr e))) '(1))
            "signal and condition-case by name")
(scsc-check (eql (catch 'scsc-tag (throw 'scsc-tag 5)) 5) "catch and throw by object")
(defun scsc-count ()
  (let ((n 0) (l '(a b c)))
    (while l (setq n (1+ n) l (cdr l)))
    n))
(scsc-check (eql (scsc-count) 3) "a while loop over a quoted list of symbols")

;; Collections keep the objects.
(let ((l (mapcar (lambda (i) (intern (format "scsc-gc-%d" i))) (number-sequence 1 500))))
  (garbage-collect)
  (garbage-collect)
  (scsc-check (equal (mapcar #'symbol-name l)
                     (mapcar (lambda (i) (format "scsc-gc-%d" i)) (number-sequence 1 500)))
              "500 symbols in a list keep their names across two collections")
  (scsc-check (let ((ok t)) (dolist (s l) (unless (eql (scsc-class s) 104) (setq ok nil))) ok)
              "and are objects")
  (dolist (s l) (set s (symbol-name s)))
  (garbage-collect)
  (scsc-check (let ((ok t)) (dolist (s l) (unless (equal (symbol-value s) (symbol-name s)) (setq ok nil))) ok)
              "and their values survive"))

(princ (format "SCSC checks=%d failures=%d %s\n" scsc-checks scsc-failures
               (if (= scsc-failures 0) "PASS" "FAIL")))
(when (> scsc-failures 0) (error "symbol canonical store check failed"))
