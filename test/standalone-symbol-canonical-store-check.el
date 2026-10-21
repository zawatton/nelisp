;;; standalone-symbol-canonical-store-check.el --- a stored symbol is its object WORD -*- lexical-binding: t; -*-

;; Doc 226 D9 stage 3.2.  GNU's Lisp_Object for a symbol is the pointer to its
;; struct Lisp_Symbol.  NeLisp stored an interned symbol as a 32-byte view
;; (a tag-4 box) in every cons cell, vector slot, variable and plist; storing
;; a view whose symbol has its object (made when the reader or `intern'
;; answered it) now stores the object's class-110 WORD instead.
;;
;; (nelisp--debug-switch 46 X) answers what X's WORD is: 4 a legacy symbol view,
;; 104 an interned symbol object, 116 an uninterned one, 2 a direct cons, -1 an
;; immediate.  (nelisp--debug-switch 47 1) turns the canonicalization off, and
;; 0 on again, so one binary can hold a symbol both ways.
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

(defun scsc-class (x) (nelisp--debug-switch 46 x))
(defun scsc-off () (nelisp--debug-switch 47 1))
(defun scsc-on () (nelisp--debug-switch 47 0))

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

;; The unbound marker, nil and t stay what they were.
(scsc-check (eq (car (list nil)) nil) "nil in a list is nil")
(scsc-check (eq (car (list t)) t) "t in a list is t")
(defvar scsc-void)
(scsc-check (not (boundp 'scsc-void)) "a defvar without a value is void")
(scsc-check (condition-case nil (progn scsc-void nil) (void-variable t)) "reading it signals")
(set 'scsc-void 1)
(makunbound 'scsc-void)
(scsc-check (not (boundp 'scsc-void)) "makunbound leaves it void")

;; One symbol held both ways.
(let (view obj)
  (scsc-off)
  (setq view (list (intern "scsc-both")))
  (scsc-on)
  (setq obj (list (intern "scsc-both")))
  (scsc-check (eql (scsc-class (car view)) 4) "with the switch off a symbol is stored as a view")
  (scsc-check (eql (scsc-class (car obj)) 104) "with it on, as its object")
  (scsc-check (eq (car view) (car obj)) "eq: view and object")
  (scsc-check (eq (car obj) (car view)) "eq: object and view")
  (scsc-check (eql (car view) (car obj)) "eql")
  (scsc-check (equal view obj) "equal on lists of either")
  (scsc-check (equal (car obj) (car view)) "equal on the symbols")
  (scsc-check (eq (memq (car view) obj) obj) "memq finds the view in a list of objects")
  (scsc-check (eq (memq (car obj) view) view) "memq finds the object in a list of views")
  (scsc-check (equal (assq (car view) (list (cons (car obj) 1))) (cons 'scsc-both 1)) "assq")
  (scsc-check (equal (delq (car view) (list (car obj) 'scsc-other)) '(scsc-other)) "delq")
  (scsc-check (equal (symbol-name (car view)) (symbol-name (car obj))) "same name")
  (scsc-check (equal (format "%S %s" (car view) (car obj)) "scsc-both scsc-both") "same print")
  (scsc-check (= (sxhash-equal (car view)) (sxhash-equal (car obj))) "same sxhash")
  (let ((h (make-hash-table :test 'equal)))
    (puthash (car view) 1 h)
    (scsc-check (eql (gethash (car obj) h) 1) "an equal table finds an object by its view"))
  (let ((h (make-hash-table :test 'eq)))
    (puthash (car view) 1 h)
    (scsc-check (eql (gethash (car obj) h) 1) "an eq table finds an object by its view"))
  (set (car view) 3)
  (scsc-check (eql (symbol-value (car obj)) 3) "a value set through the view reads through the object")
  (put (car obj) 'scsc-pp 4)
  (scsc-check (eql (get (car view) 'scsc-pp) 4) "a property put through the object reads through the view")
  (fset (car view) #'car)
  (scsc-check (equal (funcall (car obj) '(9)) 9) "a function set through the view runs through the object"))

;; Builtins are called through the symbol however it is stored (the builtin
;; dispatch reads the static id of the symbol).
(let ((fn (car (list 'car))) (plus (car (list '+))) (mem (car (list 'memq))))
  (scsc-check (eql (scsc-class fn) 104) "car is an object in a list")
  (scsc-check (equal (funcall fn '(1 2)) 1) "funcall through the object")
  (scsc-check (equal (apply plus '(1 2 3)) 6) "apply through the object")
  (scsc-check (equal (funcall mem 'b '(a b c)) '(b c)) "memq through the object")
  (scsc-check (equal (eval (list fn '(quote (7 8)))) 7) "eval of a form headed by the object"))
(scsc-off)
(let ((fn (car (list (intern "cdr")))) (plus (car (list (intern "+")))))
  (scsc-check (eql (scsc-class fn) 4) "cdr stored as a view with the switch off")
  (scsc-check (equal (funcall fn '(1 2)) '(2)) "funcall through the view")
  (scsc-check (equal (apply plus '(1 2 3)) 6) "apply through the view"))
(scsc-on)

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
