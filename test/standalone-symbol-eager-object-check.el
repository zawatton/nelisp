;;; standalone-symbol-eager-object-check.el --- a name has its symbol object when it is interned -*- lexical-binding: t; -*-

;; Doc 226 D9 stage 3.1.  GNU's intern_c_string_1 (lread.c) makes the
;; struct Lisp_Symbol with the name: an interned symbol is complete from the
;; moment `intern' answers.  NeLisp made the object on first use, so a symbol
;; with no state had none and each consumer walked the mirror's hash; the
;; intern path now makes it (void value and function, empty plist, the name
;; string in the name cell, the key in its bucket chain and the cell in the
;; cells vector).
;;
;; (nelisp--debug-switch 45 SYM) answers what SYM has in the mirror: 0 no
;; object, 1 an interned object with its name string, 2 an object with a nil
;; name cell, 3 an entry that is not an interned symbol object.  A fresh
;; name must answer 1 however it was interned (`intern', the reader, a quoted
;; literal), the symbol must behave as a symbol with no state, and nothing
;; may change across collections, the cells vector growing, `unintern' and
;; writes to the new object.

(defvar seoc-checks 0)
(defvar seoc-failures 0)
(defun seoc-check (ok label)
  (setq seoc-checks (1+ seoc-checks))
  (unless ok
    (setq seoc-failures (1+ seoc-failures))
    (princ (format "SEOC FAIL %s\n" label))))

(defun seoc-state (sym) (nelisp--debug-switch 45 sym))

;; The probe itself: it must see an object that exists, and not invent one.
(seoc-check (eql (seoc-state nil) 0) "nil has no object")
(seoc-check (eql (seoc-state t) 0) "t has no object")
(seoc-check (eql (seoc-state (make-symbol "seoc-uninterned")) 3)
            "an uninterned symbol's object is not an interned one")

;; Fresh names by `intern', across lengths and encodings.
(let ((names (list "seoc-a" "s" "seoc-with space" "seoc-\\escaped" "seoc-日本語"
                   "seoc-ünï" (make-string 100 ?x) (concat "seoc-" (make-string 300 ?y))
                   "seoc-1" "seoc-2" "seoc-+" "seoc-:keyword-like")))
  (dolist (name names)
    (let ((sym (intern name)))
      (seoc-check (eql (seoc-state sym) 1) (format "intern %S has its object with a name" name))
      (seoc-check (equal (symbol-name sym) name) (format "intern %S names itself" name))
      (seoc-check (eq sym (intern name)) (format "intern %S answers the same symbol again" name))
      (seoc-check (eq sym (intern-soft name)) (format "intern-soft %S finds it" name))
      (seoc-check (not (boundp sym)) (format "%S is void" name))
      (seoc-check (not (fboundp sym)) (format "%S has no function" name))
      (seoc-check (null (symbol-plist sym)) (format "%S has an empty plist" name))
      (seoc-check (null (symbol-function sym)) (format "%S has a nil function" name))
      (seoc-check (condition-case nil (progn (symbol-value sym) nil) (void-variable t))
                  (format "symbol-value of %S signals void-variable" name)))))

;; A keyword is its own constant value, as in GNU's `intern' (lread.c).
(let ((sym (intern ":seoc-keyword")))
  (seoc-check (eql (seoc-state sym) 1) "a keyword has its object")
  (seoc-check (and (boundp sym) (eq (symbol-value sym) sym)) "a keyword is bound to itself")
  (seoc-check (keywordp sym) "and is a keyword"))

;; The empty name.
(let ((sym (intern "")))
  (seoc-check (eql (seoc-state sym) 1) "the empty name has its object")
  (seoc-check (equal (symbol-name sym) "") "the empty name names itself"))

;; The reader interns too.
(let ((sym (car (read-from-string "seoc-read-sym"))))
  (seoc-check (eql (seoc-state sym) 1) "a read symbol has its object")
  (seoc-check (eq sym (intern "seoc-read-sym")) "a read symbol is the interned one"))
(let ((forms (car (read-from-string "(seoc-r1 seoc-r2 (seoc-r3 . seoc-r4) [seoc-r5])"))))
  (seoc-check (eql (seoc-state (car forms)) 1) "a symbol in a read list has its object")
  (seoc-check (eql (seoc-state (car (nth 2 forms))) 1) "a symbol in a nested list has its object")
  (seoc-check (eql (seoc-state (cdr (nth 2 forms))) 1) "a symbol in a dotted tail has its object")
  (seoc-check (eql (seoc-state (aref (nth 3 forms) 0)) 1) "a symbol in a vector has its object"))

;; A literal of this very file was interned when the file was read.
(seoc-check (eql (seoc-state 'seoc-quoted-literal) 1) "a quoted literal has its object")
(seoc-check (eql (seoc-state (car '(seoc-quoted-list-member))) 1)
            "a symbol in a quoted list has its object")

;; A symbol the object made for it behaves as the symbol it names.
(let ((sym (intern "seoc-writes")))
  (seoc-check (eql (seoc-state sym) 1) "seoc-writes starts with its object")
  (set sym 7)
  (seoc-check (eql (symbol-value sym) 7) "set stores in the object")
  (seoc-check (eql (seoc-state sym) 1) "set keeps the object")
  (put sym 'seoc-prop 8)
  (seoc-check (eql (get sym 'seoc-prop) 8) "put stores in the plist")
  (fset sym 'car)
  (seoc-check (eq (symbol-function sym) 'car) "fset stores in the function cell")
  (seoc-check (equal (funcall sym '(1 2)) 1) "the function runs")
  (makunbound sym)
  (seoc-check (not (boundp sym)) "makunbound makes it void again")
  (fmakunbound sym)
  (seoc-check (not (fboundp sym)) "fmakunbound makes it void again")
  (seoc-check (eql (get sym 'seoc-prop) 8) "the plist is untouched by both")
  (seoc-check (equal (symbol-name sym) "seoc-writes") "the name is untouched"))

;; unintern: the old symbol keeps its object, a new interning makes another.
(let* ((old (intern "seoc-unintern"))
       (state-before (seoc-state old)))
  (set old 1)
  (seoc-check (eql state-before 1) "the name had its object before unintern")
  (seoc-check (unintern "seoc-unintern" nil) "unintern removes the name")
  (seoc-check (null (intern-soft "seoc-unintern")) "the name is gone")
  (seoc-check (eql (symbol-value old) 1) "the removed symbol keeps its value")
  (let ((new (intern "seoc-unintern")))
    (seoc-check (eql (seoc-state new) 1) "a new interning has its object")
    (seoc-check (not (boundp new)) "the new symbol is void")
    (seoc-check (eql (symbol-value old) 1) "the old one is unchanged by the new")
    (seoc-check (equal (symbol-name new) "seoc-unintern") "the new one names itself")))

;; The apply path sorts the builtins it intercepts out by the static id in a symbol
;; object's meta word: each of them still runs.
(seoc-check (eql (eval '(+ 1 2)) 3) "eval")
(seoc-check (eql (funcall '+ 1 2) 3) "funcall")
(seoc-check (eql (apply '+ '(1 2)) 3) "apply")
(seoc-check (progn (fset 'seoc-fn-alias 'car) (eq (symbol-function 'seoc-fn-alias) 'car)) "fset and symbol-function")
(seoc-check (equal (condition-case e (signal 'seoc-sig '(1)) (seoc-sig (cdr e))) '(1)) "signal")
(seoc-check (equal (condition-case e (signal 'error '("seoc")) (error (cdr e))) '("seoc")) "signal of error")
(seoc-check (eql (funcall (intern "car") '(7 8)) 7) "a builtin named by intern")
(seoc-check (eq (condition-case e (funcall (intern "seoc-not-a-builtin-so-void") 1) (void-function (car e)))
                'void-function)
            "a function that is not a builtin is not taken for one")

;; A name interned again after unintern is a new symbol, and the old one keeps
;; its identity, hash and name through any number of cycles.
(let ((syms nil))
  (dotimes (k 5)
    (let ((s (intern "seoc-cycle")))
      (push s syms)
      (set s k)
      (seoc-check (eq s (intern "seoc-cycle")) (format "cycle %d: intern answers the symbol" k))
      (unintern "seoc-cycle" nil)))
  (seoc-check (equal (mapcar #'symbol-value syms) '(4 3 2 1 0)) "each removed symbol keeps its own value")
  (seoc-check (let ((ok t) (l syms))
                (while l
                  (let ((m (cdr l)))
                    (while m (when (eq (car l) (car m)) (setq ok nil)) (setq m (cdr m))))
                  (setq l (cdr l)))
                ok)
              "five removed symbols are five symbols")
  (seoc-check (null (memq (intern "seoc-cycle") syms)) "and the new interning is none of them")
  (seoc-check (let ((h (make-hash-table :test 'eq)) (ok t))
                (dolist (s syms) (puthash s (symbol-value s) h))
                (dolist (s syms) (unless (eql (gethash s h) (symbol-value s)) (setq ok nil)))
                (and ok (= (hash-table-count h) 5)))
              "an eq table keeps them apart")
  (seoc-check (equal (mapcar #'symbol-name syms) (make-list 5 "seoc-cycle")) "all five are named seoc-cycle"))

;; Many names: the cells vector grows from 16 on, the buckets get chains.
(let ((syms nil) (i 0) (before (length (nelisp--intern-table-symbols))))
  (while (< i 6000)
    (push (intern (format "seoc-bulk-%d" i)) syms)
    (setq i (1+ i)))
  (garbage-collect)
  (let ((ok t) (named t) (j 5999))
    (dolist (sym syms)
      (unless (eql (seoc-state sym) 1) (setq ok nil))
      (unless (equal (symbol-name sym) (format "seoc-bulk-%d" j)) (setq named nil))
      (setq j (1- j)))
    (seoc-check ok "6000 interned names all have their objects after a collection")
    (seoc-check named "6000 interned names all keep their names"))
  (seoc-check (= (- (length (nelisp--intern-table-symbols)) before) 6000)
              "the intern table lists exactly the 6000 new names")
  (seoc-check (let ((ok t) (j 0))
                (while (< j 6000)
                  (unless (eq (intern-soft (format "seoc-bulk-%d" j)) (nth (- 5999 j) syms)) (setq ok nil))
                  (setq j (+ j 37)))
                ok)
              "intern-soft finds a sample of them, as the same symbols")
  (dolist (sym syms) (set sym 1))
  (garbage-collect)
  (let ((ok t))
    (dolist (sym syms)
      (unless (eql (symbol-value sym) 1) (setq ok nil)))
    (seoc-check ok "6000 values survive a collection")))

(garbage-collect)
(seoc-check (eql (seoc-state (intern "seoc-a")) 1) "an early name still has its object")
(seoc-check (equal (symbol-name (intern "seoc-a")) "seoc-a") "and its name")

(princ (format "SEOC checks=%d failures=%d %s\n" seoc-checks seoc-failures
               (if (= seoc-failures 0) "PASS" "FAIL")))
(when (> seoc-failures 0) (error "symbol eager object check failed"))
