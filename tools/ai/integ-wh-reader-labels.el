;;; integ-wh-reader-labels.el --- #N= labels inside and across byte-code objects  -*- lexical-binding: t; -*-

;; Run with `-l' on NeLisp and on GNU Emacs 31.1; both must print the same bytes.  GNU's read substitutes the
;; #N# placeholders of a read with `substitute_object_recurse', which walks conses, vectors, records and the slots of
;; byte-code and interpreted closures.  The runtime's reader walked conses and vectors only, so the constants vector
;; of a #[...] object read after a label on an earlier byte-code string kept the placeholder
;; (nelisp--rd-label-reference . N) where GNU has the labelled object.
;; Each case reads one form and prints its printed representation, with print-circle off for an acyclic result, and the
;; eq facts that say whether sharing was kept.

(defun iwh-rl-read (text) (car (read-from-string text)))
(defun iwh-rl-case (name thunk)
  (princ (format "RL %-34s %s\n" name
                 (condition-case e (let ((print-circle nil)) (format "%S" (funcall thunk))) (error (format "ERR %S" (car e)))))))
(defmacro iwh-rl (name &rest body) `(iwh-rl-case ,name (lambda () ,@body)))
(defun iwh-rl-k (f i) (aref f i))

;; the census's form: the byte-code string carries its own label and a later object reuses it
(iwh-rl "census form prin1"
        (prin1-to-string (iwh-rl-read "(#[0 #7=\"\\300\\207\" [#6=\"E\"] 1] #[0 #7# [#6#] 1])")))
(iwh-rl "census form facts"
        (let ((l (iwh-rl-read "(#[0 #7=\"\\300\\207\" [#6=\"E\"] 1] #[0 #7# [#6#] 1])")))
          (list (eq (iwh-rl-k (car l) 1) (iwh-rl-k (cadr l) 1))
                (eq (aref (iwh-rl-k (car l) 2) 0) (aref (iwh-rl-k (cadr l) 2) 0))
                (stringp (aref (iwh-rl-k (cadr l) 2) 0)))))
(iwh-rl "code label reused three times"
        (let ((l (iwh-rl-read "(#[0 #1=\"\\300\\207\" [a] 1] #[0 #1# [b] 1] #[0 #1# [c] 1])")))
          (list (prin1-to-string l)
                (eq (iwh-rl-k (nth 0 l) 1) (iwh-rl-k (nth 1 l) 1))
                (eq (iwh-rl-k (nth 1 l) 1) (iwh-rl-k (nth 2 l) 1)))))
(iwh-rl "constants label only"
        (let ((l (iwh-rl-read "(#[0 \"\\300\\207\" [#6=\"E\"] 1] #[0 \"\\300\\207\" [#6#] 1])")))
          (list (prin1-to-string l) (eq (aref (iwh-rl-k (car l) 2) 0) (aref (iwh-rl-k (cadr l) 2) 0)))))
(iwh-rl "label in a nested vector"
        (let ((l (iwh-rl-read "(#[0 \"\\300\\207\" [#1=[1 2] x] 1] #[0 \"\\300\\207\" [#1# y] 1])")))
          (list (prin1-to-string l) (eq (aref (iwh-rl-k (car l) 2) 0) (aref (iwh-rl-k (cadr l) 2) 0)))))
(iwh-rl "label in a deeply nested constant"
        (let ((l (iwh-rl-read "(#[0 #9=\"\\300\\207\" [(a #2=[b (c #3=\"d\")] e)] 1] #[0 #9# [(f #2# #3#)] 1])")))
          (list (prin1-to-string l)
                (eq (nth 1 (car (aref (iwh-rl-k (car l) 2) 0))) (nth 1 (car (aref (iwh-rl-k (cadr l) 2) 0))))
                (eq (nth 2 (car (aref (iwh-rl-k (cadr l) 2) 0))) (nth 1 (aref (nth 1 (car (aref (iwh-rl-k (car l) 2) 0))) 1))))))
(iwh-rl "label across list and byte-code"
        (let ((l (iwh-rl-read "(#6=\"E\" #[0 \"\\300\\207\" [#6#] 1])")))
          (list (prin1-to-string l) (eq (car l) (aref (iwh-rl-k (cadr l) 2) 0)))))
(iwh-rl "byte-code first, list later"
        (let ((l (iwh-rl-read "(#[0 \"\\300\\207\" [#6=\"E\"] 1] #6#)")))
          (list (prin1-to-string l) (eq (aref (iwh-rl-k (car l) 2) 0) (cadr l)))))
(iwh-rl "doc string label"
        (let ((l (iwh-rl-read "(#[0 \"\\300\\207\" [] 1 #1=\"documented\"] #[0 \"\\300\\207\" [] 1 #1#])")))
          (list (prin1-to-string l) (eq (iwh-rl-k (car l) 4) (iwh-rl-k (cadr l) 4)))))
(iwh-rl "arglist label"
        (let ((l (iwh-rl-read "(#[#1=(a b) \"\\300\\207\" [] 1] #[#1# \"\\300\\207\" [] 1])")))
          (list (prin1-to-string l) (eq (iwh-rl-k (car l) 0) (iwh-rl-k (cadr l) 0)))))
(iwh-rl "cycle through the constants"
        (let* ((l (iwh-rl-read "#1=(#[0 \"\\300\\207\" [#1#] 1])")))
          (list (eq (aref (iwh-rl-k (car l) 2) 0) l))))
(iwh-rl "self reference"
        (let* ((f (iwh-rl-read "#1=#[0 \"\\300\\207\" [#1#] 1]")))
          (list (eq (aref (iwh-rl-k f 2) 0) f))))
;; make-interpreted-closure copies the environment (GNU keeps the list), so the environment cases print the closures and
;; make no eq fact about it
(iwh-rl "interpreted closure env label"
        (let ((l (iwh-rl-read "(#[(x) ((+ x 1)) ((a . #1=(1 2)))] #[(y) (y) ((b . #1#))])")))
          (prin1-to-string l)))
(iwh-rl "interpreted closure env shared vector"
        (let ((l (iwh-rl-read "(#[(x) (x) ((v . #1=[1 2]))] #[(y) (y) ((w . #1#) (z . #1#))])")))
          (prin1-to-string l)))
(iwh-rl "interpreted closure body label"
        (let ((l (iwh-rl-read "(#[(x) (#1=(+ x 1) #1#) ((z . 0))] 5)")))
          (list (prin1-to-string l) (eq (nth 0 (iwh-rl-k (car l) 1)) (nth 1 (iwh-rl-k (car l) 1))))))
(iwh-rl "interpreted closure arglist label"
        (let ((l (iwh-rl-read "(#[#1=(x) (x) ((z . 0))] #[#1# (x) ((z . 1))])")))
          (prin1-to-string l)))
(iwh-rl "two reads do not share labels"
        (let* ((a (iwh-rl-read "(#[0 #7=\"\\300\\207\" [#6=\"E\"] 1] #[0 #7# [#6#] 1])"))
               (b (iwh-rl-read "(#[0 #7=\"\\300\\207\" [#6=\"F\"] 1] #[0 #7# [#6#] 1])")))
          (list (prin1-to-string a) (prin1-to-string b))))
(iwh-rl "label reused in a plain list"
        (let ((l (iwh-rl-read "(#1=\"s\" #[0 #2=\"\\300\\207\" [#1#] 1] #2# #1#)")))
          (list (prin1-to-string l) (eq (car l) (aref (iwh-rl-k (cadr l) 2) 0)) (eq (nth 2 l) (iwh-rl-k (cadr l) 1)) (eq (car l) (nth 3 l)))))
(iwh-rl "undefined label"
        (iwh-rl-read "(#[0 \"\\300\\207\" [#5#] 1])"))
(iwh-rl "read-from-string end"
        (cdr (read-from-string "(#[0 #7=\"\\300\\207\" [#6=\"E\"] 1] #[0 #7# [#6#] 1]) tail")))
(princ "INTEG-WH-READER-LABELS DONE\n")
