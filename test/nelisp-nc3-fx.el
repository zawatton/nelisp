;;; nelisp-nc3-fx.el --- functions whose native code uses the nc3 lowering  -*- lexical-binding: t; -*-
;; nativecomp N2b (nc3).  Small fx-compilable functions calling GNU's opcode
;; primitives the lowering inlines (stringp listp symbolp numberp integerp consp
;; <= >= * % equal eq memq assq nth nthcdr length set symbol-value) and the
;; builtins it calls without the call-out (fboundp, string-search ->
;; nelisp--string-search-chars, funcall, apply, `*' / `<=' with more
;; arguments, max).  Their rows are harvested into
;; lisp/nelisp-fxir-native-corpus-data.el (ids 2000 and up); test/nelisp-nc3-corpus.el
;; calls them on the boundary inputs.  No condition-case here (the fx compiler
;; declines handler forms): the corpus catches errors around the calls.

(defun nc3-pred (x) (list (stringp x) (listp x) (symbolp x) (numberp x) (integerp x) (consp x)))
(defun nc3-cmp (a b) (list (<= a b) (>= a b)))
(defun nc3-cmp3 (a b c) (list (<= a b c) (>= a b c)))
(defun nc3-mul (a b) (* a b))
(defun nc3-mul3 (a b c) (* a b c))
(defun nc3-rem (a b) (% a b))
(defun nc3-equal (a b) (equal a b))
(defun nc3-eq (a b) (list (eq a b) (eq b a)))
(defun nc3-memq (x l) (memq x l))
(defun nc3-assq (k l) (assq k l))
(defun nc3-nthcdr (n l) (nthcdr n l))
(defun nc3-nth (n l) (nth n l))
(defun nc3-length (x) (length x))
(defun nc3-set (s v) (set s v))
(defun nc3-symval (s) (symbol-value s))
(defun nc3-fboundp (s) (fboundp s))
(defun nc3-search (n h s) (string-search n h s))
(defun nc3-funcall (f a) (funcall f a))
(defun nc3-funcall0 (f) (funcall f))
(defun nc3-apply (f l) (apply f l))
(defun nc3-max (a b) (list (max a b) (min a b)))
(defun nc3-loop (l)
  ;; A loop of inlined primitives: counts, sums and finds as GNU byte code would.
  (let ((n 0) (s 0) (strs 0) (cur l))
    (while (consp cur)
      (let ((x (car cur)))
        (setq n (1+ n))
        (if (integerp x) (setq s (+ s (% (* x 3) 7))) nil)
        (if (stringp x) (setq strs (+ strs (length x))) nil)
        (if (memq x '(a b c)) (setq s (+ s 100)) nil)
        (if (<= n 2) (setq s (1+ s)) nil))
      (setq cur (cdr cur)))
    (list n s strs (nth 1 l) (nthcdr 2 l) (assq 'k (list (cons 'j 1) (cons 'k 2))))))

(defvar nc3-var 1)
(defconst nc3-names
  '(nc3-pred nc3-cmp nc3-cmp3 nc3-mul nc3-mul3 nc3-rem nc3-equal nc3-eq nc3-memq nc3-assq
    nc3-nthcdr nc3-nth nc3-length nc3-set nc3-symval nc3-fboundp nc3-search nc3-funcall
    nc3-funcall0 nc3-apply nc3-max nc3-loop))
