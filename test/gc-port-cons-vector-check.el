;;; gc-port-cons-vector-check.el --- cons and make-vector against GNU: every kind of element word  -*- lexical-binding: t; -*-

;; The cons constructor stores a word as it is when it is an immediate or a tagged object and converts a
;; legacy view; make-vector fills with one word in a loop.  This prints what each result holds, for every
;; kind of car, cdr and fill the runtime has, and the error cases; tools/ai/gc-port-make-string.py runs it on
;; GNU Emacs and on the NeLisp binaries and requires identical output.
(defmacro gccv-case (label form)
  `(princ (format "%s: %s\n" ,label
                  (condition-case e (format "%S" ,form) (error (format "ERR %S" (car e)))))))
(let ((vals (list 0 1 -1 most-positive-fixnum most-negative-fixnum nil t 'sym 'nil :kw ?a 1.5 -0.0 (ash 1 70)
                  "str" "" "éあ" (make-string 3 ?x) (list 1 2) (cons 1 2) [1 2 3] (vector) "multiあ"
                  (make-symbol "u") (lambda (x) x) (make-hash-table) (make-bool-vector 3 t))) (n 0))
  (dolist (a vals)
    (setq n (1+ n))
    (gccv-case (format "car %d" n) (car (cons a 7)))
    (gccv-case (format "cdr %d" n) (cdr (cons 7 a)))
    (gccv-case (format "both %d" n) (let ((c (cons a a))) (list (car c) (cdr c) (eq (car c) (cdr c)))))
    (gccv-case (format "eq-car %d" n) (let ((c (cons a nil))) (eq (car c) a)))
    (gccv-case (format "vector %d" n) (make-vector 4 a))
    (gccv-case (format "vector-eq %d" n) (let ((v (make-vector 3 a))) (list (eq (aref v 0) (aref v 2)) (equal (aref v 1) a))))
    (gccv-case (format "list %d" n) (list a a a))
    (gccv-case (format "nested %d" n) (cons (cons a (cons a nil)) (cons nil a)))))
(dolist (n '(0 1 2 3 8 15 16 17 100 1000 5000))
  (gccv-case (format "vec %d nil" n) (let ((v (make-vector n nil))) (list (length v) (aref v (max 0 (1- n))))))
  (gccv-case (format "vec %d 0" n) (let ((v (make-vector n 0))) (list (length v) (apply #'+ (append v nil)))))
  (gccv-case (format "vec %d seven" n) (let ((v (make-vector n 7))) (list (length v) (apply #'+ (append v nil)))))
  (gccv-case (format "vec %d sym" n) (let ((v (make-vector n 'q))) (list (length v) (seq-count (lambda (x) (eq x 'q)) v))))
  (gccv-case (format "vec %d float" n) (let ((v (make-vector n 2.5))) (list (length v) (apply #'+ (append v nil)))))
  (gccv-case (format "vec %d str" n) (let ((v (make-vector n "ab"))) (list (length v) (if (> n 0) (aref v (1- n)) nil)
                                                                              (if (> n 1) (eq (aref v 0) (aref v 1)) t)))))
(gccv-case "vec string shared" (let ((v (make-vector 3 (list 1 2)))) (setcar (aref v 0) 9) v))
(gccv-case "vec aset" (let ((v (make-vector 4 'a))) (aset v 2 "x") (aset v 3 (cons 1 2)) v))
(gccv-case "negative" (make-vector -1 0))
(gccv-case "float length" (make-vector 1.5 0))
(gccv-case "string length" (make-vector "2" 0))
(gccv-case "one arg" (make-vector 3))
(gccv-case "setcar immediate" (let ((c (cons 1 2))) (setcar c 'x) (setcdr c "s") c))
(gccv-case "dotted" (cons 1 (cons 2 3)))
(gccv-case "circular car eq" (let ((c (cons nil nil))) (setcar c c) (eq c (car c))))
nil
