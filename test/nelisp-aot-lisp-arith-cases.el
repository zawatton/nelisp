;;; nelisp-aot-lisp-arith-cases.el --- Lisp arithmetic cases for the AOT differential  -*- lexical-binding: nil; -*-

;;; Commentary:

;; Data and a runner with no dependencies, loadable both by GNU Emacs and
;; by the NeLisp runtime image.  The source is the program under test;
;; `nelisp-aot-lisp-arith-cases-run' calls every case through a caller
;; supplied function and returns one text line per call, so GNU's lines
;; and the native lines can be compared with `equal'.
;;
;; Arguments are small deltas.  A defun adds the fixnum boundary itself
;; (the BIAS letter: p = most-positive-fixnum, n = most-negative-fixnum,
;; z = none), because the loader does not pass a word above 2^60 into an
;; artifact (a pre-existing limit, unrelated to the lowering).

;;; Code:

(defconst nelisp-aot-lisp-arith-cases--mp 2305843009213693951)
(defconst nelisp-aot-lisp-arith-cases--mn -2305843009213693952)

(defun nelisp-aot-lisp-arith-cases--bias (letter var)
  "Return source for VAR moved by the boundary named by LETTER."
  (cond ((eq letter 'p)
         (format "(if (integerp %s) (+ %s %d) %s)" var var
                 nelisp-aot-lisp-arith-cases--mp var))
        ((eq letter 'n)
         (format "(if (integerp %s) (+ %s %d) %s)" var var
                 nelisp-aot-lisp-arith-cases--mn var))
        (t var)))

(defun nelisp-aot-lisp-arith-cases--defs ()
  "Return a list of (NAME ARITY SOURCE) for every defun under test."
  (let ((defs nil))
    (dolist (b '(z p n))
      (let ((x (nelisp-aot-lisp-arith-cases--bias b "x")))
        (push (list (format "lad-inc-%s" b) 1
                    (format "(defun lad-inc-%s (x) (1+ %s))\n" b x))
              defs)
        (push (list (format "lad-dec-%s" b) 1
                    (format "(defun lad-dec-%s (x) (1- %s))\n" b x))
              defs)
        (push (list (format "lad-chain-%s" b) 1
                    (format "(defun lad-chain-%s (x) (let ((y (1+ %s))) (let ((z (1- y))) (- (+ y z) (1+ z)))))\n" b x))
              defs)))
    (dolist (op '("+" "-" "<" "<=" ">" "="))
      (dolist (ab '((z z) (p z) (z p) (p p) (n z) (z n) (n n) (p n)))
        (let ((name (format "lad-%s-%s%s" (cond ((equal op "+") "add") ((equal op "-") "sub")
                                                ((equal op "<") "lt") ((equal op "<=") "le")
                                                ((equal op ">") "gt") (t "eq"))
                            (car ab) (cadr ab))))
          (push (list name 2
                      (format "(defun %s (a b) (%s %s %s))\n" name op
                              (nelisp-aot-lisp-arith-cases--bias (car ab) "a")
                              (nelisp-aot-lisp-arith-cases--bias (cadr ab) "b")))
                defs))))
    ;; Loop-carried words that cross the boundary.
    (push (list "lad-loop-z" 1
                "(defun lad-loop-z (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s i)) (setq i (1+ i))) s))\n")
          defs)
    (push (list "lad-loop-p" 1
                (format "(defun lad-loop-p (n) (let ((i (- %d 3)) (c 0)) (while (< i (+ %d n)) (setq i (1+ i)) (setq c (1+ c))) (+ i c)))\n"
                        nelisp-aot-lisp-arith-cases--mp nelisp-aot-lisp-arith-cases--mp))
          defs)
    (push (list "lad-loop-n" 1
                (format "(defun lad-loop-n (n) (let ((i (+ %d 3)) (c 0)) (while (> i (+ %d (- 0 n))) (setq i (1- i)) (setq c (1+ c))) (+ i c)))\n"
                        nelisp-aot-lisp-arith-cases--mn nelisp-aot-lisp-arith-cases--mn))
          defs)
    (nreverse defs)))

(defconst nelisp-aot-lisp-arith-cases-source
  (apply #'concat (mapcar #'caddr (nelisp-aot-lisp-arith-cases--defs)))
  "The defuns under test, as source text.")

(defconst nelisp-aot-lisp-arith-cases-names
  (mapcar #'car (nelisp-aot-lisp-arith-cases--defs)))

(defun nelisp-aot-lisp-arith-cases--calls ()
  "Return the list of (NAME . ARGS) calls."
  (let* ((mk (copy-marker 1))
         (vals (list -1 0 1 1.5 'a mk))
         (un (append (list -2 2) vals (list nil "s")))
         (calls nil))
    (dolist (d (nelisp-aot-lisp-arith-cases--defs))
      (let ((name (car d)))
        (cond
         ((string-prefix-p "lad-loop" name)
          (dolist (n '(0 1 2 5)) (push (cons name (list n)) calls)))
         ((= (cadr d) 1)
          (dolist (a un) (push (cons name (list a)) calls)))
         (t
          (dolist (a vals) (dolist (b vals) (push (cons name (list a b)) calls)))))))
    (nreverse calls)))

(defun nelisp-aot-lisp-arith-cases-run (call-fn)
  "Call every case through CALL-FN (NAME ARGS) and return the text lines."
  (mapcar
   (lambda (c)
     (format "%s %S => %S" (car c)
             (mapcar (lambda (a) (if (markerp a) (list 'marker (marker-position a)) a))
                     (cdr c))
             (condition-case e (list 'value (funcall call-fn (car c) (cdr c)))
               (error (list 'signal (car e) (cdr e))))))
   (nelisp-aot-lisp-arith-cases--calls)))

(provide 'nelisp-aot-lisp-arith-cases)
;;; nelisp-aot-lisp-arith-cases.el ends here
