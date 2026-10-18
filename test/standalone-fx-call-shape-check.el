;;; standalone-fx-call-shape-check.el --- word VM calls: arity, &optional, &rest -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-fx-call-shape-check.el
;;
;; Functions with &optional and &rest run on the word VM, called from VM
;; code (arguments passed as words, no list) and from the interpreter (an
;; argument list).  Each answer below is GNU Emacs 31.1's.  Also checked:
;; wrong arity still signals `wrong-number-of-arguments', a &rest list is
;; fresh, heap results of successive calls do not alias, values held only
;; in VM locals survive a collection, and a VM-to-VM call's backtrace frame
;; shows the function name and its arguments.  Runs unchanged on GNU Emacs.
;; Prints one FX-CALL-SHAPE line; non-zero exit on failure.

(defun fxs-a (a &optional b c) (list a b c))
(defun fxs-r (a &rest r) (cons a r))
(defun fxs-or (a &optional b &rest r) (list a b r))
(defun fxs-r0 (&rest r) r)
(defun fxs-mut (&rest r) (setcar r 'x) r)
(defun fxs-cat (a b) (concat a b))
(defun fxs-ten (a b c d e f g h i j) (list j i h g f e d c b a))

(defun fxs-vm-calls ()
  (list (fxs-a 1) (fxs-a 1 2) (fxs-a 1 2 3)
        (fxs-r 1) (fxs-r 1 2 3)
        (fxs-or 1) (fxs-or 1 2) (fxs-or 1 2 3 4)
        (fxs-r0) (fxs-r0 1 2)
        (fxs-r "s" '(1 2) 1.5)
        (fxs-ten 1 2 3 4 5 6 7 8 9 10)))
(defun fxs-vm-few () (fxs-a))
(defun fxs-vm-many () (fxs-a 1 2 3 4))
(defun fxs-vm-mut () (let ((l (list 1 2))) (list (apply #'fxs-mut l) l)))
(defun fxs-vm-two () (let ((x (fxs-cat "x" "1")) (y (fxs-cat "y" "2"))) (list x y)))
(defun fxs-vm-hold ()
  (let ((s (concat "a" "b")) (l (list 1 2)) (f (* 1.5 2)))
    (garbage-collect)
    (fxs-r s l f (fxs-cat s "c"))))

(defvar fxs-frames nil)
(defvar fxs-count 0)
(defun fxs-grab ()
  (mapbacktrace (lambda (_e f a _fl) (when (memq f '(fxs-leaf fxs-rleaf)) (push (cons f a) fxs-frames)))))
(defun fxs-leaf (a b c) (fxs-grab) (+ a b c))
(defun fxs-rleaf (a &rest r) (fxs-grab) (cons a r))
(defun fxs-mid (x) (let ((y (* x 2))) (list (fxs-leaf x y 7) (fxs-rleaf x y 8))))

(defconst fxs-calls-want
  '((1 nil nil) (1 2 nil) (1 2 3) (1) (1 2 3) (1 nil nil) (1 2 nil) (1 2 (3 4))
    nil (1 2) ("s" (1 2) 1.5) (10 9 8 7 6 5 4 3 2 1)))

(let* ((fails 0)
       (check (lambda (what got want)
                (unless (equal got want)
                  (setq fails (1+ fails))
                  (princ (format "FX-CALL-SHAPE FAIL %s got %S want %S\n" what got want))))))
  (when (boundp 'nelisp--fx-table)
    (dolist (f '(fxs-a fxs-r fxs-or fxs-r0 fxs-mut fxs-ten fxs-vm-calls fxs-vm-two fxs-vm-hold
                 fxs-leaf fxs-rleaf fxs-mid))
      (funcall check (list 'covered f)
               (vectorp (gethash (nthcdr 3 (symbol-function f)) nelisp--fx-table)) t)))
  (funcall check 'vm-calls (fxs-vm-calls) fxs-calls-want)
  (funcall check 'interp-calls
           (eval '(list (fxs-a 1) (fxs-a 1 2) (fxs-a 1 2 3)
                        (fxs-r 1) (fxs-r 1 2 3)
                        (fxs-or 1) (fxs-or 1 2) (fxs-or 1 2 3 4)
                        (fxs-r0) (fxs-r0 1 2)
                        (fxs-r "s" '(1 2) 1.5)
                        (fxs-ten 1 2 3 4 5 6 7 8 9 10))
                 t)
           fxs-calls-want)
  (funcall check 'apply-calls
           (list (apply #'fxs-or '(1 2 3)) (funcall #'fxs-a 1 2) (apply #'fxs-r0 nil))
           '((1 2 (3)) (1 2 nil) nil))
  (funcall check 'vm-few (condition-case e (fxs-vm-few) (error (car e))) 'wrong-number-of-arguments)
  (funcall check 'vm-many (condition-case e (fxs-vm-many) (error (car e))) 'wrong-number-of-arguments)
  (funcall check 'interp-few (condition-case e (eval '(fxs-a) t) (error (car e))) 'wrong-number-of-arguments)
  (funcall check 'interp-many (condition-case e (eval '(fxs-a 1 2 3 4) t) (error (car e)))
           'wrong-number-of-arguments)
  (funcall check 'vm-rest-fresh (fxs-vm-mut) '((x 2) (1 2)))
  (funcall check 'interp-rest-fresh (let ((l (list 1 2))) (list (apply #'fxs-mut l) l)) '((x 2) (1 2)))
  (funcall check 'vm-heap-results (fxs-vm-two) '("x1" "y2"))
  (funcall check 'vm-gc-hold (fxs-vm-hold) '("ab" (1 2) 3.0 "abc"))
  ;; Interpreted `let' and argument lists whose init forms call the VM, and
  ;; a VM call whose argument signals after an earlier one was evaluated.
  (funcall check 'interp-let
           (eval '(let ((a (fxs-cat "p" "q")) (b (fxs-a 1 (fxs-cat "r" "s"))) (c (fxs-r 1 2)))
                    (list a b c (fxs-or (fxs-cat a "!") b))) t)
           '("pq" (1 "rs" nil) (1 2) ("pq!" (1 "rs" nil) nil)))
  (funcall check 'interp-let*
           (eval '(let* ((a (fxs-cat "p" "q")) (b (fxs-r a a))) (list a b)) t)
           '("pq" ("pq" "pq")))
  (funcall check 'interp-arg-signal
           (progn
             (setq fxs-count 0)
             (list (condition-case e (eval '(fxs-a (setq fxs-count (1+ fxs-count)) (car 1)) t)
                     (error (car e)))
                   fxs-count))
           '(wrong-type-argument 1))
  (funcall check 'interp-nested
           (eval '(fxs-r (fxs-a (fxs-cat "a" "b") (fxs-r0 1 2 3)) (fxs-ten 1 2 3 4 5 6 7 8 9 10)) t)
           '(("ab" (1 2 3) nil) (10 9 8 7 6 5 4 3 2 1)))
  (setq fxs-frames nil)
  (funcall check 'mid (fxs-mid 5) '(22 (5 10 8)))
  (funcall check 'backtrace (reverse fxs-frames) '((fxs-leaf 5 10 7) (fxs-rleaf 5 10 8)))
  (setq fxs-frames nil)
  (funcall check 'interp-leaf (eval '(let ((x 4)) (fxs-leaf x (* x 2) 9)) t) 21)
  (funcall check 'interp-backtrace fxs-frames '((fxs-leaf 4 8 9)))
  (princ (format "FX-CALL-SHAPE failures=%d %s\n" fails (if (= fails 0) "PASS" "FAIL")))
  (kill-emacs (if (= fails 0) 0 1)))
