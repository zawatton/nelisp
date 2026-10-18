;;; fx-spike-check.el --- the word VM gives the evaluator's answers -*- lexical-binding: t -*-

;; Usage: nelisp --load test/perf/fx-spike-check.el
;; tools/ai/eval-core-progress.org S1.1 and S2.  Each form must give the
;; same value on the word VM (`nelisp--fx-eval') as under `eval', and must
;; actually be covered by the VM when marked so.  Then: an effect followed
;; by a fixnum overflow happens exactly once (the VM calls the generic
;; function instead of retrying the form), and conses held only in VM
;; locals survive collections run from inside the VM.  Prints one FX-SPIKE
;; line and exits non-zero on failure.

(defvar fx-spike-failures nil)
(defvar fx-spike-count 0)
(dolist (c '(((let ((i 0) (n 0)) (while (< i n) (setq i (1+ i))) i) . t)
             ((let ((i 0) (n 1)) (while (< i n) (setq i (1+ i))) i) . t)
             ((let ((i 0) (n 1000)) (while (< i n) (setq i (1+ i))) i) . t)
             ((let ((i 1099511627776)) (1+ i)) . t)
             ((let ((a 5) (b 7)) (if (> a b) (- a b) (+ a b))) . t)
             ((let* ((s 0) (i 10)) (while (> i 0) (setq s (+ s i)) (setq i (1- i))) s) . t)
             ((let ((x 3)) (if (= x 3) t nil)) . t)
             ((let ((x "a")) x) . t)
             ((list 1 2) . t)
             ((let ((l nil) (i 0)) (while (< i 5) (setq l (cons i l)) (setq i (1+ i))) l) . t)
             ((let ((x 1.5) (y 2)) (list (+ x y) (< x y) (1+ x))) . t)
             ((let ((v (vector 1 2 3)) (s 0) (i 0)) (while (< i 3) (setq s (+ s (aref v i))) (setq i (1+ i))) s) . t)
             ((when (> 3 2) (cond ((= 1 2) 'a) (t 'b))) . t)
             ((let ((x 2)) (list (cond ((= x 1) 'one) ((> x 5)) ((+ x 10)))
                                 (and x (> x 1) 'yes) (and) (and nil (car 1))
                                 (or nil (> x 9)) (or nil x) (or))) . t)
             ((condition-case nil (car 1) (error 'caught)) . nil)
             ((let ((f (lambda (x) x))) (funcall f 1)) . nil)))
  (let* ((form (car c))
         (covered (and (nelisp--fx-compile form) t))
         (want (eval form t))
         (got (condition-case e (nelisp--fx-eval form) (error (list 'error e)))))
    (unless (and (equal got want) (eq covered (cdr c)))
      (push (list form got want covered) fx-spike-failures))))

;; Errors from a call-out propagate as the same signal.
(let ((want (condition-case e (eval '(let ((x 1)) (car x)) t) (error e)))
      (got (condition-case e (nelisp--fx-eval '(let ((x 1)) (car x))) (error e))))
  (unless (equal got want) (push (list 'signal got want) fx-spike-failures)))

;; An effect, then an overflow: the effect must happen once.
(setq fx-spike-count 0)
(let ((r (condition-case e
             (nelisp--fx-eval '(let ((x most-positive-fixnum))
                                 (setq fx-spike-count (1+ fx-spike-count))
                                 (1+ x)))
           (error (car e)))))
  (unless (= fx-spike-count 1)
    (push (list 'effect-count fx-spike-count r) fx-spike-failures)))

;; Conses held only in VM locals survive collections run from the VM.
(let ((got (nelisp--fx-eval
            '(let ((l nil) (i 0))
               (while (< i 10000)
                 (setq l (cons (* i 3) l))
                 (if (= (% i 1000) 0) (garbage-collect))
                 (setq i (1+ i)))
               (let ((ok t) (k 9999))
                 (while l
                   (if (= (car l) (* k 3)) nil (setq ok nil))
                   (setq l (cdr l))
                   (setq k (1- k)))
                 (list ok k))))))
  (unless (equal got '(t -1)) (push (list 'gc-held-conses got) fx-spike-failures)))

;; A closure whose captured environment holds only bare declarations (a
;; file's `(defvar NAME)' without a value) runs on the VM, reading and
;; setting the declared names dynamically; one that binds a declared name
;; stays with the interpreter, which binds it dynamically.
(defvar fx-spike-dx)
(defvar fx-spike-dy)
(defun fx-spike-dread () fx-spike-dx)
(defun fx-spike-dset (v) (setq fx-spike-dx v) fx-spike-dx)
(defun fx-spike-dready () (if (boundp 'fx-spike-dy) fx-spike-dy 'unbound))
(defun fx-spike-dlet (v) (let ((fx-spike-dy v)) (fx-spike-dready)))
(let ((got nil) (cov nil))
  (dotimes (_ 2)
    (setq got (list (let ((fx-spike-dx 10)) (fx-spike-dread))
                    (let ((fx-spike-dx 1)) (list (fx-spike-dset 5) fx-spike-dx))
                    (fx-spike-dlet 42)
                    (fx-spike-dready))))
  (setq cov (mapcar (lambda (s)
                      (vectorp (gethash (nthcdr 3 (symbol-function s)) nelisp--fx-table)))
                    '(fx-spike-dread fx-spike-dset fx-spike-dready fx-spike-dlet)))
  (unless (equal got '(10 (5 5) 42 unbound))
    (push (list 'declared-env got) fx-spike-failures))
  (unless (equal cov '(t t t nil))
    (push (list 'declared-env-coverage cov) fx-spike-failures)))

(dolist (f (reverse fx-spike-failures)) (princ (format "  MISMATCH %S\n" f)))
(princ (format "FX-SPIKE failures=%d %s\n" (length fx-spike-failures)
               (if fx-spike-failures "FAIL" "PASS")))
(kill-emacs (if fx-spike-failures 1 0))
