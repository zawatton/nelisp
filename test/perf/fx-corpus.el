;;; fx-corpus.el --- generated defuns for VM/interpreter equality -*- lexical-binding: t -*-
;; Usage: [FX_CORPUS_N=200] [FX_CORPUS_SEED=1] nelisp --load test/perf/fx-corpus.el
;; tools/ai/eval-core-progress.org S2.2.  Generates FX_CORPUS_N lexical
;; defuns from a fixed-seed generator over the forms the word VM covers
;; (plus a few it does not, which stay with the interpreter), calls each on
;; several argument lists, and prints one line per call: the value, or the
;; signal's symbol.  test/perf/fx-corpus-check.sh runs this with the VM and
;; with NELISP_FX_DISABLE=1 and requires identical output.  The last line,
;; FX-CORPUS covered=K/N, counts the defuns the VM runs.

(defvar fx-corpus-state 1)
(defun fx-corpus-rand (n)
  "A pseudo-random integer in [0, N): a 31-bit LCG, the same on every host."
  (setq fx-corpus-state (% (+ (* fx-corpus-state 1103515245) 12345) 2147483648))
  (% (/ fx-corpus-state 65536) n))
(defun fx-corpus-pick (l) (nth (fx-corpus-rand (length l)) l))

(defvar fx-corpus-funs nil "Generated so far: (NAME MIN MAX), MAX nil for &rest.")
(defvar fx-corpus-varno 0)

(defun fx-corpus-new-var ()
  (setq fx-corpus-varno (1+ fx-corpus-varno))
  (intern (format "v%d" fx-corpus-varno)))

(defun fx-corpus-leaf (vars)
  (let ((r (fx-corpus-rand 10)))
    (cond ((and vars (< r 5)) (fx-corpus-pick vars))
          ((< r 7) (- (fx-corpus-rand 20) 5))
          ((= r 7) (fx-corpus-pick '(nil t "s" foo 2.5)))
          ((= r 8) (fx-corpus-pick (list most-positive-fixnum most-negative-fixnum
                                         (1- most-positive-fixnum))))
          (t (list 'quote (fx-corpus-pick '((1 2 3) (a . b) [1 2] nil)))))))

(defun fx-corpus-num (d vars)
  "A form that yields a number (fixnum overflow included) unless a call signals."
  (let ((r (fx-corpus-rand 6)))
    (cond ((and vars (< r 2)) (let ((v (fx-corpus-pick vars))) `(if (numberp ,v) ,v 1)))
          ((or (<= d 0) (= r 2))
           (if (= (fx-corpus-rand 5) 0)
               (fx-corpus-pick (list most-positive-fixnum most-negative-fixnum 2.5))
             (- (fx-corpus-rand 20) 5)))
          ((= r 3) (list (fx-corpus-pick '(+ - * max)) (fx-corpus-num (1- d) vars) (fx-corpus-num (1- d) vars)))
          ((= r 4) (list (fx-corpus-pick '(1+ 1-)) (fx-corpus-num (1- d) vars)))
          (t (let ((x (fx-corpus-expr (1- d) vars))) `(if (numberp ,x) ,x (length (list ,x))))))))

(defun fx-corpus-expr (d vars)
  (if (<= d 0) (fx-corpus-leaf vars)
    (let ((e (lambda () (fx-corpus-expr (1- d) vars)))
          (r (fx-corpus-rand 22)))
      (cond
       ((= r 0) (list (fx-corpus-pick '(+ - < > =)) (fx-corpus-num (1- d) vars) (fx-corpus-num (1- d) vars)))
       ((= r 1) (list (fx-corpus-pick '(1+ 1-)) (if (= (fx-corpus-rand 6) 0) (funcall e) (fx-corpus-num (1- d) vars))))
       ((= r 2) (list (fx-corpus-pick '(* max min)) (fx-corpus-num (1- d) vars) (fx-corpus-num (1- d) vars)))
       ((= r 3) (list 'if (funcall e) (funcall e) (funcall e)))
       ((= r 4) (list (fx-corpus-pick '(and or)) (funcall e) (funcall e)))
       ((= r 5) (list 'cond (list (funcall e) (funcall e)) (list (funcall e)) (list t (funcall e))))
       ((memq r '(6 7))
        (let* ((v (fx-corpus-new-var)) (w (fx-corpus-new-var))
               (op (fx-corpus-pick '(let let*)))
               (inner (if (eq op 'let*) (cons w (cons v vars)) vars)))
          (list op (list (list v (funcall e)) (list w (fx-corpus-expr (1- d) (if (eq op 'let*) (cons v vars) vars))))
                (fx-corpus-expr (1- d) (cons w (cons v vars)))
                (fx-corpus-expr (1- d) (if (eq op 'let*) inner (cons w (cons v vars)))))))
       ((and (= r 8) vars)
        (let ((v (fx-corpus-pick vars)))
          (list 'progn (list 'setq v (funcall e)) v)))
       ((= r 9)
        (let ((k (fx-corpus-new-var)) (acc (fx-corpus-new-var)))
          `(let ((,k 0) (,acc ,(funcall e)))
             (while (< ,k 3)
               (setq ,acc (if (numberp ,acc) (+ ,acc ,k) (list ,acc ,k)))
               (setq ,k (1+ ,k)))
             ,acc)))
       ((= r 10) (list (fx-corpus-pick '(car-safe cdr-safe not null consp numberp car)) (funcall e)))
       ((= r 11) (list (fx-corpus-pick '(cons list eq equal)) (funcall e) (funcall e)))
       ((= r 12) (list 'length (list 'list (funcall e) (funcall e))))
       ((= r 13) (list 'format "%S-%S" (funcall e) (funcall e)))
       ((and (>= r 14) (<= r 17) fx-corpus-funs)
        (let* ((f (fx-corpus-pick fx-corpus-funs))
               (lo (nth 1 f)) (hi (or (nth 2 f) (+ lo 3)))
               ;; Mostly a valid count, sometimes one too few or too many.
               (n (if (= (fx-corpus-rand 24) 0) (if (> lo 0) (1- lo) (1+ hi))
                    (+ lo (fx-corpus-rand (1+ (- hi lo)))))))
          (cons (car f) (let ((args nil)) (dotimes (_ n) (push (funcall e) args)) args))))
       ((and (= r 18) fx-corpus-funs)
        (let* ((f (fx-corpus-pick fx-corpus-funs)) (n (nth 1 f)))
          (list 'apply (list 'quote (car f))
                (cons 'list (let ((args nil)) (dotimes (_ n) (push (funcall e) args)) args)))))
       ;; Forms the VM does not cover: the defun stays interpreted.
       ((= r 19) (list 'catch ''fx-tag (list 'throw ''fx-tag (funcall e))))
       ((= r 20) (list 'condition-case 'err (funcall e) '(error (list 'caught (car err)))))
       (t (fx-corpus-leaf vars))))))

(defun fx-corpus-arglist ()
  (let ((r (fx-corpus-rand 6)) (a (fx-corpus-new-var)) (b (fx-corpus-new-var)) (c (fx-corpus-new-var)))
    (cond ((= r 0) (list nil 0 0))
          ((= r 1) (list (list a) 1 1))
          ((= r 2) (list (list a b) 2 2))
          ((= r 3) (list (list a '&optional b c) 1 3))
          ((= r 4) (list (list a '&rest b) 1 nil))
          (t (list (list a b '&optional c '&rest (fx-corpus-new-var)) 2 nil)))))

(defun fx-corpus-params (arglist)
  (let ((ps nil)) (dolist (p arglist) (unless (memq p '(&optional &rest)) (push p ps))) ps))

(let* ((n (if (getenv "FX_CORPUS_N") (string-to-number (getenv "FX_CORPUS_N")) 200))
       (covered 0))
  (setq fx-corpus-state (if (getenv "FX_CORPUS_SEED") (string-to-number (getenv "FX_CORPUS_SEED")) 1))
  (dotimes (i n)
    (let* ((name (intern (format "fx-corpus-f%d" i)))
           (shape (fx-corpus-arglist))
           (body (fx-corpus-expr (+ 2 (fx-corpus-rand 3)) (fx-corpus-params (car shape)))))
      (eval `(defun ,name ,(car shape) ,body) t)
      (push (list name (nth 1 shape) (nth 2 shape)) fx-corpus-funs)))
  ;; Every valid argument count (up to two past MIN with &rest) on two
  ;; value pools, and one count too many.
  (dolist (f (reverse fx-corpus-funs))
    (dolist (args (let ((lo (nth 1 f)) (hi (or (nth 2 f) (+ (nth 1 f) 2))) (out nil))
                    (dolist (pool '((3 -4 5 6 7) (2 "x" (1 2) nil 2.5)))
                      (let ((k lo))
                        (while (<= k hi) (push (butlast pool (- 5 k)) out) (setq k (1+ k)))))
                    (when (nth 2 f) (push (make-list (1+ hi) 1) out))
                    (nreverse out)))
      (princ (format "%s %S => %S\n" (car f) args
                     (condition-case e (apply (car f) args)
                       (error (list 'signal (car e))))))))
  ;; Counted after the calls: a defun is compiled on its first call.
  (dolist (f fx-corpus-funs)
    (when (and (boundp 'nelisp--fx-table)
               (vectorp (gethash (nthcdr 3 (symbol-function (car f))) nelisp--fx-table)))
      (setq covered (1+ covered))))
  (princ (format "FX-CORPUS covered=%d/%d\n" covered n)))
