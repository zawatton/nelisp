;;; nelisp-bignum-algo-test.el --- host oracle tests for the native bignum DSL  -*- lexical-binding: t; -*-

;; The native bignum division and 10^9-chunk printing live in the build
;; script as a quoted DSL.  This test extracts those very defuns, translates
;; them to host Elisp with the DSL's 64-bit wrapping integer semantics, runs
;; them over a simulated byte heap (filled with garbage on allocation), and
;; checks the results against host Emacs bignum arithmetic.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defconst nbat--mask (1- (ash 1 64)))
(defun nbat--w (v) (let ((x (logand v nbat--mask))) (if (>= x (ash 1 63)) (- x (ash 1 64)) x)))
(defun nbat-true (c) (and c (not (eql c 0))))
(defun nbat+ (a b) (nbat--w (+ a b)))
(defun nbat- (a b) (nbat--w (- a b)))
(defun nbat* (a b) (nbat--w (* a b)))
(defun nbat/ (a b) (nbat--w (/ a b)))
(defun nbat-shl (a n) (nbat--w (ash a n)))
(defun nbat-shr (a n) (ash (logand a nbat--mask) (- n)))

(defvar nbat--mem (make-string (* 16 1024 1024) 0))
(defvar nbat--brk 64)
(defun nbat--reset () (setq nbat--brk 64))
(defun alloc-bytes (n _align)
  (let ((p (* 8 (/ (+ nbat--brk 7) 8))))
    (dotimes (i n) (aset nbat--mem (+ p i) #xA5))
    (setq nbat--brk (+ p n)) p))
(defun heap-read-u8 (p off) (aref nbat--mem (+ p off)))
(defun heap-read-u32 (p off) (let ((a (+ p off))) (logior (aref nbat--mem a) (ash (aref nbat--mem (+ a 1)) 8) (ash (aref nbat--mem (+ a 2)) 16) (ash (aref nbat--mem (+ a 3)) 24))))
(defun ptr-write-u32 (p off v) (let ((a (+ p off)) (v (logand v #xFFFFFFFF))) (dotimes (i 4) (aset nbat--mem (+ a i) (logand (ash v (* -8 i)) 255))) 0))
(defun heap-read-u64 (p off) (nbat--w (logior (heap-read-u32 p off) (ash (heap-read-u32 p (+ off 4)) 32))))
(defun ptr-write-u64 (p off v) (let ((v (logand v nbat--mask))) (ptr-write-u32 p off (logand v #xFFFFFFFF)) (ptr-write-u32 p (+ off 4) (ash v -32)) 0))
(defun sexp-tag (p) (heap-read-u64 p 0))

;; Shims for the parts that box results or signal.
(defun nl_abi_fail (_rc _pad) 0)
(defun bf_arith_error () (signal 'arith-error nil))
(defun bf_signal_overflow_error () (signal 'overflow-error nil))
(defun bf_wrong_type_int_or_marker (_p) (signal 'wrong-type-argument '(integer-or-marker-p)))
(defun bf_wrong_type_number_or_marker (_p) (signal 'wrong-type-argument '(number-or-marker-p)))
(defun nbat--limbs->int (sign p count)
  (let ((v 0)) (dotimes (i count) (setq v (logior v (ash (heap-read-u32 p (* i 4)) (* 32 i))))) (if (= sign 1) (- v) v)))
(defun nl_bignum_finish_w (sign p count _pad) (nbat--limbs->int sign p count))
(defun nl_bignum_write_w (sign p count _pad) (nbat--limbs->int sign p count))
(defun nl_bignum_write_int_w (p count sign _pad) (nbat--limbs->int sign p count))
(defun nl_alloc_zero_fill (obj off nbytes) (dotimes (i nbytes) (aset nbat--mem (+ obj off i) 0)) 0)
(defvar nbat--out nil)
(defun mut-str-push-byte (_ms b) (push b nbat--out) 0)
(defun m5_strlen (s) (length s))
(defun m5_byte_at (s i) (aref s i))

(defconst nbat--shimmed
  '(alloc-bytes heap-read-u8 heap-read-u32 ptr-write-u32 heap-read-u64 ptr-write-u64 sexp-tag nl_abi_fail
    bf_arith_error bf_signal_overflow_error bf_wrong_type_int_or_marker bf_wrong_type_number_or_marker
    nl_alloc_zero_fill nl_bignum_finish_w nl_bignum_write_w nl_bignum_write_int_w mut-str-push-byte m5_strlen m5_byte_at))

(defun nbat--tr (f)
  (pcase f
    ((pred atom) f)
    (`(quote . ,_) f)
    (`(if ,c . ,rest) `(if (nbat-true ,(nbat--tr c)) ,@(mapcar #'nbat--tr rest)))
    (`(while ,c . ,body) `(while (nbat-true ,(nbat--tr c)) ,@(mapcar #'nbat--tr body)))
    (`(and . ,xs) `(if (and ,@(mapcar (lambda (x) `(nbat-true ,(nbat--tr x))) xs)) 1 0))
    (`(or . ,xs) `(if (or ,@(mapcar (lambda (x) `(nbat-true ,(nbat--tr x))) xs)) 1 0))
    (`(seq . ,xs) `(progn ,@(mapcar #'nbat--tr xs)))
    (`(,(and k (or 'let 'let*)) ,binds . ,body)
     `(,k ,(mapcar (lambda (b) (if (consp b) (list (car b) (nbat--tr (cadr b))) b)) binds) ,@(mapcar #'nbat--tr body)))
    (`(,op . ,args)
     (let ((new (pcase op ('+ 'nbat+) ('- 'nbat-) ('* 'nbat*) ('/ 'nbat/) ('shl 'nbat-shl) ('shr 'nbat-shr) (_ op))))
       `(,new ,@(mapcar #'nbat--tr args))))))

(defvar nbat--src (make-hash-table :test 'eq))
(defvar nbat--loaded nil)
(defun nbat--collect ()
  "Collect every DSL defun held in a quoted list of the build script."
  (let ((file (locate-library "nelisp-standalone-build.el")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'defconst) (eq (car-safe (nth 2 form)) 'quote)
                         (listp (cadr (nth 2 form))))
                (dolist (f (cadr (nth 2 form)))
                  (when (and (eq (car-safe f) 'defun) (symbolp (cadr f)))
                    (puthash (cadr f) f nbat--src))))))
        (error nil)))))
(defun nbat--install (&optional patch)
  "Define the host translation of the DSL defuns.  PATCH, if non-nil, is a
function (NAME FORM) -> FORM used by negative controls."
  (nbat--collect)
  (maphash (lambda (name f)
             (unless (memq name nbat--shimmed)
               (let ((f (if patch (funcall patch name f) f)))
                 (defalias name (eval `(lambda ,(nth 2 f) ,@(mapcar #'nbat--tr (nthcdr 3 f))) t)))))
           nbat--src)
  (setq nbat--loaded t))

(defun nbat--put (n)
  "Write magnitude N (>= 0) as canonical limbs; return (PTR . COUNT)."
  (let* ((cnt (max 1 (ceiling (if (= n 0) 0 (1+ (logb n))) 32))) (p (alloc-bytes (* 4 (+ cnt 2)) 4)))
    (dotimes (i cnt) (ptr-write-u32 p (* 4 i) (logand (ash n (* -32 i)) #xFFFFFFFF)))
    (cons p cnt)))
(defun nbat--view (v)
  (let ((view (alloc-bytes 32 8)))
    (if (< (abs v) (ash 1 61))
        (progn (ptr-write-u64 view 0 2) (ptr-write-u64 view 8 v))
      (let ((m (nbat--put (abs v))))
        (ptr-write-u64 view 0 13) (ptr-write-u64 view 8 (if (< v 0) 1 0))
        (ptr-write-u64 view 16 (car m)) (ptr-write-u64 view 24 (cdr m))))
    view))

(defun nbat--rand-limb ()
  (pcase (random 8) (0 0) (1 1) (2 #x7FFFFFFF) (3 #x80000000) (4 #xFFFFFFFF) (_ (random #x100000000))))
(defun nbat--rand-mag (limbs)
  (let ((v 0)) (dotimes (i limbs) (setq v (logior (ash v 32) (nbat--rand-limb)))) v))

(defun nbat--divrem (a b)
  (nbat--reset)
  (let* ((pa (nbat--put a)) (pb (nbat--put b))
         (m (cdr pa)) (n (cdr pb))
         (q (alloc-bytes (* 4 (+ m 4)) 4)) (r (alloc-bytes (* 4 (+ m n 4)) 4)))
    (nl_bignum_zero_limbs q (+ m 4)) (nl_bignum_zero_limbs r (+ m n 4))
    (nl_bignum_divrem_mag (car pa) m (car pb) n q r)
    (cons (nbat--limbs->int 0 q (+ m 4)) (nbat--limbs->int 0 r (+ m n 4)))))

(defun nbat--check-divrem-cases (count)
  (let ((bad nil) (special (list (1- (ash 1 64)) (ash 1 63) (ash 1 95) (1- (ash 1 96)) (ash 1 32) (1- (ash 1 32)))))
    (dotimes (_ count)
      (let* ((m (1+ (random 10))) (n (1+ (random m)))
             (a (nbat--rand-mag m)) (b (max 1 (nbat--rand-mag n))))
        (when (and (= (random 6) 0) special) (setq b (nth (random (length special)) special)))
        (when (= (random 8) 0) (setq a (* b (nbat--rand-mag (random 4)))))   ; exact multiples
        (when (= (random 8) 0) (setq a (1- (* b (1+ (nbat--rand-mag 1))))))  ; just below multiple
        (let ((got (nbat--divrem a b)))
          (unless (equal got (cons (/ a b) (% a b))) (push (list a b got) bad)))))
    bad))

(ert-deftest nelisp-bignum-algo-knuth-d-random ()
  (unless nbat--loaded (nbat--install))
  (random "nelisp-bignum-d")
  (should (null (nbat--check-divrem-cases 3000))))

(ert-deftest nelisp-bignum-algo-knuth-d-edges ()
  (unless nbat--loaded (nbat--install))
  (dolist (c (list (cons 0 1) (cons 5 7) (cons 7 7) (cons (ash 1 64) (ash 1 32)) (cons (ash 1 96) (1+ (ash 1 63)))
                   (cons (1- (ash 1 128)) (1- (ash 1 64))) (cons (ash 1 127) (ash 1 63))
                   ;; Knuth's qhat-correction / add-back stressors (divisor top limb 0x80000000).
                   (cons (logior (ash #x7FFFFFFF 64) (ash #xFFFFFFFF 32) #xFFFFFFFF) (logior (ash #x80000000 32) 1))
                   (cons (logior (ash #x80000000 96) (ash 0 64) (ash #x7FFFFFFF 32) #xFFFFFFFF) (logior (ash #x80000000 32) #x1))
                   (cons (1- (ash 1 192)) (logior (ash #x80000000 64) (ash 0 32) 1))))
    (should (equal (nbat--divrem (car c) (cdr c)) (cons (/ (car c) (cdr c)) (% (car c) (cdr c)))))))

(defun nbat--ints (mode a b)
  (nbat--reset)
  (nl_bignum_div_ints_w (nbat--view a) (nbat--view b) mode))

(defun nbat--oracle (mode a b)
  (pcase mode (0 (/ a b)) (1 (% a b)) (2 (mod a b)) (3 (floor a b)) (4 (ceiling a b)) (5 (round a b))))

(ert-deftest nelisp-bignum-algo-signed-modes ()
  (unless nbat--loaded (nbat--install))
  (random "nelisp-bignum-s")
  (let ((bad nil))
    (dotimes (_ 1200)
      (let* ((a (* (if (= (random 2) 0) 1 -1) (nbat--rand-mag (random 8))))
             (b (* (if (= (random 2) 0) 1 -1) (max 1 (nbat--rand-mag (1+ (random 6)))))))
        (when (= (random 5) 0) (setq a (* b (nbat--rand-mag (random 3)))))
        (when (= (random 7) 0) (setq a (+ (* 2 b (nbat--rand-mag 1)) (/ b 2))))   ; near half ties
        (dotimes (mode 6)
          (let ((got (nbat--ints mode a b)) (want (nbat--oracle mode a b)))
            (unless (equal got want) (push (list mode a b got want) bad))))))
    (should (null bad))
    (dolist (mode '(0 1 2 3 4 5))
      (should (equal (condition-case e (nbat--ints mode (ash 1 80) 0) (arith-error 'arith)) 'arith))
      (should (equal (condition-case e (nbat--ints mode 0 0) (arith-error 'arith)) 'arith)))))

(ert-deftest nelisp-bignum-algo-chunk-print ()
  (unless nbat--loaded (nbat--install))
  (random "nelisp-bignum-p")
  (dotimes (_ 300)
    (let* ((v (* (if (= (random 2) 0) 1 -1) (nbat--rand-mag (random 40))))
           (v (if (= (random 6) 0) (expt 10 (* 9 (random 30))) v)))
      (nbat--reset)
      (setq nbat--out nil)
      (let ((view (nbat--view v)))
        (if (= (sexp-tag view) 13)
            (progn (m5_push_bignum nil view)
                   (should (equal (apply #'string (reverse nbat--out)) (number-to-string v))))
          (should t))))))

(ert-deftest nelisp-bignum-algo-chunk-read ()
  (unless nbat--loaded (nbat--install))
  (random "nelisp-bignum-r")
  (dotimes (_ 200)
    (let* ((v (* (if (= (random 2) 0) 1 -1) (1+ (nbat--rand-mag (random 40)))))
           (s (number-to-string v)))
      (when (= (random 5) 0) (setq s (concat s ".")) (setq v (string-to-number s)))
      (nbat--reset)
      (should (equal (nl_read_int_or_bignum_w s 0) v)))))

(setq integer-width 200000)
(ert-deftest nelisp-bignum-algo-integer-width ()
  (unless nbat--loaded (nbat--install))
  (nbat--reset)
  (let ((ok (nbat--put (1- (ash 1 65536)))) (big (nbat--put (ash 1 65536))))
    (should (equal (nl_bignum_checked_write_w 0 (car ok) (cdr ok)) (1- (ash 1 65536))))
    (should (eq (condition-case nil (nl_bignum_checked_write_w 0 (car big) (cdr big)) (overflow-error 'ovf)) 'ovf))))

;; Negative controls: a broken copy of one DSL step must be caught by the
;; same checks that pass above (the clean DSL is reinstalled afterwards).
(defun nbat--patch-text (name from to)
  (lambda (n f)
    (if (eq n name)
        (let* ((s (prin1-to-string f)) (new (replace-regexp-in-string (regexp-quote from) to s t t)))
          (when (equal s new) (error "negative control did not apply: %S" from))
          (read new))
      f)))

(ert-deftest nelisp-bignum-algo-negative-controls ()
  (unwind-protect
      (progn
        ;; 1. Algorithm D without the add-back step.
        (nbat--install (nbat--patch-text 'nl_bignum_divstep "(while (= neg 1)" "(while (= neg 7)"))
        (random "nelisp-bignum-d")
        (should (nbat--check-divrem-cases 3000))
        ;; 2. 10^9 chunks printed without zero padding.
        (nbat--install (nbat--patch-text 'm5_push_bignum "(* ci 4)) 1)" "(* ci 4)) 0)"))
        (nbat--reset) (setq nbat--out nil)
        (let ((v (expt 10 30)))
          (m5_push_bignum nil (nbat--view v))
          (should-not (equal (apply #'string (reverse nbat--out)) (number-to-string v)))))
    (nbat--install)))

(provide 'nelisp-bignum-algo-test)
