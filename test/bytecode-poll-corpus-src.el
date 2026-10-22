;;; bytecode-poll-corpus-src.el --- live values across VM safepoints -*- lexical-binding: t -*-

;; Not loaded by NeLisp.  `test/bytecode-poll-corpus-gen.el' byte-compiles
;; every `defun' below with host GNU Emacs, calls each case, and writes the
;; compiled functions with GNU's answers to `test/bytecode-poll-corpus.eld',
;; which `test/standalone-bytecode-poll-check.el' replays on NeLisp with a
;; collection at every VM safepoint (GNU bytecode.c:797 at Bcall, :861-866
;; every 255th backward branch).
;;
;; Every function keeps freshly allocated values alive only in the places a
;; collection at a safepoint must find: the operand stack of the frame, its
;; locals, a dynamic binding, a handler's saved state, an unwind form, the
;; frames of a recursion and the temporaries of a native builtin that calls
;; back into bytecode.  A value the collector missed is overwritten by the
;; next allocation, and the checksums and the returned structures differ.

(defvar bpc-dyn nil)
(defvar bpc-log nil)

(defun bpc-mk (i) (list i (number-to-string i) (vector i (* i i))))
(defun bpc-sum (x) (+ (car x) (length (nth 1 x)) (aref (nth 2 x) 1)))
(defun bpc-id3 (a b c) (list a b c))
(defun bpc-key (x) (car x))
(defun bpc-opt (a &optional b) (list a b (bpc-mk 7)))
(defun bpc-reader () (+ (car bpc-dyn) (length (nth 1 bpc-dyn))))

;; Arguments live only on the operand stack while the next argument is built.
(defun bpc-args (n)
  (let ((i 0) (acc 0))
    (while (< i n)
      (let ((r (bpc-id3 (bpc-mk i) (bpc-mk (1+ i)) (bpc-mk (+ i 2)))))
        (setq acc (+ acc (bpc-sum (nth 0 r)) (bpc-sum (nth 1 r)) (bpc-sum (nth 2 r)))))
      (setq i (1+ i)))
    acc))

;; A growing list in a local across back edges and calls.
(defun bpc-acc (n)
  (let ((i 0) (acc nil))
    (while (< i n)
      (setq acc (cons (bpc-mk i) acc))
      (setq i (1+ i)))
    (let ((s 0))
      (dolist (x acc) (setq s (+ s (bpc-sum x))))
      (list s (length acc) (car acc)))))

;; Only the backward-branch safepoint: no call inside the loop.
(defun bpc-spin (n)
  (let ((i 0) (acc nil) (v (vector 1 2 3)))
    (while (< i n)
      (setq acc (cons (list i v) acc))
      (setq i (1+ i)))
    (list (length acc) (car acc) (aref v 2))))

;; Frames of one activation, each holding live data across the next call.
(defun bpc-rec (d)
  (if (= d 0)
      (bpc-mk 0)
    (let* ((mine (bpc-mk d)) (sub (bpc-rec (1- d))))
      (list mine sub (bpc-sum mine)))))

;; A deep recursion in one activation: 150 frames, each holding live data
;; across the next call, scanned by every collection.
(defun bpc-deep (d)
  (if (= d 0)
      (list 0)
    (let* ((mine (bpc-mk d)) (sub (bpc-deep (1- d))))
      (cons (bpc-sum mine) sub))))

;; The slow call path (a callee with an optional argument).
(defun bpc-slow (n)
  (let ((i 0) (acc nil))
    (while (< i n)
      (setq acc (cons (bpc-opt (bpc-mk i)) acc))
      (setq i (1+ i)))
    (list (length acc) (car acc) (car (last acc)))))

;; Dynamic bindings of heap values around calls and back edges.
(defun bpc-bind (n)
  (let ((i 0) (acc 0))
    (while (< i n)
      (let ((bpc-dyn (bpc-mk i)))
        (setq acc (+ acc (bpc-reader) (bpc-sum bpc-dyn))))
      (setq i (1+ i)))
    (list acc bpc-dyn)))

;; Handlers: a catch and a condition-case in a loop, throwing and signalling
;; fresh data through frames that hold other fresh data.
(defun bpc-catch (n)
  (let ((i 0) (acc nil))
    (while (< i n)
      (setq acc (cons (catch 'bpc-tag
                        (let ((m (bpc-mk i)))
                          (if (= (% i 3) 0)
                              (throw 'bpc-tag (bpc-mk (+ i 100)))
                            (bpc-sum m))))
                      acc))
      (setq i (1+ i)))
    acc))

(defun bpc-ccase (n)
  (let ((i 0) (acc nil))
    (while (< i n)
      (setq acc (cons (condition-case err
                          (let ((m (bpc-mk i)))
                            (if (= (% i 4) 0)
                                (signal 'wrong-type-argument (list (bpc-mk i)))
                              (bpc-sum m)))
                        (wrong-type-argument (bpc-sum (cadr err))))
                      acc))
      (setq i (1+ i)))
    acc))

;; An unwind form that allocates, run after a value is already built.
(defun bpc-unwind (n)
  (setq bpc-log nil)
  (let ((i 0) (acc nil))
    (while (< i n)
      (setq acc (cons (unwind-protect (bpc-mk i)
                        (setq bpc-log (cons (bpc-mk (- i)) bpc-log)))
                      acc))
      (setq i (1+ i)))
    (list (length acc) (length bpc-log) (car bpc-log) (car acc))))

;; Native builtins that hold their own temporaries while a bytecode callback
;; runs (and polls).
(defun bpc-mapcar (n)
  (let ((l nil) (i 0))
    (while (< i n) (setq l (cons i l) i (1+ i)))
    (mapcar #'bpc-mk l)))

(defun bpc-sort (n)
  (let ((l nil) (i 0))
    (while (< i n) (setq l (cons (bpc-mk (% (* i 7) n)) l) i (1+ i)))
    (sort l (lambda (a b) (< (bpc-key a) (bpc-key b))))))

(defun bpc-maphash (n)
  (let ((h (make-hash-table :test 'equal)) (i 0) (acc 0))
    (while (< i n) (puthash (number-to-string i) (bpc-mk i) h) (setq i (1+ i)))
    (maphash (lambda (k v) (setq acc (+ acc (length k) (bpc-sum v)))) h)
    acc))

(defun bpc-mapconcat (n)
  (mapconcat (lambda (i) (number-to-string (bpc-sum (bpc-mk i)))) (number-sequence 1 n) ","))

(defun bpc-apply (n)
  (let ((i 0) (acc 0))
    (while (< i n)
      (setq acc (+ acc (apply #'bpc-sum (list (bpc-mk i))) (funcall #'bpc-sum (bpc-mk (1+ i)))))
      (setq i (1+ i)))
    acc))

;; What lexical closures share, compiled by GNU (a mutated captured variable
;; becomes a cons box the closures and their creator share): the same shapes as
;; test/standalone-closure-sharing-check.el, run through the VM with a
;; collection at every safepoint.
(defun bpc-reader2 () bpc-dyn)
(defun bpc-make-counter ()
  (let ((n 0)) (cons (lambda () (setq n (1+ n))) (lambda () n))))
(defun bpc-clos-counters ()
  (let* ((c (bpc-make-counter)) (a (car c)) (b (cdr c)))
    (funcall a) (funcall a)
    (let ((c2 (bpc-make-counter)))
      (funcall (car c2))
      (list (funcall b) (funcall (cdr c2))))))
(defun bpc-clos-loops (n)
  (let (fs ws (k 0))
    (dotimes (i n) (let ((j i)) (push (lambda () (setq j (1+ j))) fs)))
    (while (< k n) (let ((m k)) (push (lambda () (setq m (+ m 10))) ws)) (setq k (1+ k)))
    (list (mapcar #'funcall fs) (mapcar #'funcall fs)
          (mapcar #'funcall ws) (mapcar #'funcall ws))))
(defun bpc-clos-share (n)
  (let* ((x 1) (f (lambda () x)) (g (lambda (v) (setq x v))))
    (let ((r1 (funcall f)))
      (setq x n)
      (let ((r2 (funcall f)))
        (funcall g (* n 2))
        (list r1 r2 (funcall f) x)))))
(defun bpc-clos-dyn ()
  (let ((lex 'lexical) (bpc-dyn (list 'bound)))
    (let ((f (lambda () (list lex bpc-dyn (bpc-reader2)))))
      (list (funcall f) (let ((bpc-dyn (list 'rebound))) (funcall f))))))
(defun bpc-clos-exit ()
  (let ((log nil) (x 1))
    (condition-case err
        (unwind-protect (progn (setq x 2) (error "boom %d" x))
          (push (list 'cleanup x) log)
          (setq x (1+ x)))
      (error (push (list 'handler (car err) x (cdr err)) log)))
    (list x (reverse log))))

(defconst bpc-cases
  '((bpc-args 12) (bpc-acc 12) (bpc-spin 600) (bpc-rec 12) (bpc-deep 150) (bpc-slow 12)
    (bpc-bind 12) (bpc-catch 12) (bpc-ccase 12) (bpc-unwind 12)
    (bpc-mapcar 12) (bpc-sort 12) (bpc-maphash 12) (bpc-mapconcat 10) (bpc-apply 12)
    (bpc-clos-counters) (bpc-clos-loops 6) (bpc-clos-share 5) (bpc-clos-dyn) (bpc-clos-exit)))
