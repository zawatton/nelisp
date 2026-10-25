;;; nelisp-directcall-leaf-standalone.el --- Leaf directcall parity corpus -*- lexical-binding: t; -*-
;; w1leaf: runs unchanged on GNU Emacs and the NeLisp binary; the two stdout
;; streams must be byte-identical.  Hits and failing calls of every leaf id
;; through bytecode (function as operand), funcall, apply and the walkers; error
;; data and the backtrace of a failing call; the eval-depth behaviour.
(fset 'ndl-bc1 #[514 "\1\1!\207" [] 4])
(fset 'ndl-bc2 #[771 "\2\2\2\"\207" [] 6])
(fset 'ndl-bc3 #[1028 "\3\3\3\3#\207" [] 8])
(defvar ndl-n 0)
(defun ndl-row (name v) (setq ndl-n (1+ ndl-n)) (princ (format "LEAF CASE %s %S\n" name v)))
(defun ndl-cc (thunk) (condition-case e (funcall thunk) (error (list 'err e))))
(defmacro ndl-case (name form) `(ndl-row ,name (ndl-cc (lambda () ,form))))

(defvar ndl-h (make-hash-table :test 'eq))
(defvar ndl-he (make-hash-table :test 'equal))
(puthash 1 'one ndl-h) (puthash "k" 'str ndl-he)
(defvar ndl-big most-positive-fixnum)

;; Hits through a bytecode Bcall with the function as an operand.
(dolist (fn '(car cdr identity 1+ 1-))
  (dolist (x (list '(1 2 3) nil 41 -3 ndl-big (- ndl-big) 'sym "s" [1 2]))
    (ndl-case (format "hit1-%s-%S" fn x) (ndl-bc1 fn x))))
(dolist (fn '(+ - cons logand nth))
  (dolist (a (list 0 7 -7 ndl-big '(4 5 6) nil 'sym))
    (dolist (b (list 0 3 '(8 9) ndl-big nil 'sym))
      (ndl-case (format "hit2-%s-%S-%S" fn a b) (ndl-bc2 fn a b)))))
(dolist (fn '(+ - logand))
  (ndl-case (format "hit3-%s" fn) (ndl-bc3 fn 7 5 3))
  (ndl-case (format "hit3-big-%s" fn) (ndl-bc3 fn ndl-big 1 1)))
(dolist (x (list [3 5 7] '(1 2 3) "abc" nil 5 '(1 2 . 3) (make-bool-vector 3 t)))
  (ndl-case (format "length-%S" x) (ndl-bc1 'length x)))
(ndl-case "aset-ok" (let ((v (vector 1 2 3))) (list (ndl-bc3 'aset v 1 'z) v)))
(ndl-case "aset-oob" (ndl-bc3 'aset (vector 1 2) 2 0))
(ndl-case "aset-neg" (ndl-bc3 'aset (vector 1 2) -1 0))
(ndl-case "aset-str" (let ((s (copy-sequence "abc"))) (list (ndl-bc3 'aset s 1 ?z) s)))
(ndl-case "aset-list" (ndl-bc3 'aset '(1 2) 0 0))
(ndl-case "gethash-hit" (ndl-bc2 'gethash 1 ndl-h))
(ndl-case "gethash-miss" (ndl-bc2 'gethash 2 ndl-h))
(ndl-case "gethash-default" (ndl-bc3 'gethash 2 ndl-h 'dflt))
(ndl-case "gethash-equal" (ndl-bc2 'gethash "k" ndl-he))
(ndl-case "puthash-new" (list (ndl-bc3 'puthash 9 'nine ndl-h) (gethash 9 ndl-h) (hash-table-count ndl-h)))
(ndl-case "puthash-existing" (list (ndl-bc3 'puthash 9 'nine2 ndl-h) (gethash 9 ndl-h) (hash-table-count ndl-h)))
(ndl-case "remhash" (list (ndl-bc2 'remhash 9 ndl-h) (gethash 9 ndl-h) (hash-table-count ndl-h)))
(ndl-case "count" (ndl-bc1 'hash-table-count ndl-h))
;; Growth through puthash in a loop, then every key present.
(ndl-case "puthash-grow"
  (let ((h (make-hash-table :test 'eql)) (ok t))
    (dotimes (i 300) (ndl-bc3 'puthash i (* i 2) h))
    (dotimes (i 300) (unless (eq (ndl-bc2 'gethash i h) (* i 2)) (setq ok nil)))
    (list ok (ndl-bc1 'hash-table-count h))))

;; Failing calls: error data (the reference route owns it).
(dolist (c '((car 5) (cdr 5) (car "x") (1+ a) (1- "s") (+ 1 a) (- a) (identity) (cons 1) (car 1 2)
             (logand a 1) (logand 1 a) (nth a (1 2)) (nth -1 (1 2)) (nth 1 5) (length 5)
             (length (1 . 2)) (hash-table-count 3) (gethash 1 3) (puthash 1 2 3) (remhash 1 3)
             (aset 5 0 0) (gethash 1) (puthash 1 2)))
  (ndl-case (format "err-funcall-%S" c) (apply (car c) (mapcar (lambda (a) (if (consp a) (copy-sequence a) a)) (cdr c))))
  (ndl-case (format "err-bc-%S" c)
    (pcase (length (cdr c))
      (0 (funcall (car c)))
      (1 (ndl-bc1 (car c) (nth 0 (cdr c))))
      (2 (ndl-bc2 (car c) (nth 0 (cdr c)) (nth 1 (cdr c))))
      (_ (ndl-bc3 (car c) (nth 0 (cdr c)) (nth 1 (cdr c)) (nth 2 (cdr c)))))))

;; Backtrace of a failing leaf call through a bytecode Bcall: the frames from
;; the primitive up to the bytecode driver.
(defun ndl-bt (thunk)
  (let* ((frames nil)
         (debug-on-error t)
         (debugger (lambda (&rest _)
                     (let ((i 0) f)
                       (while (setq f (backtrace-frame i))
                         (push (if (symbolp (nth 1 f)) (list (nth 1 f) (nth 2 f)) '(closure)) frames) (setq i (1+ i))))
                     (setq frames (nreverse frames))
                     (throw 'ndl-bt nil))))
    (catch 'ndl-bt (funcall thunk))
    ;; The first three frames from the one naming a leaf primitive.
    (let ((out nil) (on nil))
      (dolist (f frames)
        (when (and (not on) (memq (car f) '(car cdr 1+ gethash logand aset nth length + puthash cons)))
          (setq on t))
        (when (and on (< (length out) 3)) (push f out)))
      (nreverse out))))
;; GNU enters the debugger once per process in batch (when_entered_debugger),
;; so one backtrace row runs per process: LEAF_BT names it; unset runs none.
(let ((which (getenv "LEAF_BT")))
  (when which
    (ndl-row which
             (ndl-bt (pcase which
                       ("bt-car" (lambda () (ndl-bc1 'car 5)))
                       ("bt-gethash" (lambda () (ndl-bc2 'gethash 'k 3)))
                       ("bt-logand" (lambda () (ndl-bc2 'logand 'a 1)))
                       ("bt-aset" (lambda () (ndl-bc3 'aset (vector 1) 5 0)))
                       ("bt-funcall-car" (lambda () (funcall 'car 5)))
                       ("bt-mapcar-car" (lambda () (mapcar 'car '(1)))))))
    (kill-emacs 0)))

;; Walkers with leaf callbacks.
(ndl-case "mapcar-car" (mapcar 'car '((1) (2 3) nil)))
(ndl-case "mapcar-1+" (mapcar '1+ '(1 2 3)))
(ndl-case "mapc-identity" (let ((n 0)) (mapc 'identity '(1 2 3)) n))
(ndl-case "mapcar-cdr-vec" (mapcar 'cdr [(1 . 2) (3 . 4)]))
(ndl-case "mapcar-err" (mapcar 'car '((1) 5)))
(ndl-case "mapconcat-identity" (mapconcat 'identity '("a" "b") "-"))
(ndl-case "maphash-count" (let ((n 0)) (maphash (lambda (_k _v) (setq n (1+ n))) ndl-h) n))
;; Redefinition while a leaf id is cached by a walker.
(ndl-case "redefine-car-mid-walk"
  (let ((old (symbol-function 'car)) (r nil))
    (unwind-protect
        (progn (setq r (mapcar (lambda (x) (when (eq x 2) (fset 'car (lambda (_) 'redef))) (car (list x))) '(1 2 3))))
      (fset 'car old))
    (list r (car '(9)))))

;; The depth limit: a leaf call at the bytecode depth limit.
(defun ndl-deep (n thunk) (if (= n 0) (funcall thunk) (1+ (ndl-deep (1- n) thunk))))
(ndl-case "depth-leaf-shallow"
  (let ((max-lisp-eval-depth 200)) (ndl-deep 5 (lambda () (ndl-bc1 'car '(1))))))
(ndl-case "depth-limit-error-class"
  (let ((max-lisp-eval-depth 100))
    (condition-case e (ndl-deep 5000 (lambda () (ndl-bc1 'car '(1))))
      (error (car e)))))

(princ (format "LEAF SUMMARY checked=%d\n" ndl-n))
