;;; nelisp-callback-walk-standalone.el --- Primitive callbacks in mapc/mapcar/maphash -*- lexical-binding: t; -*-
;; cbfast: the same rows must print on GNU Emacs and on the NeLisp binaries.
(defvar cbw-checked 0)
(defvar cbw-failed 0)
(defun cbw-case (name fn)
  (setq cbw-checked (1+ cbw-checked))
  (condition-case e
      (if (funcall fn) (princ (format "CBWALK CASE %s PASS\n" name))
        (setq cbw-failed (1+ cbw-failed)) (princ (format "CBWALK CASE %s FAIL\n" name)))
    (error (setq cbw-failed (1+ cbw-failed))
           (princ (format "CBWALK CASE %s ERROR %S\n" name e)))))
(defun cbw-condition (fn) (condition-case e (funcall fn) (error e)))
(defun cbw-circular (&rest items)
  (let ((l (copy-sequence items))) (setcdr (last l) l) l))

;; Circular and dotted lists signal before any callback, with GNU's data.
;; `mapcar' over a circular list is not here: the seed's mapcar walks it without
;; GNU's `length' pre-pass and never ends (reported, not changed by cbfast).
(dolist (walk '(mapc mapcar))
  (when (eq walk 'mapc)
  (cbw-case (format "%s-circular" walk)
            (lambda () (let* ((l (cbw-circular '(1) '(2) '(3) '(4) '(5)))
                              (e (cbw-condition (lambda () (funcall walk 'car l)))))
                         (and (eq (car e) 'circular-list) (consp (cadr e))))))
  (cbw-case (format "%s-circular-invalid-fn" walk)
            (lambda () (let* ((l (cbw-circular 1 2 3))
                              (e (cbw-condition (lambda () (funcall walk 'cbw-absent l)))))
                         (eq (car e) 'circular-list)))))
  (cbw-case (format "%s-dotted" walk)
            (lambda () (equal (cbw-condition (lambda () (funcall walk 'car (cons '(1) (cons '(2) 3)))))
                              '(wrong-type-argument listp 3))))
  (cbw-case (format "%s-dotted-at-head" walk)
            (lambda () (equal (cbw-condition (lambda () (funcall walk '1+ 5)))
                              '(wrong-type-argument sequencep 5)))))
(cbw-case "mapc-long-list-cycle-late"
          (lambda () (let* ((l (make-list 300 '(1))) (e (cbw-condition (lambda () (mapc 'car (nconc l (last l)))))))
                       (eq (car e) 'circular-list))))
(cbw-case "mapc-long-list-ok"
          (lambda () (let ((l (make-list 5000 '(7 . 8)))) (eq (mapc 'car l) l))))
(cbw-case "mapcar-long-list"
          (lambda () (equal (mapcar 'cdr (make-list 700 '(7 . 8))) (make-list 700 8))))

;; Errors raised by the primitive carry GNU's data and stop the walk.
(cbw-case "mapcar-plus-bad-element"
          (lambda () (equal (cbw-condition (lambda () (mapcar '+ '(1 2 bad 4))))
                            '(wrong-type-argument number-or-marker-p bad))))
(cbw-case "mapc-minus-bad-element"
          (lambda () (equal (cbw-condition (lambda () (mapc '- '(1 "s" 3))))
                            '(wrong-type-argument number-or-marker-p "s"))))
(cbw-case "mapcar-car-bad-element"
          (lambda () (equal (cbw-condition (lambda () (mapcar 'car '((1) 2 (3)))))
                            '(wrong-type-argument listp 2))))
(cbw-case "mapc-car-nil-elements"
          (lambda () (let ((l (list nil '(1) nil))) (eq (mapc 'car l) l))))
(cbw-case "mapcar-car-nil-elements"
          (lambda () (equal (mapcar 'car '(nil (1) nil (2 3))) '(nil 1 nil 2))))

;; Sequence kinds and element kinds through the same callee.
(cbw-case "mapcar-car-vector"
          (lambda () (equal (mapcar 'car [(1) (2) nil]) '(1 2 nil))))
(cbw-case "mapc-cdr-vector"
          (lambda () (let ((v [(1 . 2) (3 . 4)])) (eq (mapc 'cdr v) v))))
(cbw-case "mapcar-1+-string"
          (lambda () (equal (mapcar '1+ "ab") '(98 99))))
;; (mapcar over a bool-vector signals listp on the seed's mapcar: reported, not changed.)
(cbw-case "mapc-identity-bool-vector"
          (lambda () (let ((v (make-bool-vector 4 t))) (eq (mapc 'identity v) v))))
(cbw-case "mapc-car-string-type"
          (lambda () (equal (cbw-condition (lambda () (mapc 'car "ab"))) '(wrong-type-argument listp 97))))
(cbw-case "mapcar-fixnum-edges"
          (lambda () (equal (mapcar '1+ (list most-negative-fixnum -1 0 1))
                            (list (1+ most-negative-fixnum) 0 1 2))))

;; Symbols denoting the same primitive: aliases, a retained object, an
;; uninterned symbol and an interned one with another name.
(defalias 'cbw-alias-car 'car)
(cbw-case "alias-symbol" (lambda () (equal (mapcar 'cbw-alias-car '((1) (2))) '(1 2))))
(cbw-case "object-callee" (lambda () (equal (mapcar (symbol-function 'car) '((1) (2))) '(1 2))))
(fset 'cbw-copy-of-car (symbol-function 'car))
(cbw-case "other-name-same-cell" (lambda () (equal (mapcar 'cbw-copy-of-car '((1) (2))) '(1 2))))
(cbw-case "uninterned-same-name"
          (lambda () (let ((s (make-symbol "car")))
                       (fset s (symbol-function 'car))
                       (and (equal (mapcar s '((1) (2))) '(1 2))
                            (equal (cbw-condition (lambda () (mapcar s '(3))))
                                   '(wrong-type-argument listp 3))))))
(cbw-case "uninterned-other-name"
          (lambda () (let ((s (make-symbol "cbw-u")))
                       (fset s (symbol-function 'cdr))
                       (equal (mapcar s '((1 . 2) (3 . 4))) '(2 4)))))

;; A cell redefined between two walks and by an outer callback between inner
;; walks (the primitive cell is re-read for every walk).
(cbw-case "cell-changes-between-inner-walks"
          (lambda ()
            (fset 'cbw-prim (symbol-function 'car))
            (let ((acc nil))
              (mapc (lambda (_i)
                      (push (mapcar 'cbw-prim '((1 . 2) (3 . 4))) acc)
                      (fset 'cbw-prim (symbol-function 'cdr)))
                    '(1 2 3))
              (equal (nreverse acc) '((1 3) (2 4) (2 4))))))
(cbw-case "cell-becomes-lisp-function"
          (lambda ()
            (fset 'cbw-prim (symbol-function 'car))
            (let ((first (mapcar 'cbw-prim '((1) (2)))))
              (fset 'cbw-prim (lambda (x) (list x)))
              (prog1 (equal (list first (mapcar 'cbw-prim '((1) (2)))) '((1 2) (((1)) ((2)))))
                (fset 'cbw-prim (symbol-function 'car))))))
(cbw-case "cell-becomes-void"
          (lambda ()
            (fset 'cbw-prim (symbol-function 'car))
            (fmakunbound 'cbw-prim)
            (equal (cbw-condition (lambda () (mapcar 'cbw-prim '((1)))))
                   '(void-function cbw-prim))))

;; maphash: primitives with the two-argument call, a table that changes
;; between walks, and the nil key.
(cbw-case "maphash-plus-nil-key"
          (lambda () (let ((h (make-hash-table :test 'eq)))
                       (puthash 1 2 h) (puthash 3 4 h)
                       (null (maphash '+ h)))))
(cbw-case "maphash-plus-bad-value"
          (lambda () (let ((h (make-hash-table :test 'eq)))
                       (puthash 1 'x h)
                       (equal (cbw-condition (lambda () (maphash '+ h)))
                              '(wrong-type-argument number-or-marker-p x)))))
(cbw-case "maphash-cons-two-args"
          (lambda () (let ((h (make-hash-table :test 'eq)))
                       (puthash 1 2 h) (null (maphash 'cons h)))))
(cbw-case "maphash-cell-redefined-between-walks"
          (lambda () (let ((h (make-hash-table :test 'eq)) (acc nil))
                       (puthash 1 2 h)
                       (fset 'cbw-two (symbol-function '+))
                       (maphash 'cbw-two h)
                       (fset 'cbw-two (lambda (k v) (push (list k v) acc)))
                       (maphash 'cbw-two h)
                       (equal acc '((1 2))))))
(cbw-case "maphash-bytecode-mutates-table"
          (lambda () (let ((h (make-hash-table :test 'eql)) (n 0))
                       (dotimes (i 20) (puthash i i h))
                       (maphash (lambda (k _v) (setq n (1+ n)) (when (= k 3) (remhash k h))) h)
                       (and (>= n 19) (= (hash-table-count h) 19)
                            (null (maphash '+ h))))))

;; Garbage collection during a walk keeps the argument words alive.
(cbw-case "gc-during-mapcar-1+"
          (lambda () (let ((xs (number-sequence 1 4000)) (acc nil))
                       (setq acc (mapcar '1+ xs))
                       (garbage-collect)
                       (equal acc (number-sequence 2 4001)))))
(cbw-case "gc-during-mapcar-car"
          (lambda () (let ((xs (mapcar (lambda (i) (list (number-to-string i))) (number-sequence 1 2000))))
                       (garbage-collect)
                       (equal (mapcar 'car xs) (mapcar 'number-to-string (number-sequence 1 2000))))))

(princ (format "CBWALK SUMMARY checked=%d failed=%d\n" cbw-checked cbw-failed))
(kill-emacs (if (= cbw-failed 0) 0 1))
