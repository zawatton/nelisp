;;; nelisp-primitive-callback-standalone.el --- Primitive walk parity -*- lexical-binding: t; -*-
(defvar npc-checked 0)
(defvar npc-failed 0)
(defvar npc-native (not (equal (getenv "DIRECTCALL_ENGINE") "gnu")))
(defvar npc-events nil)
(defun npc-condition (fn) (condition-case e (funcall fn) (error e)))
(defun npc-arity-p (condition fn count)
  ;; GNU funcall reports the subr object; the retained native route names it.
  ;; Both must identify exactly FN and preserve the supplied argument count.
  (or (equal condition (list 'wrong-number-of-arguments fn count))
      (equal condition (list 'wrong-number-of-arguments (symbol-function fn) count))))
(defun npc-case (name fn)
  (setq npc-checked (1+ npc-checked))
  (condition-case e
      (if (funcall fn) (princ (format "PRIMCB CASE %s PASS\n" name))
        (setq npc-failed (1+ npc-failed)) (princ (format "PRIMCB CASE %s FAIL\n" name)))
    (error (setq npc-failed (1+ npc-failed))
           (princ (format "PRIMCB CASE %s ERROR %S\n" name e)))))

(dolist (row '((car ((1 . 2) (3 . 4) nil) (1 3 nil))
               (cdr ((1 . 2) (3 . 4) nil) (2 4 nil))
               (identity (1 nil "x") (1 nil "x"))
               (1+ (0 -1 7) (1 0 8)) (1- (0 -1 7) (-1 -2 6))
               (+ (0 -1 7) (0 -1 7)) (- (0 -1 7) (0 1 -7))))
  (let ((fn (car row)) (xs (cadr row)) (ys (nth 2 row)))
    (npc-case (format "mapcar-%s" fn) (lambda () (equal (mapcar fn xs) ys)))
    (npc-case (format "mapcar-object-%s" fn)
              (lambda () (equal (mapcar (symbol-function fn) xs) ys)))
    (npc-case (format "mapc-%s" fn) (lambda () (eq (mapc fn xs) xs)))
    (npc-case (format "mapc-vector-%s" fn)
              (lambda () (let ((v (apply 'vector xs))) (eq (mapc fn v) v))))))

(npc-case "mapcar-empty-invalid" (lambda () (null (mapcar 'npc-absent nil))))
(npc-case "mapc-empty-invalid" (lambda () (null (mapc 'npc-absent nil))))
(npc-case "mapcar-string" (lambda () (equal (mapcar '1+ "abc") '(98 99 100))))
(npc-case "mapc-string" (lambda () (let ((s "abc")) (eq (mapc '1+ s) s))))
(npc-case "mapc-float-decline" (lambda () (let ((v [1 2.5 3])) (eq (mapc '+ v) v))))

(dolist (fn '(+ - cons))
  (npc-case (format "maphash-%s" fn)
            (lambda () (let ((h (make-hash-table :test 'eq)))
                         (puthash 2 3 h) (puthash 4 5 h)
                         (and (null (maphash fn h)) (= (hash-table-count h) 2))))))
(npc-case "maphash-nonfixnum-decline"
          (lambda () (let ((h (make-hash-table :test 'eq)))
                       (puthash 2 3.5 h) (null (maphash '+ h)))))
(dolist (fn '(car cdr 1+ 1- identity))
  (npc-case (format "maphash-arity-%s" fn)
            (lambda () (let ((h (make-hash-table :test 'eq)))
                         (puthash 2 3 h)
                         (npc-arity-p (npc-condition (lambda () (maphash fn h))) fn 2)))))
(dolist (walk '(mapcar mapc))
  (npc-case (format "%s-cons-arity" walk)
            (lambda () (npc-arity-p (npc-condition (lambda () (funcall walk 'cons '(1)))) 'cons 1)))
  (dolist (row '((car listp) (cdr listp) (1+ number-or-marker-p)
                 (1- number-or-marker-p) (+ number-or-marker-p) (- number-or-marker-p)))
    (let ((fn (car row)) (predicate (cadr row)))
      (npc-case (format "%s-type-%s" walk fn)
                (lambda () (equal (npc-condition (lambda () (funcall walk fn '(bad))))
                                   (list 'wrong-type-argument predicate 'bad)))))))

(npc-case "increment-bignum-overflow"
          (lambda () (let ((x most-positive-fixnum))
                       (equal (mapcar '1+ (list 1 x 2)) (list 2 (+ x 1) 3)))))
(npc-case "decrement-bignum-overflow"
          (lambda () (let ((x most-negative-fixnum))
                       (equal (mapcar '1- (list 1 x 2)) (list 0 (- x 1) 1)))))
(npc-case "minus-bignum-overflow"
          (lambda () (equal (mapcar '- (list most-negative-fixnum))
                             (list (- most-negative-fixnum)))))
(npc-case "plus-bignum-and-float"
          (lambda () (let ((x (+ most-positive-fixnum 1)))
                       (equal (mapcar '+ (list 1 x 2.5)) (list 1 x 2.5)))))
(npc-case "maphash-plus-overflow"
          (lambda () (let ((h (make-hash-table :test 'eq)))
                       (puthash most-positive-fixnum 1 h) (null (maphash '+ h)))))
(npc-case "marker-number"
          (lambda () (with-temp-buffer
                       (insert "abcd") (goto-char 3)
                       (let ((m (point-marker)))
                         (and (equal (mapcar '+ (list m)) '(3))
                              (equal (mapcar '1+ (list m)) '(4))
                              (equal (mapcar '1- (list m)) '(2)))))))
(npc-case "direct-plus-minus-arity-and-overflow"
          (lambda () (and (= (funcall '+) 0) (= (funcall '-) 0)
                          (= (funcall '+ 1 2 3) 6) (= (funcall '- 1 2 3) -4)
                          (= (funcall '+ most-positive-fixnum 1 -1) most-positive-fixnum))))

;; Real advice modifies the function cell during the walk. Restoration always
;; uses the saved object; the original symbol remains the walk's authority.
(let ((inhibit-message t) (message-log-max nil))
  (if npc-native
      (progn (add-to-list 'load-path (expand-file-name "lisp" (getenv "DIRECTCALL_ROOT")))
             (require 'nelisp-nadvice-substrate))
    (require 'nadvice)))
(defvar npc-saved-identity (symbol-function 'identity))
(defun npc-identity-next (x) (list 'new x))
(defun npc-identity-advice (orig x)
  (setq npc-events (cons x npc-events))
  (fset 'identity 'npc-identity-next)
  (funcall orig x))
(npc-case "advice-redefinition-during-mapcar"
          (lambda () (unwind-protect
                         (progn (advice-add 'identity :around 'npc-identity-advice)
                                (equal (mapcar 'identity '(1 2 3)) '(1 (new 2) (new 3))))
                       (advice-remove 'identity 'npc-identity-advice)
                       (fset 'identity npc-saved-identity))))
(npc-case "retained-primitive-object"
          (lambda () (let ((object (symbol-function 'identity)))
                       (unwind-protect
                           (progn (fset 'identity 'npc-identity-next)
                                  (equal (mapcar object '(1 2 3)) '(1 2 3)))
                         (fset 'identity object)))))
(defun npc-record-frames (orig x)
  (let ((seen nil))
    (mapbacktrace (lambda (_ fn args _flags)
                    (when (memq fn '(identity mapcar mapc maphash))
                      (setq seen (cons (list fn args) seen)))))
    (setq npc-events (cons seen npc-events)))
  (funcall orig x))
(npc-case "advised-mapcar-backtrace"
          (lambda () (unwind-protect
                         (progn (setq npc-events nil)
                                (advice-add 'identity :around 'npc-record-frames)
                                (and (equal (mapcar 'identity '(7)) '(7))
                                     (assoc 'mapcar (car npc-events))))
                       (advice-remove 'identity 'npc-record-frames)
                       (fset 'identity npc-saved-identity))))
(npc-case "identity-gc-retains-arguments"
          (lambda () (let ((xs (make-list 1000 "alive")))
                       (garbage-collect)
                       (and (equal (mapcar 'identity xs) xs)
                            (eq (mapc 'identity xs) xs)))))
(princ (format "PRIMCB SUMMARY checked=%d failed=%d\n" npc-checked npc-failed))
(kill-emacs (if (= npc-failed 0) 0 1))
