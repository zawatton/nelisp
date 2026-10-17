;;; standalone-cl-destructure-check.el --- full CL lambda lists, constant let bindings -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-cl-destructure-check.el
;;
;; `cl-destructuring-bind' and `cl-defmacro' took flat symbols and `&rest'
;; only: `&key' was bound as a variable and `()' as the constant nil, and
;; binding nil made `let' abort the whole top-level form without a signal.
;; GNU's ert.el defines `ert-deftest' as (cl-defmacro ert-deftest (name ()
;; &body ...)) and destructures ((&key ...) body), so no GNU test file
;; could define a test.  Each expected value below is GNU Emacs 31.1's.
;; Prints one CL-DESTRUCTURE line and exits non-zero on failure.

(defvar cl-destructure-cases
  '((((a b) c) ((1 2) 3) (list a b c))
    ((a &optional (b 5 b-p) c) (1) (list a b b-p c))
    ((a &optional (b 5 b-p)) (1 2) (list a b b-p))
    ((a &rest r) (1 2 3) (list a r))
    ((a . r) (1 2 3) (list a r))
    ((&key a (b 7) ((:cee c) nil c-p)) (:cee 3 :a 1) (list a b c c-p))
    (((&key (expected-result nil er-p) (tags nil tags-p)) body) ((:tags (x)) (y)) (list expected-result er-p tags tags-p body))
    ((name () &body body) (foo nil 1 2) (list name body))
    ((name () &body body) (foo (1) 1 2) (list name body))
    ((a b) (1) (list a b))
    ((a b) (1 2 3) (list a b))
    ((&key a) (:a 1 :z 2) a)
    ((&key a &allow-other-keys) (:a 1 :z 2) a)
    ((&key a) (:a 1 :z 2 :allow-other-keys t) a)
    ((&whole w a &rest r) (1 2) (list w a r))
    ((a &aux (b (* a 2)) c) (3) (list a b c))
    ((&optional ((x y) '(8 9))) () (list x y))
    ((a &rest r &key k) (1 :k 2) (list a r k))))

(defvar cl-destructure-failures nil)

(let ((want '((1 2 3) (1 5 nil nil) (1 2 t) (1 (2 3)) (1 (2 3)) (1 7 3 t) (nil nil (x) t (y)) (foo (1 2)) (ERR wrong-number-of-arguments) (ERR wrong-number-of-arguments) (ERR wrong-number-of-arguments) (ERR error) 1 1 ((1 2) 1 (2)) (3 6 nil) (8 9) (1 (:k 2) 2)))
      (got (mapcar (lambda (c)
                     (condition-case e
                         (eval `(cl-destructuring-bind ,(nth 0 c) ',(nth 1 c) ,(nth 2 c)) t)
                       (error (list 'ERR (car e)))))
                   cl-destructure-cases))
      (i 0))
  (while want
    (unless (equal (car got) (car want))
      (push (list i (car got) (car want)) cl-destructure-failures))
    (setq want (cdr want) got (cdr got) i (1+ i))))

(cl-defmacro cl-destructure-check-m (name () &body body) `(list ',name ,@body))
(unless (equal (macroexpand-1 '(cl-destructure-check-m foo () 1 2)) '(list 'foo 1 2))
  (push 'cl-defmacro-empty-pattern cl-destructure-failures))

(let ((got (mapcar (lambda (f) (condition-case e (eval f t) (error (list 'ERR e))))
                   '((let (nil) 1) (let ((nil 1)) 2) (let* ((t 1)) 2) (let* ((1 2)) 3)
                     (let (("a" 1)) 2) (let ((:k :k)) 1)))))
  (unless (equal got '((ERR (setting-constant nil)) (ERR (setting-constant nil)) (ERR (setting-constant t)) (ERR (wrong-type-argument symbolp 1)) (ERR (wrong-type-argument symbolp "a")) 1))
    (push (list 'let-constants got) cl-destructure-failures)))

(dolist (f (reverse cl-destructure-failures))
  (princ (format "  MISMATCH %S\n" f)))
(princ (format "CL-DESTRUCTURE failures=%d %s\n" (length cl-destructure-failures)
               (if cl-destructure-failures "FAIL" "PASS")))
(kill-emacs (if cl-destructure-failures 1 0))

;;; standalone-cl-destructure-check.el ends here
