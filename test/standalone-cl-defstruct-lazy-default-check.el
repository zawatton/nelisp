;;; standalone-cl-defstruct-lazy-default-check.el --- cl-defstruct defaults are lazy -*- lexical-binding: t -*-

;; GNU Emacs evaluates a slot's default form only when the constructor is
;; not given that keyword.  GNU ert.el depends on it: `make-ert-test' has
;; `(body (cl-assert nil))'.  Run on NeLisp standalone and on GNU 31.1:
;;   target/nelisp --load test/standalone-cl-defstruct-lazy-default-check.el
;;   emacs --batch -l test/standalone-cl-defstruct-lazy-default-check.el
;; Both print CL-DEFSTRUCT-LAZY-DEFAULT-OK.

(require 'cl-lib)

(defvar cl-defstruct-lazy--evals 0)

(cl-defstruct (cl-defstruct-lazy-s (:constructor cl-defstruct-lazy-make))
  (a (error "default of a evaluated"))
  (b (setq cl-defstruct-lazy--evals (1+ cl-defstruct-lazy--evals)))
  (c 3)
  (d :kw))

(let* ((r1 (cl-defstruct-lazy-make :a 1 :b 2))
       (ok1 (and (equal (cl-defstruct-lazy-s-a r1) 1)
                 (equal (cl-defstruct-lazy-s-b r1) 2)
                 (equal (cl-defstruct-lazy-s-c r1) 3)
                 (eq (cl-defstruct-lazy-s-d r1) :kw)
                 (= cl-defstruct-lazy--evals 0)))
       (r2 (cl-defstruct-lazy-make :a nil))
       (ok2 (and (null (cl-defstruct-lazy-s-a r2))
                 (= (cl-defstruct-lazy-s-b r2) 1)
                 (= cl-defstruct-lazy--evals 1)))
       (ok3 (equal (condition-case e (progn (cl-defstruct-lazy-make) 'no-error)
                     (error (cadr e)))
                   "default of a evaluated")))
  (princ (if (and ok1 ok2 ok3)
             "CL-DEFSTRUCT-LAZY-DEFAULT-OK\n"
           (format "CL-DEFSTRUCT-LAZY-DEFAULT-FAIL %S %S %S\n" ok1 ok2 ok3))))

;;; standalone-cl-defstruct-lazy-default-check.el ends here
