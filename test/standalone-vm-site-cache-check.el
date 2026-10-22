;;; standalone-vm-site-cache-check.el --- a call site follows its callee's definition -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-vm-site-cache-check.el
;;
;; One call site (`cs-site' calls the symbol `cs-target') is run through the
;; VM (`nelisp--fx-register') and the interpreter while the callee's function
;; cell changes: fset to another function, back, defun, a lambda, and finally
;; a lambda of another arity.  The site must read the live function cell on
;; every call (GNU bytecode.c:803, eval.c:3206), and the arity change must
;; signal wrong-number-of-arguments.  The answer is the same under GNU Emacs
;; (test/standalone-vm-site-cache-check.expected is its output).

(defun cs-old (n) (list 'old n))
(defun cs-other (n) (list 'other n))
(defun cs-site (n) (cs-target n))
(fset 'cs-target #'cs-old)
(when (fboundp 'nelisp--fx-register) (nelisp--fx-register 'cs-site))

(defun cs-run ()
  (let (r)
    (push (cs-site 1) r)
    (push (cs-site 2) r)
    (fset 'cs-target #'cs-other)
    (push (cs-site 3) r)
    (fset 'cs-target #'cs-old)
    (push (cs-site 4) r)
    (defalias 'cs-target (lambda (n) (list 'new n)))
    (push (cs-site 5) r)
    (push (cs-site 6) r)
    (fset 'cs-target (lambda (n) (list 'redefined n)))
    (push (cs-site 7) r)
    (push (cs-site 8) r)
    (fset 'cs-target (lambda (a b) (list a b)))
    (push (condition-case e (cs-site 9) (error (car e))) r)
    (nreverse r)))

(princ (format "SITE-CACHE %S\n" (cs-run)))
(princ (format "SITE-CACHE-AGAIN %S\n" (progn (fset 'cs-target #'cs-old) (cs-run))))
(kill-emacs 0)

;;; standalone-vm-site-cache-check.el ends here
