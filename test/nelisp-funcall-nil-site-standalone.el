;;; nelisp-funcall-nil-site-standalone.el --- funcall of nil in a defined function -*- lexical-binding: t; -*-
;; Runs unchanged on GNU Emacs and the NeLisp binary; the two stdout streams
;; must be byte-identical.  `(funcall nil)' inside a function reached by
;; `defalias'/`defun' used to read the callee cache of a fresh funcall site
;; (slots 3 and 4 both nil) as a hit and dereference a null record (SIGSEGV
;; in `m5_fx_funcall_site').  GNU signals (void-function nil).
(defun fnil-a () (funcall nil))
(defun fnil-b (f) (funcall f))
(defun fnil-c () (car-safe (funcall nil)))
(defvar fnil-v nil)
(defun fnil-d () (funcall fnil-v 1 2))
(defmacro fnil-case (name form)
  `(princ (format "FNIL %s %S\n" ,name (condition-case e ,form (error (list 'err e))))))
(fnil-case "defun-literal" (fnil-a))
(fnil-case "defun-arg" (fnil-b nil))
(fnil-case "defun-nested" (fnil-c))
(fnil-case "defun-var-args" (fnil-d))
(fnil-case "repeat" (list (condition-case e (fnil-a) (error e)) (condition-case e (fnil-a) (error e))))
(fnil-case "valid-after-nil" (progn (condition-case nil (fnil-b nil) (error nil)) (fnil-b (lambda () 7))))
(princ "FNIL-DONE\n")
