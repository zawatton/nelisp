;;; standalone-fx-late-special-check.el --- VM lets track later defvars -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-fx-late-special-check.el
;;
;; Lexical defuns are compiled for the word VM when they are defined, and a
;; `let' there binds lexically.  The interpreter decides at each evaluation,
;; so once a later `defvar' makes the variable special the same `let' binds
;; it dynamically, visible to callees (GNU 31.1 answers `local' below).
;; magit and org let-bind many names their later files `defvar', and M-x
;; hung on a binding the callee never saw.  Deep recursion through VM code
;; must also signal `excessive-lisp-nesting' rather than overflow the
;; native stack.  Prints one FX-LATE-SPECIAL line; non-zero exit on failure.

(defun fx-late-get () (symbol-value 'fx-late-flag))
(defun fx-late-bind (x) (let ((fx-late-flag x)) (fx-late-get)))
(defun fx-late-bind-before () (fx-late-bind 'early))
(defvar fx-late-before (condition-case e (fx-late-bind-before) (error (car e))))
(defvar fx-late-flag 'global)

(defun fx-late-deep (n) (if (< n 1) 0 (1+ (fx-late-deep (1- n)))))

(let* ((fails 0)
       (check (lambda (got want)
                (unless (equal got want)
                  (setq fails (1+ fails))
                  (princ (format "FX-LATE-SPECIAL FAIL got %S want %S\n" got want))))))
  ;; Before the defvar: unbound, as the lexical binding is invisible.
  (funcall check fx-late-before 'void-variable)
  (funcall check (fx-late-bind 'local) 'local)
  (funcall check fx-late-flag 'global)
  (funcall check (fx-late-deep 500) 500)
  (funcall check (condition-case e (fx-late-deep 1000000) (error (car e)))
           'excessive-lisp-nesting)
  (princ (format "FX-LATE-SPECIAL failures=%d %s\n" fails (if (= fails 0) "PASS" "FAIL")))
  (kill-emacs (if (= fails 0) 0 1)))
