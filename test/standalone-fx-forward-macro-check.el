;;; standalone-fx-forward-macro-check.el --- VM code never calls a macro -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-fx-forward-macro-check.el
;;
;; Lexical defuns are compiled for the word VM when they are defined.  A
;; call to a name that is not yet defined, or that is later redefined as a
;; macro, must still behave as the interpreter does (expand the macro at
;; evaluation), not call the macro as a function: vendor subr.el's
;; `load-history-filename-element' calls `save-match-data' before the
;; library defines it, and signalled invalid-function from a heap image.
;; Prints one FX-FORWARD-MACRO line and exits non-zero on failure.

(defun fx-fwd-check-a (x) (fx-fwd-check-later x 1))
(defmacro fx-fwd-check-later (a b) `(list ',a ,b))

(defun fx-fwd-check-g (x) (+ x 1))
(defun fx-fwd-check-b (x) (fx-fwd-check-g x))
(defun fx-fwd-check-b-before () (fx-fwd-check-b 1))
(defvar fx-fwd-check-before (fx-fwd-check-b-before))
(defmacro fx-fwd-check-g (x) `(list 'macro ',x))

;; A stub macro replaced by the real one (nemacs's bulk stubs expand to
;; nil until the defining library loads): GNU source load keeps the eager
;; expansion in the function, including a stub's nil expansion.
(defmacro fx-fwd-check-stub (&rest _) nil)
(defun fx-fwd-check-uses-stub (x) (let ((y (* x 2))) (fx-fwd-check-stub (+ y 1))))
(defmacro fx-fwd-check-stub (form) form)

;; A call to a function defined later: compiled once the callee exists.
(defun fx-fwd-check-c (x) (fx-fwd-check-d x))
(defun fx-fwd-check-d (x) (* x 3))
(defun fx-fwd-check-vm-p (name)
  (if (boundp 'nelisp--fx-table)
      (vectorp (gethash (nthcdr 3 (symbol-function name)) nelisp--fx-table))
    t))

(let* ((fails 0)
       (check (lambda (got want)
                (unless (equal got want)
                  (setq fails (1+ fails))
                  (princ (format "FX-FORWARD-MACRO FAIL got %S want %S\n" got want))))))
  (funcall check (condition-case e (fx-fwd-check-a 5) (error e)) '(x 1))
  (funcall check fx-fwd-check-before 2)
  (funcall check (condition-case e (fx-fwd-check-b 5) (error e)) '(macro x))
  ;; GNU's source load eagerly expands the stub and retains that nil.
  (funcall check (fx-fwd-check-uses-stub 3) nil)
  (funcall check (fx-fwd-check-c 4) 12)
  (funcall check (fx-fwd-check-vm-p 'fx-fwd-check-c) t)
  (funcall check (fx-fwd-check-vm-p 'fx-fwd-check-a) t)
  (funcall check (fx-fwd-check-vm-p 'fx-fwd-check-b) t)
  (princ (format "FX-FORWARD-MACRO failures=%d %s\n" fails (if (= fails 0) "PASS" "FAIL")))
  (kill-emacs (if (= fails 0) 0 1)))
