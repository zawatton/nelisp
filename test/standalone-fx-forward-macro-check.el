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

(let* ((fails 0)
       (check (lambda (got want)
                (unless (equal got want)
                  (setq fails (1+ fails))
                  (princ (format "FX-FORWARD-MACRO FAIL got %S want %S\n" got want))))))
  (funcall check (condition-case e (fx-fwd-check-a 5) (error e)) '(x 1))
  (funcall check fx-fwd-check-before 2)
  (funcall check (condition-case e (fx-fwd-check-b 5) (error e)) '(macro x))
  (princ (format "FX-FORWARD-MACRO failures=%d %s\n" fails (if (= fails 0) "PASS" "FAIL")))
  (kill-emacs (if (= fails 0) 0 1)))
