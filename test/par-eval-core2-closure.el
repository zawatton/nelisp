;;; par-eval-core2-closure.el --- Bytecode closure allocation -*- lexical-binding: t; -*-

(require 'bytecomp)

(let* ((prototype #[0 "\300\207" [nil keep] 1 "doc" nil])
       (capture (list 'state))
       (original (symbol-function 'make-closure))
       (caller (byte-compile '(lambda (p x) (make-closure p x))))
       (a (funcall original prototype capture))
       (b (funcall caller prototype capture)))
  (garbage-collect)
  (prin1 (list 'copy (subrp original) (eq (funcall a) capture) (eq (funcall b) capture)
               (eq (aref a 1) (aref prototype 1))
               (eq (aref a 2) (aref b 2))
               (aref a 2) (aref prototype 2)
               (documentation a) (interactive-form a)))
  (terpri)
  (prin1 (list 'errors
               (condition-case e (funcall original 'invalid)
                 (wrong-type-argument e))
               (condition-case e (funcall original prototype 1 2 3)
                 (error e))))
  (terpri)
  (unwind-protect
      (progn
        (fset 'make-closure (lambda (_p x) (list 'replacement x)))
        (prin1 (list 'live-cell (funcall caller prototype 'new)
                     (funcall (funcall original prototype 'held))))
        (terpri))
    (fset 'make-closure original)))
