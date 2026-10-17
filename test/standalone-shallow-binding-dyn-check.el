;;; standalone-shallow-binding-dyn-check.el --- dynamic-binding mode lets -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-shallow-binding-dyn-check.el
;;
;; Forms evaluated by `(eval FORM nil)' use dynamic binding: every `let'
;; and every parameter of a function defined there binds dynamically.
;; GNU Emacs answers are the reference.  Prints one SHALLOW-BINDING-DYN
;; line; non-zero exit on failure.

(eval '(progn
         (defun sbd-read (sym) (if (boundp sym) (symbol-value sym) 'void))
         (defun sbd-param (sbd-p) (sbd-read 'sbd-p))
         (defun sbd-inner () (sbd-read 'sbd-local)))
      nil)

(let* ((fails 0)
       (check (lambda (name got want)
                (unless (equal got want)
                  (setq fails (1+ fails))
                  (princ (format "SHALLOW-BINDING-DYN FAIL %s got %S want %S\n" name got want))))))
  (funcall check 'let (eval '(let ((sbd-local 3)) (sbd-inner)) nil) 3)
  (funcall check 'after (sbd-read 'sbd-local) 'void)
  (funcall check 'param (sbd-param 6) 6)
  (funcall check 'param-after (sbd-read 'sbd-p) 'void)
  (funcall check 'signal
           (eval '(condition-case nil (let ((sbd-local 1)) (error "x"))
                    (error (sbd-read 'sbd-local)))
                 nil)
           'void)
  (funcall check 'setq-inside (eval '(let ((sbd-local 1)) (setq sbd-local 2) (sbd-inner)) nil) 2)
  (funcall check 'setq-after (sbd-read 'sbd-local) 'void)
  (princ (format "SHALLOW-BINDING-DYN failures=%d %s\n" fails (if (= fails 0) "PASS" "FAIL")))
  (kill-emacs (if (= fails 0) 0 1)))
