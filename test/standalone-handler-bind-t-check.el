;;; standalone-handler-bind-t-check.el --- handler-bind with condition t -*- lexical-binding: t -*-

;; GNU's handler-bind treats a `t' condition as matching every signal
;; (eval.c find_handler_clause); GNU ert.el runs each test body under
;; (handler-bind ((t debugfun)) ...).  Prints HANDLER-BIND-T-OK on GNU 31.1
;; and on NeLisp:
;;   target/nelisp --load test/standalone-handler-bind-t-check.el
;;   emacs --batch -l test/standalone-handler-bind-t-check.el

(require 'cl-lib)
(define-error 'handler-bind-t-check-error "Check error")

(let* ((r1 (catch 'tag
             (handler-bind ((t (lambda (_) (throw 'tag 'thrown))))
               (signal 'handler-bind-t-check-error '(x)))))
       (r2 (cl-block blk
             (let ((k (lambda () (cl-return-from blk 'returned))))
               (handler-bind ((t (lambda (_) (funcall k))))
                 (signal 'handler-bind-t-check-error '(y))))))
       (seen nil)
       (r3 (condition-case err
               (handler-bind ((t (lambda (e) (setq seen (car e)))))
                 (signal 'handler-bind-t-check-error '(z)))
             (handler-bind-t-check-error (list 'outer (cdr err))))))
  (princ (if (and (eq r1 'thrown) (eq r2 'returned)
                  (equal r3 '(outer (z))) (eq seen 'handler-bind-t-check-error))
             "HANDLER-BIND-T-OK\n"
           (format "HANDLER-BIND-T-FAIL %S %S %S %S\n" r1 r2 r3 seen))))

;;; standalone-handler-bind-t-check.el ends here
