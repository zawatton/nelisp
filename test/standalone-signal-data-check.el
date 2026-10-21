;;; standalone-signal-data-check.el --- condition and data of raised errors -*- lexical-binding: t; -*-

;; Each line names one error a builtin raises and prints the error object
;; a `condition-case' handler receives: the condition symbol and its data,
;; as GNU's signal_or_quit hands them to the handler (eval.c:1943-2064).
;; The cases cover the raise sites that record a pending exit, so a change
;; to how a raise site stores the condition and data shows up here.
;; GNU 31.1 prints the reference:
;;   emacs -Q --batch -l test/standalone-signal-data-check.el

(defvar sdc-unbound-probe)
(define-error 'sdc-user-condition "SDC user condition")
(makunbound 'sdc-unbound-probe)

(defmacro sdc-case (name form)
  `(princ (format "%s %S\n" ,name
                  (condition-case err (list 'returned ,form)
                    (t (list 'caught err))))))

(defmacro sdc-head (name form)
  "Like `sdc-case' but print only the condition and the data length."
  `(princ (format "%s %S\n" ,name
                  (condition-case err (list 'returned ,form)
                    (t (list 'caught (car err) (length (cdr err))))))))

(sdc-case "car-listp" (car 1))
(sdc-case "setcar-consp" (setcar 1 2))
(sdc-case "length-sequencep" (length 5))
(sdc-case "aref-arrayp" (aref 1 0))
(sdc-case "aref-range" (aref [1 2] 5))
(sdc-case "aref-string-range" (aref "abc" 10))
(sdc-case "substring-range" (substring "abc" 5))
(sdc-case "div-zero" (/ 1 0))
(sdc-case "plus-number" (+ 'a 1))
(sdc-case "lt-number" (< 1 'b))
(sdc-case "symbol-name-symbolp" (symbol-name 1))
(sdc-case "string-to-char-stringp" (string-to-char 1))
(sdc-case "gethash-hash-table" (gethash 1 2))
(sdc-case "char-table-range" (char-table-range 1 nil))
(sdc-case "setting-constant" (eval '(setq nil 1) t))
(sdc-case "void-variable" (symbol-value 'sdc-unbound-probe))
(sdc-case "void-variable-eval" (eval 'sdc-unbound-probe t))
(sdc-case "void-function" (eval '(sdc-undefined-function-probe 1) t))
(sdc-case "funcall-void" (funcall 'sdc-undefined-function-probe))
(sdc-case "funcall-invalid" (funcall 1))
(sdc-case "funcall-string" (funcall "x"))
(sdc-case "read-syntax" (read ")"))
(sdc-case "require-missing" (require 'sdc-no-such-feature-probe))
(sdc-case "error-format" (error "Probe %d" 7))
(sdc-case "signal-user" (signal 'sdc-user-condition '(1 "two" three)))
(sdc-case "signal-nil-data" (signal 'sdc-user-condition nil))
(sdc-case "nth-listp" (nth 1 5))
(sdc-case "elt-range" (elt '(1 2) 5))
(sdc-case "concat-sequencep" (concat 1))
(sdc-case "string-equal-type" (string= 1 2))
(sdc-case "intern-stringp" (intern 1))
(sdc-case "puthash-hash-table" (puthash 1 2 3))
(sdc-case "nreverse-sequencep" (nreverse 1))
(sdc-case "mapcar-sequencep" (mapcar #'identity 1))
(sdc-case "plist-get-ok" (plist-get '(a 1) 'a))
(sdc-head "wrong-args-lambda" (apply (lambda (a) a) nil))
(sdc-head "wrong-args-subr" (funcall #'car))
(sdc-head "wrong-args-make-char-table" (funcall #'make-char-table))
(sdc-head "wrong-args-bool-vector-p" (funcall #'bool-vector-p))
(sdc-head "circular-apply" (let ((l (list 1 2))) (setcdr (cdr l) l) (apply #'+ l)))
(sdc-head "cyclic-indirection"
          (progn (defalias 'sdc-cycle-a 'sdc-cycle-b)
                 (defalias 'sdc-cycle-b 'sdc-cycle-a)
                 (funcall 'sdc-cycle-a)))
(sdc-head "excessive-nesting"
          (progn (defalias 'sdc-deep (lambda (n) (1+ (sdc-deep (1+ n)))))
                 (sdc-deep 0)))
(sdc-head "symbol-function-symbolp" (symbol-function 1))
(sdc-head "fset-symbolp" (fset 1 2))
(kill-emacs 0)
