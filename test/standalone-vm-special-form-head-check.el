;;; standalone-vm-special-form-head-check.el --- source heads beat function cells -*- lexical-binding: t; -*-

;; Usage: nelisp --load test/standalone-vm-special-form-head-check.el
;; Exercise first-call compilation as well as subsequent VM execution.
(defvar vsh-failures 0)
(defvar vsh-cases 0)
(defun vsh-check (label thunk expected)
  (let ((got (condition-case err (funcall thunk)
               (error (list 'error (car err))))))
    (setq vsh-cases (1+ vsh-cases))
    (unless (equal got expected)
      (setq vsh-failures (1+ vsh-failures))
      (princ (format "VM-SPECIAL-HEAD FAIL %S got %S want %S\n" label got expected)))))

(defun throw (&rest arguments) (throw (car arguments) (cadr arguments)))
(dotimes (i 6)
  (vsh-check (list 'throw i) (lambda () (catch 'sample (apply #'throw '(sample 42)))) 42))

(defun vsh-when () (when t 42))
(defun vsh-unless () (unless nil 42))
(defun vsh-prog2 () (prog2 1 42 3))
(defun vsh-dolist ()
  (let ((sum 0)) (dolist (x '(10 12 20) sum) (setq sum (+ sum x)))))
(defun vsh-lambda () (funcall (lambda () 42)))
(defun vsh-ordinary () 42)
(defun vsh-defalias () (defalias 'vsh-alias #'vsh-ordinary) (vsh-alias))
(defun vsh-wrong (&rest _args) 'wrong)

;; A function cell and a macro cell must both lose to the native source head.
(dolist (head '(when unless prog2 dolist lambda defalias))
  (let ((saved (symbol-function head))
        (caller (intern (concat "vsh-" (symbol-name head)))))
    (unwind-protect
        (progn
          (fset head #'vsh-wrong)
          (dotimes (i 6) (vsh-check (list head 'function i) caller 42))
          ;; A fresh body forces compilation after installing a macro cell.
          (fset head '(macro lambda (&rest _args) '(quote wrong)))
          (let ((body (copy-tree (symbol-function caller))))
            (defalias caller body))
          (dotimes (i 6) (vsh-check (list head 'macro i) caller 42))
          (when (memq head '(when unless prog2))
            (vsh-check (list head 'compiled)
                       (lambda ()
                         (vectorp (gethash (nthcdr 3 (symbol-function caller)) nelisp--fx-table)))
                       t)))
      (fset head saved))))

(defun vsh-ordinary-caller () (vsh-ordinary))
(dotimes (i 6) (vsh-check (list 'ordinary-before i) #'vsh-ordinary-caller 42))
(fset 'vsh-ordinary (lambda () 73))
(dotimes (i 6) (vsh-check (list 'ordinary-after i) #'vsh-ordinary-caller 73))

(defvar vsh-short-effects 0)
;; GNU's prog2 macro requires two arguments; expansion must fail before effects.
(defun vsh-short-prog2 ()
  (macroexpand '(prog2 (setq vsh-short-effects (1+ vsh-short-effects)))))
(dotimes (i 6)
  ;; GNU: prog2 is a macro of two required arguments, so (prog2 X) signals its
  ;; arity error before X runs, in the interpreter and in the compiled function alike.
  (vsh-check (list 'short-prog2 i) #'vsh-short-prog2 '(error wrong-number-of-arguments))
  (vsh-check (list 'short-prog2-effects i) (lambda () vsh-short-effects) 0))

(princ (format "VM-SPECIAL-HEAD %s %d/%d cases\n"
               (if (= vsh-failures 0) "PASS" "FAIL")
               (- vsh-cases vsh-failures) vsh-cases))
(kill-emacs (if (= vsh-failures 0) 0 1))

;;; standalone-vm-special-form-head-check.el ends here
