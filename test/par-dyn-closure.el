;;; par-dyn-closure.el --- Interpreted closure slots and advice

(require 'advice)
(require 'nadvice)
(require 'oclosure)

(defmacro dc-row (name form)
  `(princ (format "%s %S\n" ,name
                  (condition-case err ,form (error (car err))))))

(defun dc-double (x)
  (* x 2))

(defun dc-dynamic-scope (x)
  (symbol-value 'x))

(defun dc-command (x &optional y)
  "Return the sum."
  (interactive "p")
  (+ x (or y 0)))

(eval '(defun dc-lexical-double (x) (* x 2)) t)

(defun dc-slots (fn)
  (list (closurep fn) (interpreted-function-p fn) (length fn)
        (aref fn 0) (aref fn 1) (aref fn 2)))

(defun dc-round-trip (fn)
  (let ((copy (read (prin1-to-string fn))))
    (list (equal fn copy) (eq fn copy) (funcall copy 3))))

(defun dc-around (original &rest args)
  (1+ (apply original args)))

(defun dc-legacy-target (x)
  (* x 2))

(defvar dc-original-command nil)

(dc-row "dynamic call"
        (list (dc-double 3) (dc-dynamic-scope 3)
              (eval '(list (dc-dynamic-scope 4)
                           (funcall #'dc-dynamic-scope 5)
                           (apply #'dc-dynamic-scope '(6)))
                    t)))
(dc-row "dynamic slots" (dc-slots (symbol-function 'dc-double)))
(dc-row "dynamic bounds"
        (let ((fn (symbol-function 'dc-double)))
          (condition-case err (aref fn 3)
            (args-out-of-range (list (eq fn (nth 1 err)) (nth 2 err))))))
(dc-row "dynamic arity" (func-arity 'dc-command))
(dc-row "dynamic arglist" (help-function-arglist 'dc-command))
(dc-row "dynamic doc and interactive"
        (let ((fn (symbol-function 'dc-command)))
          (list (length fn) (aref fn 3) (aref fn 4) (aref fn 5)
                (documentation fn) (interactive-form fn))))
(dc-row "dynamic modes"
        (let ((fn (eval '(lambda () (interactive "p" text-mode) 1) nil)))
          (list (length fn) (aref fn 5) (interactive-form fn))))
(dc-row "dynamic empty body"
        (let ((fn (eval '(lambda ()) nil)))
          (list (aref fn 1) (funcall fn))))
(dc-row "dynamic copies"
        (let ((fn (symbol-function 'dc-double))
              (copy (make-interpreted-closure '(x) '((* x 2)) nil))
              (other (make-interpreted-closure '(x) '((* x 2)) t)))
          (list (equal fn copy) (eq fn copy) (equal fn other)
                (condition-case err (copy-sequence fn)
                  (wrong-type-argument (nth 1 err))))))
(dc-row "legacy advice"
        (let ((ad-default-compilation-action 'never))
          (defadvice dc-legacy-target (after dc-legacy activate)
            (setq ad-return-value (+ ad-return-value 10)))
          (dc-legacy-target 3)))
(dc-row "nadvice and oclosure"
        (progn
          (setq dc-original-command (symbol-function 'dc-command))
          (advice-add 'dc-command :around #'dc-around)
          (unwind-protect
              (let ((fn (symbol-function 'dc-command)))
                (list (dc-command 3 4) (advice--p fn) (oclosure-type fn)
                      (interactive-form 'dc-command)
                      (help-function-arglist 'dc-command)))
            (advice-remove 'dc-command #'dc-around))))
(dc-row "nadvice removed"
        (list (dc-command 3 4) (advice--p (symbol-function 'dc-command))
              (eq dc-original-command (symbol-function 'dc-command))))

(dc-row "lexical slots" (dc-slots (symbol-function 'dc-lexical-double)))
(dc-row "lexical print" (prin1-to-string (symbol-function 'dc-lexical-double)))
(dc-row "lexical round trip" (dc-round-trip (symbol-function 'dc-lexical-double)))
(dc-row "capture round trip"
        (let ((fn (eval '(let ((y 4)) (lambda (x) (+ x y))) t)))
          (list (prin1-to-string fn) (dc-round-trip fn))))
(dc-row "constructor nil"
        (let ((fn (make-interpreted-closure '(x) '(x) nil)))
          (list (aref fn 2) (prin1-to-string fn) (funcall fn 3))))
(dc-row "constructor t"
        (let ((fn (make-interpreted-closure '(x) '(x) t)))
          (list (aref fn 2) (prin1-to-string fn) (funcall fn 3)
                (condition-case err (read (prin1-to-string fn))
                  (invalid-read-syntax (car err))))))
(dc-row "constructor alist"
        (let* ((env (list (cons 'y 4) (cons 'z 2) t))
               (fn (make-interpreted-closure '(x) '((+ x y z)) env)))
          (list (aref fn 2) (prin1-to-string fn) (dc-round-trip fn))))
(dc-row "constructor shared binding"
        (let* ((env (list (cons 'count 0)))
               (bump (make-interpreted-closure nil '((setq count (1+ count))) env))
               (peek (make-interpreted-closure nil '(count) env)))
          (list (funcall bump) (funcall bump) (funcall peek) (cdr (car env)))))
(dc-row "constructor metadata"
        (let ((fn (make-interpreted-closure '(x) '((1+ x)) nil
                                           "Add one." '(interactive "p"))))
          (list (length fn) (aref fn 4) (aref fn 5) (dc-round-trip fn))))
(dc-row "dynamic print" (prin1-to-string (symbol-function 'dc-double)))
(dc-row "dynamic round trip" (dc-round-trip (symbol-function 'dc-double)))
