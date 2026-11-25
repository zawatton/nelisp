;;; -*- lexical-binding: t; -*-

(defun par-primitive--check-cdr (xs)
  (cdr xs))

(defun par-primitive--assign-cdr (xs)
  (let* ((tail xs)
         (last-assigned (setq tail (cdr xs) xs (cdr tail))))
    (list tail xs last-assigned)))

(defun par-primitive--functionp (object)
  (functionp object))

(defun par-primitive--signal-selectors ()
  (list
   (condition-case nil
       (signal 'par-primitive-child nil)
     (par-primitive-child 'child)
     (error 'error)
     (par-primitive-parent 'parent))
   (condition-case nil
       (signal 'par-primitive-child nil)
     ((t) 'catch-all))))

(defvar native-comp-enable-subr-trampolines)

(defun par-primitive-dispatch-run ()
  (let ((native-comp-enable-subr-trampolines nil)
        (original-cdr (symbol-function 'cdr))
        (original-subrp (symbol-function 'subrp))
        (original-indirect (symbol-function 'indirect-function)))
    (dotimes (_ 3)
      (par-primitive--check-cdr '(1 2 3))
      (par-primitive--assign-cdr '((1 2 3) 4 5)))
    (let ((before (par-primitive--check-cdr '(1 2 3)))
          (during nil)
          (assigned-before (par-primitive--assign-cdr '((1 2 3) 4 5)))
          (assigned-during nil))
      (unwind-protect
          (progn
            (fset 'cdr (symbol-function 'car))
            (setq during (par-primitive--check-cdr '(1 2 3))
                  assigned-during (par-primitive--assign-cdr '((1 2 3) 4 5))))
        (fset 'cdr original-cdr))
      (prin1 (list 'live-call before during (par-primitive--check-cdr '(1 2 3))))
      (terpri)
      (prin1 (list 'local-pairs assigned-before assigned-during
                   (par-primitive--assign-cdr '((1 2 3) 4 5))))
      (terpri))
    (dotimes (_ 3) (par-primitive--functionp 'car))
    (let ((direct nil) (aliased nil))
      (unwind-protect
          (progn
            (fset 'subrp (lambda (_) nil))
            (fset 'indirect-function (lambda (&rest _) nil))
            (fset 'par-primitive--alias 'car)
            (setq direct (par-primitive--functionp (symbol-function 'car))
                  aliased (par-primitive--functionp 'par-primitive--alias)))
        (fset 'subrp original-subrp)
        (fset 'indirect-function original-indirect)
        (fmakunbound 'par-primitive--alias))
      (prin1 (list 'predicate direct aliased))
      (terpri)))
  (let ((condition (make-symbol "par-primitive-condition"))
        (data (list 'detail)))
    (put condition 'error-conditions (list condition 'error))
    (prin1
     (list 'signal
           (condition-case caught
               (signal condition data)
             (t (list (eq (car caught) condition) (eq (cdr caught) data))))
           (condition-case caught (signal nil) (t caught))
           (condition-case caught (signal 'par-primitive-undefined nil) (t caught))))
    (terpri)
    (prin1 (list 'signal-object
                 (condition-case caught (signal (list nil condition data)) (t caught))
                 (condition-case caught (signal nil (cons condition data))
                   (t (list (eq (car caught) condition) (eq (cdr caught) data))))))
    (terpri))
  (unwind-protect
      (progn
        (put 'par-primitive-child 'error-conditions '(par-primitive-parent))
        (let ((first (par-primitive--signal-selectors)))
          (dotimes (_ 3)
            (condition-case nil (par-primitive--signal-selectors) (t nil)))
          (prin1 (list 'selectors first (par-primitive--signal-selectors)))
          (terpri)))
    (put 'par-primitive-child 'error-conditions nil)))

(par-primitive-dispatch-run)
