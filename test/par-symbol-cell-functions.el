;;; par-symbol-cell-functions.el --- Live function resolution and rebinding -*- lexical-binding: t; -*-

(require 'nadvice)

(defun par-symbol-cell-fn (value) (list 'original value))
(defun par-symbol-cell-replacement (value) (list 'replacement value))
(defun par-symbol-cell-advice (value) (list 'advised value))
(defalias 'par-symbol-cell-fn-alias 'par-symbol-cell-fn)
(defun par-symbol-cell-named-call (value) (par-symbol-cell-fn value))

;; Bconstant, Bconstant+1, Bcall+1, Breturn.
(defun par-symbol-cell-byte-call (symbol value)
  (funcall (make-byte-code 0 (unibyte-string 192 193 33 135)
                           (vector symbol value) 2)))

(prin1
 (list 'cells
       (list (par-symbol-cell-named-call 1)
             (par-symbol-cell-byte-call 'par-symbol-cell-fn-alias 1))
       (progn (advice-add 'par-symbol-cell-fn :override #'par-symbol-cell-advice)
              (list (par-symbol-cell-named-call 2)
                    (par-symbol-cell-byte-call 'par-symbol-cell-fn-alias 2)))
       (progn (advice-remove 'par-symbol-cell-fn #'par-symbol-cell-advice)
              (eval '(par-symbol-cell-fn 3) t))
       (eval '(par-symbol-cell-fn
               (progn (fset 'par-symbol-cell-fn #'par-symbol-cell-replacement) 4))
             t)
       (par-symbol-cell-byte-call 'par-symbol-cell-fn-alias 5)
       (funcall 'par-symbol-cell-fn
                (progn (advice-add 'par-symbol-cell-fn :override #'par-symbol-cell-advice) 6))
       (progn (advice-remove 'par-symbol-cell-fn #'par-symbol-cell-advice)
              (fmakunbound 'par-symbol-cell-fn)
              (list (fboundp 'par-symbol-cell-fn)
                    (symbol-function 'par-symbol-cell-fn)
                    (condition-case err (eval '(par-symbol-cell-fn 6) t)
                      (void-function err))))
       (progn (fset 'par-symbol-cell-fn #'par-symbol-cell-replacement)
              (par-symbol-cell-byte-call 'par-symbol-cell-fn 7))))
(terpri)

(let* ((name "par-symbol-cell-private-function")
       (table (obarray-make))
       (old (intern name table)))
  (fset old (lambda (value) (cons 'old value)))
  (unintern old table)
  (let ((new (intern name table)))
    (fset new (lambda (value) (cons 'new value)))
    (prin1
     (list 'identity
           (eq old new)
           (par-symbol-cell-byte-call old 1)
           (par-symbol-cell-byte-call new 2)
           (progn (fmakunbound old) (fboundp old))
           (funcall new 3)))
    (terpri)))
