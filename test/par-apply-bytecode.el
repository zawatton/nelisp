;;; par-apply-bytecode.el --- Spreading arguments to GNU byte code -*- lexical-binding: t; -*-

;; GNU 31.1 compilation of (lambda (a &optional b &rest c) (list a b c)).
(defconst par-apply-bytecode-fn
  (make-byte-code 641 (unibyte-string 2 2 2 69 135) [] 6))

(defun par-apply-bytecode--result (function)
  (list (apply function '(1))
        (apply function 2 nil)
        (apply function 3 4 '(5 6))
        (apply function '(7 8 9))
        (condition-case err (apply function nil) (error (car err)))
        (condition-case err (apply function 1 '(2 . 3)) (error (car err)))
        (let ((cycle (list 2 3)))
          (setcdr (cdr cycle) cycle)
          (condition-case err (apply function 1 cycle) (error (car err))))))

(defun par-apply-bytecode-run ()
  (let ((i 0) result)
    (while (< i 100)
      (setq result (par-apply-bytecode--result par-apply-bytecode-fn)
            i (1+ i)))
    (prin1 result)
    (terpri)))

(fset 'par-apply-bytecode-target par-apply-bytecode-fn)

(defun par-apply-bytecode--named (argument)
  (par-apply-bytecode-target argument))

(defun par-apply-bytecode-redefinition ()
  (let ((i 0) result)
    (while (< i 100)
      (setq result (par-apply-bytecode--named 3)
            i (1+ i)))
    (prin1
     (list result
           (progn (fset 'par-apply-bytecode-target (lambda (x) (1+ x)))
                  (par-apply-bytecode--named 3))
           (progn (fset 'par-apply-bytecode-target 'car)
                  (par-apply-bytecode--named '(new old)))
           (progn (fmakunbound 'par-apply-bytecode-target)
                  (condition-case err (par-apply-bytecode--named 3)
                    (error (car err)))))))
  (terpri))

