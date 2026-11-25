;;; par-fx-core-source.el --- Source evaluation and interpreted apply -*- lexical-binding: t; -*-

(defvar par-fx-core-source-dynamic 'outside)

(eval '(defun par-fx-core-source-read (par-fx-core-source-dynamic)
         (list (symbol-value 'par-fx-core-source-dynamic)
               (backtrace-frame 0 'par-fx-core-source-read)))
      nil)

(defalias 'par-fx-core-source-alias #'par-fx-core-source-read)

(let ((original (symbol-function 'par-fx-core-source-read)))
  (unwind-protect
      (let ((direct (eval '(par-fx-core-source-read 'direct) t))
            (aliased (eval '(par-fx-core-source-alias
                            (progn
                              (fset 'par-fx-core-source-read
                                    (lambda (x) (list 'replacement x)))
                              'aliased))
                          t)))
        (prin1 (list 'source direct (car aliased)
                     (funcall 'par-fx-core-source-alias 'next)
                     (car (apply original '(applied)))
                     par-fx-core-source-dynamic))
        (terpri))
    (fset 'par-fx-core-source-read original)))
