;;; par-eval-core2-apply.el --- FX argument spreading -*- lexical-binding: t; -*-

(defun par-core2-spread-target (a &optional b &rest tail)
  (list a b (length tail) (car tail)))

(defun par-core2-spread (target tail)
  (apply target 'head tail))

(let* ((target (symbol-function 'par-core2-spread-target))
       (short (list 'first 'second))
       (long (make-list 300 'item))
       (cycle (list 'loop)))
  (setcdr cycle cycle)
  (prin1 (list 'spread (par-core2-spread target nil)
               (par-core2-spread target short)
               (progn (setcdr short (list 'changed 'extra))
                      (par-core2-spread target short))
               (par-core2-spread target long)))
  (terpri)
  (prin1 (list 'bad-spread
               (condition-case e (par-core2-spread target '(a . b))
                 (error (car e)))
               (condition-case e (par-core2-spread target cycle)
                 (error (car e)))))
  (terpri))
