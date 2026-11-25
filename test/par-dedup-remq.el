;;; par-dedup-remq.el --- GNU subr.el remq -*- lexical-binding: t; -*-

(let* ((input (list 'skip 'retain))
       (result (remq 'skip input)))
  (prin1 (list 'retained-tail (eq result (cdr input)) input))
  (terpri))

(prin1 (list 'dotted-list
             (condition-case err
                 (remq 'skip '(skip . tail))
               (wrong-type-argument err))))
(terpri)
