;;; -*- lexical-binding: t; -*-
(defvar n4-gc-seen nil)
(mapbacktrace (lambda (_e f _a _flags) (garbage-collect) (when (eq f 'mapbacktrace) (setq n4-gc-seen t))))
(princ "map-recursion-gc|") (prin1 n4-gc-seen) (terpri)
(defun n4-gc-helper () (backtrace-eval '(progn (garbage-collect) (setq n4-gc-x (list 8 9))) 0 'n4-gc-helper))
(defun n4-gc-live () (let ((n4-gc-x (list 7))) (list (n4-gc-helper) n4-gc-x)))
(princ "eval-gc-shared|") (prin1 (n4-gc-live)) (terpri)
(princ "N4-GC-REFERENCE-DONE\n")
nil
