;;; -*- lexical-binding: t; -*-
(defvar n4-extra-x 1)
(defvar n4-extra-events nil)
(defun n4-extra-w (_s v op _where) (push (list v op) n4-extra-events))
(let ((n4-extra-x 2) (n4-extra-x 3))
  (add-variable-watcher 'n4-extra-x #'n4-extra-w))
(remove-variable-watcher 'n4-extra-x #'n4-extra-w)
(prin1 (reverse n4-extra-events)) (terpri)
(princ "N4-LATE-WATCHER-DONE\n")
nil
