;;; nelisp-n3-duplicate-dynamic-reference.el --- Known watcher gap -*- lexical-binding: t; -*-
;; GNU has two unlet callbacks for the repeated dynamic bindings below.
;; The reader's parallel-let binding table retains only the last cell.
;; let* is a passing control. Keep this reference separate from the passing
;; N3 suite.
(defvar n3dup-x 1)
(defvar n3dup-events nil)
(defun n3dup-w (_s v op _where) (push (list v op) n3dup-events))
(add-variable-watcher 'n3dup-x #'n3dup-w)
(unwind-protect
    (progn
      (let ((n3dup-x 2) (n3dup-x 3)) nil)
      (princ "let|") (prin1 (reverse n3dup-events)) (terpri)
      (setq n3dup-events nil)
      (let* ((n3dup-x 2) (n3dup-x 3)) nil)
      (princ "let-star|") (prin1 (reverse n3dup-events)) (terpri))
  (remove-variable-watcher 'n3dup-x #'n3dup-w))
(princ "N3-DUPLICATE-DONE\n")
nil
