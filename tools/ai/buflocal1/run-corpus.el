;;; run-corpus.el --- evaluate each form of $CORPUS, print "N (ok VAL)|(err E)"  -*- lexical-binding: t; -*-
(let* ((text (with-temp-buffer (insert-file-contents (getenv "CORPUS")) (buffer-string)))
       (pos 0) (i 0) (len (length text)) r)
  (while (condition-case nil (progn (setq r (read-from-string text pos)) t) (error nil))
    (setq pos (cdr r) i (1+ i))
    (let ((res (condition-case e (list 'ok (eval (car r) t)) (error (list 'err e)))))
      (princ (format "%d %S\n" i (if (and (eq (car res) 'err) (not (memq (car (cadr res)) '(args-out-of-range wrong-type-argument void-variable void-function setting-constant)))) (list 'err (car (cadr res))) res))))
    (ignore-errors (set-buffer (get-buffer-create "*scratch*")))))
