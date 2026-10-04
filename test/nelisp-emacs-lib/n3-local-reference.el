;;; -*- lexical-binding: t; -*-
(defvar n3local-events nil)
(defun n3local-watch (s v op where)
  (push (list s v op (and where (buffer-name where)) (and (boundp s) (symbol-value s))) n3local-events))
(defmacro n3local-row (name form)
  `(progn (princ ,name) (princ "|") (prin1 (condition-case e ,form (error (list 'ERR (car e))))) (terpri)))
(n3local-row "watch-local" (progn (setq n3local-events nil) (defvar n3local-x 1) (add-variable-watcher 'n3local-x #'n3local-watch) (unwind-protect (with-current-buffer (get-buffer-create "n3-local") (make-local-variable 'n3local-x) (set 'n3local-x 2) (let ((n3local-x 3)) nil) (set-default 'n3local-x 4) (makunbound 'n3local-x) (kill-local-variable 'n3local-x) (set 'n3local-x 5) (reverse n3local-events)) (remove-variable-watcher 'n3local-x #'n3local-watch))))
(n3local-row "watch-local-explicit" (progn (setq n3local-events nil) (add-variable-watcher 'n3local-x #'n3local-watch) (unwind-protect (with-current-buffer (get-buffer-create "n3-local-explicit") (set (make-local-variable 'n3local-x) 9) (reverse n3local-events)) (remove-variable-watcher 'n3local-x #'n3local-watch))))
(n3local-row "watch-alias-set" (progn (defvaralias 'n3local-alias 'n3local-base) (setq n3local-base 1 n3local-events nil) (add-variable-watcher 'n3local-base #'n3local-watch) (unwind-protect (progn (set 'n3local-alias 2) (setq n3local-alias 3) (makunbound 'n3local-alias) (reverse n3local-events)) (remove-variable-watcher 'n3local-base #'n3local-watch))))
(n3local-row "watch-alias-let" (progn (setq n3local-base 1 n3local-events nil) (add-variable-watcher 'n3local-base #'n3local-watch) (unwind-protect (list (let ((n3local-alias 7)) (symbol-value 'n3local-base)) (reverse n3local-events)) (remove-variable-watcher 'n3local-base #'n3local-watch))))
(princ "N3-LOCAL-DONE\n")
nil
