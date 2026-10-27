;;; driver.el --- run the lexbc corpus  -*- lexical-binding: t; -*-
;; env LEXBC_OVERLAY: optional file loaded first (prototype a regenerated block on a
;; prebuilt binary).
(let ((ov (getenv "LEXBC_OVERLAY")))
  (when (and ov (> (length ov) 0)) (load ov nil t)))
(load (expand-file-name "corpus.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(let ((i 0) (print-length nil) (print-level nil) (print-escape-newlines t))
  (dolist (c lexbc-cases)
    (setq i (1+ i))
    (princ (format "%d\t%S\n" i (condition-case err (eval c t) (error (list 'ERR err)))))))
(kill-emacs 0)
