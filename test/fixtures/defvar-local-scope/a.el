;;; a.el --- bodyless defvar file-local scope smoke -*- lexical-binding: t; -*-

;; A bodyless top-level (defvar SYM) declares SYM special for the rest of
;; this file only (GNU eval.c Fdefvar + readevalloop's per-file
;; `internal-interpreter-environment'); it is not globally special.

(defvar dvls-a)
(defvar dvls-top nil)
(defvar dvls-b nil)
(defun dvls-read (sym) (condition-case _ (symbol-value sym) (void-variable 'void)))
(defun dvls-t1 () (let ((dvls-a 5)) (dvls-read 'dvls-a)))
(defun dvls-t2 () (defvar dvls-c) (let ((dvls-c 9)) (dvls-read 'dvls-c)))
(defun dvls-t3 () (let ((dvls-d 3)) (defvar dvls-d) (let ((dvls-d 4)) (dvls-read 'dvls-d))))
(defun dvls-t4 () (let ((dvls-e 7)) (dvls-read 'dvls-e)))
(setq dvls-top (let ((dvls-a 6)) (dvls-read 'dvls-a)))
(load (expand-file-name "b.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(princ (format "t1=%S t2=%S t3=%S t4=%S top=%S b=%S special=%S\n"
               (dvls-t1) (dvls-t2) (dvls-t3) (dvls-t4) dvls-top dvls-b
               (list (special-variable-p 'dvls-a) (special-variable-p 'dvls-c))))
