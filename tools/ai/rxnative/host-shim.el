;;; host-shim.el --- run the Lisp side of rxnative in GNU Emacs against refvm  -*- lexical-binding: t; -*-
;; Defines `nelisp--rx-exec' on top of the Lisp reference VM, then loads the
;; regexp library so `nlre-string-match' takes the native route.
(let ((root (expand-file-name "../../.." (file-name-directory load-file-name))))
  (load (expand-file-name "tools/ai/rxnative/refvm.el" root) nil t)
  (defvar nelisp-casetab--epoch 0)
  (defvar nelisp-casetab--epoch-owner nil)
  (defun nelisp--rx-exec (prog string start limit flags capvec _scratch)
    (let* ((ng (aref prog 4)) (r (rxn-exec prog string start limit (= (mod flags 2) 1))))
      (cond ((eq r t) t) ((null r) nil)
            (t (let ((g 0) (caps (cdr r)))
                 (while (< g ng)
                   (let ((c (nth g caps)))
                     (aset capvec (* 2 g) (if c (car c) -1))
                     (aset capvec (1+ (* 2 g)) (if c (cdr c) -1)))
                   (setq g (1+ g)))
                 (+ (* (car r) 2147483648) (cdr (car (cdr r))))))))) 
  (load (expand-file-name "lisp/nelisp-stdlib-regexp.el" root) nil t)
  ;; host: the canon memo is trivially the built-in one
  (setq nlre--translate-memo-buffer (current-buffer) nlre--translate-memo-epoch 0 nlre--translate-memo nil)
  (setq nelisp-casetab--epoch-owner (symbol-function 'set-case-table)))
(provide 'host-shim)
