;;; native-cache-load-fns.el --- native cache load bench fixture -*- lexical-binding: t -*-
;; Small string functions shaped like the Windows SKK IME's merge code.
(defun ncl-eq (a b) (string= a b))
(defun ncl-split (s) (split-string s "/" t))
(defun ncl-dedup (l) (let (out) (dolist (x l) (unless (member x out) (push x out))) (nreverse out)))
;;; native-cache-load-fns.el ends here
