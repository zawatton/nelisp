;;; aotscale-unitequal.el --- are two unit-cache files the same unit?  -*- lexical-binding: t; -*-
;; emacs --batch -Q -l tools/ai/aotscale-unitequal.el A.unit B.unit
;; Reads both (so `#N=' shared-structure labels, which only record which
;; strings the writing process happened to share, do not count), masks the
;; sha256 in specialized helper names (`F--nelisp-context-<sha256>-N'), and
;; prints UNITEQUAL equal|ctxname|differ.
(setq max-lisp-eval-depth 20000)
(defun aotscale-unitequal--read (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents file))
    (goto-char (point-min)) (read (current-buffer))))
(defun aotscale-unitequal--mask (x)
  (cond ((stringp x) (replace-regexp-in-string "--nelisp-context-[0-9a-f]\\{64\\}-" "--nelisp-context-X-" x t t))
        ((consp x) (let ((out nil))
                     (while (consp x) (push (aotscale-unitequal--mask (car x)) out) (setq x (cdr x)))
                     (nconc (nreverse out) (aotscale-unitequal--mask x))))
        ((vectorp x) (apply #'vector (mapcar #'aotscale-unitequal--mask x)))
        (t x)))
(let* ((a (aotscale-unitequal--read (nth 0 command-line-args-left)))
       (b (aotscale-unitequal--read (nth 1 command-line-args-left))))
  (setq command-line-args-left nil)
  (princ (format "UNITEQUAL %s\n"
                 (cond ((equal a b) "equal")
                       ((equal (aotscale-unitequal--mask a) (aotscale-unitequal--mask b)) "ctxname")
                       (t "differ")))))
