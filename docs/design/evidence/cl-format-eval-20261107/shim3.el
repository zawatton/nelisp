;;; -*- lexical-binding: t; -*-
;; Evaluation-only: names and behavior the core binary lacks.
(unless (fboundp 'font-lock-add-keywords) (defun font-lock-add-keywords (&rest _) nil))
(unless (fboundp 'xor) (defun xor (a b) (cond ((not a) b) ((not b) a))))
(unless (boundp 'float-pi) (defconst float-pi 3.141592653589793))
(unless (boundp 'pi) (defconst pi float-pi))
(unless (fboundp 'capitalize-word)
  (defun capitalize-word (n)
    (dotimes (_ n)
      (when (re-search-forward "[[:alnum:]]+" nil t)
        (let ((w (match-string 0))) (replace-match (capitalize w) t t))))))
;; A radix-only stand-in for calc-eval, which the core does not provide in this form.
(require 'calc)
(defun calc-eval (spec &rest _)
  (let* ((n (car spec)) (b (nth 2 spec)) (neg (< n 0)) (m (abs n)) (s ""))
    (while (progn (setq s (concat (substring "0123456789abcdefghijklmnopqrstuvwxyz" (% m b) (1+ (% m b))) s) m (/ m b)) (> m 0)))
    (concat (if neg "-" "") (number-to-string b) "#" s)))
