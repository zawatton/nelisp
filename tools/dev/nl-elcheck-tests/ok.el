;;; ok.el --- clean  -*- lexical-binding: t; -*-
(defun ok-a (x)
  "Return X plus one.  Quote \"like this\" is fine; so is a (paren) in text."
  (1+ x))
(defvar ok-b '("a" "b") "A list.")
(defun ok-c () (list ?\" ?\( "s"))
(provide 'ok)
