;;; par-dedup-regexp-quote.el --- GNU search.c regexp quoting -*- lexical-binding: t; -*-

(let* ((plain (propertize "(a|b){2}]" 'face 'bold))
       (quoted (regexp-quote plain)))
  (prin1 (list 'unchanged (eq plain quoted)
               (get-text-property 0 'face quoted) quoted))
  (terpri))

(let* ((text (propertize "é.$*+?[^\\" 'face 'bold))
       (quoted (regexp-quote text)))
  (prin1 (list 'escaped (multibyte-string-p quoted)
               (get-text-property 0 'face quoted) quoted))
  (terpri))

(let* ((raw (unibyte-string 255 ?.))
       (quoted (regexp-quote raw)))
  (prin1 (list 'unibyte (multibyte-string-p quoted) (append quoted nil)))
  (terpri))

(prin1 (list 'bad-argument
             (condition-case err (regexp-quote [1 2]) (error err))))
(terpri)
