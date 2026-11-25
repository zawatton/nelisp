;;; par-staged-subr.el --- GNU subr.el string and list helpers -*- lexical-binding: t; -*-

(defvar par-staged-subr-order nil)

(let ((par-staged-subr-order nil))
  (add-to-ordered-list 'par-staged-subr-order 'b 20)
  (add-to-ordered-list 'par-staged-subr-order 'a 10)
  (add-to-ordered-list 'par-staged-subr-order 'c 30)
  (prin1 (list 'order (copy-sequence par-staged-subr-order)
               (copy-sequence (add-to-ordered-list 'par-staged-subr-order 'c -1))
               (add-to-ordered-list 'par-staged-subr-order 'a t)))
  (terpri))

(let ((text (copy-sequence "aba")))
  (let ((result (subst-char-in-string ?a ?x text t)))
    (prin1 (list 'in-place (eq result text) text result))
    (terpri)))

(prin1 (list 'shell-quoting (shell-quote-argument "safe-path")
             (condition-case err
                 (shell-quote-argument "a b'c" t)
               (error (car err)))))
(terpri)

(prin1 (list 'string-list
             (member-ignore-case "a" '(1 "B" "A" "z"))
             (split-string-and-unquote
              (combine-and-quote-strings '("plain" "two words" "a\"b" "a\\b")))))
(terpri)

(prin1 (cons 'owners
             (mapcar (lambda (name)
                       (file-name-base (symbol-file name 'defun)))
                     '(add-to-ordered-list member-ignore-case shell-quote-argument
                       subst-char-in-string combine-and-quote-strings
                       split-string-and-unquote))))
(terpri)
