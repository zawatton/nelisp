;;; par-url-expand-owners.el --- GNU URL expansion ownership -*- lexical-binding: t; -*-

(defvar smime-openssl-program nil)
(prin1 (list 'before-require (fboundp 'url-expander-remove-relative-links)))
(terpri)

(require 'url-expand)
(prin1 (cons 'owners
             (mapcar (lambda (name)
                       (let ((file (symbol-file name 'defun)))
                         (and file (equal (file-name-base file) "url-expand") t)))
                     '(url-default-expander url-expander-remove-relative-links))))
(terpri)
(prin1 (list 'relative-links
             (url-expander-remove-relative-links "")
             (url-expander-remove-relative-links "../a/./b/..")
             (url-expander-remove-relative-links "/x/../../new")))
(terpri)
