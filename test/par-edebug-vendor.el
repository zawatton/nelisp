;;; par-edebug-vendor.el --- GNU Edebug entry points -*- lexical-binding: t; -*-

(prin1 (list 'before-require (fboundp 'edebug-before) (fboundp 'edebug-after)))
(terpri)

(require 'edebug)
(dolist (entry '((edebug-before 0) (edebug-after 0 1 value)))
  (let* ((name (car entry))
         (file (symbol-file name 'defun)))
    (prin1 (list name
                 (and file (equal (file-name-base file) "edebug") t)
                 (condition-case err
                     (apply name (cdr entry))
                   (error (car err)))))
    (terpri)))
