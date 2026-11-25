;;; par-calendar-vendor.el --- GNU calendar date conversion -*- lexical-binding: t; -*-

(unless (fboundp 'calendar-gregorian-from-absolute)
  (require 'calendar))
(prin1 (cons 'owners
             (mapcar (lambda (name)
                       (let ((file (symbol-file name 'defun)))
                         (and file (equal (file-name-base file) "calendar") t)))
                     '(calendar-absolute-from-gregorian
                       calendar-gregorian-from-absolute))))
(terpri)
(dolist (date '((1 1 1) (12 31 -1) (2 29 2000) (3 1 1900) (1 1 0)))
  (prin1 (cons date
               (condition-case err
                   (let ((absolute (calendar-absolute-from-gregorian date)))
                     (list absolute (calendar-gregorian-from-absolute absolute)))
                 (error (list (car err))))))
  (terpri))
