;;; par-time-date.el --- GNU time-date day conversions -*- lexical-binding: t; -*-

(prin1 (list 'epoch (days-to-time 0) (days-to-time 1)
             (days-to-time -1) (days-to-time 0.5)))
(terpri)

(set-time-zone-rule "JST-9")
(prin1 (list 'local-day (time-to-days -32401) (time-to-days -32400)
             (time-to-days 53999) (time-to-days 54000)))
(terpri)

(prin1 (cons 'owners
             (mapcar (lambda (name)
                       (file-name-base (symbol-file name 'defun)))
                     '(days-to-time time-to-days seconds-to-time))))
(terpri)
