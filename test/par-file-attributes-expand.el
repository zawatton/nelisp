;;; par-file-attributes-expand.el --- Error boundary for file attributes -*- lexical-binding: t; -*-

;; With no bookmark file, GNU bookmark.el asks for (file-attributes nil).
;; dired.c suppresses conditions raised by filename expansion only.
(defun par-file-attributes-handler (operation filename &rest _args)
  (unless (and (eq operation 'expand-file-name)
               (equal filename "/par-attrs:no-expansion"))
    (signal 'file-error (list operation))))

(defun par-file-attributes-result (&rest arguments)
  (condition-case err
      (apply #'file-attributes arguments)
    (error err)))

(let ((file-name-handler-alist
       '(("\\`/par-attrs:" . par-file-attributes-handler))))
  (prin1 (list (par-file-attributes-result nil)
               (par-file-attributes-result 7 'integer)
               (par-file-attributes-result "/par-attrs:path")
               (par-file-attributes-result "/par-attrs:no-expansion")))
  (terpri)
  (let ((inhibit-file-name-operation 'expand-file-name)
        (inhibit-file-name-handlers '(par-file-attributes-handler)))
    (prin1 (par-file-attributes-result "/par-attrs:path" 'integer))
    (terpri)))
