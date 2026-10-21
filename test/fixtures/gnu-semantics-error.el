;;; gnu-semantics-error.el --- GNU error quote fixture -*- lexical-binding: t; -*-
(dolist (style '(nil curve grave straight))
  (let ((text-quoting-style style))
    (dolist (args '(("`name' has %s" "`value'") ("already ‘curved’")
                    ("\\=`literal\\='") ("percent %% %S" foo)))
      (prin1 (list style (condition-case e (apply #'error args) (error e))))
      (terpri))))
