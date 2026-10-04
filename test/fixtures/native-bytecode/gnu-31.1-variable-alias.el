;;; gnu-31.1-variable-alias.el --- source-free alias call fixture -*- lexical-binding: t; -*-

(defvar nelisp-va-base-value 'initial)
(define-obsolete-variable-alias 'nelisp-va-old-value
  'nelisp-va-base-value "28.1")
(provide 'gnu-31.1-variable-alias)
;;; gnu-31.1-variable-alias.el ends here
