;;; nemacs-next-elpa-dependent.el --- dependent fixture package -*- lexical-binding: t; -*-

(require 'nemacs-next-elpa-base)

(defun nemacs-next-elpa-dependent-value ()
  "Return the dependent fixture value."
  (list 'dependent-ready (nemacs-next-elpa-base-value)))

(provide 'nemacs-next-elpa-dependent)

;;; nemacs-next-elpa-dependent.el ends here
