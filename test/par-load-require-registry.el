;;; par-load-require-registry.el --- Require consults the feature registry -*- lexical-binding: t; -*-

(let ((original (symbol-function 'featurep)))
  (provide 'par-load-provided)
  (unwind-protect
      (progn
        (fset 'featurep (lambda (&rest _args) nil))
        (prin1 (list 'requires-provided
                     (require 'par-load-provided nil t)))
        (terpri))
    (fset 'featurep original)
    (setq features (delq 'par-load-provided features))))

(prin1 (list 'requires-symbol
             (condition-case err
                 (require 42 nil t)
               (wrong-type-argument (cdr err)))))
(terpri)
