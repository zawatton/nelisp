;;; emacs-cc-binding-locus-1.el --- Binding owner inspection -*- lexical-binding: t; -*-

(unless (fboundp 'variable-binding-locus)
  (defun variable-binding-locus (&rest arguments)
    "Return the current buffer when VARIABLE has a local binding, else nil."
    (unless (= (length arguments) 1)
      (signal 'wrong-number-of-arguments
              (list 'variable-binding-locus (length arguments))))
    (let ((variable (car arguments)))
      (unless (symbolp variable)
        (signal 'wrong-type-argument (list 'symbolp variable)))
      (when (local-variable-p variable)
        (current-buffer)))))

(provide 'emacs-cc-binding-locus-1)
;;; emacs-cc-binding-locus-1.el ends here
