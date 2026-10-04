;;; emacs-cc-aggregate-arity-1.el --- Named predicate arity errors -*- lexical-binding: t; -*-

(defun emacs-aggregate--call (name arguments)
  "Validate NAME's public arity before calling its captured predicate."
  (unless (= (length arguments) 1)
    (signal 'wrong-number-of-arguments (list name (length arguments))))
  (apply (get name 'emacs-aggregate-original) arguments))

(when (fboundp 'nelisp--repr)
  (dolist (name '(bool-vector-p char-table-p hash-table-p recordp))
    (unless (get name 'emacs-aggregate-original)
      (put name 'emacs-aggregate-original (symbol-function name))
      (fset name
            (list 'lambda '(&rest arguments)
                  (list 'emacs-aggregate--call (list 'quote name) 'arguments))))))

(provide 'emacs-cc-aggregate-arity-1)
;;; emacs-cc-aggregate-arity-1.el ends here
