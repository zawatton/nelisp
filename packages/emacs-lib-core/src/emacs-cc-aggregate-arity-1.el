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
      ;; gnu562: a lexical closure, which the fx VM compiles, rather than a
      ;; raw (lambda ...) list, which every call interpreted: `recordp' on a
      ;; string cost 13 us and `emacs-char-table-p' 29 us through it.  The
      ;; arity check and its error data are unchanged.
      (fset name
            (eval `(lambda (&rest arguments)
                     (if (and (consp arguments) (null (cdr arguments)))
                         (funcall (get ',name 'emacs-aggregate-original) (car arguments))
                       (emacs-aggregate--call ',name arguments)))
                  t)))))

(provide 'emacs-cc-aggregate-arity-1)
;;; emacs-cc-aggregate-arity-1.el ends here
