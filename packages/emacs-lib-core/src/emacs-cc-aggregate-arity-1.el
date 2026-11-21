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
      ;; one-argument call goes straight to an alias of the original, not
      ;; through `get' and `funcall' (recordp 1.7 -> 0.7 us).  The arity
      ;; check and its error data are unchanged.
      (let ((alias (intern (format "emacs-aggregate--%s-original" name))))
        (defalias alias (symbol-function name))
        (fset name
              (eval `(lambda (&rest arguments)
                       (if (and (consp arguments) (null (cdr arguments)))
                           (,alias (car arguments))
                         (emacs-aggregate--call ',name arguments)))
                    t))))))

(provide 'emacs-cc-aggregate-arity-1)
;;; emacs-cc-aggregate-arity-1.el ends here
