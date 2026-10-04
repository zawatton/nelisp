;;; emacs-cc-variable-alias-1.el --- Variable alias inspection -*- lexical-binding: t; -*-

(unless (fboundp 'indirect-variable)
  (defun indirect-variable (object)
    "Follow OBJECT's variable alias chain, preserving non-symbols."
    (let ((seen nil) next)
      (while (and (symbolp object)
                  (boundp 'nelisp--defvaralias-registry)
                  (setq next (assq object nelisp--defvaralias-registry)))
        (when (memq object seen)
          (signal 'cyclic-variable-indirection (list object)))
        (setq seen (cons object seen) object (cdr next)))
      object)))

(unless (fboundp 'internal-delete-indirect-variable)
  (defun internal-delete-indirect-variable (symbol)
    "Remove SYMBOL's alias registration and unbind it."
    (unless (symbolp symbol)
      (signal 'wrong-type-argument (list 'symbolp symbol)))
    (when (boundp 'nelisp--defvaralias-registry)
      (setq nelisp--defvaralias-registry
            (assq-delete-all symbol nelisp--defvaralias-registry))
      (when (fboundp 'nelisp--alias-rebuild-reverse)
        (nelisp--alias-rebuild-reverse)))
    (makunbound symbol)
    symbol))

;; The standalone evaluator stores variable aliases as copied values.  Keep
;; that bootstrap representation coherent while forwarding Lisp accessors to
;; the registered canonical variable.  Native special-form dispatch defers
;; to this function when its function cell is installed.
(when (and (fboundp 'nelisp--repr)
           (not (and (boundp 'nelisp--alias-provider-installed)
                     nelisp--alias-provider-installed)))
  (defvar nelisp--alias-provider-installed nil)
  (defvar nelisp--defvaralias-registry nil)
  (defvar nelisp--defvaralias-reverse nil)
  (let ((nelisp--alias-raw-set (symbol-function 'set))
        (nelisp--alias-raw-symbol-value (symbol-function 'symbol-value))
        (nelisp--alias-raw-boundp (symbol-function 'boundp))
        (nelisp--alias-raw-makunbound (symbol-function 'makunbound)))
    (defun nelisp--alias-canonical (symbol)
      (let ((seen nil) (next symbol))
        (while (and (symbolp next)
                    (setq symbol (assq next nelisp--defvaralias-registry)))
          (when (memq next seen)
            (signal 'cyclic-variable-indirection (list next)))
          (setq seen (cons next seen) next (cdr symbol)))
        next))
    (defun set (symbol value)
      (let ((target (nelisp--alias-canonical symbol)))
        (funcall nelisp--alias-raw-set target value)
        (when nelisp--defvaralias-reverse
          (let ((emacs-parity-misc--inhibit-watchers t))
            (dolist (alias (cdr (assq target nelisp--defvaralias-reverse)))
              (funcall nelisp--alias-raw-set alias value))))
        value))
    (defun symbol-value (symbol)
      (funcall nelisp--alias-raw-symbol-value
               (nelisp--alias-canonical symbol)))
    (defun boundp (symbol)
      (funcall nelisp--alias-raw-boundp
               (nelisp--alias-canonical symbol)))
    (defun makunbound (symbol)
      (let* ((entry (assq symbol nelisp--defvaralias-registry))
             (target (if entry symbol (nelisp--alias-canonical symbol))))
        (when entry
          (setq nelisp--defvaralias-registry
                (assq-delete-all symbol nelisp--defvaralias-registry))
          (nelisp--alias-rebuild-reverse))
        (funcall nelisp--alias-raw-makunbound target)
        (when nelisp--defvaralias-reverse
          (let ((emacs-parity-misc--inhibit-watchers t))
            (dolist (alias (cdr (assq target nelisp--defvaralias-reverse)))
              (funcall nelisp--alias-raw-makunbound alias))))
        symbol))
    (defun defvaralias (new-alias base-variable &optional docstring)
      (unless (symbolp new-alias)
        (signal 'wrong-type-argument (list 'symbolp new-alias)))
      (when (or (memq new-alias '(nil t))
                (keywordp new-alias)
                (get new-alias 'constant))
        (error "Cannot make a constant an alias: %S" new-alias))
      (unless (symbolp base-variable)
        (signal 'wrong-type-argument (list 'symbolp base-variable)))
      (when (fboundp 'emacs-parity-misc--notify)
        (emacs-parity-misc--notify new-alias base-variable 'defvaralias nil))
      (let ((emacs-parity-misc--inhibit-watchers t)
            (doc docstring)
            (target (nelisp--alias-canonical base-variable))
            (alias-bound (funcall nelisp--alias-raw-boundp new-alias)))
        (when (or (eq new-alias base-variable) (eq new-alias target))
          (signal 'cyclic-variable-indirection (list new-alias)))
        (when (and alias-bound
                   (not (funcall nelisp--alias-raw-boundp target)))
          (funcall nelisp--alias-raw-set target
                   (funcall nelisp--alias-raw-symbol-value new-alias)))
        (setq nelisp--defvaralias-registry
              (cons (cons new-alias base-variable)
                    (assq-delete-all new-alias nelisp--defvaralias-registry)))
        (nelisp--alias-rebuild-reverse)
        (when (funcall nelisp--alias-raw-boundp target)
          (funcall nelisp--alias-raw-set new-alias
                   (funcall nelisp--alias-raw-symbol-value target)))
        (when doc
          (put new-alias 'variable-documentation doc))
        ;; GNU discards the alias's former watcher list when its variable
        ;; cell becomes an indirection. Future registration uses the base.
        (when (boundp 'emacs-parity-misc--variable-watchers)
          (remhash new-alias emacs-parity-misc--variable-watchers)
          (emacs-parity-misc--watcher-flag))
        base-variable))
    (defun nelisp--alias-rebuild-reverse ()
        (setq nelisp--defvaralias-reverse nil)
        (dolist (entry nelisp--defvaralias-registry)
          (let* ((canonical (nelisp--alias-canonical (car entry)))
                 (group (assq canonical nelisp--defvaralias-reverse)))
            (if group
                (setcdr group (cons (car entry) (cdr group)))
              (setq nelisp--defvaralias-reverse
                    (cons (cons canonical (list (car entry)))
                          nelisp--defvaralias-reverse))))))
    (setq nelisp--alias-provider-installed t)))

(provide 'emacs-cc-variable-alias-1)
;;; emacs-cc-variable-alias-1.el ends here
