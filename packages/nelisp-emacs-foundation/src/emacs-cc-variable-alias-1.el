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
        (nelisp--alias-raw-makunbound (symbol-function 'makunbound))
        ;; Canonical-cache keys are proper symbol lists, a shape supported
        ;; by the native comparator.  The general compatibility `equal'
        ;; walks conses in the interpreter to add vector/marker semantics.
        (nelisp--alias-name-equal
         (symbol-function (if (fboundp 'nelisp--native-equal)
                              'nelisp--native-equal 'equal)))
        (nelisp--alias-values-cache nil))
    (defun nelisp--alias-canonical (symbol)
      (let ((seen nil) (next symbol))
        (while (and (symbolp next)
                    (setq symbol (assq next nelisp--defvaralias-registry)))
          (when (memq next seen)
            (signal 'cyclic-variable-indirection (list next)))
          (setq seen (cons next seen) next (cdr symbol)))
        next))
    (defun set (symbol value)
      (let ((target (if (assq symbol nelisp--defvaralias-registry)
                        (nelisp--alias-canonical symbol) symbol)))
        (funcall nelisp--alias-raw-set target value)
        (when nelisp--defvaralias-reverse
          (let ((emacs-parity-misc--inhibit-watchers t))
            (dolist (alias (cdr (assq target nelisp--defvaralias-reverse)))
              (funcall nelisp--alias-raw-set alias value))))
        value))
    (defun symbol-value (symbol)
      (funcall nelisp--alias-raw-symbol-value
               (if (assq symbol nelisp--defvaralias-registry)
                   (nelisp--alias-canonical symbol) symbol)))
    (defun boundp (symbol)
      (funcall nelisp--alias-raw-boundp
               (if (assq symbol nelisp--defvaralias-registry)
                   (nelisp--alias-canonical symbol) symbol)))
    (defun emacs-variable-alias-values (symbols unbound)
      "Snapshot SYMBOLS' live values in order, using UNBOUND for void cells.
Resolve aliases through this provider and respect dynamic bindings.  Cache
only canonical names, never values.  Native mapping reads ordinary bound
cells without an interpreted accessor frame for every symbol."
      (let ((entries nelisp--alias-values-cache) entry targets)
        (while (and entries (not entry))
          (when (and (eq nelisp--defvaralias-registry (aref (car entries) 1))
                     (funcall nelisp--alias-name-equal symbols (aref (car entries) 0)))
            (setq entry (car entries)))
          (setq entries (cdr entries)))
        (unless entry
          (setq targets
                (mapcar (lambda (symbol)
                          (unless (symbolp symbol)
                            (signal 'wrong-type-argument (list 'symbolp symbol)))
                          (if (assq symbol nelisp--defvaralias-registry)
                              (nelisp--alias-canonical symbol) symbol)) symbols)
                entry (vector (copy-sequence symbols)
                              nelisp--defvaralias-registry targets)
                nelisp--alias-values-cache
                (cons entry (when nelisp--alias-values-cache
                              (list (car nelisp--alias-values-cache))))))
        (setq targets (aref entry 2))
        (let ((flags (mapcar nelisp--alias-raw-boundp targets)))
          (if (not (memq nil flags))
              (apply #'vector (mapcar nelisp--alias-raw-symbol-value targets))
            ;; Mixed bound/void sets are normal during library bootstrap.
            ;; Avoid signalling once per switch just to discover a void cell.
            ;; The native pass already queried every cell's bound status.
            (let ((values (apply #'vector flags)) (index 0) symbol)
              (while targets
                (setq symbol (car targets))
                (aset values index
                      (if (aref values index)
                          (funcall nelisp--alias-raw-symbol-value symbol)
                        unbound))
                (setq targets (cdr targets) index (1+ index)))
              values)))))
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
        (setq nelisp--alias-values-cache nil
              nelisp--defvaralias-reverse nil)
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
