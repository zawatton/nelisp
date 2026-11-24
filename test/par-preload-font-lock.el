;;; par-preload-font-lock.el --- Elisp font-lock and reader state -*- lexical-binding: t; -*-

(prin1
 (with-temp-buffer
   (emacs-lisp-mode)
   (insert "(defun foo () \"doc\")")
   (font-lock-ensure)
   (mapcar (lambda (pos) (get-text-property pos 'face)) '(2 8 15))))
(terpri)

(prin1
 (list (default-value 'read-symbol-shorthands)
       (local-variable-if-set-p 'read-symbol-shorthands)
       (get 'read-symbol-shorthands 'safe-local-variable)
       (with-temp-buffer
         (hack-one-local-variable 'read-symbol-shorthands '(("p-" . "probe-")))
         (list (local-variable-p 'read-symbol-shorthands)
               (symbol-value 'read-symbol-shorthands)
               (with-temp-buffer (symbol-value 'read-symbol-shorthands))))))
(terpri)
