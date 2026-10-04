;;; emacs-cc-buffer-constructor-1.el --- Canonical buffer construction -*- lexical-binding: t; -*-

(defun emacs-cc-buffer-constructor--generate-new-buffer (name &optional inhibit-buffer-hooks)
  "Create a new buffer using NAME and canonical unique name selection.
Pass INHIBIT-BUFFER-HOOKS to `get-buffer-create'."
  (get-buffer-create (generate-new-buffer-name name) inhibit-buffer-hooks))

(when (fboundp 'nelisp--repr)
  (fset 'generate-new-buffer
        (symbol-function 'emacs-cc-buffer-constructor--generate-new-buffer)))

(provide 'emacs-cc-buffer-constructor-1)
;;; emacs-cc-buffer-constructor-1.el ends here
