;;; nelisp-cc-variable-watchers.el --- Embed the shared watcher policy -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun nelisp-cc-variable-watchers-source (root)
  "Return the foundation's watcher forms, without loading unrelated shims."
  (let ((names '(emacs-parity-misc--variable-watchers
                 emacs-parity-misc--has-user-watchers
                 nelisp--defvaralias-notifications emacs-parity-misc--watching
                 emacs-parity-misc--inhibit-watchers emacs-parity-misc--remove-equal
                 emacs-parity-misc--watcher-flag emacs-parity-misc--watcher-symbol
                 emacs-parity-misc--notify add-variable-watcher
                 remove-variable-watcher get-variable-watchers))
        forms)
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "packages/nelisp-emacs-foundation/src/emacs-parity-misc.el" root))
      (emacs-lisp-mode)
      (check-parens)
      (goto-char (point-min))
      (while (progn (forward-comment (point-max)) (not (eobp)))
        (let* ((form (read (current-buffer)))
               (definition (if (eq (car-safe form) 'unless) (nth 2 form) form))
               (name (and (memq (car-safe definition) '(defvar defun))
                          (cadr definition))))
          (when (memq name names)
            (push form forms)
            (setq names (delq name names))))))
    (when names (error "Missing watcher providers: %S" names))
    (mapconcat (lambda (form) (concat (prin1-to-string form) "\n"))
               (nreverse forms) "")))

(provide 'nelisp-cc-variable-watchers)
;;; nelisp-cc-variable-watchers.el ends here
