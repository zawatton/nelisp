;;; nemacs-next-elpa-dependent-autoloads.el --- fixture autoloads -*- lexical-binding: t; -*-

(add-to-list
 'load-path
 (or (and load-file-name
          (directory-file-name (file-name-directory load-file-name)))
     (car load-path)))

(setq nemacs-next-elpa-activation-order
      (append (and (boundp 'nemacs-next-elpa-activation-order)
                   nemacs-next-elpa-activation-order)
              '(dependent)))

;;;###autoload
(autoload 'nemacs-next-elpa-dependent-value
  "nemacs-next-elpa-dependent" nil t)

(provide 'nemacs-next-elpa-dependent-autoloads)

;;; nemacs-next-elpa-dependent-autoloads.el ends here
