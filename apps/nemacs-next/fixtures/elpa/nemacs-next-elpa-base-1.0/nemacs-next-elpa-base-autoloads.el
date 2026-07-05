;;; nemacs-next-elpa-base-autoloads.el --- fixture autoloads -*- lexical-binding: t; -*-

(add-to-list
 'load-path
 (or (and load-file-name
          (directory-file-name (file-name-directory load-file-name)))
     (car load-path)))

(setq nemacs-next-elpa-activation-order
      (append (and (boundp 'nemacs-next-elpa-activation-order)
                   nemacs-next-elpa-activation-order)
              '(base)))

;;;###autoload
(autoload 'nemacs-next-elpa-base-value "nemacs-next-elpa-base" nil t)

(provide 'nemacs-next-elpa-base-autoloads)

;;; nemacs-next-elpa-base-autoloads.el ends here
