;;; emacs-cc-emacs-module-1.el --- module C primitives -*- lexical-binding: t; -*-

;;; Code:

(unless (fboundp 'module-load)
  (defun module-load (file)
    "Load module FILE."
    (unless (stringp file)
      (signal 'wrong-type-argument (list 'stringp file)))
    (let* ((path (if (string-match "\0" file)
                     (substring file 0 (match-beginning 0))
                   file))
           (reason
            (cond
             ((string-empty-p path) nil)
             ((file-directory-p path) "cannot read file data: Is a directory")
             ((not (file-exists-p path))
              "cannot open shared object file: No such file or directory")
             (t "invalid ELF header"))))
      (if (null reason)
          (signal 'module-not-gpl-compatible (list file))
        (signal 'module-open-failed
                (list file (concat path ": " reason)))))))

(provide 'emacs-cc-emacs-module-1)
;;; emacs-cc-emacs-module-1.el ends here
