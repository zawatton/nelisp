;;; emacs-cc-emacs-1.el --- emacs.c C primitive replacements  -*- lexical-binding: t; -*-

;;; Code:

(unless (fboundp 'daemon-initialized)
  (defun daemon-initialized ()
    "Mark the Emacs daemon as being initialized.
This finishes the daemonization process by doing the other half of detaching
from the parent process and its tty file descriptors."
    (unless (and (fboundp 'daemonp) (daemonp))
      (error "This function can only be called if emacs is run as a daemon"))
    (setq daemon-initialized t)
    nil))

(provide 'emacs-cc-emacs-1)

;;; emacs-cc-emacs-1.el ends here
