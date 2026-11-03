;;; emacs-cc-features-1.el --- Register the Emacs startup feature  -*- lexical-binding: t; -*-

;;; Commentary:

;; GNU Emacs provides `emacs' at startup.  Register it for standalone
;; runtimes whose initial feature registry does not yet contain it.

;;; Code:

(unless (featurep 'emacs)
  (provide 'emacs))

;;; emacs-cc-features-1.el ends here
