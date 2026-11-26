;;; emacs-easy-mmode.el --- GNU easy-mmode feature dependency  -*- lexical-binding: t; -*-
;; The GNU macros are loaded during standalone bootstrap and retain ownership.
;; GNU subr.el initializes this before the mode macros are loaded.
(defvar global-minor-modes nil
  "A list of the currently enabled global minor modes.
This is a list of symbols.")

(require 'easy-mmode)

(provide 'emacs-easy-mmode)
;;; emacs-easy-mmode.el ends here
