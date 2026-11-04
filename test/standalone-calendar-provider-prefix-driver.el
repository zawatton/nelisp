;;; standalone-calendar-provider-prefix-driver.el --- genuine calendar load probe -*- lexical-binding: t; -*-

(let ((root (or (getenv "NELISP_REPO_ROOT") default-directory)))
  (require 'nelisp-substitute-command-keys)
  (require 'nelisp-display-batch)
  (unless (and (fboundp 'display-color-cells)
               (= (display-color-cells) 0))
    (error "batch display-color-cells provider was not installed"))
  (add-to-list 'load-path
               (expand-file-name "vendor/emacs-lisp/emacs-lisp" root))
  (require 'easymenu)
  (add-to-list 'load-path
               (expand-file-name "vendor/emacs-lisp/calendar" root))
  (require 'calendar)
  (princ "calendar-provider-prefix: PASS\n"))

(provide 'standalone-calendar-provider-prefix-driver)
;;; standalone-calendar-provider-prefix-driver.el ends here
