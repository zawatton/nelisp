;;; emacs-buffer-core-test.el --- ERT tests for emacs-buffer-core  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-buffer-core)

(ert-deftest emacs-buffer-core-test/provides-buffer-feature-set ()
  (dolist (feature '(emacs-buffer-core
                     emacs-buffer-builtins
                     emacs-search-builtins
                     emacs-line-builtins))
    (should (featurep feature))))

(ert-deftest emacs-buffer-core-test/nelisp-emacs-uses-buffer-core-entry ()
  (require 'nelisp-emacs)
  (should (memq 'emacs-buffer-core nelisp-emacs-library-features))
  (should (featurep 'emacs-buffer-core)))

(provide 'emacs-buffer-core-test)

;;; emacs-buffer-core-test.el ends here
