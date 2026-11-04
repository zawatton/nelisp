;;; emacs-io-test.el --- ERT tests for emacs-io  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-io)

(ert-deftest emacs-io-test/provides-io-feature-set ()
  (dolist (feature '(emacs-io
                     emacs-sqlite
                     emacs-fileio-builtins
                     emacs-standalone
                     emacs-process-builtins))
    (should (featurep feature))))

(ert-deftest emacs-io-test/nelisp-emacs-uses-io-entry ()
  (require 'nelisp-emacs)
  (should (memq 'emacs-io nelisp-emacs-library-features))
  (should (featurep 'emacs-io)))

(provide 'emacs-io-test)

;;; emacs-io-test.el ends here
