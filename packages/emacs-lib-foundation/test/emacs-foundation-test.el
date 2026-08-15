;;; emacs-foundation-test.el --- ERT tests for emacs-foundation  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-foundation)

(ert-deftest emacs-foundation-test/provides-foundation-feature-set ()
  (dolist (feature '(emacs-foundation
                     emacs-fns
                     emacs-eval
                     emacs-list
                     emacs-hash
                     emacs-symbol
                     emacs-callproc
                     emacs-vars
                     emacs-backquote
                     emacs-error
                     emacs-string
                     cl-lib
                     emacs-stub
                     emacs-os-detect
                     emacs-easy-mmode
                     emacs-pcase
                     emacs-cl-macros
                     emacs-time
                     emacs-numeric
                     emacs-subr-extras
                     emacs-edebug-stubs))
    (should (featurep feature))))

(ert-deftest emacs-foundation-test/nelisp-emacs-uses-foundation-entry ()
  (require 'nelisp-emacs)
  (should (memq 'emacs-foundation nelisp-emacs-library-features))
  (should (featurep 'emacs-foundation)))

(provide 'emacs-foundation-test)

;;; emacs-foundation-test.el ends here
