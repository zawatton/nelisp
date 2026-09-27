;;; emacs-editing-test.el --- ERT tests for emacs-editing  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-buffer-core)
(require 'emacs-editing)

(ert-deftest emacs-editing-test/provides-editing-feature-set ()
  (dolist (feature '(emacs-editing
                     emacs-undo-builtins
                     emacs-edit-builtins))
    (should (featurep feature))))

(ert-deftest emacs-editing-test/loader-groups-editing-builtins ()
  (require 'emacs-editing)
  (dolist (feature '(emacs-undo-builtins
                     emacs-edit-builtins))
    (should (memq feature emacs-editing-features))
    (should (featurep feature))))

(provide 'emacs-editing-test)

;;; emacs-editing-test.el ends here
