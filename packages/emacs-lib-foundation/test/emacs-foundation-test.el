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
  (let ((file (locate-library "nelisp-emacs")))
    (should file)
    (with-temp-buffer
      (insert-file-contents (if (string-match-p "\\.elc\\'" file)
                                (substring file 0 -1)
                              file))
      (should (search-forward "(require 'emacs-foundation)" nil t))
      (dolist (feature '(emacs-fns
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
        (goto-char (point-min))
        (should-not
         (search-forward (format "(require '%s)" feature) nil t))))))

(provide 'emacs-foundation-test)

;;; emacs-foundation-test.el ends here
