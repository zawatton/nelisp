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
  (let ((file (locate-library "nelisp-emacs")))
    (should file)
    (with-temp-buffer
      (insert-file-contents (if (string-match-p "\\.elc\\'" file)
                                (substring file 0 -1)
                              file))
      (should (search-forward "(require 'emacs-io)" nil t))
      (dolist (feature '(emacs-sqlite
                         emacs-fileio-builtins
                         emacs-standalone
                         emacs-process-builtins))
        (goto-char (point-min))
        (should-not
         (search-forward (format "(require '%s)" feature) nil t))))))

(provide 'emacs-io-test)

;;; emacs-io-test.el ends here
