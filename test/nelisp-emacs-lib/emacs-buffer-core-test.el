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
  (let ((file (locate-library "nelisp-emacs")))
    (should file)
    (with-temp-buffer
      (insert-file-contents (if (string-match-p "\\.elc\\'" file)
                                (substring file 0 -1)
                              file))
      (should (search-forward "(require 'emacs-buffer-core)" nil t))
      (dolist (feature '(emacs-buffer-builtins
                         emacs-search-builtins
                         emacs-line-builtins))
        (goto-char (point-min))
        (should-not
         (search-forward (format "(require '%s)" feature) nil t))))))

(provide 'emacs-buffer-core-test)

;;; emacs-buffer-core-test.el ends here
