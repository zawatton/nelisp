;;; emacs-core-test.el --- ERT tests for emacs-core  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-core)
(require 'nelisp-emacs)

(ert-deftest emacs-core-test/provides-core-feature-set ()
  (dolist (feature '(emacs-core
                     emacs-buffer-core
                     emacs-editing
                     emacs-minibuffer-builtins
                     emacs-command-loop-builtins
                     emacs-keymap-builtins
                     emacs-frame-builtins
                     emacs-window-builtins
                     emacs-faces-builtins
                     emacs-mode-builtins
                     emacs-info
                     emacs-help))
    (should (featurep feature))))

(ert-deftest emacs-core-test/nelisp-emacs-uses-core-entry ()
  (should (memq 'emacs-core nelisp-emacs-library-features))
  (let ((entry (assq 'core nelisp-emacs-library-packages)))
    (should entry)
    (should (eq (plist-get (cdr entry) :feature) 'emacs-core)))
  (let ((file (locate-library "nelisp-emacs")))
    (should file)
    (with-temp-buffer
      (insert-file-contents (if (string-match-p "\\.elc\\'" file)
                                (substring file 0 -1)
                              file))
      (dolist (feature '(emacs-info
                         emacs-help
                         emacs-editing
                         emacs-minibuffer-builtins
                         emacs-command-loop-builtins
                         emacs-keymap-builtins
                         emacs-frame-builtins
                         emacs-window-builtins
                         emacs-faces-builtins
                         emacs-mode-builtins))
        (goto-char (point-min))
        (should-not
         (search-forward (format "(require '%s)" feature) nil t))))))

(provide 'emacs-core-test)

;;; emacs-core-test.el ends here
