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
  (let ((file (locate-library "emacs-editing")))
    (should file)
    (with-temp-buffer
      (insert-file-contents (if (string-match-p "\\.elc\\'" file)
                                (substring file 0 -1)
                              file))
      (dolist (feature '(emacs-undo-builtins
                         emacs-edit-builtins))
        (goto-char (point-min))
        (should
         (search-forward (format "(require '%s)" feature) nil t))))))

(provide 'emacs-editing-test)

;;; emacs-editing-test.el ends here
