;;; nelisp-menu-bar-final-items-provider-test.el --- pinned preloaded var -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path (expand-file-name "scripts" default-directory))
(require 'nelisp-standalone-build)

(ert-deftest nelisp-menu-bar-final-items/exact-pinned-assignment ()
  (let ((source (nelisp-standalone--vendor-menu-bar-final-items-source)))
    (should (equal (string-trim source)
                   "(setq menu-bar-final-items '(help-menu))"))))

(ert-deftest nelisp-menu-bar-final-items/staged-before-comint-prerequisites ()
  (let* ((source (nelisp-standalone--load-path-src))
         (assignment (string-match "(setq menu-bar-final-items" source))
         (passwords (string-match "password-word-equivalents" source)))
    (should assignment)
    (should passwords)
    (should (< assignment passwords))))

(provide 'nelisp-menu-bar-final-items-provider-test)

;;; nelisp-menu-bar-final-items-provider-test.el ends here
