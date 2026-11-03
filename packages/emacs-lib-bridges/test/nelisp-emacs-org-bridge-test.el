;;; nelisp-emacs-org-bridge-test.el --- focused tests for Org bridge -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused host-side tests for the standalone Org bridge loader.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-emacs-org-bridge)

(ert-deftest nelisp-emacs-org-bridge-test/repo-root-falls-back-without-file-truename ()
  "Repo-root resolution should not require `file-truename' at bridge entry."
  (let ((nelisp-emacs-org-bridge-repo-root nil)
        (orig-fboundp (symbol-function 'fboundp))
        (load-file-name "/tmp/demo/src/nelisp-emacs-org-bridge.el"))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (symbol)
                 (and (not (eq symbol 'file-truename))
                      (funcall orig-fboundp symbol)))))
      (should (equal (nelisp-emacs-org-bridge--repo-root)
                     "/tmp/demo")))))

(ert-deftest nelisp-emacs-org-bridge-test/repo-root-explicit-still-normalizes ()
  "Explicit roots should still normalize through the helper."
  (let ((nelisp-emacs-org-bridge-repo-root "/tmp/demo/../demo"))
    (cl-letf (((symbol-function 'file-truename)
               (lambda (path)
                 (concat "TRUENAME:" path))))
      (should (equal (nelisp-emacs-org-bridge--repo-root)
                     "TRUENAME:/tmp/demo/../demo")))))

(provide 'nelisp-emacs-org-bridge-test)

;;; nelisp-emacs-org-bridge-test.el ends here
