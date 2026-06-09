;;; emacs-package-test.el --- tests for minimal package facade -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(let ((src-dir (expand-file-name
                "../src"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (load (expand-file-name "package.el" src-dir) nil t))

(ert-deftest emacs-package-test/installed-p-uses-registry ()
  (let* ((desc (package-desc-create :name 'demo
                                    :version '(1 0)
                                    :summary "demo"))
         (package-alist `((demo . (,desc))))
         (package-activated-list nil))
    (should (package-desc-p desc))
    (should (package-installed-p 'demo))
    (should (package-installed-p "demo"))
    (should-not (package-installed-p 'missing))))

(ert-deftest emacs-package-test/installed-p-accepts-loaded-feature ()
  (let ((package-alist nil))
    (provide 'emacs-package-test-feature)
    (unwind-protect
        (should (package-installed-p 'emacs-package-test-feature))
      (setq features (remove 'emacs-package-test-feature features)))))

(ert-deftest emacs-package-test/activate-records-installed-package ()
  (let* ((desc (package-desc-create :name 'demo))
         (package-alist `((demo . (,desc))))
         (package-activated-list nil))
    (should (package-activate 'demo))
    (should (memq 'demo package-activated-list))
    (should-not (package-activate 'missing))))

(ert-deftest emacs-package-test/archive-operations-signal-clearly ()
  (should-error (package-refresh-contents) :type 'user-error)
  (should-error (package-install 'demo) :type 'user-error))

(provide 'emacs-package-test)

;;; emacs-package-test.el ends here
