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

(ert-deftest emacs-package-test/activate-loads-local-package ()
  (let* ((dir (make-temp-file "nemacs-pkg-" t))
         (file (expand-file-name "demopkg.el" dir)))
    (unwind-protect
        (let* ((desc (package-desc-create :name 'demopkg :dir dir))
               (package-alist `((demopkg . (,desc))))
               (package-activated-list nil)
               (load-path load-path)
               (features (copy-sequence features)))
          (with-temp-file file
            (insert "(provide 'demopkg)\n(defvar demopkg-test-loaded t)\n"))
          (should (package-activate 'demopkg))
          ;; the package's feature is loaded and its dir is on load-path
          (should (featurep 'demopkg))
          (should (member dir load-path))
          (should (memq 'demopkg package-activated-list)))
      (when (file-directory-p dir) (delete-directory dir t))
      (setq features (remove 'demopkg features)))))

(ert-deftest emacs-package-test/activate-all-activates-registry ()
  (let* ((d1 (package-desc-create :name 'pkga))
         (d2 (package-desc-create :name 'pkgb))
         (package-alist `((pkga . (,d1)) (pkgb . (,d2))))
         (package-activated-list nil)
         (features (copy-sequence features)))
    (package-activate-all)
    (should (memq 'pkga package-activated-list))
    (should (memq 'pkgb package-activated-list))))

(ert-deftest emacs-package-test/initialize-activates-unless-no-activate ()
  (let* ((d1 (package-desc-create :name 'pkgc))
         (package-alist `((pkgc . (,d1))))
         (package-activated-list nil)
         (features (copy-sequence features)))
    (package-initialize)
    (should (memq 'pkgc package-activated-list))
    (setq package-activated-list nil)
    (package-initialize t)
    (should-not (memq 'pkgc package-activated-list))))

(provide 'emacs-package-test)

;;; emacs-package-test.el ends here
