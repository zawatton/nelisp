;;; emacs-package-test.el --- tests for package.el activation -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(let ((src-dir (expand-file-name
                "../src"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (load (expand-file-name "package.el" src-dir) nil t))

(defconst emacs-package-test--fixture-elpa
  (expand-file-name
   "../apps/nemacs-next/fixtures/elpa"
   (file-name-directory (or load-file-name buffer-file-name)))
  "ELPA-style fixture package-user-dir.")

(defvar nemacs-next-elpa-activation-order nil
  "Activation order marker set by fixture autoload files.")

(defun emacs-package-test--clear-fixture-state ()
  "Remove fixture package functions/features between tests."
  (dolist (fn '(nemacs-next-elpa-base-value
                nemacs-next-elpa-dependent-value))
    (when (fboundp fn)
      (fmakunbound fn)))
  (dolist (feature '(nemacs-next-elpa-base
                     nemacs-next-elpa-dependent
                     nemacs-next-elpa-base-autoloads
                     nemacs-next-elpa-dependent-autoloads))
    (setq features (remove feature features))))

(defmacro emacs-package-test--with-fixture (&rest body)
  "Run BODY with package.el pointed at the ELPA fixture."
  (declare (indent 0))
  `(let ((package-user-dir emacs-package-test--fixture-elpa)
         (package-directory-list nil)
         (package-alist nil)
         (package-activated-list nil)
         (package--activated nil)
         (package--initialized nil)
         (package-archive-contents nil)
         (package-quickstart nil)
         (package-load-list '(all))
         (load-path load-path)
         (features (copy-sequence features)))
     (emacs-package-test--clear-fixture-state)
     (let ((nemacs-next-elpa-activation-order nil))
       ,@body)))

(ert-deftest emacs-package-test/load-all-descriptors-scans-elpa-layout ()
  (emacs-package-test--with-fixture
    (package-load-all-descriptors)
    (should (assq 'nemacs-next-elpa-base package-alist))
    (should (assq 'nemacs-next-elpa-dependent package-alist))
    (should (package-installed-p 'nemacs-next-elpa-base))
    (should (package-installed-p 'nemacs-next-elpa-dependent))))

(ert-deftest emacs-package-test/activate-all-loads-autoloads-and-load-path ()
  (emacs-package-test--with-fixture
    (package-activate-all)
    (let ((base-dir (expand-file-name "nemacs-next-elpa-base-1.0"
                                      package-user-dir))
          (dependent-dir (expand-file-name "nemacs-next-elpa-dependent-1.0"
                                           package-user-dir)))
      (should package--activated)
      (should (member base-dir load-path))
      (should (member dependent-dir load-path))
      (should (fboundp 'nemacs-next-elpa-base-value))
      (should (fboundp 'nemacs-next-elpa-dependent-value))
      (should-not (featurep 'nemacs-next-elpa-base))
      (should-not (featurep 'nemacs-next-elpa-dependent))
      (should (equal nemacs-next-elpa-activation-order
                     '(base dependent)))
      (should (eq (require 'nemacs-next-elpa-base)
                  'nemacs-next-elpa-base))
      (should (eq (require 'nemacs-next-elpa-dependent)
                  'nemacs-next-elpa-dependent))
      (should (equal (nemacs-next-elpa-dependent-value)
                     '(dependent-ready base-ready))))))

(ert-deftest emacs-package-test/initialize-can-skip-activation ()
  (emacs-package-test--with-fixture
    (package-initialize t)
    (should (assq 'nemacs-next-elpa-base package-alist))
    (should-not package--activated)
    (should-not (fboundp 'nemacs-next-elpa-base-value))))

(ert-deftest emacs-package-test/startup-activation-honors-enable-flag ()
  (emacs-package-test--with-fixture
    (let ((package-enable-at-startup nil))
      (require 'nemacs-loadup)
      (nemacs-activate-packages-at-startup)
      (should-not package--activated)
      (should-not (fboundp 'nemacs-next-elpa-base-value)))))

(ert-deftest emacs-package-test/archive-operations-signal-clearly ()
  (should-error (package-refresh-contents) :type 'user-error)
  (should-error (package-install 'demo) :type 'user-error)
  (should-error (package-install-selected-packages) :type 'user-error))

(provide 'emacs-package-test)

;;; emacs-package-test.el ends here
