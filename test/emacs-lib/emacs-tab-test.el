;;; emacs-tab-test.el --- tests for minimal tab-bar/tab-line -*- lexical-binding: t; -*-

;;; Code:

(require 'nemacs-test-paths
         (expand-file-name "nemacs-test-paths"
                           (file-name-directory (or load-file-name buffer-file-name))))

(require 'ert)

(load (nemacs-test-paths-source "tab-bar.el") nil t)
(load (nemacs-test-paths-source "tab-line.el") nil t)

(ert-deftest emacs-tab-test/tab-bar-basic-state ()
  (let ((tab-bar--tabs nil)
        (tab-bar--selected-index 0)
        (tab-bar-mode nil))
    (should (= 1 (length (tab-bar-tabs))))
    (should (= 0 (tab-bar-current-tab-index)))
    (tab-new)
    (should (= 2 (length (tab-bar-tabs))))
    (should (= 1 (tab-bar-current-tab-index)))
    (tab-previous)
    (should (= 0 (tab-bar-current-tab-index)))
    (tab-next)
    (should (= 1 (tab-bar-current-tab-index)))
    (tab-rename "work")
    (should (equal "work" (cdr (assq 'name (tab-bar-current-tab)))))
    (tab-close)
    (should (= 1 (length (tab-bar-tabs))))
    (should (= 0 (tab-bar-current-tab-index)))))

(ert-deftest emacs-tab-test/tab-bar-height-follows-mode ()
  (let ((tab-bar--tabs nil)
        (tab-bar--selected-index 0)
        (tab-bar-mode nil))
    (if (subrp (symbol-function 'tab-bar-height))
        (should (integerp (tab-bar-height)))
      (should (= 0 (tab-bar-height)))
      (tab-bar-mode 1)
      (should (= 1 (tab-bar-height)))
      (tab-bar-mode 0)
      (should (= 0 (tab-bar-height))))))

(ert-deftest emacs-tab-test/tab-line-height-follows-mode ()
  (let ((tab-line-mode nil)
        (global-tab-line-mode nil)
        (tab-line-format nil))
    (if (subrp (symbol-function 'window-tab-line-height))
        (should (integerp (window-tab-line-height)))
      (should (= 0 (window-tab-line-height)))
      (tab-line-mode 1)
      (should (= 1 (window-tab-line-height)))
      (tab-line-mode 0)
      (should (= 0 (window-tab-line-height)))
      (global-tab-line-mode 1)
      (should (= 1 (window-tab-line-height))))))

(ert-deftest emacs-tab-test/tab-line-buffer-names ()
  (with-temp-buffer
    (rename-buffer "tab-line-test-buffer" t)
    (should (equal "tab-line-test-buffer"
                   (tab-line-tab-name-buffer (current-buffer))))))

(provide 'emacs-tab-test)

;;; emacs-tab-test.el ends here
