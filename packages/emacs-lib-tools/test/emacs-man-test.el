;;; emacs-man-test.el --- ERT for emacs-man  -*- lexical-binding: t; -*-

;;; Commentary:

;; man viewer tests.  Page fetch drives the real `man' program (skipped when
;; absent); the nonblank predicate + install are pure units.

;;; Code:

(require 'ert)
(require 'emacs-man)

(ert-deftest emacs-man-test/nonblank-p ()
  (should (emacs-man--nonblank-p "x"))
  (should (emacs-man--nonblank-p "  hi  "))
  (should-not (emacs-man--nonblank-p "   \n\t"))
  (should-not (emacs-man--nonblank-p ""))
  (should-not (emacs-man--nonblank-p nil)))

(ert-deftest emacs-man-test/displays-page ()
  (skip-unless (executable-find "man"))
  (skip-unless (executable-find "col"))
  (let ((buf (emacs-man "true")))       ; coreutils `true' is a stable page
    (unwind-protect
        (with-current-buffer buf
          (should (eq major-mode 'Man-mode))
          (should (string-match-p "NAME" (buffer-string)))
          (should (string-match-p "true" (buffer-string)))
          (should (equal "*Man true*" (buffer-name))))
      (kill-buffer buf))))

(ert-deftest emacs-man-test/not-found-signals ()
  (skip-unless (executable-find "man"))
  (should-error (emacs-man "no-such-manual-page-xyzzy-12345")))

(ert-deftest emacs-man-test/install-binds-man-and-woman ()
  (emacs-man-install)
  (should (fboundp 'man))
  (should (fboundp 'woman))
  ;; `defalias' to #'emacs-man records the alias as the symbol `emacs-man'.
  (should (eq (symbol-function 'man) 'emacs-man))
  (should (eq (symbol-function 'woman) 'emacs-man)))

(provide 'emacs-man-test)

;;; emacs-man-test.el ends here
