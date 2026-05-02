;;; emacs-callproc-test.el --- Tests for emacs-callproc  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the `getenv' / `setenv' / `process-environment' polyfills.
;;
;; Under regular Emacs the real C-core implementations win, so most
;; assertions exercise behaviour that is identical between Emacs and
;; the polyfill.  The Phase 1.6 stub semantics (= empty
;; `process-environment' under NeLisp standalone, no real OS access)
;; are exercised in the dedicated `phase-1-6-stub-semantics' test by
;; binding `process-environment' explicitly.

;;; Code:

(require 'ert)
(require 'emacs-callproc)

;;;; --- getenv / setenv via process-environment ----------------------------

(ert-deftest emacs-callproc-test/getenv-finds-entry ()
  (let ((process-environment '("MY_TEST_VAR=hello" "OTHER=x")))
    (should (equal (getenv "MY_TEST_VAR") "hello"))))

(ert-deftest emacs-callproc-test/getenv-missing-returns-nil ()
  (let ((process-environment '("OTHER=x")))
    (should (null (getenv "MISSING")))))

(ert-deftest emacs-callproc-test/getenv-empty-environment-returns-nil ()
  (let ((process-environment nil))
    (should (null (getenv "ANYTHING")))))

(ert-deftest emacs-callproc-test/getenv-handles-equals-in-value ()
  (let ((process-environment '("KEY=val=with=equals")))
    (should (equal (getenv "KEY") "val=with=equals"))))


;;;; --- setenv -------------------------------------------------------------

(ert-deftest emacs-callproc-test/setenv-prepends-new-entry ()
  (let ((process-environment nil))
    (setenv "FOO" "bar")
    (should (equal (getenv "FOO") "bar"))))

(ert-deftest emacs-callproc-test/setenv-replaces-existing ()
  (let ((process-environment '("FOO=old")))
    (setenv "FOO" "new")
    (should (equal (getenv "FOO") "new"))
    ;; Only one entry remains.
    (should (= 1 (length process-environment)))))

(ert-deftest emacs-callproc-test/setenv-nil-value-removes-entry ()
  (let ((process-environment '("FOO=val" "BAR=baz")))
    (setenv "FOO" nil)
    (should (null (getenv "FOO")))
    (should (equal (getenv "BAR") "baz"))))


(provide 'emacs-callproc-test)

;;; emacs-callproc-test.el ends here
