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

(ert-deftest emacs-callproc-test/getenv-falls-through-to-nelisp-sys ()
  (let ((process-environment nil)
        (had (fboundp 'nelisp-sys-getenv))
        (before (and (fboundp 'nelisp-sys-getenv)
                     (symbol-function 'nelisp-sys-getenv))))
    (unwind-protect
        (progn
          (fset 'nelisp-sys-getenv
                (lambda (variable)
                  (and (equal variable "FROM_SYS") "sys-value")))
          (should (equal (emacs-callproc-getenv "FROM_SYS") "sys-value"))
          (should-not (emacs-callproc-getenv "MISSING_SYS")))
      (if had
          (fset 'nelisp-sys-getenv before)
        (fmakunbound 'nelisp-sys-getenv)))))

(ert-deftest emacs-callproc-test/process-environment-overrides-sys-getenv ()
  (let ((process-environment '("FROM_SYS=overlay-value"))
        (had (fboundp 'nelisp-sys-getenv))
        (before (and (fboundp 'nelisp-sys-getenv)
                     (symbol-function 'nelisp-sys-getenv))))
    (unwind-protect
        (progn
          (fset 'nelisp-sys-getenv
                (lambda (_variable) "runtime-value"))
          (should (equal (emacs-callproc-getenv "FROM_SYS")
                         "overlay-value")))
      (if had
          (fset 'nelisp-sys-getenv before)
        (fmakunbound 'nelisp-sys-getenv)))))

(ert-deftest emacs-callproc-test/sys-getenv-recursion-guard ()
  (let ((process-environment nil)
        (had (fboundp 'nelisp-sys-getenv))
        (before (and (fboundp 'nelisp-sys-getenv)
                     (symbol-function 'nelisp-sys-getenv))))
    (unwind-protect
        (progn
          (fset 'nelisp-sys-getenv
                (lambda (variable)
                  (getenv variable)))
          (should-not (emacs-callproc-getenv "RECURSIVE_SYS")))
      (if had
          (fset 'nelisp-sys-getenv before)
        (fmakunbound 'nelisp-sys-getenv)))))

(ert-deftest emacs-callproc-test/getenv-falls-through-to-nl-syscall ()
  (let ((process-environment nil)
        (had-sys (fboundp 'nelisp-sys-getenv))
        (before-sys (and (fboundp 'nelisp-sys-getenv)
                         (symbol-function 'nelisp-sys-getenv)))
        (had-nl (fboundp 'nl-syscall-getenv))
        (before-nl (and (fboundp 'nl-syscall-getenv)
                        (symbol-function 'nl-syscall-getenv))))
    (unwind-protect
        (progn
          (when had-sys
            (fmakunbound 'nelisp-sys-getenv))
          (fset 'nl-syscall-getenv
                (lambda (variable)
                  (and (equal variable "FROM_NL") "nl-value")))
          (should (equal (emacs-callproc-getenv "FROM_NL") "nl-value")))
      (if had-sys
          (fset 'nelisp-sys-getenv before-sys)
        (fmakunbound 'nelisp-sys-getenv))
      (if had-nl
          (fset 'nl-syscall-getenv before-nl)
        (fmakunbound 'nl-syscall-getenv)))))


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


;;;; --- standalone-reader environment seeding (/proc/self/environ) ---------

(ert-deftest emacs-callproc-test/split-on-nul-drops-empties ()
  (should (equal (emacs-callproc--split-on-nul "PATH=/bin\0HOME=/root\0")
                 '("PATH=/bin" "HOME=/root")))
  (should (equal (emacs-callproc--split-on-nul "A=1") '("A=1")))
  (should (null (emacs-callproc--split-on-nul "")))
  (should (null (emacs-callproc--split-on-nul "\0\0"))))

(ert-deftest emacs-callproc-test/read-proc-environ-uses-rdf ()
  "`emacs-callproc--read-proc-environ' parses `rdf' of /proc/self/environ."
  (let ((seen nil))
    (cl-letf (((symbol-function 'rdf)
               (lambda (file)
                 (setq seen file)
                 "PATH=/usr/bin:/bin\0HOME=/home/x\0")))
      (should (equal (emacs-callproc--read-proc-environ)
                     '("PATH=/usr/bin:/bin" "HOME=/home/x")))
      (should (equal seen emacs-callproc--proc-self-environ)))))

(ert-deftest emacs-callproc-test/read-proc-environ-nil-without-rdf ()
  "Without `rdf' (= host Emacs) the reader yields nil rather than erroring."
  (let ((had (fboundp 'rdf))
        (before (and (fboundp 'rdf) (symbol-function 'rdf))))
    (unwind-protect
        (progn
          (when had (fmakunbound 'rdf))
          (should (null (emacs-callproc--read-proc-environ))))
      (when had (fset 'rdf before)))))

(ert-deftest emacs-callproc-test/populate-seeds-only-when-empty ()
  "Populate seeds an empty `process-environment' and leaves a full one alone."
  (cl-letf (((symbol-function 'rdf)
             (lambda (_file) "PATH=/seeded\0")))
    ;; Empty -> seeded from rdf.
    (let ((process-environment nil))
      (emacs-callproc-populate-process-environment)
      (should (equal process-environment '("PATH=/seeded")))
      (should (equal (getenv "PATH") "/seeded")))
    ;; Already populated -> untouched.
    (let ((process-environment '("PATH=/real")))
      (emacs-callproc-populate-process-environment)
      (should (equal process-environment '("PATH=/real"))))))

(provide 'emacs-callproc-test)

;;; emacs-callproc-test.el ends here
