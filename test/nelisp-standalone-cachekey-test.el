;;; nelisp-standalone-cachekey-test.el --- incremental build key tests -*- lexical-binding: t; -*-

;; Unit-level tests for the standalone unit-cache key, entry verification
;; and reproducible-identity helpers.  No build is run.
;; Run: emacs --batch -Q -L scripts -L lisp -L src -l test/nelisp-standalone-cachekey-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'nelisp-version)

(defconst nelisp-cachekey-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defmacro nelisp-cachekey-test--with-env (bindings &rest body)
  "Run BODY with `process-environment' extended by BINDINGS (VAR . VALUE or VAR)."
  `(let ((process-environment
          (append (mapcar (lambda (b) (if (consp b) (format "%s=%s" (car b) (cdr b)) b))
                          ',bindings)
                  process-environment)))
     ,@body))

(ert-deftest nelisp-cachekey-version-pinned-ignores-git ()
  (nelisp-cachekey-test--with-env
      ((NELISP_REPRODUCIBLE_BUILD . 1) (NELISP_VERSION . "pin-1")
       (NELISP_SOURCE_REVISION . "0123456789abcdef0123456789abcdef01234567"))
    (should (equal (nelisp-build-version nelisp-cachekey-test--root) "pin-1"))
    (should (equal (nelisp-build-source-revision nelisp-cachekey-test--root)
                   "0123456789abcdef0123456789abcdef01234567"))))

(ert-deftest nelisp-cachekey-version-pinned-no-describe ()
  "Without NELISP_VERSION the pinned build still never calls git describe."
  (nelisp-cachekey-test--with-env
      ((NELISP_REPRODUCIBLE_BUILD . 1) "NELISP_VERSION="
       (NELISP_SOURCE_REVISION . "0123456789abcdef0123456789abcdef01234567"))
    (should (equal (nelisp-build-version nelisp-cachekey-test--root)
                   "unknown-0123456789abcdef0123456789abcdef01234567"))))

(ert-deftest nelisp-cachekey-version-unpinned-negative-control ()
  "Negative control: without the flag the source revision is the git HEAD."
  (skip-unless (executable-find "git"))
  (nelisp-cachekey-test--with-env
      ("NELISP_REPRODUCIBLE_BUILD=" (NELISP_SOURCE_REVISION . "0123456789abcdef0123456789abcdef01234567"))
    (let ((rev (nelisp-build-source-revision nelisp-cachekey-test--root)))
      (skip-unless rev)
      (should-not (equal rev "0123456789abcdef0123456789abcdef01234567")))))

(provide 'nelisp-standalone-cachekey-test)
;;; nelisp-standalone-cachekey-test.el ends here
