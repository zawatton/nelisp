;;; nelisp-regexp-syncache-test.el --- syntax-cache regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-stdlib-regexp)
(defvar regex-syncache-matcher)
(defvar regex-syncache-data-reader)
(defvar regex-syncache-output)

(defconst nelisp-regexp-syncache-test-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest nelisp-regexp-syncache-gnu-fixture ()
  "Actual matcher results equal the pinned GNU 31.1 output."
  (let ((default-directory nelisp-regexp-syncache-test-root))
    (should
     (equal
      (with-temp-buffer
        (let ((regex-syncache-matcher 'nlre-string-match)
              (regex-syncache-data-reader 'nlre-match-data)
              (regex-syncache-output (current-buffer))
              (nlre--last-buffer nil)
              (nlre--last-caps nil))
          (load (expand-file-name "test/fixtures/regex-syncache.el") nil t))
        (buffer-string))
      (with-temp-buffer
        (insert-file-contents "test/fixtures/regex-syncache.gnu-31.1.expected")
        (buffer-string))))))

(ert-deftest nelisp-regexp-syncache-table-identity ()
  "Equal table contents do not imply table identity."
  (let ((default-directory nelisp-regexp-syncache-test-root))
    (with-output-to-string
      (load (expand-file-name "test/fixtures/regex-syncache-identity.el") nil t))))

(ert-deftest nelisp-regexp-syncache-public-api ()
  "Clear is repeatable, preserves plain entries and observes mutation."
  (let ((default-directory nelisp-regexp-syncache-test-root))
    (with-output-to-string
      (load (expand-file-name "test/fixtures/regex-syncache-api.el") nil t))))

(ert-deftest nelisp-regexp-syncache-public-api-documentation ()
  "The direct-aset limitation is part of the public contract."
  (let ((doc (documentation 'nlre-clear-syntax-cache)))
    (should (stringp doc))
    (should (string-match-p "modify-syntax-entry" doc))
    (should (string-match-p "aset" doc))))

(provide 'nelisp-regexp-syncache-test)
