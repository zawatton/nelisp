;;; standalone-regexp-syncache-check.el --- mutation and cache contract -*- lexical-binding: nil; -*-
;; The sole --load driver prints the final nil; the .expected includes it.
(load "test/fixtures/regex-syncache.el" nil t)
(load "test/fixtures/regex-syncache-identity.el" nil t)
(load "test/fixtures/regex-syncache-api.el" nil t)
(load "test/fixtures/regex-syntax-run.el" nil t)
nil
