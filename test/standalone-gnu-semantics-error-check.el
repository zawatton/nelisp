;;; standalone-gnu-semantics-error-check.el --- GNU error messages -*- lexical-binding: t; -*-
;; The ledger separately checks locale-derived defaults in C and C.UTF-8.
(let ((internal--text-quoting-flag t))
  (load (expand-file-name "fixtures/gnu-semantics-error.el"
                          (file-name-directory load-file-name)) nil t))
