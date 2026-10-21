;;; standalone-syntax-rt-semantics-check.el --- Doc 225 GNU 31.1 fixture as a standalone check -*- lexical-binding: t; -*-

;; Runs test/fixtures/syntax-rt-semantics.el; its output must equal
;; standalone-syntax-rt-semantics-check.expected, GNU 31.1's own output for the
;; same file (test/fixtures/syntax-rt-semantics.gnu-31.1.expected).
(load (expand-file-name "fixtures/syntax-rt-semantics.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; standalone-syntax-rt-semantics-check.el ends here
